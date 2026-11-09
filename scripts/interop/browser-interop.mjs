#!/usr/bin/env node
/**
 * T3b · A2: interoperabilidad TLS del canal local en Firefox, Chromium y Edge.
 *
 * Abre un espejo TLS local (Node https) con el certificado sintetico de prueba y
 * EXACTAMENTE la lista de suites que el canal de Autofirma habilita tras T3, y comprueba
 * que los tres navegadores completan el handshake y reciben respuesta.
 *
 * Motivo del espejo: el canal real responde solo a mensajes del protocolo (marco con
 * idSession y fin de mensaje) y su confirmacion de firma exige interaccion en la GUI. La
 * compatibilidad que T3 puede romper es la negociacion TLS, y eso es lo que se prueba
 * aqui; el canal real se comprueba con openssl en A1 y la firma con certificado real en B.
 *
 * ALCANCE: la firma completa desde el navegador (dialogo de confirmacion de la GUI) sigue
 * siendo PRUEBA MANUAL antes de publicar cada version. Este script no la automatiza.
 *
 * Variables:
 *   INTEROP_CERTS_DIR  directorio con rsa.crt/rsa.key (por defecto /tmp/afirma-interop-certs)
 *   INTEROP_OUT        directorio de artefactos (por defecto interop-local)
 *   INTEROP_BROWSERS   lista separada por comas (por defecto firefox,chromium,msedge)
 *   INTEROP_TARGET     si vale "channel", apunta al canal real (INTEROP_PORT) en vez del espejo
 */
import { chromium, firefox } from 'playwright';
import http from 'node:http';
import https from 'node:https';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';

const CERTS = process.env.INTEROP_CERTS_DIR || '/tmp/afirma-interop-certs';
const OUT = process.env.INTEROP_OUT || 'interop-local';
const MODE = process.env.INTEROP_TARGET || 'mirror';
const CHANNEL_PORT = process.env.INTEROP_PORT || '8881';
const WANTED = (process.env.INTEROP_BROWSERS || 'firefox,chromium,msedge').split(',').map((s) => s.trim());

// Las 10 suites que habilita ServiceInvocationManager tras T3.
const SUITES = [
  'TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384',
  'TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256',
  'TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384',
  'TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256',
  'TLS_DHE_RSA_WITH_AES_256_GCM_SHA384',
  'TLS_DHE_RSA_WITH_AES_128_GCM_SHA256',
  'TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384',
  'TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384',
  'TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256',
  'TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256',
].join(':');

const PAGE = `<!doctype html><meta charset="utf-8"><title>interop</title>
<script src="autoscript.js"></script>
<body><pre id="r">pendiente</pre></body>`;

async function startMirror() {
  const negotiated = [];
  const cert = await readFile(path.join(CERTS, 'rsa.crt'));
  const key = await readFile(path.join(CERTS, 'rsa.key'));
  // INTEROP_SELFTEST=1 configura el espejo en modo regresion (TLS 1.0 y suites antiguas):
  // los navegadores DEBEN fallar. Sirve para demostrar que la prueba no es trivial.
  const selfTest = process.env.INTEROP_SELFTEST === '1';
  const tlsOptions = selfTest
    ? { cert, key, minVersion: 'TLSv1', maxVersion: 'TLSv1.1', ciphers: 'AES256-SHA' }
    : { cert, key, minVersion: 'TLSv1.2', maxVersion: 'TLSv1.2', ciphers: SUITES };
  const server = https.createServer(tlsOptions,
    (req, res) => { res.writeHead(200, { 'content-type': 'text/plain' }); res.end('interop-mirror-ok'); });
  server.on('secureConnection', (socket) => {
    negotiated.push({ protocol: socket.getProtocol(), cipher: socket.getCipher()?.name ?? null });
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  const { port } = server.address();
  return { server, url: `https://127.0.0.1:${port}/`, negotiated };
}

// Pagina de prueba servida desde el mismo espacio de direcciones (127.0.0.1) para que
// la politica de acceso a red privada de Chromium no bloquee la peticion al canal local.
async function startPageServer() {
  const server = http.createServer((req, res) => {
    if ((req.url || '').startsWith('/autoscript.js')) {
      res.writeHead(200, { 'content-type': 'text/javascript' });
      res.end('/* autoscript.js */');
      return;
    }
    res.writeHead(200, { 'content-type': 'text/html' });
    res.end(PAGE);
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  return { server, url: `http://127.0.0.1:${server.address().port}/` };
}

async function probe(launcher, channel, target, pageUrl) {
  const browser = await launcher.launch({ channel });
  try {
    const context = await browser.newContext({ ignoreHTTPSErrors: true });
    const page = await context.newPage();
    await page.goto(pageUrl);
    const res = await page.evaluate(async (t) => {
      try {
        const r = await fetch(t, { method: 'POST', body: 'interop-probe', mode: 'no-cors' });
        return { ok: true, status: r.status, type: r.type };
      } catch (e) {
        return { ok: false, error: String(e) };
      }
    }, target);
    await context.close();
    return res;
  } finally {
    await browser.close();
  }
}

async function main() {
  await mkdir(OUT, { recursive: true });
  let mirror = null;
  let target;
  if (MODE === 'channel') {
    target = `https://127.0.0.1:${CHANNEL_PORT}/`;
  } else {
    mirror = await startMirror();
    target = mirror.url;
  }

  const pageServer = await startPageServer();
  const summary = [];
  let failures = 0;
  for (const name of WANTED) {
    const spec = name === 'firefox'
      ? { launcher: firefox, channel: undefined }
      : { launcher: chromium, channel: name === 'msedge' ? 'msedge' : undefined };
    try {
      const res = await probe(spec.launcher, spec.channel, target, pageServer.url);
      const last = mirror ? mirror.negotiated[mirror.negotiated.length - 1] ?? {} : {};
      const entry = {
        browser: name,
        handshake: res.ok,
        httpStatus: res.status ?? null,
        error: res.error ?? null,
        target,
        protocol: last.protocol ?? null,
        cipher: last.cipher ?? null,
      };
      if (!res.ok) failures += 1;
      summary.push(entry);
      console.log(`${res.ok ? 'OK' : 'FAIL'}: ${name} ${res.error ?? 'HTTP ' + res.status}` +
        (entry.protocol ? ` [${entry.protocol} ${entry.cipher}]` : ''));
    } catch (e) {
      failures += 1;
      summary.push({ browser: name, handshake: false, error: String(e), target });
      console.log(`FAIL: ${name} no se pudo lanzar: ${e}`);
    }
  }

  pageServer.server.close();
  if (mirror) mirror.server.close();
  await writeFile(path.join(OUT, 'browsers.json'), JSON.stringify(summary, null, 2));
  if (process.env.INTEROP_SELFTEST === '1') {
    // En modo regresion el resultado correcto es que TODOS fallen.
    if (failures !== WANTED.length) {
      console.error('interop selftest: algun navegador acepto TLS obsoleto');
      process.exit(1);
    }
    console.log(`interop selftest: OK (${WANTED.join(', ')} rechazan TLS obsoleto)`);
    return;
  }

  if (failures > 0) {
    console.error(`interop navegadores: ${failures} fallo(s)`);
    process.exit(1);
  }
  console.log(`interop navegadores: OK (${WANTED.join(', ')}; modo ${MODE})`);
}

await main();
