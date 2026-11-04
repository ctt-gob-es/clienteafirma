package es.gob.afirma.standalone.protocol;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.Timer;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;

/**
 * Reproduccion minima de H6 contra el canal local real: un canal sin idsession y una
 * peticion con un Origin desconocido. Se compila igual contra la base y contra la rama
 * de T8; solo usa APIs presentes en ambas.
 */
public final class LocalChannelRepro {

    private LocalChannelRepro() { }

    /** Envia una peticion HTTP en crudo y devuelve la respuesta. */
    private static String sendRaw(final int port, final String request) throws Exception {
        // nosemgrep: java.lang.security.audit.crypto.unencrypted-socket.unencrypted-socket -- canal local loopback bajo prueba, test
        try (Socket socket = new Socket(InetAddress.getByName("127.0.0.1"), port)) {
            socket.setSoTimeout(10000);
            final OutputStream out = socket.getOutputStream();
            out.write(request.getBytes(StandardCharsets.UTF_8));
            out.flush();
            final InputStream in = socket.getInputStream();
            final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            final byte[] chunk = new byte[4096];
            int read;
            while ((read = in.read(chunk)) != -1) {
                buffer.write(chunk, 0, read);
            }
            return new String(buffer.toByteArray(), StandardCharsets.UTF_8);
        }
    }

    private static String header(final String response, final String name) {
        for (final String line : response.split("\r?\n")) {
            final int sep = line.indexOf(':');
            if (sep > 0 && line.substring(0, sep).trim().equalsIgnoreCase(name)) {
                return line.substring(sep + 1).trim();
            }
        }
        return "<ausente>";
    }

    private static String body(final String response) {
        final int idx = response.indexOf("\n\n");
        if (idx < 0) {
            return "<sin cuerpo>";
        }
        try {
            return new String(Base64.decode(response.substring(idx + 2).trim(), true), StandardCharsets.UTF_8);
        }
        catch (final Exception e) {
            return "<no decodificable>";
        }
    }

    public static void main(final String[] args) throws Exception {
        // A) Canal HTTP sin idsession y con Origin desconocido
        // nosemgrep: java.lang.security.audit.crypto.unencrypted-socket.unencrypted-socket -- canal local loopback bajo prueba, test
        final ServerSocket serverSocket = new ServerSocket(0, 5, InetAddress.getByName("127.0.0.1"));
        final Timer timer = new Timer(600000, evt -> { });
        timer.setRepeats(false);
        final Thread acceptor = new Thread(() -> {
            while (true) {
                try {
                    new CommandProcessorThread(serverSocket.accept(), timer, null,
                            ProtocolVersion.getInstance(ProtocolVersion.VERSION_4)).start();
                }
                catch (final Exception e) {
                    return;
                }
            }
        });
        acceptor.setDaemon(true);
        acceptor.start();

        final StringBuilder sb = new StringBuilder();
        sb.append("GET / HTTP/1.1\r\nHost: 127.0.0.1\r\n");
        sb.append("Origin: https://evil.example\r\n\r\n");
        sb.append("echo=");
        for (int i = 0; i < 60; i++) {
            sb.append('A');
        }
        sb.append("@EOF");

        final String httpResponse = sendRaw(serverSocket.getLocalPort(), sb.toString());
        System.out.println("A) HTTP sin idsession -> " + ("OK".equals(body(httpResponse)) ? "ACEPTADA (brecha)" : "RECHAZADA"));
        System.out.println("A) CORS Access-Control-Allow-Origin: " + header(httpResponse, "Access-Control-Allow-Origin"));
        System.out.println("A) CORS Vary: " + header(httpResponse, "Vary"));

        // B) Websocket sin idsession, canal sin identificador
        // nosemgrep: java.lang.security.audit.crypto.unencrypted-socket.unencrypted-socket -- canal local loopback bajo prueba, test
        final ServerSocket probe = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"));
        final int wsPort = probe.getLocalPort();
        probe.close();
        final AfirmaWebSocketServerV4Sup server = new AfirmaWebSocketServerV4Sup(wsPort, null,
                ProtocolVersion.getInstance(ProtocolVersion.VERSION_4));
        server.start();
        Thread.sleep(500);

        final AtomicReference<String> received = new AtomicReference<>();
        final CountDownLatch latch = new CountDownLatch(1);
        final WebSocketClient client = new WebSocketClient(URI.create("ws://127.0.0.1:" + wsPort)) {
            @Override
            public void onOpen(final ServerHandshake handshake) { }
            @Override
            public void onMessage(final String message) {
                received.set(message);
                latch.countDown();
            }
            @Override
            public void onClose(final int code, final String reason, final boolean remote) {
                latch.countDown();
            }
            @Override
            public void onError(final Exception ex) {
                latch.countDown();
            }
        };
        client.addHeader("Origin", "https://evil.example");
        client.connectBlocking(10, TimeUnit.SECONDS);
        client.send("echo=@EOF");
        latch.await(10, TimeUnit.SECONDS);
        final String message = received.get();
        System.out.println("B) WS sin idsession -> " + ("OK".equals(message) ? "ACEPTADA (brecha)" : "RECHAZADA"));

        Runtime.getRuntime().halt(0);
    }
}
