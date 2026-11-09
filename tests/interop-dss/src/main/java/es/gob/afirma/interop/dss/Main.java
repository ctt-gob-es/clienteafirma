package es.gob.afirma.interop.dss;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Base64;
import java.util.HexFormat;
import java.util.Map;
import java.util.logging.Logger;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

/**
 * Servicio HTTP sin estado que valida firmas con EU DSS para el gate de interoperabilidad.
 *
 * <ul>
 * <li><code>POST /validate</code> con <code>{"signature": base64, "ca": base64 DER}</code>.</li>
 * <li><code>GET /health</code>.</li>
 * </ul>
 *
 * Escucha solo en el puerto indicado (8080 por defecto), limita el cuerpo a 10 MB y nunca
 * registra el contenido de la firma ni de la CA: solo sus hashes SHA-256.
 */
public final class Main {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma.interop.dss"); //$NON-NLS-1$
	private static final int MAX_BODY_BYTES = 10 * 1024 * 1024;
	private static final ObjectMapper MAPPER = new ObjectMapper();

	private Main() {
		// No instanciable
	}

	/** Punto de entrada.
	 * @param args Sin uso.
	 * @throws IOException Si no se puede abrir el puerto. */
	public static void main(final String[] args) throws IOException {

		final int port = Integer.parseInt(System.getProperty("interop.dss.port", "8080")); //$NON-NLS-1$ //$NON-NLS-2$
		final HttpServer server = HttpServer.create(new InetSocketAddress("0.0.0.0", port), 0); //$NON-NLS-1$
		server.createContext("/health", Main::handleHealth); //$NON-NLS-1$
		server.createContext("/validate", Main::handleValidate); //$NON-NLS-1$
		server.setExecutor(null);
		server.start();
		LOGGER.info(() -> "interop-dss-validator escuchando en el puerto " + port); //$NON-NLS-1$
	}

	private static void handleHealth(final HttpExchange exchange) throws IOException {
		if (!"GET".equals(exchange.getRequestMethod())) { //$NON-NLS-1$
			respond(exchange, 405, Map.of("error", "metodo no permitido")); //$NON-NLS-1$ //$NON-NLS-2$
			return;
		}
		respond(exchange, 200, Map.of("status", "UP")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static void handleValidate(final HttpExchange exchange) throws IOException {
		if (!"POST".equals(exchange.getRequestMethod())) { //$NON-NLS-1$
			respond(exchange, 405, Map.of("error", "metodo no permitido")); //$NON-NLS-1$ //$NON-NLS-2$
			return;
		}

		final byte[] body;
		try (InputStream in = exchange.getRequestBody()) {
			body = readLimited(in, MAX_BODY_BYTES);
		}
		if (body == null) {
			respond(exchange, 413, Map.of("error", "cuerpo mayor de 10 MB")); //$NON-NLS-1$ //$NON-NLS-2$
			return;
		}

		final JsonNode json;
		try {
			json = MAPPER.readTree(body);
		}
		catch (final IOException e) {
			respond(exchange, 400, Map.of("error", "JSON no valido")); //$NON-NLS-1$ //$NON-NLS-2$
			return;
		}

		final byte[] signature;
		final byte[] ca;
		final byte[] content;
		final java.util.List<byte[]> anchors = new java.util.ArrayList<>();
		try {
			signature = Base64.getDecoder().decode(requireText(json, "signature")); //$NON-NLS-1$
			// 'ca' admite una cadena (una sola ancla) o un array (cadena completa).
			final JsonNode caNode = json.get("ca"); //$NON-NLS-1$
			if (caNode != null && caNode.isArray()) {
				for (final JsonNode item : caNode) {
					anchors.add(Base64.getDecoder().decode(item.asText()));
				}
			}
			else {
				anchors.add(Base64.getDecoder().decode(requireText(json, "ca"))); //$NON-NLS-1$
			}
			if (anchors.isEmpty()) {
				throw new IllegalArgumentException("sin anclas de confianza"); //$NON-NLS-1$
			}
			ca = anchors.get(0);
			content = json.hasNonNull("content") ? Base64.getDecoder().decode(json.get("content").asText()) : null; //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IllegalArgumentException | NullPointerException e) {
			respond(exchange, 400, Map.of("error", "faltan 'signature' y 'ca' en base64")); //$NON-NLS-1$ //$NON-NLS-2$
			return;
		}

		// Trazabilidad sin contenido: solo hashes.
		LOGGER.info(() -> "validate signature_sha256=" + sha256(signature) + " ca_count=" + anchors.size() //$NON-NLS-1$ //$NON-NLS-2$
			+ (content == null ? "" : " content_sha256=" + sha256(content))); //$NON-NLS-1$ //$NON-NLS-2$

		try {
			final DssValidator.Result result = DssValidator.validate(signature, anchors, content);
			final ObjectNode response = MAPPER.createObjectNode();
			response.put("indication", result.indication()); //$NON-NLS-1$
			response.put("subIndication", result.subIndication()); //$NON-NLS-1$
			response.put("signatureFormat", result.signatureFormat()); //$NON-NLS-1$
			response.put("validSignaturesCount", result.validSignaturesCount()); //$NON-NLS-1$
			respond(exchange, 200, response);
		}
		catch (final DssValidator.ValidationException e) {
			LOGGER.warning(() -> "validacion no procesable: " + e.getMessage()); //$NON-NLS-1$
			respond(exchange, 400, Map.of("error", e.getMessage())); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final RuntimeException | Error e) {
			// Un Error (p. ej. NoClassDefFoundError del classpath sombreado) mataria el hilo
			// del servidor y dejaria la conexion sin respuesta: se captura y se informa.
			LOGGER.severe(() -> "error interno: " + e.getClass().getName() + ": " + e.getMessage()); //$NON-NLS-1$ //$NON-NLS-2$
			respond(exchange, 500, Map.of("error", "error interno de validacion: " + e.getClass().getSimpleName())); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	private static String requireText(final JsonNode json, final String field) {
		final JsonNode node = json.get(field);
		if (node == null || !node.isTextual()) {
			throw new IllegalArgumentException("campo ausente: " + field); //$NON-NLS-1$
		}
		return node.asText();
	}

	/** Lee como maximo {@code limit} bytes; devuelve {@code null} si el cuerpo lo supera. */
	private static byte[] readLimited(final InputStream in, final int limit) throws IOException {
		final byte[] buffer = new byte[8192];
		final java.io.ByteArrayOutputStream out = new java.io.ByteArrayOutputStream();
		int read;
		while ((read = in.read(buffer)) != -1) {
			if (out.size() + read > limit) {
				return null;
			}
			out.write(buffer, 0, read);
		}
		return out.toByteArray();
	}

	private static void respond(final HttpExchange exchange, final int status, final Object payload) throws IOException {
		final byte[] body = (payload instanceof String s ? s : MAPPER.writeValueAsString(payload)).getBytes(StandardCharsets.UTF_8);
		exchange.getResponseHeaders().add("Content-Type", "application/json; charset=utf-8"); //$NON-NLS-1$ //$NON-NLS-2$
		exchange.sendResponseHeaders(status, body.length);
		try (OutputStream out = exchange.getResponseBody()) {
			out.write(body);
		}
	}

	private static String sha256(final byte[] data) {
		try {
			return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(data)); //$NON-NLS-1$
		}
		catch (final Exception e) {
			return "unavailable"; //$NON-NLS-1$
		}
	}
}
