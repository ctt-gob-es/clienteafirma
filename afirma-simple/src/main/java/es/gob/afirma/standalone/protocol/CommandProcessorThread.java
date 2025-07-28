package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.Timer;

import es.gob.afirma.core.misc.Base64;

/**
 * Hilo para la recepci&oacute;n de peticiones de operaciones de Autofirma
 * a trav&eacute;s de un socket.
 */
class CommandProcessorThread extends Thread {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int READ_BUFFER_SIZE = 2048;

	/** Tama&ntilde;o del segmento de cada fragmento de datos que se lea del <code>socket</code> que se mantendr&aacute;
	 * almacenado por si el segmento de fin de entrada queda dividido entre dos fragmentos. El valor
	 * permite que quepa al completo en un subfragmento la etiqueta <i>EOF</i> y la etiqueta <i>idSession</i> con
	 * su valor. */
	private static final int BUFFERED_SECURITY_RANGE = 36;

	/** N&uacute;mero m&aacute;ximo de intentos de lectura consecutivos en el <i>buffer</i> sin que se encuentren datos. */
	private static final int MAX_READING_BUFFER_TRIES = 10;

	private static final String MEMORY_ERROR = "MEMORY_ERROR";//$NON-NLS-1$
	// ip locales para que no nos vengan peticiones externas
	private static final String LOCALHOST = "localhost"; //$NON-NLS-1$
	private static final String LOOP_DIR = "127.0.0.1";  //$NON-NLS-1$
	private static final String LOOP_DIR_2 = "0:0:0:0:0:0:0:1";  //$NON-NLS-1$


	// cadenas usadas dentro de las peticiones
	private static final String RESET = "-" ; //$NON-NLS-1$
	private static final String SEPARADOR = "@" ;   //$NON-NLS-1$
	private static final String EOF = SEPARADOR+"EOF" ; //$NON-NLS-1$
	private static final String IDSESSION = "idsession"; //$NON-NLS-1$
	// respuesta que podemos mandar.
	private static final String MORE_DATA_NEED = "MORE_DATA_NEED"; //$NON-NLS-1$
	private static final String OK = "OK"; //$NON-NLS-1$
	private static final String SAVE_OK = "SAVE_OK"; //$NON-NLS-1$
	private static final String CANCEL = "CANCEL"; //$NON-NLS-1$

	/** M&aacute;ximo numero de caracteres que podemos enviar en una respuesta. */
	private static final int RESPONSE_MAX_SIZE = 1000000;
	//  peticiones que podemos recibir
	private static final String CMD = "cmd="; //$NON-NLS-1$
	private static final String ECHO = "echo="; //$NON-NLS-1$
	private static final String FRAGMENT = "fragment="; //$NON-NLS-1$
	private static final String SEND = "send="; //$NON-NLS-1$
	private static final String SIGN = "firm="; //$NON-NLS-1$
	private static final String AFIRMA = "afirma://" ; //$NON-NLS-1$
	private static final String AFIRMA2 = "afirma://service?" ; //$NON-NLS-1$
	private static final String AFIRMA3 = "afirma://service/?" ; //$NON-NLS-1$
	private static final String SAVE = "afirma://save?"; //$NON-NLS-1$
	private static final String SAVE2 = "afirma://save/?"; //$NON-NLS-1$

	private final static List<String> request = new ArrayList<>();
	private final static List<String> toSend = new ArrayList<>();
	private static int parts = 0;

	private final Socket localSocket;

	private final Timer timer;

	private final String idSession;

	private final int protocolVersion;

	public CommandProcessorThread(final Socket socket, final Timer timer, final String idSession, final int protocolVersion) {
		this.localSocket = socket;
		this.timer = timer;
		this.idSession = idSession;
		this.protocolVersion = protocolVersion;
	}

	@Override
	public void run() {

		LOGGER.info("Detectada conexion entrante"); //$NON-NLS-1$

		// Comprobamos que la direccion es local. Si no es local se descarta la peticion
		final InetSocketAddress requestorAddress = (InetSocketAddress) this.localSocket.getRemoteSocketAddress();
		if (!isLocalAddress(requestorAddress)) {
			LOGGER.severe(
					"Se ha detectado un acceso no autorizado desde " + //$NON-NLS-1$
							requestorAddress.getHostString() +
							". Se cerrara el socket por seguridad." //$NON-NLS-1$
					);
			closeSocket(this.localSocket);
			return;
		}

		// Leemos los datos del socket
		String httpRequest;
		try {
			httpRequest = read(this.localSocket.getInputStream());
		}
		catch(final IllegalArgumentException e) {
			LOGGER.severe("Se proporciono un ID de sesion erroneo. Se rechaza la conexion: " + e); //$NON-NLS-1$
			sendError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, this.localSocket, "ID de sesion erroneo"); //$NON-NLS-1$
			closeSocket(this.localSocket);
			return;
		}
		catch(final Exception e) {
			LOGGER.severe("Error en la lectura de datos del socket: " + e); //$NON-NLS-1$
			sendError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, this.localSocket, "No se pudieron leer los datos del socket"); //$NON-NLS-1$
			closeSocket(this.localSocket);
			return;
		}

		// Procesamos la peticion
		try {
			// comprobamos que la peticion no es vacia
			if (httpRequest == null) {
				LOGGER.warning("Se ha recibido una peticion vacia"); //$NON-NLS-1$
			}
			else {
				LOGGER.fine("Peticion HTTP recibida:\n" + httpRequest); //$NON-NLS-1$
				processCommand(httpRequest, this.localSocket);

			}
		}
		catch (final IllegalArgumentException e) {
			LOGGER.severe("Los parametros recibidos a traves del socket no son validos: " + e); //$NON-NLS-1$
			sendError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, this.localSocket, "Parametros incorrectos"); //$NON-NLS-1$
		}
		catch(final IOException e) {
			LOGGER.severe("Error en el envio a traves del socket: " + e); //$NON-NLS-1$
			sendError(ProtocolInvocationLauncherErrorManager.ERROR_SENDING_RESULT, this.localSocket, "Envio del resultado a la aplicacion"); //$NON-NLS-1$
		}
		catch (final OutOfMemoryError e){
			LOGGER.severe("Se ha producido un error por falta memoria de la maquina virtual: " + e); //$NON-NLS-1$
			sendMemoryError(this.localSocket);
		}
		catch(final Exception e) {
			LOGGER.severe("Error al procesar el comando enviado: " + e); //$NON-NLS-1$
			sendError(ProtocolInvocationLauncherErrorManager.ERROR_SIGNATURE_FAILED, this.localSocket, "Error al procesar el comando"); //$NON-NLS-1$
		}

		closeSocket(this.localSocket);
	}


	/** Comprueba que la direcci&oacute;n que intenta conectarse es local.
	 * @param a Direcci&oacute;n a probar.
	 * @return <code>true</code> si es direcci&oacute;n local, <code>false</code> en caso contrario. */
	private static boolean isLocalAddress(final InetSocketAddress a) {
		final String hostString = a.getHostString();
		if (LOOP_DIR_2.equals(hostString) ||
				LOOP_DIR.equals(hostString) ||
				LOCALHOST.equals(hostString)) {
			return true;
		}
		return false;
	}

	/** Analiza la petici&oacute;n recibida y realiza las operaciones pertinentes.<br>
	 * Las peticiones permitidas son:
	 * <ul>
	 * 		<li>{@code cmd=} Iniciar una operaci&oacute;n que viene sin fragmentar.</li>
	 * 		<li>{@code echo=} Petici&oacute;n echo para comprobar que la aplicaci&oacute;n esta lista</li>
	 * 		<li>{@code fragment=} Inicia el env&iacute;o de los datos fragment&aacute;ndolos en varias peticiones.</li>
	 * 		<li>{@code firm=} Inicia una operaci&oacute;n juntando los datos fragmentados de las peticiones anteriores.</li>
	 * 		<li>{@code send=} Env&iacute;a la respuesta de la una operaci&oacute;n realizada. Si es demasiado grande se fragmenta en varios env&iacute;os.</li>
	 * </ul>
	 * @param httpRequest Petici&oacute;n recibida con la operaci&oacute;n a realizar.
	 * @param socket El {@code Socket} en el que se escucha la petici&oacute;n.
	 * @throws IOException Cuando se produce un error al procesar la petici&oacute;n.
	 * @throws IllegalArgumentException Cuando se produce un error en la llamada.
	 */
	private void processCommand(final String httpRequest, final Socket socket)
			throws IOException, IllegalArgumentException {

		if (httpRequest == null) {
			throw new IllegalArgumentException("Los datos recibidos por HTTP son nulos"); //$NON-NLS-1$
		}

		final String uriType = getUriTypeFromRequest(httpRequest);
		LOGGER.info("Recibido comando de tipo: " + uriType); //$NON-NLS-1$

		try {
			switch (uriType) {

				case ECHO:
					doEchoPetition(httpRequest.substring(httpRequest.indexOf(ECHO) + ECHO.length()), socket);
					break;

				case CMD:
					doCmdPetition(httpRequest.substring(httpRequest.indexOf(CMD) + CMD.length()), socket);
					break;

				case FRAGMENT:
					doFragmentPetition(httpRequest.substring(httpRequest.indexOf(FRAGMENT) + FRAGMENT.length()), socket);
					break;

				case SIGN:
					doFragmentedProcess(socket);
					break;

				case SEND:
					doSendPetition(httpRequest.substring(httpRequest.indexOf(SEND) + SEND.length()), socket);
					break;

				default: // Nunca deberia entrar aqui
					throw new IllegalStateException("Comando no permitido: " + uriType); //$NON-NLS-1$
			}
		}
		catch(final IllegalStateException e) {
			throw new IllegalArgumentException(
				"Error al procesar el comando de tipo '" + uriType + "': " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch(final IOException e) {
			throw e;
		}
		catch(final Exception e) {
			throw new IOException("Error durante la operacion por socket", e); //$NON-NLS-1$
		}
	}


	/** Devuelve el <code>uriType</code> de la petici&oacute;n recibida. Lanza una excepci&oacute;n en caso
	 * de que la petici&oacute;n no sea petici&oacute;n.
	 * @param httpRequest La petici&oacute;n a tratar.
	 * @return String <i>uriType</i> de la petici&oacute;n. */
	private static String getUriTypeFromRequest(final String httpRequest) {
		final String[] supportedUriTypes = new String[] {CMD, ECHO, FRAGMENT, SIGN, SEND};

		int i = 0;
		String uriType = null;
		while (uriType == null && i < supportedUriTypes.length) {
			if (httpRequest.indexOf(supportedUriTypes[i]) != -1) {
				uriType = supportedUriTypes[i];
			}
			i++;
		}
		if (uriType == null) {
			throw new IllegalArgumentException(
				"Los datos recibidos por HTTP no contienen comando reconocido: " + httpRequest //$NON-NLS-1$
			);
		}
		return uriType;
	}

	/** Realiza las acciones pertinentes en caso de que la petici&oacute;n contenga una petici&oacute;n <i>echo</i>.
	 * Se reinician las variables de control de la aplicaci&oacute;n para desechar cualquier petici&oacute;n anterior.
	 * @param cmd Comando URI.
	 * @param socket <i>Socket</i> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en el env&iacute;o de datos. */
	private void doEchoPetition(final String cmd, final Socket socket) throws IOException {
		// paramos el timer mientras la aplicacion realiza operaciones
		this.timer.stop();
		// solo reseteamos las variables de control si la peticion es echo=- en caso de que sea un echo= simplemente respondemos
		if (cmd.contains(RESET)){
			reset();
		}

		LOGGER.info("Comando URI recibido por HTTP: " + ECHO); //$NON-NLS-1$
		sendData(createHttpResponse(true, OK), socket, ECHO);
	}

	/** Realiza la operaci&oacute;n que corresponda cuando ya se han recibido todos los
	 * fragmentos de la petici&oacute;n (<code>firm=</code>).
	 * @param socket <code>Socket</code> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en la lectura o env&iacute;o de datos. */
	private void doFragmentedProcess(final Socket socket) throws IOException {
		// paramos el timer mientras la aplicacion realiza operaciones
		boolean isSave = false;
	    this.timer.stop();
		LOGGER.info("Comando URI recibido por HTTP (Fragmento)"); //$NON-NLS-1$
		// en caso de que sea la primera vez que lo ejecutamos, realizamos la operacion.
		// si la respuesta no llega al JS y vuelve a realizar la misma peticion, ya tenemos
		// formada la respuesta y solo hay que devolver el numero de peticiones.
		if (toSend.isEmpty()) {
			final StringBuilder totalhttpRequest = new StringBuilder();
			for (final String object: request){
				totalhttpRequest.append(object);
			}

			// Si la operacion es de guardado (Respuesta fija)
			if (totalhttpRequest.toString().startsWith("afirma://save?") || totalhttpRequest.toString().startsWith("afirma://save/?")){ //$NON-NLS-1$  //$NON-NLS-2$
			    isSave = true;
				final String operationResult = ProtocolInvocationLauncher.launch(totalhttpRequest.toString(), this.protocolVersion, true);
				if (operationResult.equals(OK)){
					sendData(createHttpResponse(true, SAVE_OK), socket, "Operacion save realizada con exito"); //$NON-NLS-1$
				}
				else if (operationResult.equals(CANCEL)){
				    sendData(createHttpResponse(true, CANCEL), socket, "Cancelado por el usuario"); //$NON-NLS-1$
				}
				else {
					throw new IllegalArgumentException(
						"Error al realizar la operacion save" //$NON-NLS-1$
					);
				}
			}
			// Si hay que devolver el valor obtenido
			else {
				final String operationResult = ProtocolInvocationLauncher.launch(totalhttpRequest.toString(), this.protocolVersion, true);
				calculateNumberPartsResponse(operationResult);
			}
		}
		else {
			LOGGER.info("Se habia calculado el numero de partes anteriormente"); //$NON-NLS-1$
		}
		// si no es una operacion save y nos vuelven a pedir una parte.
		if (!isSave){
		    sendData(
	    		createHttpResponse(true, Integer.toString(parts)), socket, "Se mandaran " + parts + " partes"  //$NON-NLS-1$//$NON-NLS-2$
    		);
		}
	}

	/** Realiza las acciones pertinentes en caso de que la petici&oacute;n contenga una petici&oacute;n <code>cmd=</code>.
	 * @param cmd Valor del par&aacute;metro <code>cmd</code>.
	 * @param socket <i>Socket</i> donde se recibe la petici&oacute;n.
	 * @throws IOException Error en la lectura o en el env&iacute;o de datos. */
	private void doCmdPetition (final String cmd, final Socket socket) throws IOException{
		final String cmdUri = new String(Base64.decode(cmd.trim(), true));
		if (cmdUri.startsWith(AFIRMA) && !(cmdUri.startsWith(AFIRMA2) || cmdUri.startsWith(AFIRMA3))) {
			// paramos el timer mientras la aplicacion realiza operaciones
			this.timer.stop();
			LOGGER.info("Comando URI recibido por HTTP: " + cmdUri); //$NON-NLS-1$
			if (cmdUri.startsWith(SAVE) || cmdUri.startsWith(SAVE2)) {
				final String operationResult = ProtocolInvocationLauncher.launch(cmdUri.toString(), this.protocolVersion, true);
				if (operationResult.equals(OK)) {
					sendData(createHttpResponse(true, SAVE_OK), socket, "Guardar datos"); //$NON-NLS-1$
				}
				else if (operationResult.equals(CANCEL)) {
                    sendData(createHttpResponse(true, CANCEL), socket, "Cancelado por el usuario"); //$NON-NLS-1$
                }
				else {
					throw new IllegalArgumentException(
						"Error al realizar la operacion 'save'" //$NON-NLS-1$
					);
				}
			}
			else {
				if (toSend.isEmpty()){
					// Usamos la url que acabamos de recibir sin fragmentar
					final String operationResult = ProtocolInvocationLauncher.launch(cmdUri.toString(), this.protocolVersion, true);
					calculateNumberPartsResponse(operationResult);
				}
				sendData(createHttpResponse(true, Integer.toString(parts)), socket, "Se mandaran " + parts + " partes"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		else {
			throw new IllegalArgumentException(
				"Los datos recibidos en el parametro 'cmd' por HTTP no son una URI del tipo 'afirma://': " + cmdUri //$NON-NLS-1$
			);
		}
	}

	/** Realiza las acciones pertinentes en caso de que la petici&oacute;n contenta una petici&oacute;n <code>fragment=</code>.
	 * @param fragment httpRequest URL de la que hay que substraer el commandUri.
	 * @param socket <code>Socket</code> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en el tratamiento de datos. */
	private void doFragmentPetition (final String fragment, final Socket socket) throws IOException{

		// paramos el timer mientras la aplicacion realiza operaciones
		this.timer.stop();
		LOGGER.info("Comando URI recibido por HTTP (fragmento)"); //$NON-NLS-1$
		final String[] petition = fragment.split(SEPARADOR);
		final int part = Integer.parseInt(petition [1]);
		final int partTotal = Integer.parseInt(petition [2]);
		final String save = new String (Base64.decode(petition[3].trim(), true));

		if (request.size() == part ){
			LOGGER.info("sustituimos la parte " + part); //$NON-NLS-1$
			request.set(part-1, save);
		}
		else {
			LOGGER.info("insertamos la parte " + part); //$NON-NLS-1$
			request.add(part-1, save);
		}
		if (part == partTotal){
			sendData(createHttpResponse(true, OK), socket, "Mandada la ultima parte " + part +"de " + partTotal); //$NON-NLS-1$ //$NON-NLS-2$
		}
		else {
			sendData(createHttpResponse(true, MORE_DATA_NEED), socket, "Mandar resto de datos de la firma, parte " + part + "de " + partTotal);  //$NON-NLS-1$//$NON-NLS-2$
		}
	}

	/** Realiza el env&iacute;o de datos.
	 * @param send Configuraci&oacute;n para el env&iacute;o de datos.
	 * @param socket <i>Socket</i> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en el tratamiento de datos. */
	private void doSendPetition (final String send, final Socket socket) throws IOException {

		// paramos el timer mientras la aplicacion realiza operaciones
		this.timer.stop();
		LOGGER.info("Comando URI recibido por HTTP: " + send); //$NON-NLS-1$
		final String[] petition = send.split(SEPARADOR);
		final int part = Integer.parseInt(petition [1]);
		final int partTotal = Integer.parseInt(petition [2]);
		if (part < 1 || part > partTotal) {
			throw new IllegalArgumentException(
				"Se ha solicitado enviar un fragmento invalido: " + part + "de " + partTotal //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		sendData(createHttpResponse(true, toSend.get(part-1)), socket, "Mandada la parte " + part + " de " + partTotal); //$NON-NLS-1$ //$NON-NLS-2$
	}


	/** Calcula en cuantas partes hay que realizar el env&iacute;o de la operaci&oacute;n y divide la respuesta en dichas partes.
	 * @param operationResult La operaci&oacute;n resultante que hay que dividir. */
	private static void calculateNumberPartsResponse(final String operationResult) {

		parts = (int) Math.ceil(operationResult.length() / (double) RESPONSE_MAX_SIZE);
		LOGGER.info("Se mandaran " + parts + "partes");  //$NON-NLS-1$//$NON-NLS-2$
		LOGGER.info("tam total=" + operationResult.length()); //$NON-NLS-1$
		// si recibimos la misma peticion otra vez ya tenemos los datos preparados, solo devolvemos las peticiones
		int offset;
		for (int i = 0; i < parts; i++){
			offset = RESPONSE_MAX_SIZE * i;
			toSend.add(operationResult.substring(offset, Math.min(offset + RESPONSE_MAX_SIZE, operationResult.length())));
			LOGGER.info("Tamano de la parte " + (i+1) + " =" + toSend.get(i).length()); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}


	/** Reinicia las variables de control si se realiza una nueva llamada y hay que descartar
	 * todas las las operaciones pendientes. */
	private static void reset(){
		request.clear();
		toSend.clear();
		parts = 0;
	}


	/** Manda los datos de respuesta a la aplicaci&oacute;n y, si estaba parada la
	 * cuenta atr&aacute;s para el cierre de la aplicaci&oacute;n, la reinicia desde el
	 * principio.
	 * @param response Respuesta al env&iacute;o.
	 * @param socket <code>Socket</code> a donde mandar la respuesta.
	 * @param petition Petici&oacute;n que se manda, para registrarla en el <i>log</i>.
	 * @throws IOException Si hay errores en el env&iacute;o. */
	private void sendData(final byte[] response, final Socket socket, final String petition) throws IOException {
		socket.getOutputStream().write(response);
		socket.getOutputStream().flush();

		LOGGER.info("Mandando respuesta a la aplicacion: " + petition);  //$NON-NLS-1$
		// Si el Timer estaba parado es que estabamos procesando una operacion, tras lo cual,
		// se reinicia el temporizador. En cambio, si estaba en ejecucion, es que nunca lo hemos
		// parado, por lo que el error se ha producido antes de empezar a procesar una peticion validad.
		// No lo reiniciaremos en este ultimo caso, ya que podria tratarse de un ataque con peticiones
		// invalidas que busque que la aplicacion no se cierre en ningun momento
		if (!this.timer.isRunning()) {
			this.timer.restart();
		}

	}

	/**
	 * Manda como respuesta a la aplicacion uno de los mensajes de error registrados.
	 * @param safError C&oacute;digo SAF del error.
	 * @param socket Socket a trav&eacute;s del que enviar el mensaje.
	 * @param petition Descripcion del mensaje que se trato de enviar.
	 */
	private void sendError(final String safError, final Socket socket, final String petition) {
		try {
			sendData(createHttpResponse(true, ProtocolInvocationLauncherErrorManager.getErrorMessage(safError)), this.localSocket, "ID de sesion erroneo"); //$NON-NLS-1$
		} catch (final IOException ex) {
			LOGGER.warning("No se ha podido informar a la aplicacion del error producido: " + ex); //$NON-NLS-1$
		}
	}

	/**
	 * Envia un mensaje de error de memoria.
	 * @param socket Socket a trav&eacute;s del que enviar el mensaje.
	 */
	private void sendMemoryError(final Socket socket) {
		try {
			sendData(createHttpResponse(true, MEMORY_ERROR), this.localSocket, "Error de memoria"); //$NON-NLS-1$
		} catch (final IOException ex) {
			LOGGER.warning("No se ha podido informar a la aplicacion del error de memoria: " + ex); //$NON-NLS-1$
		}
	}

	/** Crea una respuesta HTTP para enviar a traves del <i>socket</i>.
	 * @param ok Indica si la operacion finaliz&oacute; bien o mal.
	 * @param response La respuesta que se mandar&aacute; en el HTTP.
	 * @return Devuelve el byte array con la respuesta en formato HTTP. */
	private static byte[] createHttpResponse(final boolean ok, final String response) {
		final StringBuilder sb = new StringBuilder();
		if (ok) {
			sb.append("HTTP/1.1 200 OK\n"); //$NON-NLS-1$
		}
		else  {
			sb.append("HTTP/1.1 500 Internal Server Error"); //$NON-NLS-1$
		}
		sb.append("Connection: close\n"); //$NON-NLS-1$
		sb.append("Pragma: no-cache\n"); //$NON-NLS-1$
		sb.append("Server: Cliente @firma\n"); //$NON-NLS-1$
		sb.append("Content-Type: text/html; charset=utf-8\n"); //$NON-NLS-1$
		sb.append("Access-Control-Allow-Origin: *\n"); //$NON-NLS-1$
		sb.append('\n');
		if (response != null) {
			sb.append(Base64.encode(response.getBytes(StandardCharsets.UTF_8), true));
		}
		return sb.toString().getBytes(StandardCharsets.UTF_8);
	}


	/** Lee los datos recibidos en el <i>socket</i>. Si se recibe una petici&oacute;n sin datos o compuesto de
	 * caracteres blancos (como ocurre cuando el certificado SSL no es correcto) se devuelve nulo.
	 * @param socketIs Flujo de entrada de datos del <i>socket</i>.
	 * @return Los datos recibidos o {@code null} si se recibe una petici&oacute;n vac&iacute;a.
	 * @throws IOException Si ocurren errores durante la lectura.
	 * @throws IllegalArgumentException Si se env&iacute;a un identificador de sesi&oacute;n err&oacute;neo. */
	private String read(final InputStream socketIs) throws IOException, IllegalArgumentException {
		// Buffer en el que se ira almacenando toda la entrada
		final StringBuilder data = new StringBuilder();
		// Cadena que se ira actualizando para contener siempre la union de los
		// 2 ultimos fragmentos con un tamano suficiente para contener el
		// fragmento de fin y el identificador de sesion
		String subFragment = ""; //$NON-NLS-1$
		final byte[] reqBuffer = new byte[READ_BUFFER_SIZE];
		boolean readed = true;
		String insert;
		// Limite alcanzado de lecturas vacias del socket
		int readingTries;
		while (readed) {

			// Leemos del socket hasta obtener algo mas que caracteres vacios
			readingTries = 0;
			do {
				readingTries++;
				final int bytesRead = socketIs.read(reqBuffer);
				insert = new String(reqBuffer, 0, bytesRead, StandardCharsets.UTF_8);
			} while (insert.trim().isEmpty() && readingTries <= MAX_READING_BUFFER_TRIES);

			// Para evitar un error de memoria, si llegamos al maximo numero de intentos en el
			// que solo hemos leido caracteres vacios, entendemos que era una peticion invalida
			// y develvemos nulo para que se ignore
			if (readingTries > MAX_READING_BUFFER_TRIES) {
				return null;
			}

			subFragment += insert.substring(0, Math.min(BUFFERED_SECURITY_RANGE, insert.length()));

			// Comprobamos si la etiqueta de fin se encuentra en el nuevo fragmento o si esta a medias
			// entre este y el anterior. En base a eso, tendremos que agregar o quitar datos del buffer
			if (subFragment.indexOf(EOF) != -1 || insert.indexOf(EOF) != -1) {

				readed = false;

				final boolean findOnSubFragment;
				int eofPos;
				int idSessionPos;
				String requestSessionId = null;

				// Si se detecta el fragmento de fin en la union de ambos fragmentos,
				// comprobamos si parte del contenido del buffer sobra o si hay algo que agregar
				if (subFragment.indexOf(EOF) != -1) {
					findOnSubFragment = true;
					eofPos = subFragment.indexOf(EOF);
					idSessionPos = subFragment.indexOf(IDSESSION);
					if (idSessionPos != -1) {
						requestSessionId = subFragment.substring(
								idSessionPos + IDSESSION.length() + 1,
								eofPos);
					}
				}
				else {
					findOnSubFragment = false;
					eofPos = insert.indexOf(EOF);
					idSessionPos = insert.indexOf(IDSESSION);
					if (idSessionPos != -1) {
						requestSessionId = insert.substring(
								idSessionPos + IDSESSION.length() + 1,
								eofPos);
					}
				}

				// Comprobamos que el id de sesion transmitido es correcto
				checkIdSession(requestSessionId);

				if (findOnSubFragment) {
					// Si el id de session es anterior al rango de seguridad, en el buffer de
					// la peticion se habra introducido parte de este identificador y hay que
					// borrarlos.
					if (idSessionPos != -1 && idSessionPos < BUFFERED_SECURITY_RANGE) {
						data.replace(data.length() - (BUFFERED_SECURITY_RANGE - idSessionPos), data.length(), ""); //$NON-NLS-1$
					}
					// Si la posicion del EOF es anterior al rango de seguridad, entonces
					// en el buffer de la peticion se ha introducido parte de esta etiqueta
					// y hay que borrarla.
					else if (eofPos < BUFFERED_SECURITY_RANGE) {
						data.replace(data.length() - (BUFFERED_SECURITY_RANGE - eofPos), data.length(), ""); //$NON-NLS-1$
					}
					// Si tanto el EOF como el id de sesion (en caso de haber) son posteriores
					// al rango de seguridad, solo habra que incorporar los datos encontrados
					// antes de estas posiciones (siempre y cuando no esten al principio, en
					// cuyo caso, no hay que agregar nada)
					else if (idSessionPos > 0 || eofPos > 0) {
						data.append(subFragment.substring(0, idSessionPos > -1 ? idSessionPos : eofPos));
					}
				}
				else {
					data.append(insert.substring(0, idSessionPos > -1 ? idSessionPos : eofPos));
				}
			}
			else {
				// Agregamos los datos al total, ya que no se ha encontrado la cadena de fin de lectura
				data.append(insert);
				subFragment = insert.substring(Math.max(0, insert.length() - BUFFERED_SECURITY_RANGE));
			}
		}
		return data.toString();
	}


	/** Comprueba que el <code>idSession</code> de la petici&oacute;n recibida coincida con el
	 * <code>idSession</code> generado al abrir la aplicaci&oacute;n por <i>socket</i>.
	 * @param requestSessionId Identificador de sesi&oacute;n enviado en la petici&oacute;n.
	 * @throws IllegalArgumentException Cuando el identificador no sea correcto. */
	private void checkIdSession (final String requestSessionId) throws IllegalArgumentException {
		// se esperaba un idSession y no se ha recibido
		if (this.idSession != null && !this.idSession.equals(requestSessionId)) {
			throw new IllegalArgumentException("No se ha recibido el idSession esperado."); //$NON-NLS-1$
		}
	}

	/**
	 * Trata de cerrar un socket y muestra un log de advertencia en caso no no poder hacerlo.
	 * @param socket Socket a cerrar.
	 */
	private static void closeSocket(final Socket socket) {
		try {
			socket.close();
		}
		catch (final Exception e) {
			LOGGER.warning("Error al cerrar el socket: " + e); //$NON-NLS-1$
		}
	}
}
