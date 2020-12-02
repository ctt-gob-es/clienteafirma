package es.gob.afirma.standalone.plugins;

import java.util.List;

/**
 * Procesados de los datos de entrada para la identificaci&oacute;n de las operaciones de firma a realizar.
 */
public abstract class SignDataProcessor {

	/** Versi&oacute;n de protocolo usado por el script que invoca la operaci&oacute;n. */
	private final int protocolVersion;

	/**
	 *
	 * @param protocolVersion Versi&oacute;n de protocolo del script de llamada.
	 */
	public SignDataProcessor(final int protocolVersion) {
		this.protocolVersion = protocolVersion;
	}

	public abstract void setCipherKey(byte[] key);

	/**
	 * Compone las operaciones de firma que deben realizarse a partir de los datos de
	 * @param operation Configuraci&oacute;n de la operaci&oacute;n solicitada.
	 * @return Listado de operaciones a realizar.
	 */
	public abstract List<SignOperation> preProcess(SignOperation operation);

	/**
	 * Compone la respuesta a la operaci&oacute;n de firma del cliente.
	 * @param results Listado de resultados de las operaciones de firma.
	 * @param operation Configuraci&oacute;n de la operaci&oacute;n solicitada.
	 * @return Buffer con las respuestas en forma de cadena de texto.
	 * @throws EncryptingException Cuando se produce un error durante un proceso de encriptado
	 * de la respuesta (si se encriptase.
	 * @throws PluginControlledException Cuando ocurre cualquier otro tipo de error durante el
	 * procesado de las firmas.
	 */
	public abstract StringBuilder postProcess(List<SignResult> results, SignOperation operation)
					throws EncryptingException, PluginControlledException;

	/**
	 * Comprueba si el procesador deber&iacute;a activarse para esta petici&oacute;n.
	 * @param operation Configuraci&oacute;n de la operaci&oacute;n solicitada.
	 * @return {@code true} si la operaci&oacute;n deber&iacute;a
	 */
	public abstract boolean checkTrigger(SignOperation operation);

	/**
	 * Indica si se deber&iacute;a continuar con el procesado de peticiones de firma despu&eacute;s
	 * de detectar un error en una de ellas.
	 * @return {@code true} para continuar con el procesado, {@code false} para detenerlo. Por
	 * defecto, {@code false}.
	 */
	@SuppressWarnings("static-method")
	public boolean isErrorsAllowed() {
		return false;
	}

	/**
	 * Recupera la versi&oacute;n de protocolo de invocaci&oacute;n.
	 * @return Versi&oacute;n de protocolo.
	 */
	public final int getProtocolVersion() {
		return this.protocolVersion;
	}
}
