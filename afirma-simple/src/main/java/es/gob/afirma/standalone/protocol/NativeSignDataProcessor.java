package es.gob.afirma.standalone.protocol;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.standalone.crypto.NativeDataCipher;
import es.gob.afirma.standalone.plugins.DataCipher;
import es.gob.afirma.standalone.plugins.EncryptingException;
import es.gob.afirma.standalone.plugins.PluginControlledException;
import es.gob.afirma.standalone.plugins.SignDataProcessor;
import es.gob.afirma.standalone.plugins.SignOperation;
import es.gob.afirma.standalone.plugins.SignResult;


public class NativeSignDataProcessor extends SignDataProcessor {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final char RESULT_SEPARATOR = '|';

	private DataCipher cipher;

	public NativeSignDataProcessor(final int protocolVersion) {
		super(protocolVersion);
		this.cipher = null;
	}

	@Override
	public void setCipherKey(final byte[] key) {
		if (key != null) {
			this.cipher = new NativeDataCipher(key);
		}
	}

	@Override
	public List<SignOperation> preProcess(final SignOperation operation) {
		return Arrays.asList(operation);
	}

	/**
	 * Compone la respuesta a partir del resultado de la primera firma, ya que nativamente
	 * s&oacute;lo puede generarse una &uacute;nica firma.
	 * @param results Listado de firmas resultando (s&oacute;lo debe haber un elemento).
	 * @return Respuesta de la operaci&oacute;n.
	 * @throws EncryptingException Cuando ocurre un error durante el encryptado de los datos.
	 * @throws PluginControlledException Cuando ocurre cualquier otro error durante el
	 * postprocesado de los datos.
	 */
	@Override
	public StringBuilder postProcess(final List<SignResult> results, final SignOperation signOperation)
					throws es.gob.afirma.standalone.plugins.EncryptingException, PluginControlledException {

		// Solo enviaremos los resultados de la primera firma, ya que solo deberia haber
		// una firma
		final SignResult result = results.get(0);

		final byte[] sign = result.getSignature();
		final byte[] certEncoded = result.getCertificate();
		final Properties extraData = result.getExtraData();

		final StringBuilder dataToSend = new StringBuilder();

		// Si tenemos clave de cifrado, ciframos los datos
		if (this.cipher != null) {
			LOGGER.info("Se cifran los datos resultantes con la clave de cifrado proporcionada"); //$NON-NLS-1$
			try {
				// El CipherData devuelve los datos directamente en Base64
				dataToSend.append(this.cipher.cipher(certEncoded));
				dataToSend.append(RESULT_SEPARATOR);
				dataToSend.append(this.cipher.cipher(sign));

				// A partir del protocolo version 3, si se cargo un fichero, se devuelve el nombre
				if (extraData != null && !extraData.isEmpty() && getProtocolVersion() >= 3) {
					dataToSend.append(RESULT_SEPARATOR);
					dataToSend.append(this.cipher.cipher(buildExtraDataResult(extraData)));
				}
			}
			catch (final Exception e) {
				throw new EncryptingException("Error al cifrar los datos a enviar", e); //$NON-NLS-1$
			}
		}
		else {
			LOGGER.fine("Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado"); //$NON-NLS-1$

			dataToSend.append(Base64.encode(certEncoded, true));
			dataToSend.append(RESULT_SEPARATOR);
			// Se hace una doble codificacion Base64, una de los datos y otras
			// del cifrado. La codificacion se realiza incluso si el cifrado
			// no se hiciera
			dataToSend.append(Base64.encode(sign, true));

			// A partir del protocolo version 3, si se cargo un fichero, se devuelve el nombre
			if (extraData != null && !extraData.isEmpty() && getProtocolVersion() >= 3) {
				dataToSend.append(RESULT_SEPARATOR);
				dataToSend.append(Base64.encode(buildExtraDataResult(extraData), true));
			}
		}

		return dataToSend;
	}


	/**
	 * Construye una cadena de texto con un objeto JSON de datos extra que enviar en la respuesta.
	 * @param extraData Conjunto de propiedades que imprimir en el JSON.
	 * @return Cadena con el JSON de datos extra.
	 */
	private static byte[] buildExtraDataResult(final Properties extraData) {

		final StringBuilder buffer = new StringBuilder();
		buffer.append("{"); //$NON-NLS-1$
		final String[] keys = extraData.keySet().toArray(new String[0]);
		for (int i = 0; i < keys.length; i++) {
			buffer.append("\"").append(keys[i]).append("\": \"") //$NON-NLS-1$ //$NON-NLS-2$
				.append(extraData.get(keys[i])).append("\""); //$NON-NLS-1$
			if (i < keys.length - 1) {
				buffer.append(", "); //$NON-NLS-1$
			}
		}
		buffer.append("}"); //$NON-NLS-1$
		return buffer.toString().getBytes(StandardCharsets.UTF_8);
	}

	@Override
	public boolean checkTrigger(final SignOperation operation) {
		return true;
	}

	@Override
	public boolean isErrorsAllowed() {
		return false;
	}
}
