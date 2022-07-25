package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.json.JSONException;
import org.json.JSONObject;

import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.signers.batch.client.LocalDataParser;

public class LocalBatchSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SKIPPED_SIGN = "SKIPPED"; //$NON-NLS-1$
	private static final String ERROR_SIGN = "ERROR"; //$NON-NLS-1$

	/**
	 * Metodo encargado de firmar todas las firmas monof&aacute;sicas que conforman un lote.
	 * @param urlParamsToSign Par&aacute;metros de las firmas.
	 * @param pke Clave privada usada por el certificado.
	 * @return Resultado del proceso.
	 */
	public static String signLocalBatch(final List<UrlParametersToSign> urlParamsToSign, final PrivateKeyEntry pke) {

		final StringBuilder result = new StringBuilder("{\n"); //$NON-NLS-1$

		boolean errorOcurred = false;

		for (final UrlParametersToSign urlParam : urlParamsToSign) {

			if (errorOcurred && urlParam.getStopOnError()) {
				result.append("\nid:").append(urlParam.getId()) //$NON-NLS-1$
				.append("\nresult:").append(SKIPPED_SIGN); //$NON-NLS-1$
			} else {
				try {
					final StringBuilder signResult = ProtocolInvocationLauncherSign.processSign(urlParam, 1, pke);
					result.append("\nid:").append(urlParam.getId()) //$NON-NLS-1$
					.append("\nresult:").append(signResult); //$NON-NLS-1$
				}
				catch (final SocketOperationException e) {
					errorOcurred = true;
					LOGGER.severe("Error al procesar la firma de lotes monofasica con ID: " + urlParam.getId() + "Excepcion :" + e); //$NON-NLS-1$ //$NON-NLS-2$
					result.append("\nid:").append(urlParam.getId()) //$NON-NLS-1$
					.append("\nresult:").append(ERROR_SIGN) //$NON-NLS-1$
					.append("\ndescription:").append(e.getMessage()); //$NON-NLS-1$
				}
			}
		}

		result.append("\n}"); //$NON-NLS-1$

		return result.toString();
	}

	/**
	 * Obtiene los par&aacute;metros necesarios para cada firma, dentro de una firma por lotes monof&aacute;sica.
	 * @param batchB64 petici&oacute;n JSON
	 * @return lista de par&aacute;metros para ejecutar la firma
	 * @throws IOException error en la lectura o escritura de los datos
	 * @throws ParameterException error en uno de los parametros indicados en la petici&oacute;n JSON
	 */
	public static List<UrlParametersToSign> getUrlParamsFromSingleSigns(final byte[] batchB64)
			throws IOException,  ParameterException {

		JSONObject jsonObject = null;

		try {
			jsonObject = new JSONObject(new String(batchB64, StandardCharsets.UTF_8));
		}
		catch (final JSONException e){
			LOGGER.severe("Error al parsear JSON: " + e); //$NON-NLS-1$
			throw new JSONException(
					"El JSON de definicion de lote de firmas no esta formado correctamente", e //$NON-NLS-1$
		 		);
		}

		List<UrlParametersToSign> signsUrlParams = new ArrayList<UrlParametersToSign>();

		try {
			signsUrlParams = LocalDataParser.parseJSONToUrlParamsToSign(jsonObject);
		}
		catch (final JSONException e) {
			LOGGER.severe("Error al parsear JSON: " + e); //$NON-NLS-1$
			throw new JSONException(
					"El JSON de definicion de lote de firmas no esta formado correctamente", e //$NON-NLS-1$
		 		);
		}
		catch (final ParameterException e) {
			LOGGER.severe("Error al parsear parametros: " + e); //$NON-NLS-1$
			throw new ParameterException(
					"Uno o varios parametros no son correctos", e //$NON-NLS-1$
		 		);
		}
		catch (final IOException e) {
			LOGGER.severe("Error al leer o escribir datos: " + e); //$NON-NLS-1$
			throw new IOException(
					"Error al leer o escribir datos", e //$NON-NLS-1$
		 		);
		}

		return signsUrlParams;
	}


}
