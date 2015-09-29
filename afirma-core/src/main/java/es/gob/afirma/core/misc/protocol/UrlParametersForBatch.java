package es.gob.afirma.core.misc.protocol;

import java.net.URL;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import es.gob.afirma.core.misc.AOUtil;

/** Par&aacute;metros para el proceso de lotes de firmas predefinidos en XML.
 * En este caso, los datos son el XML de definici&oacute;n de lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class UrlParametersForBatch extends UrlParameters {

	/** N&uacute;mero m&aacute;ximo de caracteres permitidos para el identificador de sesi&oacute;n de la operaci&oacute;n. */
	private static final int MAX_ID_LENGTH = 20;

	/** Par&aacute;metro de entrada con el identificador de sesi&oacute;n de la operaci&oacute;n. */
	private static final String PARAM_ID = "id"; //$NON-NLS-1$

	private static final String PARAM_BATCH_POSTSIGNER = "batchpostsignerurl"; //$NON-NLS-1$
	private static final String PARAM_BATCH_PRESIGNER = "batchpresignerurl"; //$NON-NLS-1$

	private String batchPreSignerUrl = null;
	private String batchPostSignerUrl = null;

	/** Obtiene la URL del servicio de preprocesado de lotes de firma.
	 * @return URL del servicio de preprocesado de lotes de firma. */
	public String getBatchPresignerUrl() {
		return this.batchPreSignerUrl;
	}

	void setBatchPresignerUrl(final String url) {
		this.batchPreSignerUrl = url;
	}

	/** Obtiene la URL del servicio de preprocesado de lotes de firma.
	 * @return URL del servicio de preprocesado de lotes de firma. */
	public String getBatchPostSignerUrl() {
		return this.batchPostSignerUrl;
	}

	void setBatchPostsignerUrl(final String url) {
		this.batchPostSignerUrl = url;
	}

	void setBatchParameters(final Map<String, String> params) throws ParameterException {

		if (!params.containsKey(PARAM_BATCH_POSTSIGNER)) {
			throw new ParameterException(
				"No se ha recibido la URL del postprocesador de lotes" //$NON-NLS-1$
			);
		}
		if (!params.containsKey(PARAM_BATCH_PRESIGNER)) {
			throw new ParameterException(
				"No se ha recibido la URL del preprocesador de lotes" //$NON-NLS-1$
			);
		}

		// idSession para el service Web. Con socket no se usa
		if (params.containsKey(PARAM_ID) || params.containsKey(FILE_ID_PARAM)) {
			// Comprobamos que el identificador de sesion de la firma no sea mayor de un cierto numero de caracteres
			final String signatureSessionId = params.containsKey(PARAM_ID) ? params.get(PARAM_ID) : params.get(FILE_ID_PARAM);
			if (signatureSessionId.length() > MAX_ID_LENGTH) {
				throw new ParameterException("La longitud del identificador para la firma es mayor de " + MAX_ID_LENGTH + " caracteres."); //$NON-NLS-1$ //$NON-NLS-2$
			}

			// Comprobamos que el identificador de sesion de la firma sea alfanumerico (se usara como nombre de fichero)
			for (final char c : signatureSessionId.toLowerCase(Locale.ENGLISH).toCharArray()) {
				if ((c < 'a' || c > 'z') && (c < '0' || c > '9')) {
					throw new ParameterException("El identificador de la firma debe ser alfanumerico."); //$NON-NLS-1$
				}
			}

			setSessionId(signatureSessionId);
		}

		setDefaultKeyStore(verifyDefaultKeyStoreName(params));

		setBatchPostsignerUrl(
			validateURL(
				params.get(PARAM_BATCH_POSTSIGNER)
			).toString()
		);

		setBatchPresignerUrl(
			validateURL(
				params.get(PARAM_BATCH_PRESIGNER)
			).toString()
		);

		// Comprobamos la validez de la URL del servlet de guardado en caso de indicarse
		if (params.containsKey(STORAGE_SERVLET_PARAM)) {

			// Comprobamos que la URL sea valida
			URL storageServletUrl;
			try {
				storageServletUrl = validateURL(params.get(STORAGE_SERVLET_PARAM));
			}
			catch (final ParameterLocalAccessRequestedException e) {
				throw new ParameterLocalAccessRequestedException("La URL del servicio de guardado no puede ser local", e); //$NON-NLS-1$
			}
			catch (final ParameterException e) {
				throw new ParameterException("Error al validar la URL del servicio de guardado: " + e, e); //$NON-NLS-1$
			}
			setStorageServletUrl(storageServletUrl);
		}

		String props = null;
		if (params.containsKey(PROPERTIES_PARAM)) {
			props = params.get(PROPERTIES_PARAM);
		}

		if (props != null) {
			try {
				setExtraParams(AOUtil.base642Properties(props));
			}
			catch (final Exception e) {
				setExtraParams(new Properties());
			}
		}
		else {
			setExtraParams(new Properties());
		}

		setDefaultKeyStore(verifyDefaultKeyStoreName(params));
	}

}
