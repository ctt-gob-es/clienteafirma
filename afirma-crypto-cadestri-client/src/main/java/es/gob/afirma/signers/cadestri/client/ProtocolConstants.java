package es.gob.afirma.signers.cadestri.client;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;

final class ProtocolConstants {

	private ProtocolConstants() {
		// No instanciable
	}

	static final String HTTP_CGI = "?"; //$NON-NLS-1$
	static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$

	static final String CADES_FORMAT = "CAdES"; //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	/** Indica si la postfirma requiere el identificador o contenido del documento. */
	static final String PROPERTY_NAME_NEED_DATA = "NEED_DATA"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro de c&oacute;digo de operaci&oacute;n en la URL de llamada al servidor de firma. */
	static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica en la URL del servidor de firma. */
	static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de prefirma en servidor. */
	static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	static String properties2Base64(final Properties p) throws IOException {
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		p.store(baos, ""); //$NON-NLS-1$
		return Base64.encode(baos.toByteArray(), true);
	}

	static Properties base642Properties(final String base64) throws IOException {
		final Properties p = new Properties();
		p.load(new ByteArrayInputStream(Base64.decode(base64, true)));
		return p;
	}

}
