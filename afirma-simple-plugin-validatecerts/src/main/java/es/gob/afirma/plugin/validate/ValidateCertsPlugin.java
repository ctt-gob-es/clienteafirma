package es.gob.afirma.plugin.validate;

import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.PluginControlledException;

/**
 * Plugin para permitir la validaci&oacute;n de certificados.
 */
public class ValidateCertsPlugin extends AfirmaPlugin {

	private static final String INTERNAL_NAME = "validate_certs"; //$NON-NLS-1$
	private static final int VERSION_CODE = 1;

	@Override
	public byte[] preSignProcess(byte[] data, String format) throws PluginControlledException {
		final String log = " ============= PREPROCESAMOS LOS DATOS =================\n" +
				"Formato: " + format;
		Logger.getLogger("es.gob.afirma").info(log);

		return super.preSignProcess(data, format);
	}

	@Override
	public byte[] postSignProcess(byte[] signature, String format, Certificate[] cert) throws PluginControlledException {
		final String log = " ============= POSTPROCESAMOS LA FIRMA =================\n" +
		"Formato: " + format + "\n" +
		"Certificado: " + ((X509Certificate) cert[0]).getSubjectDN();
		Logger.getLogger("es.gob.afirma").info(log);

		return super.postSignProcess(signature, format, cert);
	}

}
