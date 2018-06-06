package es.gob.afirma.standalone.ui;

import java.io.File;
import java.security.cert.X509Certificate;
import java.util.List;

/**
 * Visor de resultados de firma.
 */
public interface SignatureResultViewer {

	/**
	 * Muestra la informaci&oacute;n de un conjunto de firmas.
	 * @param signConfig Configuraci&oacute;n de las firmas generadas.
	 * @param outDir Directorio en el que se almacenan las firmas.
	 * @param signingCert Certificado de firma utilizado.
	 */
	void showResultsInfo(List<SignOperationConfig> signConfig, File outDir, X509Certificate signingCert);

	/**
	 * Muestra la informaci&oacute;n de una firma.
	 * @param signature Firma generada.
	 * @param signConfig Configuraci&oacute;n de la firma generada.
	 * @param signingCert Certificado de firma utilizado.
	 */
	void showResultsInfo(byte[] signature, SignOperationConfig signConfig, X509Certificate signingCert);
}
