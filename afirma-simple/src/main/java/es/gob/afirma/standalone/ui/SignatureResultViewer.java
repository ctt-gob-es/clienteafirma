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
	 * @param signConfig Configuraci&oacute;n de la firma.
	 * @param signingCert Certificado de firma utilizado.
	 */
	void showResultsInfo(List<SignOperationConfig> signConfig, X509Certificate signingCert);

	/**
	 * Muestra la informaci&oacute;n de una firma.
	 * @param signature Firma generada.
	 * @param signatureFile Fichero en donde se almacena la firma.
	 * @param signingCert Certificado de firma utilizado.
	 */
	void showResultsInfo(byte[] signature, File signatureFile, X509Certificate signingCert);
}
