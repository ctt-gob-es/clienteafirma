package es.gob.afirma.standalone.signdetails;

import java.util.List;

public class SignDetailsFormatter {

	public static String formatToHTML(final SignAnalyzer analyzer) {
		String result = ""; //$NON-NLS-1$
		final List<SignDetails> signDetailsParent = analyzer.getAllSignDetails();
		final String dataLocation = analyzer.getDataLocation();
		if (dataLocation != null) {
			result += "<h3>Localizaci&oacute;n de los datos: " + dataLocation + "</h3>";  //$NON-NLS-1$//$NON-NLS-2$
		}
		result += "<h3>Formato: " + analyzer.getSignFormat() + "</h3>";  //$NON-NLS-1$//$NON-NLS-2$
		result += parseSignsToHTML(signDetailsParent);
		return result;
	}

	private static String parseSignsToHTML(final List <SignDetails> details) {
		String result = ""; //$NON-NLS-1$
		for (final SignDetails detail : details) {
			result += "<div style=\"border:5px solid black;border-radius:25px;\"><ul><li><b>Formato</b>: " + detail.getFormat() + "</li>" //$NON-NLS-1$
					+ "<li><b>Algoritmo de firma</b>: " + detail.getAlgorithm()  + "</li>";//$NON-NLS-1$
			if (detail.getPolicy() != null) {
				result += "<li><b>Pol&iacute;tica de firma</b>: " + detail.getPolicy().getName() + "</li>"
						+ "<ul><li>Identificador: " + detail.getPolicy().getPolicy().getPolicyIdentifier() + "</li>"
						+ "<li>Hash: " + detail.getPolicy().getPolicy().getPolicyIdentifierHash() + "</li>"
						+ "<li>Algoritmo: " + detail.getPolicy().getPolicy().getPolicyIdentifierHashAlgorithm() + "</li>"
						+ "<li>Calificador: " + detail.getPolicy().getPolicy().getPolicyQualifier() + "</li></ul>";

			}
			if (detail.getMetadata() != null && detail.getMetadata().size() > 0) {
				result += "<li><b>Metadatos </b>:<ul>"; //$NON-NLS-1$
				for(final Object key : detail.getMetadata().keySet()) {
				   result += "<li>" + key + ": " + detail.getMetadata().getProperty((String) key) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
				result += "</ul></li>"; //$NON-NLS-1$
			}
			if (detail.getDataObjectFormats() != null && detail.getDataObjectFormats().size() > 0) {
				result += "<li><b>Informaci&oacute;n sobre datos firmados:</b>";
				for (int i = 0 ; i < detail.getDataObjectFormats().size() ; i++) {
					result += "<ul><li>Referencia: " + detail.getDataObjectFormats().get(i).getIdentifier() + "</li>";
					if (detail.getDataObjectFormats().get(i).getDescription() != null) {
						result += "<li>Descripcion: " + detail.getDataObjectFormats().get(i).getDescription() + "</li>";
					}
					if (detail.getDataObjectFormats().get(i).getEncoding() != null) {
						result += "<li>Encoding: " + detail.getDataObjectFormats().get(i).getEncoding() + "</li>";
					}
					result += "<li>Mimetype: " + detail.getDataObjectFormats().get(i).getMimeType() + "</li></ul>";
				}
				result += "</li>";
			}
			if (detail.getValidityResult() != null) {
				result += "<br>Resultado de la validacion :<ul>"; //$NON-NLS-1$
				for(final Object key : detail.getValidityResult().keySet()) {
				   result += "<li>" + key + ": " + detail.getValidityResult().getProperty((String) key) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
				result += "</ul>"; //$NON-NLS-1$
			}

			result += parseCertificatesToHTML(detail.getSigners());

			result += "</ul></div>"; //$NON-NLS-1$
		}
		return result;
	}

	private static String parseCertificatesToHTML(final List <CertificateDetails> certsDetails) {
		String result = "&nbsp&nbsp<b><u>Certificados</u></b><ul>"; //$NON-NLS-1$
		for (final CertificateDetails cert : certsDetails) {
			result += "<li>Nombre: " + cert.getName() + "</li>" //$NON-NLS-1$
					+ "<li>Emisor: " + cert.getIssuerName() + "</li>" //$NON-NLS-1$
					+ "<li>Fecha de caducidad: " + cert.getExpirationDate() + "</li>"; //$NON-NLS-1$
			if (cert.getValidityResult() != null) {
				result += "<li>Resultado de la validacion :<ul>"; //$NON-NLS-1$
				for(final Object key : cert.getValidityResult().keySet()) {
				   result += "<li>" + key + ": " + cert.getValidityResult().getProperty((String) key) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
				result += "</ul></li>"; //$NON-NLS-1$
			}
		}
		return result;
	}

}
