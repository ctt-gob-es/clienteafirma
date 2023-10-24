package es.gob.afirma.standalone.signdetails;

import java.util.List;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signvalidation.SignValidity;

public class SignDetailsFormatter {

	public static String formatToHTML(final SignAnalyzer analyzer, final SignValidity generalValidation) {
		String result = ""; //$NON-NLS-1$
		final List<SignDetails> signDetailsParent = analyzer.getAllSignDetails();
		result += "<h3>Formato: " + analyzer.getSignFormat() + "</h3>";  //$NON-NLS-1$//$NON-NLS-2$
		final String dataLocation = analyzer.getDataLocation();
		if (dataLocation != null) {
			result += "<h3>Localizaci&oacute;n de los datos: " + dataLocation + "</h3>";  //$NON-NLS-1$//$NON-NLS-2$
		}
		if (signDetailsParent.get(0).getDataObjectFormats() != null && signDetailsParent.get(0).getDataObjectFormats().size() > 0) {
			result += "<h3>Informaci&oacute;n sobre datos firmados:</h3>"; //$NON-NLS-1$
			for (int i = 0 ; i < signDetailsParent.get(0).getDataObjectFormats().size() ; i++) {
				result += "<ul>"; //$NON-NLS-1$
				if (signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding() != null) {
					result += "<li>Encoding: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				if (signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription() != null) {
					result += "<li>Descripcion: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				result += "<li>Mimetype: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getMimeType() + "</li></ul>"; //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		final AOTreeModel signersTree = analyzer.getSignersTree();
		result += "<h3>&Aacute;rbol de firmantes:</h3>";  //$NON-NLS-1$
		result += parseSignersTree((AOTreeNode) signersTree.getRoot());
		result += "<h3>Resultado de la validaci&oacute;n: </h3>" + generalValidation;  //$NON-NLS-1$
		result += parseSignsToHTML(signDetailsParent);
		return result;
	}

	private static String parseSignsToHTML(final List <SignDetails> details) {
		String result = ""; //$NON-NLS-1$
		for (final SignDetails detail : details) {
			result += "<div style=\"border:5px solid black;border-radius:25px;\"><ul><li><b>Perfil de firma</b>: " + detail.getFormat() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li><b>Algoritmo de firma</b>: " + detail.getAlgorithm()  + "</li>" ;//$NON-NLS-1$ //$NON-NLS-2$
			if (detail.getValidityResult() != null) {
				result += "<br>Resultado de la validacion :<ul>"; //$NON-NLS-1$
				for(final SignValidity validity : detail.getValidityResult()) {
				   result += "<li>" + validity + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				result += "</ul>"; //$NON-NLS-1$
			}
			if (detail.getPolicy() != null) {
				result += "<li><b>Pol&iacute;tica de firma</b>: " + detail.getPolicy().getName() + "</li>"  //$NON-NLS-1$//$NON-NLS-2$
						+ "<ul><li>Identificador: " + detail.getPolicy().getPolicy().getPolicyIdentifier() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
						+ "<li>Hash: " + detail.getPolicy().getPolicy().getPolicyIdentifierHash() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
						+ "<li>Algoritmo: " + detail.getPolicy().getPolicy().getPolicyIdentifierHashAlgorithm() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
						+ "<li>Calificador: " + detail.getPolicy().getPolicy().getPolicyQualifier() + "</li></ul>"; //$NON-NLS-1$ //$NON-NLS-2$

			}
			if (detail.getMetadata() != null && detail.getMetadata().size() > 0) {
				result += "<li><b>Metadatos </b>:<ul>"; //$NON-NLS-1$
				for(final Object key : detail.getMetadata().keySet()) {
				   result += "<li>" + key + ": " + detail.getMetadata().getProperty((String) key) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
				result += "</ul></li>"; //$NON-NLS-1$
			}

			result += parseCertificatesToHTML(detail.getSigners());

			result += "</ul></div>"; //$NON-NLS-1$
		}
		return result;
	}

	private static String parseCertificatesToHTML(final List <CertificateDetails> certsDetails) {
		String result = "&nbsp&nbsp<b><u>Certificados</u></b><ul>"; //$NON-NLS-1$
		for (final CertificateDetails cert : certsDetails) {
			result += "<li>Nombre: " + cert.getName() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Emisor: " + cert.getIssuerName() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Fecha de caducidad: " + cert.getExpirationDate() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
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

	private static String parseSignersTree(final AOTreeNode signersNode) {
		String result = "<ul>"; //$NON-NLS-1$
		for (int i = 0; i < AOTreeModel.getChildCount(signersNode); i++) {
			final AOTreeNode node = (AOTreeNode) AOTreeModel.getChild(signersNode, i);
			result += "<li>" + node.toString() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
			if (node.getChildCount() > 0) {
				result += parseSignersTree(node);
			}
		}
		result += "</ul>"; //$NON-NLS-1$
		return result;
	}

}
