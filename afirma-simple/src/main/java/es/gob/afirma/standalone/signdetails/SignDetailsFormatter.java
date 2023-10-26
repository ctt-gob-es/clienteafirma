package es.gob.afirma.standalone.signdetails;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PreferencesPanelXades;

public class SignDetailsFormatter {

    public static final Map<String, String> MIMETYPE_MAPPER;

    public static final String STREET_ADDRESS_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.11"); //$NON-NLS-1$
    public static final String POSTAL_CODE_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.8"); //$NON-NLS-1$
    public static final String CITY_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.6"); //$NON-NLS-1$
    public static final String PROVINCE_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.7"); //$NON-NLS-1$
    public static final String COUNTRY_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.9"); //$NON-NLS-1$


    static {
    	MIMETYPE_MAPPER = new HashMap<>();
    	MIMETYPE_MAPPER.put(MimeHelper.DEFAULT_MIMETYPE, "SHA1withRSA"); //$NON-NLS-1$
	}

	public static String formatToHTML(final SignAnalyzer analyzer, final SignValidity generalValidation) {
		String result = ""; //$NON-NLS-1$
		final List<SignDetails> signDetailsParent = analyzer.getAllSignDetails();
		if (analyzer.getSignFormat() != null) {
			result += "<h3>Formato: " + analyzer.getSignFormat() + "</h3>";  //$NON-NLS-1$//$NON-NLS-2$
		}
		final String dataLocation = analyzer.getDataLocation();
		if (dataLocation != null) {
			result += "<h3>Localizaci&oacute;n de los datos: </h3>" + dataLocation;  //$NON-NLS-1$
		}
		if (signDetailsParent.get(0).getDataObjectFormats() != null && signDetailsParent.get(0).getDataObjectFormats().size() > 0) {
			result += "<h3>Informaci&oacute;n sobre datos firmados:</h3>"; //$NON-NLS-1$
			for (int i = 0 ; i < signDetailsParent.get(0).getDataObjectFormats().size() ; i++) {
				result += "<ul>"; //$NON-NLS-1$
				if (signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding() != null && !signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding().isEmpty()) {
					result += "<li>Encoding: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				if (signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription() != null && !signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription().isEmpty()) {
					result += "<li>Descripcion: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				result += "<li>Tipo: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getMimeType() + "</li></ul>"; //$NON-NLS-1$ //$NON-NLS-2$
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
				result += "Pol&iacute;tica de firma: <ul>" ; //$NON-NLS-1$

				if (detail.getPolicy().getName() != null && !detail.getPolicy().getName().isEmpty()) {
					result += "<li>Descripci&oacute;n: " + detail.getPolicy().getName() + "</li>" ; //$NON-NLS-1$//$NON-NLS-2$
				}

				if (!AOFacturaESigner.POLICY_FACTURAE_30.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					&& !AOFacturaESigner.POLICY_FACTURAE_31.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					&& !PreferencesPanelXades.POLICY_XADES_AGE_1_9.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())){

					result += "<li>Identificador: " + detail.getPolicy().getPolicy().getPolicyIdentifier() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Hash: " + detail.getPolicy().getPolicy().getPolicyIdentifierHash() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Algoritmo: " + detail.getPolicy().getPolicy().getPolicyIdentifierHashAlgorithm() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Calificador: " + detail.getPolicy().getPolicy().getPolicyQualifier() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				result += "</ul>"; //$NON-NLS-1$
			}
			if (detail.getMetadata() != null && detail.getMetadata().size() > 0) {
				result += "<li><b>Metadatos </b>:<ul>"; //$NON-NLS-1$
				String claimedRoles = ""; //$NON-NLS-1$
				String metadata = ""; //$NON-NLS-1$
				for(final Object key : detail.getMetadata().keySet()) {
					if (((String) key).startsWith("claimedRole")) { //$NON-NLS-1$
						claimedRoles += detail.getMetadata().get(key) + ","; //$NON-NLS-1$
					}
				}

				if (detail.getMetadata().containsKey(STREET_ADDRESS_METADATA)) {
					metadata += "<li>" + STREET_ADDRESS_METADATA + ": " + detail.getMetadata().get(STREET_ADDRESS_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(POSTAL_CODE_METADATA)) {
					metadata += "<li>" + POSTAL_CODE_METADATA + ": " + detail.getMetadata().get(POSTAL_CODE_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(CITY_METADATA)) {
					metadata += "<li>" + CITY_METADATA + ": " + detail.getMetadata().get(CITY_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(PROVINCE_METADATA)) {
					metadata += "<li>" + PROVINCE_METADATA + ": " + detail.getMetadata().get(PROVINCE_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(COUNTRY_METADATA)) {
					metadata += "<li>" + COUNTRY_METADATA + ": " + detail.getMetadata().get(COUNTRY_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (!claimedRoles.isEmpty()) {
					result += "<li>Roles: " + claimedRoles.substring(0, claimedRoles.length() - 1) + "</li>";  //$NON-NLS-1$//$NON-NLS-2$
				}
				result += metadata;
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
