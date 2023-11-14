package es.gob.afirma.standalone.signdetails;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class SignDetailsFormatter {

    public static final String STREET_ADDRESS_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.11"); //$NON-NLS-1$
    public static final String POSTAL_CODE_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.8"); //$NON-NLS-1$
    public static final String CITY_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.6"); //$NON-NLS-1$
    public static final String PROVINCE_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.7"); //$NON-NLS-1$
    public static final String COUNTRY_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.9"); //$NON-NLS-1$
    public static final String SIGN_REASON_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.18"); //$NON-NLS-1$
    public static final String LOCATION_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.19"); //$NON-NLS-1$
    public static final String CONTACT_INFO_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.20"); //$NON-NLS-1$
    public static final String LOCALITY_METADATA = SimpleAfirmaMessages.getString("ValidationInfoDialog.23"); //$NON-NLS-1$

    public static final Map<String, String> MIMETYPE_MAPPER;
    static {
    	MIMETYPE_MAPPER = new HashMap<>();
    	MIMETYPE_MAPPER.put("data", SimpleAfirmaMessages.getString("ValidationInfoDialog.24")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/pdf", SimpleAfirmaMessages.getString("ValidationInfoDialog.25")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("text/xml", SimpleAfirmaMessages.getString("ValidationInfoDialog.26")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/octet-stream", SimpleAfirmaMessages.getString("ValidationInfoDialog.27")); //$NON-NLS-1$ //$NON-NLS-2$
	}

    /**
     * Transforma las firmas analizadas y validaciones a formato HTML.
     * @param analyzer Analizador que contiene las firmas.
     * @param generalValidation Validacioacute;n de la firma.
     * @return Cadena en HTML.
     */
	public static String formatToHTML(final SignAnalyzer analyzer, final List<SignValidity> generalValidation) {
		String result = ""; //$NON-NLS-1$
		final List<SignDetails> signDetailsParent = analyzer.getAllSignDetails();
		if (analyzer.getSignFormat() != null) {
			result += "<p><b>Formato</b>: " + analyzer.getSignFormat() + "</p>";  //$NON-NLS-1$ //$NON-NLS-2$
		}
		final String dataLocation = analyzer.getDataLocation();
		if (dataLocation != null) {
			result += "<p><b>Localizaci&oacute;n de los datos: </b>" + dataLocation + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (signDetailsParent.get(0).getDataObjectFormats() != null && signDetailsParent.get(0).getDataObjectFormats().size() > 0) {
			result += "<br><b>Informaci&oacute;n sobre datos firmados:</b>"; //$NON-NLS-1$
			for (int i = 0 ; i < signDetailsParent.get(0).getDataObjectFormats().size() ; i++) {
				result += "<ul>"; //$NON-NLS-1$
				if (signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding() != null && !signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding().isEmpty()) {
					result += "<li>Encoding: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getEncoding() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				if (signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription() != null && !signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription().isEmpty()) {
					result += "<li>Descripcion: " + signDetailsParent.get(0).getDataObjectFormats().get(i).getDescription() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				String type = signDetailsParent.get(0).getDataObjectFormats().get(i).getMimeType();
				if (MIMETYPE_MAPPER.containsKey(type)) {
					type = MIMETYPE_MAPPER.get(type);
				}
				result += "<li>Tipo: " + type + "</li></ul>"; //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		final AOTreeModel signersTree = analyzer.getSignersTree();
		result += "<br><b>&Aacute;rbol de firmantes:</b>";  //$NON-NLS-1$
		result += parseSignersTree((AOTreeNode) signersTree.getRoot());
		result += "<b>Resultado de la validaci&oacute;n</b>: " + generalValidation.get(0) +"<ul>";  //$NON-NLS-1$ //$NON-NLS-2$
		for (int i = 0 ; i < signDetailsParent.size() ; i++) {
			for (int j = 0 ; j < signDetailsParent.get(i).getValidityResult().size() ; j++) {
				result += "<li>Firma " + (i+1) + ": " + signDetailsParent.get(i).getValidityResult().get(j) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		}
		result += "</ul>"; //$NON-NLS-1$
		result += parseSignsToHTML(signDetailsParent);
		return result;
	}

	/**
	 * Transforma las firmas en HTML.
	 * @param details Firmas analizadas.
	 * @return Cadena en HTML.
	 */
	private static String parseSignsToHTML(final List <SignDetails> details) {
		String result = ""; //$NON-NLS-1$
		for (int i = 0; i < details.size() ; i++) {
			final SignDetails detail = details.get(i);
			result += "<h1>Firma " + (i+1) + "</h1>";  //$NON-NLS-1$//$NON-NLS-2$
			result += "<div style=\"border:5px outset black;padding-left: 25px;\"><p><b>Perfil de firma</b>: " + detail.getSignProfile() + "</p>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<p><b>Algoritmo de firma</b>: " + detail.getAlgorithm() + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$
			if (detail.getValidityResult() != null) {
				result += "<br><b>Resultado de la validacion</b> :<ul>"; //$NON-NLS-1$
				for(final SignValidity validity : detail.getValidityResult()) {
				   result += "<li>" + validity + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
				}
				result += "</ul>"; //$NON-NLS-1$
			}
			if (detail.getPolicy() != null) {
				result += "<b>Pol&iacute;tica de firma</b>" ; //$NON-NLS-1$

				if (AOFacturaESigner.POLICY_FACTURAE_30.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| AOFacturaESigner.POLICY_FACTURAE_31.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_XADES_AGE_1_9.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_CADES_AGE_1_9.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_PADES_AGE_1_9.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())){
					result += ": " + detail.getPolicy().getName() ; //$NON-NLS-1$
				} else {
					result += "<ul><li>Descripci&oacute;n: " + detail.getPolicy().getName() + "</li>" //$NON-NLS-1$//$NON-NLS-2$
					+ "<li>Identificador: " + detail.getPolicy().getPolicy().getPolicyIdentifier() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Hash: " + detail.getPolicy().getPolicy().getPolicyIdentifierHash() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Algoritmo: " + detail.getPolicy().getPolicy().getPolicyIdentifierHashAlgorithm() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
					+ "<li>Calificador: " + detail.getPolicy().getPolicy().getPolicyQualifier() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
					result += "</ul>"; //$NON-NLS-1$
				}

			}
			if (detail.getMetadata() != null && detail.getMetadata().size() > 0) {
				result += "<p><b>Metadatos </b>:<ul>"; //$NON-NLS-1$
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

				if (detail.getMetadata().containsKey(LOCALITY_METADATA)) {
					metadata += "<li>" + LOCALITY_METADATA + ": " + detail.getMetadata().get(LOCALITY_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(PROVINCE_METADATA)) {
					metadata += "<li>" + PROVINCE_METADATA + ": " + detail.getMetadata().get(PROVINCE_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(COUNTRY_METADATA)) {
					metadata += "<li>" + COUNTRY_METADATA + ": " + detail.getMetadata().get(COUNTRY_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(SIGN_REASON_METADATA)) {
					metadata += "<li>" + SIGN_REASON_METADATA + ": " + detail.getMetadata().get(SIGN_REASON_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(LOCATION_METADATA)) {
					metadata += "<li>" + LOCATION_METADATA + ": " + detail.getMetadata().get(LOCATION_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (detail.getMetadata().containsKey(CONTACT_INFO_METADATA)) {
					metadata += "<li>" + CONTACT_INFO_METADATA + ": " + detail.getMetadata().get(CONTACT_INFO_METADATA) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}

				if (!claimedRoles.isEmpty()) {
					result += "<li>Roles: " + claimedRoles.substring(0, claimedRoles.length() - 1) + "</li>";  //$NON-NLS-1$//$NON-NLS-2$
				}
				result += metadata;
				result += "</ul></p>"; //$NON-NLS-1$
			}

			result += parseCertificatesToHTML(detail.getSigners());

			result += "</ul></div>"; //$NON-NLS-1$
		}
		return result;
	}

	/**
	 * Transforma los certificados a HTML.
	 * @param certsDetails Certificados a transformar.
	 * @return Cadena con los detalles de los certificados en HTML.
	 */
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

	/**
	 * Transforma el &aacute;rbol de firmantes a HTML.
	 * @param signersNode &aacuerbol a transformar.
	 * @return Cadena en HTML con el &aacuerbol.
	 */
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
