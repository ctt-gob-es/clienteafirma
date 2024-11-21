package es.gob.afirma.standalone.signdetails;

import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
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

    static Integer signerIndex = new Integer(1);

    private static String signFormat;

    public static final Map<String, String> MIMETYPE_MAPPER;
    static {
    	MIMETYPE_MAPPER = new HashMap<>();
    	MIMETYPE_MAPPER.put("data", SimpleAfirmaMessages.getString("ValidationInfoDialog.24")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/pdf", SimpleAfirmaMessages.getString("ValidationInfoDialog.25")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("text/xml", SimpleAfirmaMessages.getString("ValidationInfoDialog.26")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("plain/xml", SimpleAfirmaMessages.getString("ValidationInfoDialog.26")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application-xml", SimpleAfirmaMessages.getString("ValidationInfoDialog.26")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/octet-stream", SimpleAfirmaMessages.getString("ValidationInfoDialog.24")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/postscript", SimpleAfirmaMessages.getString("ValidationInfoDialog.27")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("text/html", SimpleAfirmaMessages.getString("ValidationInfoDialog.28")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("image/tiff", SimpleAfirmaMessages.getString("ValidationInfoDialog.29")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("image/gif", SimpleAfirmaMessages.getString("ValidationInfoDialog.30")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("image/jpg", SimpleAfirmaMessages.getString("ValidationInfoDialog.39")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("image/jpeg", SimpleAfirmaMessages.getString("ValidationInfoDialog.31")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("image/png", SimpleAfirmaMessages.getString("ValidationInfoDialog.32")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("video/mpeg", SimpleAfirmaMessages.getString("ValidationInfoDialog.33")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("text/sgml", SimpleAfirmaMessages.getString("ValidationInfoDialog.34")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/msword", SimpleAfirmaMessages.getString("ValidationInfoDialog.35")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/vnd.ms-excel", SimpleAfirmaMessages.getString("ValidationInfoDialog.36")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/vnd.ms-project", SimpleAfirmaMessages.getString("ValidationInfoDialog.37")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/vnd.ms-powerpoint", SimpleAfirmaMessages.getString("ValidationInfoDialog.38")); //$NON-NLS-1$ //$NON-NLS-2$
    	MIMETYPE_MAPPER.put("application/vnd.ms-works", SimpleAfirmaMessages.getString("ValidationInfoDialog.39")); //$NON-NLS-1$ //$NON-NLS-2$
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
			signFormat = analyzer.getSignFormat();
			result += "<p><b>Formato</b>: " + analyzer.getSignFormat() + "</p>";  //$NON-NLS-1$ //$NON-NLS-2$
		}
		final String dataLocation = analyzer.getDataLocation();
		if (dataLocation != null) {
			result += "<p><b>Localizaci&oacute;n de los datos: </b>" + dataLocation + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (signDetailsParent.size() > 0 && signDetailsParent.get(0).getDataObjectFormats() != null && signDetailsParent.get(0).getDataObjectFormats().size() > 0) {
			result += "<div><p><b>Informaci&oacute;n sobre datos firmados:</b></p>"; //$NON-NLS-1$
			result += "<ul style=\"margin-bottom: 0;\">"; //$NON-NLS-1$
			for (int i = 0 ; i < signDetailsParent.get(0).getDataObjectFormats().size() ; i++) {
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
				result += "<li>Tipo de certificado: " + type + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
			}
			result += "</ul></div>"; //$NON-NLS-1$
		}
		final AOTreeModel signersTree = analyzer.getSignersTree();
		if (signersTree != null) {
			result += "<div style=\"margin-bottom: 0;\"><p><b>&Aacute;rbol de firmantes:</b><ul style=\"margin-bottom: 0;\">";  //$NON-NLS-1$
			result += parseSignersTree((AOTreeNode) signersTree.getRoot());
			result += "</ul></p></div>"; //$NON-NLS-1$
		}
		signerIndex = new Integer(1);
		final String generalValidationDesc = generalValidation.get(0).validityTypeToString();
		result += "<p><b>Resultado de la validaci&oacute;n</b>: " + generalValidationDesc + "</p>";  //$NON-NLS-1$ //$NON-NLS-2$
		boolean isULAdded = false;

		for (int k = 0 ; k < generalValidation.size(); k++) {
			if (generalValidation.get(k).getError() != null &&
					(VALIDITY_ERROR.MODIFIED_FORM.equals(generalValidation.get(k).getError())
					|| VALIDITY_ERROR.MODIFIED_DOCUMENT.equals(generalValidation.get(k).getError())
					|| VALIDITY_ERROR.OVERLAPPING_SIGNATURE.equals(generalValidation.get(k).getError())
					|| VALIDITY_ERROR.SUSPECTED_SIGNATURE.equals(generalValidation.get(k).getError())
					|| VALIDITY_ERROR.CANT_VALIDATE_EXTERNALLY_DETACHED.equals(generalValidation.get(k).getError())
					|| VALIDITY_ERROR.NO_DATA.equals(generalValidation.get(k).getError())
				)) {
				if (!isULAdded) {
					result += "<ul>"; //$NON-NLS-1$
					isULAdded = true;
				}
				result += "<li>" + generalValidation.get(k) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		for (int i = 0; i < signDetailsParent.size(); i++) {
			for (int m = 0 ; m < signDetailsParent.get(i).getValidityResult().size() ; m++) {
				//Se evitan asi errores duplicados de certificados
				if (signDetailsParent.get(i).getValidityResult().get(m).getError() != null
					&& !signDetailsParent.get(i).getValidityResult().get(m).getError().equals(VALIDITY_ERROR.CERTIFICATE_PROBLEM)
					&& !signDetailsParent.get(i).getValidityResult().get(m).getError().equals(VALIDITY_ERROR.CERTIFICATE_EXPIRED)
					&& !signDetailsParent.get(i).getValidityResult().get(m).getError().equals(VALIDITY_ERROR.CANT_VALIDATE_CERT)
					&& !signDetailsParent.get(i).getValidityResult().get(m).getError().equals(VALIDITY_ERROR.CERTIFICATE_NOT_VALID_YET)) {
						if (!isULAdded) {
							result += "<ul>"; //$NON-NLS-1$
							isULAdded = true;
						}
						result += "<li>Firma " + (i+1) + ": " + signDetailsParent.get(i).getValidityResult().get(m) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
			}

			final CertificateDetails certDetails = signDetailsParent.get(i).getSigner();
			if (certDetails != null) {
				final String validation = (String) certDetails.getValidityResult().get("Validacion"); //$NON-NLS-1$
				if (!SimpleAfirmaMessages.getString("ValidationInfoDialog.40").equals(validation)) { //$NON-NLS-1$
					if (!isULAdded) {
						result += "<ul>"; //$NON-NLS-1$
						isULAdded = true;
					}
					result += "<li>Firma " + (i + 1) + ": " + validation + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
			}
		}
		if (isULAdded) {
			result += "</ul>"; //$NON-NLS-1$
		}
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
			result += "<h1>Firma " + (i+1);  //$NON-NLS-1$
			if (detail.getSigner() != null) {
				final String certName = detail.getSigner().getName();
				if (certName != null && !certName.isEmpty()) {
					result += " : " + certName; //$NON-NLS-1$
				}
			}
			result += "</h1>"; //$NON-NLS-1$
			result += "<div style=\"border:5px outset black;padding-left: 25px;padding-bottom: 10px;\">"; //$NON-NLS-1$
			if (!FacturaESignAnalyzer.FACTURAE.equals(signFormat)) {
				result += "<p><b>Perfil de firma</b>: " + detail.getSignProfile() + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (PAdESSignAnalyzer.PADES.equals(signFormat)) {
				if (detail.getCertificationLevel() > -1 && detail.getCertificationSign() == true) {
					switch (detail.getCertificationLevel()) {
					case 1:
						result += "<p><b>PDF certificado</b>: " + SimpleAfirmaMessages.getString("ValidationInfoDialog.41") + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						break;
					case 2:
						result += "<p><b>PDF certificado</b>: " + SimpleAfirmaMessages.getString("ValidationInfoDialog.42") + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						break;
					case 3:
						result += "<p><b>PDF certificado</b>: " + SimpleAfirmaMessages.getString("ValidationInfoDialog.43") + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						break;
					}
				}
			}
			if (detail.getAlgorithm() != null && !detail.getAlgorithm().isEmpty()) {
				result += "<p><b>Algoritmo de firma</b>: " + detail.getAlgorithm() + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (detail.getSigningTime() != null) {
				result += "<p><b>Fecha y hora de firma</b>: " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(detail.getSigningTime()) + "</p>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
			if (detail.getValidityResult() != null
				&& !detail.getValidityResult().get(0).getValidity().equals(SIGN_DETAIL_TYPE.OK)) {
					result += "<div><br><b>Resultado de la validacion</b> :<ul style=\"margin-bottom: 0;\">"; //$NON-NLS-1$
					for(final SignValidity validity : detail.getValidityResult()) {
						result += "<li>" + validity + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
					}
					result += "</ul></div>"; //$NON-NLS-1$
			}
			if (detail.getPolicy() != null) {
				result += "<div><p><b>Pol&iacute;tica de firma: </b>" ; //$NON-NLS-1$

				if (AOFacturaESigner.POLICY_FACTURAE_30.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| AOFacturaESigner.POLICY_FACTURAE_31.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_XADES_AGE_1_9.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_XADES_AGE_1_8.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_CADES_AGE_1_9.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_CADES_AGE_1_8.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())
					|| SignDetails.POLICY_PADES_AGE_1_9.getPolicyIdentifier().equals(detail.getPolicy().getPolicy().getPolicyIdentifier())){
					result += detail.getPolicy().getName() + "</p>"; //$NON-NLS-1$
				} else {
					result += "<ul style=\"margin-bottom: 0;\">"; //$NON-NLS-1$
					final String name = detail.getPolicy().getName();
					if (name != null && !name.isEmpty()) {
						result += "<li>Descripci&oacute;n: " + name + "</li>"; //$NON-NLS-1$//$NON-NLS-2$
					}
					final String polId = detail.getPolicy().getPolicy().getPolicyIdentifier();
					if (polId != null && !polId.isEmpty()) {
						result += "<li>Identificador: " + polId + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
					}
					final String polIdHash = detail.getPolicy().getPolicy().getPolicyIdentifierHash();
					if (polIdHash != null && !polIdHash.isEmpty()) {
						result += "<li>Hash: " + polIdHash + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
					}
					final String polIdHashAlgo = detail.getPolicy().getPolicy().getPolicyIdentifierHashAlgorithm();
					if (polIdHashAlgo != null && !polIdHashAlgo.isEmpty()) {
						result += "<li>Algoritmo: " + polIdHashAlgo + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
					}
					final String polQual = detail.getPolicy().getPolicy().getPolicyQualifier().toString();
					if (polQual != null && !polQual.isEmpty()) {
						result += "<li>Calificador: " + polQual + "</li>";//$NON-NLS-1$ //$NON-NLS-2$
					}
					result += "</ul></p>"; //$NON-NLS-1$
				}
				result += "</div>"; //$NON-NLS-1$
			}
			if (detail.getMetadata() != null && detail.getMetadata().size() > 0) {
				result += "<div><p><b>Metadatos </b>:</p><ul style=\"margin-bottom: 0;\">"; //$NON-NLS-1$
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
				result += "</ul></div>"; //$NON-NLS-1$
			}

			if (detail.getSigner() != null) {
				result += parseCertificatesToHTML(detail.getSigner());
			}

			result += "</ul></div>"; //$NON-NLS-1$
		}
		return result;
	}

	/**
	 * Transforma los certificados a HTML.
	 * @param certDetail Certificado a transformar.
	 * @return Cadena con los detalles de los certificados en HTML.
	 */
	private static String parseCertificatesToHTML(final CertificateDetails certDetail) {
		String result = "<br><b>Certificado:</b><ul>"; //$NON-NLS-1$
		result += "<li>Nombre: " + certDetail.getName() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
				+ "<li>Emisor: " + certDetail.getIssuerName() + "</li>" //$NON-NLS-1$ //$NON-NLS-2$
				+ "<li>Fecha de caducidad: " + certDetail.getExpirationDate() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$
		if (certDetail.getValidityResult() != null) {
			result += "<li>Resultado de la validacion :<ul>"; //$NON-NLS-1$
			for (final Object key : certDetail.getValidityResult().keySet()) {
				result += "<li>" + key + ": " + certDetail.getValidityResult().getProperty((String) key) + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
			result += "</ul></li>"; //$NON-NLS-1$
		}
		return result;
	}

	/**
	 * Transforma el &aacute;rbol de firmantes a HTML.
	 * @param signersNode &aacuerbol a transformar.
	 * @param signID N&uacute;mero de firma.
	 * @return Cadena en HTML con el &aacuerbol.
	 */
	private static String parseSignersTree(final AOTreeNode signersNode) {
		String result = ""; //$NON-NLS-1$
		for (int i = 0; i < AOTreeModel.getChildCount(signersNode); i++) {
			final AOTreeNode node = (AOTreeNode) AOTreeModel.getChild(signersNode, i);
			result += "<li>Firma " + signerIndex + ": " + node.toString() + "</li>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			signerIndex++;
			if (node.getChildCount() > 0) {
				result += "<ul style=\"margin-bottom: 0;\">"; //$NON-NLS-1$
				result += parseSignersTree(node);
				result += "</ul>"; //$NON-NLS-1$
			}
		}
		return result;
	}

}
