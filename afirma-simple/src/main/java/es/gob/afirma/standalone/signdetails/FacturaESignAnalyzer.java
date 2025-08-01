package es.gob.afirma.standalone.signdetails;

import java.io.ByteArrayInputStream;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.XAdESConstants;
import es.gob.afirma.signers.xades.XAdESUtil;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignatureFormatDetectorXades;
import es.gob.afirma.signvalidation.ValidateXMLSignature;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;

public class FacturaESignAnalyzer implements SignAnalyzer {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	List <SignDetails> signDetailsList;
	List<CertificateDetails> certDetailsList;
	Document signDocument;
	AOTreeModel signersTree;

	public static final String FACTURAE = "Factura Electr&oacute;nica"; //$NON-NLS-1$s

    public static final String URL_SHA1_RSA    = "http://www.w3.org/2000/09/xmldsig#rsa-sha1"; //$NON-NLS-1$
    private static final String URL_SHA256_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"; //$NON-NLS-1$
    private static final String URL_SHA384_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"; //$NON-NLS-1$
    private static final String URL_SHA512_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"; //$NON-NLS-1$
    public static final String URL_SHA1_ECDSA   = "http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha1"; //$NON-NLS-1$
    private static final String URL_SHA256_ECDSA  = "http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha256"; //$NON-NLS-1$
    private static final String URL_SHA384_ECDSA  = "http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha384"; //$NON-NLS-1$
    private static final String URL_SHA512_ECDSA  = "http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha512"; //$NON-NLS-1$

    public static final Map<String, String> SIGN_ALGOS_URI;
    public static final Map<String, String> FACTURAE_CLAIMED_ROLES;

    static {
    	SIGN_ALGOS_URI = new HashMap<>();
    	SIGN_ALGOS_URI.put(URL_SHA1_RSA, "SHA1withRSA"); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put(URL_SHA256_RSA, "SHA256withRSA"); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put(URL_SHA384_RSA, "SHA384withRSA"); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put(URL_SHA512_RSA, "SHA512withRSA"); //$NON-NLS-1$
		SIGN_ALGOS_URI.put(URL_SHA1_ECDSA, "SHA1withECDSA"); //$NON-NLS-1$
		SIGN_ALGOS_URI.put(URL_SHA256_ECDSA, "SHA256withECDSA"); //$NON-NLS-1$
		SIGN_ALGOS_URI.put(URL_SHA384_ECDSA, "SHA384withECDSA"); //$NON-NLS-1$
		SIGN_ALGOS_URI.put(URL_SHA512_ECDSA, "SHA512withECDSA"); //$NON-NLS-1$

    	FACTURAE_CLAIMED_ROLES = new HashMap<>();
    	FACTURAE_CLAIMED_ROLES.put("emisor", "Emisor"); //$NON-NLS-1$ //$NON-NLS-2$
    	FACTURAE_CLAIMED_ROLES.put("supplier", "Emisor"); //$NON-NLS-1$ //$NON-NLS-2$
    	FACTURAE_CLAIMED_ROLES.put("receptor", "Receptor"); //$NON-NLS-1$ //$NON-NLS-2$
    	FACTURAE_CLAIMED_ROLES.put("customer", "Receptor"); //$NON-NLS-1$ //$NON-NLS-2$
    	FACTURAE_CLAIMED_ROLES.put("tercero", "Tercero"); //$NON-NLS-1$ //$NON-NLS-2$
    	FACTURAE_CLAIMED_ROLES.put("third party", "Tercero"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	static final String[] SUPPORTED_XADES_NAMESPACE_URIS = new String[] {
			XAdESConstants.NAMESPACE_XADES_NO_VERSION,
		    XAdESConstants.NAMESPACE_XADES_1_2_2,
		    XAdESConstants.NAMESPACE_XADES_1_3_2,
		    XAdESConstants.NAMESPACE_XADES_1_4_1
	};

	public FacturaESignAnalyzer(final byte [] data) throws Exception {
    	try {

            CompleteSignInfo signInfo;
            signInfo = SignAnalyzer.getSignInfo(data);

            this.signersTree = signInfo.getSignsTree();
    		this.signDetailsList = new ArrayList<>();
    		this.certDetailsList = new ArrayList<>();
    		this.signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(data));
    		final NodeList signaturesList = this.signDocument.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
    		createSignDetails(signaturesList);
    	}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}
	}

	@Override
	public AOTreeModel getSignersTree() {
		return this.signersTree;
	}

	@Override
	public List<SignDetails> getAllSignDetails() {
		return this.signDetailsList;
	}

	@Override
	public String getSignFormat() {
		return FACTURAE;
	}

	@Override
	public String getDataLocation() {
        return null;
	}

	/**
	 * Construye los detalles de la firma.
	 * @param signaturesList Lista con datos de firmas.
	 * @throws AOInvalidFormatException Error formateando los datos.
	 */
	private void createSignDetails(final NodeList signaturesList) throws AOInvalidFormatException {
    	try {
    		for (int i = 0 ; i < signaturesList.getLength() ; i++) {
    			final Element signature = (Element) signaturesList.item(i);
    			final String signProfile = SignatureFormatDetectorXades.resolveSignerXAdESFormat(signature);
    			final SignDetails signDetails = buildSignDetails(signature, signProfile);
    			final List<SignValidity> validity = ValidateXMLSignature.validateSign(signature, signProfile, false);
    			signDetails.setValidityResult(validity);
    			this.signDetailsList.add(signDetails);
    		}
    	}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}
	}

	/**
	 * Construye los detalles de una firma a partir de un elemento CML.
	 * @param signElement Elemento XML con datos de la firma.
	 * @param signProfile Perfil de la firma.
	 * @return Detalles de la firma.
	 * @throws AOInvalidFormatException Error formateando datos.
	 */
	private static SignDetails buildSignDetails(final Element signElement, final String signProfile) throws AOInvalidFormatException {

		final SignDetails xadesSignDetails = new SignDetails();
		xadesSignDetails.setSignProfile(signProfile);
		final Element signatureMethodElement = XAdESUtil.getSignatureMethodElement(signElement);
		if (signatureMethodElement == null) {
			throw new AOInvalidFormatException("El elemento SignatureMethod no existe"); //$NON-NLS-1$
		}
		String algorithm = SIGN_ALGOS_URI.get(signatureMethodElement.getAttribute("Algorithm")); //$NON-NLS-1$
		if (algorithm == null) {
			algorithm = signatureMethodElement.getAttribute("Algorithm"); //$NON-NLS-1$
		}
		xadesSignDetails.setAlgorithm(algorithm);

		// FIRMANTE
		final CertificateDetails cert = buildCertDetails(signElement.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Data").item(0)); //$NON-NLS-1$
		xadesSignDetails.setSigner(cert);

		// QUALIFYING PROPERTIES
		final NodeList qualifyingPropsNodeList = signElement
				.getElementsByTagNameNS("*", XAdESConstants.TAG_QUALIFYING_PROPERTIES); //$NON-NLS-1$

		boolean existingNamespace = false;

		if (qualifyingPropsNodeList.getLength() > 0) {
			final Element qualifyingProps = (Element) qualifyingPropsNodeList.item(0);
			final String namespaceUri = qualifyingProps.getNamespaceURI();

			for (final String xadesNameSpace : SUPPORTED_XADES_NAMESPACE_URIS) {
				if (xadesNameSpace.equals(namespaceUri)) {
					existingNamespace = true;
				}
			}

			if (!existingNamespace) {
				throw new AOInvalidFormatException(
						"Una de las firmas encontradas en el documento contiene una version inexistente de XAdES"); //$NON-NLS-1$
			}

			// SIGNING TIME
			final Date signingTime = obtainSigningTime(qualifyingProps, namespaceUri);
			xadesSignDetails.setSigningTime(signingTime);

			// POLITICA
			final NodeList signaturePolicyIdentifierList = qualifyingProps.getElementsByTagNameNS(namespaceUri,
					XAdESConstants.TAG_SIGNATURE_POLICY_IDENTIFIER);
			if (signaturePolicyIdentifierList != null && signaturePolicyIdentifierList.getLength() > 0) {

				final Element signPolicyIdElement = (Element) signaturePolicyIdentifierList.item(0);
				final String policyOID = signPolicyIdElement
						.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_IDENTIFIER).item(0).getFirstChild()
						.getNodeValue();

				final Node descriptionElement = signPolicyIdElement.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_DESCRIPTION).item(0);
				String descriptionPolicy = ""; //$NON-NLS-1$
				if (descriptionElement != null) {
					descriptionPolicy = descriptionElement.getTextContent().trim();
				}

				if (AOFacturaESigner.POLICY_FACTURAE_30.getPolicyIdentifier().equals(policyOID)) {
					final SignaturePolicy facturaE30signPolicy = new SignaturePolicy(
							SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.1"), //$NON-NLS-1$
							AOFacturaESigner.POLICY_FACTURAE_30);
					xadesSignDetails.setPolicy(facturaE30signPolicy);
				} else if (AOFacturaESigner.POLICY_FACTURAE_31.getPolicyIdentifier().equals(policyOID))  {
					final SignaturePolicy facturaE31signPolicy = new SignaturePolicy(
							SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.2"), //$NON-NLS-1$
							AOFacturaESigner.POLICY_FACTURAE_31);
					xadesSignDetails.setPolicy(facturaE31signPolicy);
				} else {
					final String signElementNS = signElement.getNamespaceURI();
					final String digestValue = signPolicyIdElement
							.getElementsByTagNameNS(signElementNS, XAdESConstants.TAG_DIGEST_VALUE).item(0).getTextContent().trim();
					final String digestMethod = ((Element) signPolicyIdElement
							.getElementsByTagNameNS(signElementNS, XAdESConstants.TAG_DIGEST_METHOD).item(0))
									.getAttribute("Algorithm"); //$NON-NLS-1$
					String spuri = ""; //$NON-NLS-1$
					final NodeList spuriNodeList = signPolicyIdElement.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_SPURI);
					if (spuriNodeList.getLength() > 0) {
						spuri = signPolicyIdElement.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_SPURI)
								.item(0).getTextContent().trim();
					}

					final AdESPolicy policy = new AdESPolicy(policyOID, digestValue, digestMethod, spuri);

					final SignaturePolicy signPolicy = new SignaturePolicy(descriptionPolicy, policy);
					xadesSignDetails.setPolicy(signPolicy);
				}

			}

			// METADATOS
			final NodeList signatureProductionPlaceList = qualifyingProps.getElementsByTagNameNS(namespaceUri,
					XAdESConstants.TAG_SIGNATURE_PRODUCTION_PLACE);
			Map<String, String> metadata = new HashMap<>();
			if (signatureProductionPlaceList.getLength() > 0) {
				metadata = getProductionPlaceMetadata(signatureProductionPlaceList, namespaceUri);
			} else {
				final NodeList signatureProductionPlaceV2List = qualifyingProps.getElementsByTagNameNS(namespaceUri,
						XAdESConstants.TAG_SIGNATURE_PRODUCTION_PLACE_V2);
				if (signatureProductionPlaceV2List.getLength() > 0) {
					metadata = getProductionPlaceMetadata(signatureProductionPlaceV2List, namespaceUri);
				}
			}

			final NodeList signerRoleList = qualifyingProps.getElementsByTagNameNS(namespaceUri,
					XAdESConstants.TAG_SIGNER_ROLE);
			if (signerRoleList.getLength() > 0) {
				final String[] signerRoleArray = getClaimedRoleMetadata((Element) signerRoleList.item(0), namespaceUri);
				if (signerRoleArray != null) {
					for (int i = 0 ; i < signerRoleArray.length ; i++) {
						metadata.put("claimedRole" + i, signerRoleArray[i]); //$NON-NLS-1$
					}
				}
			} else {
				final NodeList signerRoleV2List = qualifyingProps.getElementsByTagNameNS(namespaceUri,
						XAdESConstants.TAG_SIGNER_ROLE_V2);
				if (signerRoleV2List.getLength() > 0) {
					final String[] signerRoleV2Array = getClaimedRoleMetadata((Element) signerRoleV2List.item(0), namespaceUri);
					if (signerRoleV2Array != null) {
						for (int i = 0 ; i < signerRoleV2Array.length ; i++) {
							metadata.put("claimedRole" + i, signerRoleV2Array[i]); //$NON-NLS-1$
						}
					}
				}
			}

			xadesSignDetails.setMetadata(metadata);
		}

		return xadesSignDetails;

	}

	/**
	 * Construye los detalles del certificado indicado.
	 * @param dataCertNode Datos del certificado.
	 * @return Devuelve los detalles del certificado.
	 */
	private static CertificateDetails buildCertDetails(final Node dataCertNode) {
		if (dataCertNode != null) {
			final Element certElement = (Element) ((Element) dataCertNode).getElementsByTagNameNS(XMLConstants.DSIGNNS, XAdESConstants.TAG_X509_CERTIFICATE).item(0);
			final X509Certificate cert = Utils.getCertificate(certElement);
			if (cert != null) {
				final CertificateDetails certDetails = new CertificateDetails(cert);
				return certDetails;
			}
		}
		return null;
	}

	/**
	 * Obtiene los datos de localizaci&oacute;n de una firma.
	 * @param signatureProductionPlaceList Lista con datos de localizaci&oacute;n.
	 * @param namespaceUri URI del namespace.
	 * @return Mapa con datos de la localizaci&oacute;n.
	 */
    private static Map<String, String> getProductionPlaceMetadata(final NodeList signatureProductionPlaceList, final String namespaceUri) {
    	final Map<String, String> metadata = new HashMap<>();
		final Element signProdPlaceNode = (Element) signatureProductionPlaceList.item(0);
		final NodeList streetAddressNode = signProdPlaceNode
				.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_STREET_ADDRESS);
		if (streetAddressNode.getLength() > 0) {
			metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.11"), //$NON-NLS-1$
					streetAddressNode.item(0).getTextContent().trim());
		}
		final NodeList postalCodeNode = signProdPlaceNode
				.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_POSTAL_CODE);
		if (postalCodeNode.getLength() > 0) {
			metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.8"), //$NON-NLS-1$
					postalCodeNode.item(0).getTextContent().trim());
		}
		final NodeList cityNode = signProdPlaceNode.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_CITY);
		if (cityNode.getLength() > 0) {
			metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.6"), //$NON-NLS-1$
					cityNode.item(0).getTextContent().trim());
		}
		final NodeList stateOrProvinceNode = signProdPlaceNode.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_STATE_OR_PROVINCE);
		if (stateOrProvinceNode.getLength() > 0) {
			metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.7"), //$NON-NLS-1$
					stateOrProvinceNode.item(0).getTextContent().trim());
		}
		final NodeList countryNode = signProdPlaceNode.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_COUNTRY_NAME);
		if (countryNode.getLength() > 0) {
			metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.9"), //$NON-NLS-1$
					countryNode.item(0).getTextContent().trim());
		}
		return metadata;

    }

    /**
     * Obtiene los datos sobre los roles.
     * @param signerRole Elemento con los datos.
     * @param namespaceUri URI del namespace.
     * @return Array con los roles.
     */
    private static String[] getClaimedRoleMetadata(final Element signerRole, final String namespaceUri) {
    	String [] result = null;
    	final NodeList claimedRolesNode = signerRole
    			.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_CLAIMED_ROLE);
    	if (claimedRolesNode.getLength() > 0) {
    		result = new String[claimedRolesNode.getLength()];
			for (int i = 0 ; i < claimedRolesNode.getLength() ; i++) {
				final String role = claimedRolesNode.item(i).getTextContent().trim();
				final String roleValue = FACTURAE_CLAIMED_ROLES.getOrDefault(role, role);
				result[i] = roleValue;
			}
    	}
    	return result;
    }

    /**
     * Se obtiene la fecha y hora de firma.
     * @param qualProps Elemento QualifyingProperties.
     * @param namespace URL del namespace.
     * @return Fecha y hora de firma.
     */
    private static Date obtainSigningTime(final Element qualProps, final String namespace) {
    	final Element signingTimeElement = (Element) qualProps.getElementsByTagNameNS(namespace, "SigningTime").item(0); //$NON-NLS-1$
    	if (signingTimeElement != null) {
    		try {
    			return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").parse( //$NON-NLS-1$
    					signingTimeElement.getTextContent()
    					);
    		}
    		catch (final Exception e) {
    			LOGGER.log(Level.WARNING, "No se ha podido recuperar la fecha de firma", e); //$NON-NLS-1$
    		}
    	}
    	else {
    		LOGGER.info("No se ha encontrado la hora de firma de una firma"); //$NON-NLS-1$
    	}
    	return null;
    }


}
