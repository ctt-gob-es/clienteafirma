package es.gob.afirma.standalone.signdetails;

import java.io.ByteArrayInputStream;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xades.XAdESConstants;
import es.gob.afirma.signers.xades.XAdESUtil;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PreferencesPanelXades;

public class XAdESSignAnalyzer implements SignAnalyzer {

	List <SignDetails> signDetailsList;
	List<CertificateDetails> certDetailsList;
	Document signDocument;
	private static final String FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	static final String[] SUPPORTED_XADES_NAMESPACE_URIS = new String[] {
			XAdESConstants.NAMESPACE_XADES_NO_VERSION,
		    XAdESConstants.NAMESPACE_XADES_1_2_2,
		    XAdESConstants.NAMESPACE_XADES_1_3_2,
		    XAdESConstants.NAMESPACE_XADES_1_4_1
	};

	public XAdESSignAnalyzer(final byte [] data) throws Exception {
    	try {
    		this.signDetailsList = new ArrayList<SignDetails>();
    		this.certDetailsList = new ArrayList<CertificateDetails>();
    		this.signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(data));
    		final NodeList signaturesList = this.signDocument.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
    		createSignDetails(signaturesList);
    	}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}
	}

	private void createSignDetails(final NodeList signaturesList) throws AOInvalidFormatException {
    	try {
    		for (int i = 0 ; i < signaturesList.getLength() ; i++) {
    			final Element signature = (Element) signaturesList.item(i);
    			buildSignDetails(signature);
    		}
    	}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}
	}

	private void buildSignDetails(final Element signElement) throws AOInvalidFormatException {
		final XAdESSignDetails xadesSignDetails =  new XAdESSignDetails();

		final String format = SignatureFormatDetectorXades.resolveSignerXAdESFormat(signElement);
		xadesSignDetails.setFormat(format);
		final Element signatureMethodElement = XAdESUtil.getSignatureMethodElement(signElement);
		xadesSignDetails.setAlgorithm(signatureMethodElement.getAttribute("Algorithm")); //$NON-NLS-1$

		//ARBOL DE FIRMANTES
		buildCertDetails(signElement.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Data").item(0) , xadesSignDetails.getSigners()); //$NON-NLS-1$

		//QUALIFYING PROPERTIES
    	final NodeList qualifyingPropsList = signElement.getElementsByTagNameNS("*", XAdESConstants.TAG_QUALIFYING_PROPERTIES); //$NON-NLS-1$

    	boolean existingNamespace = false;

    	for (int l = 0 ;l < qualifyingPropsList.getLength() ; l++) {
    		final String namespaceUri = qualifyingPropsList.item(l).getNamespaceURI();

    		for (final String xadesNameSpace : SUPPORTED_XADES_NAMESPACE_URIS) {
    			if (xadesNameSpace.equals(namespaceUri)) {
    				existingNamespace = true;
    			}
    		}

        	if (!existingNamespace) {
        		throw new AOInvalidFormatException("Una de las firmas encontradas en el documento contiene una version inexistente de XAdES"); //$NON-NLS-1$
        	}

    		//ARBOL DE FIRMANTES
    		buildCertDetails(((Element)qualifyingPropsList.item(l)).getElementsByTagNameNS(namespaceUri, "X509Data").item(0) , xadesSignDetails.getSigners()); //$NON-NLS-1$

    		//INFORMACION DECLARADA DE DATOS
    		final NodeList signedDataObjectPropertiesList = ((Element) qualifyingPropsList.item(l)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_SIGNED_DATA_OBJECT_PROPERTIES);
    		final NodeList dataObjectFormatList = ((Element) signedDataObjectPropertiesList.item(0)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_DATA_OBJECT_FORMAT);
    		final List<DataObjectFormat> dataObjectFormats = new ArrayList<DataObjectFormat>();
    		for (int m = 0 ; m < dataObjectFormatList.getLength() ; m++) {
    			final DataObjectFormat dof = new DataObjectFormat();
    			final Element dataObjectFormaElmt = (Element) dataObjectFormatList.item(m);
    			final String ref = dataObjectFormaElmt.getAttribute("ObjectReference"); //$NON-NLS-1$
    			dof.setIdentifier(ref);
    			final Node descriptionChild = dataObjectFormaElmt.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_DESCRIPTION).item(0).getFirstChild();
    			if (descriptionChild != null) {
        			dof.setDescription(dataObjectFormaElmt.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_DESCRIPTION).item(0).getFirstChild().getNodeValue());
    			}
    			dof.setMimeType(dataObjectFormaElmt.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_MIME_TYPE).item(0).getFirstChild().getNodeValue());
    			dataObjectFormats.add(dof);
    		}

    		xadesSignDetails.setDataObjectFormats(dataObjectFormats);

    		//POLITICA
    		final NodeList signaturePolicyIdentifierList = ((Element) qualifyingPropsList.item(l)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_SIGNATURE_POLICY_IDENTIFIER);
    		if (signaturePolicyIdentifierList != null && signaturePolicyIdentifierList.getLength() > 0) {
    			final Element signPolicyIdElement = (Element) signaturePolicyIdentifierList.item(0);
    			final String policyOID = signPolicyIdElement.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_IDENTIFIER).item(0).getFirstChild().getNodeValue();

    			if (PreferencesPanelXades.POLICY_XADES_AGE_1_9.getPolicyIdentifier().equals(policyOID)) {
    				final SignaturePolicy signPolicy = new SignaturePolicy(SimpleAfirmaMessages.getString("PreferencesPanel.73"),PreferencesPanelXades.POLICY_XADES_AGE_1_9); //$NON-NLS-1$
    				xadesSignDetails.setPolicy(signPolicy);
    			} else {
        			final String digestValue = signPolicyIdElement.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_DIGEST_VALUE).item(0).getFirstChild().getNodeValue();
        			final String digestMethod = ((Element)signPolicyIdElement.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_DIGEST_METHOD).item(0)).getAttribute("Algorithm"); //$NON-NLS-1$
        			final String spuri = signPolicyIdElement.getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_SPURI).item(0).getFirstChild().getNodeValue();

        			final AdESPolicy policy  = new AdESPolicy(
        					policyOID,
        					digestValue,
        					digestMethod,
        					spuri
        				);

    				final SignaturePolicy signPolicy = new SignaturePolicy(policyOID, policy);
    				xadesSignDetails.setPolicy(signPolicy);
    			}

    		}

    		//METADATOS
    		final NodeList signatureProductionPlaceList = ((Element) qualifyingPropsList.item(l)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_SIGNATURE_PRODUCTION_PLACE);
    		final Properties metadata = new Properties();
    		if (signatureProductionPlaceList.getLength() > 0) {
    			final Node cityNode = ((Element) signatureProductionPlaceList.item(0)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_CITY).item(0);
    			if (cityNode != null) {
    				metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.6"), cityNode.getFirstChild().getNodeValue()); //$NON-NLS-1$
    			}
    			final Node stateOrProvinceNode = ((Element) signatureProductionPlaceList.item(0)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_STATE_OR_PROVINCE).item(0);
    			if (stateOrProvinceNode != null) {
    				metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.7"), stateOrProvinceNode.getFirstChild().getNodeValue()); //$NON-NLS-1$
    			}
    			final Node postalCodeNode = ((Element) signatureProductionPlaceList.item(0)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_POSTAL_CODE).item(0);
    			if (postalCodeNode != null) {
    				metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.8"), postalCodeNode.getFirstChild().getNodeValue()); //$NON-NLS-1$
    			}
    			final Node countryNode = ((Element) signatureProductionPlaceList.item(0)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_COUNTRY_NAME).item(0);
    			if (countryNode != null) {
    				metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.9"), countryNode.getFirstChild().getNodeValue()); //$NON-NLS-1$
    			}
    		}

   			final Node claimedRoleNode = ((Element) qualifyingPropsList.item(l)).getElementsByTagNameNS(namespaceUri, XAdESConstants.TAG_CLAIMED_ROLE).item(0);
			if (claimedRoleNode != null) {
				metadata.put(SimpleAfirmaMessages.getString("ValidationInfoDialog.10"), claimedRoleNode.getFirstChild().getNodeValue()); //$NON-NLS-1$
			}

			xadesSignDetails.setMetadata(metadata);

    		this.signDetailsList.add(xadesSignDetails);
    	}

	}

	private void buildCertDetails(final Node dataCertNode, final List<CertificateDetails> signersList) {
		if (dataCertNode != null) {
			final Element certElement = (Element) ((Element) dataCertNode).getElementsByTagNameNS(XMLConstants.DSIGNNS, XAdESConstants.TAG_X509_CERTIFICATE).item(0);
			final X509Certificate cert = Utils.getCertificate(certElement);
			final CertificateDetails certDetails = new CertificateDetails(cert);
			final Node childCertElement = certElement.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Data").item(0);
			if (childCertElement != null) {
				buildCertDetails(childCertElement, certDetails.getSubCertDetails());
			}
			signersList.add(certDetails);
		}
	}

	@Override
	public List<SignDetails> getAllSignDetails() {
		return this.signDetailsList;
	}

	@Override
	public String getSignFormat() {
		return FORMAT_XADES;
	}

	@Override
	public String getDataLocation() {
        // Tomamos la raiz del documento
        final Element rootSig = this.signDocument.getDocumentElement();

        // Identificamos el tipo de la firma por medio de las referencias de la primera de ellas
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(this.signDocument.getDocumentElement());

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

        // Establecemos la variante de firma
    	if (AOXAdESSigner.isSignatureElementEnveloped(signatureElement, dataReferenceList)) {
        	return AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED;
        }
        else if (AOXAdESSigner.isSignatureElementExternallyDetached(dataReferenceList)) {
        	return AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED;
        }
        else if (AOXAdESSigner.isSignatureElementInternallyDetached(rootSig, dataReferenceList)) {
        	return AOSignConstants.SIGN_FORMAT_XADES_DETACHED;
        }
        else if (AOXAdESSigner.isSignatureElementEnveloping(signatureElement, dataReferenceList)) {
        	return AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING;
        }

    	return ISignatureFormatDetector.FORMAT_UNRECOGNIZED;
	}

}
