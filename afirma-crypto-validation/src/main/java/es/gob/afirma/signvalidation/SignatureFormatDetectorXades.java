package es.gob.afirma.signvalidation;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.signers.xades.XAdESConstants;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;

/**
 * <p>Class that contains all the functionality related to recognize the signature formats.</p>
 * <b>Project:</b><p>Library for the integration with the services of @Firma, eVisor and TS@.</p>
 * @version 1.2, 14/03/2017.
 */
public final class SignatureFormatDetectorXades implements ISignatureFormatDetector {

    /**
     * Constructor method for the class SignatureFormatDetector.java.
     */
    private SignatureFormatDetectorXades() {
    }

    /**
     * Method that obtains the format of the most advanced XML signature format contained inside of a signed XML document.
     * @param signature Parameter that represents the XML document to evaluate.
     * @return the most advanced XML signature format. The value to return will be on of these:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LTA_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LT_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_B_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_A}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_C}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_UNRECOGNIZED}.</li>
     * </ul>
     */
    public static String getSignatureFormat(final byte[ ] signature) {

	// Por defecto definimos que el formato no estÃ¡ reconocido
	String format = ISignatureFormatDetector.FORMAT_UNRECOGNIZED;
	if (AOFileUtils.isXML(signature)) {
	    try {
		// Obtenemos el documento XML
		final Document doc = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(signature));

		// Obtenemos la lista de elementos ds:Signature que no
		// correspondan
		// con un sello de tiempo XML
		final List<Element> listSignatureElements = getListSignatures(doc);

		// Comprobamos si alguna de las firmas del documento XML tiene
		// el
		// formato XAdES-EPES
		if (isXAdESEPES(listSignatureElements)) {
		    // Comprobamos si alguna de las firmas del documento XML
		    // tiene
		    // un formato Baseline, estableciendo que, al menos, el
		    // formato
		    // es XAdES-EPES
		    format = resolveXAdESBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_EPES, listSignatureElements);

		    // Si ninguna de las firmas del documento XML tiene un
		    // formato
		    // Baseline, es decir, el formato por ahora es XAdES-EPES,
		    // comprobamos si posee un formato mÃ¡s avanzado no Baseline
		    if (format.equals(ISignatureFormatDetector.FORMAT_XADES_EPES)) {
		    	format = resolveXAdESNoBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_EPES, listSignatureElements);
		    }

		}
		// Comprobamos si alguna de las firmas del documento XML tiene
		// el
		// formato XAdES-BES
		else if (isXAdESBES(listSignatureElements)) {
		    // Comprobamos si alguna de las firmas del documento XML
		    // tiene
		    // un formato Baseline, estableciendo que, al menos, el
		    // formato
		    // es XAdES-BES
		    format = resolveXAdESBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_BES, listSignatureElements);

		    // Si ninguna de las firmas del documento XML tiene un
		    // formato
		    // Baseline, es decir, el formato por ahora es XAdES-BES,
		    // comprobamos si posee un formato mÃ¡s avanzado no Baseline
		    if (format.equals(ISignatureFormatDetector.FORMAT_XADES_BES)) {
		    	format = resolveXAdESNoBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_BES, listSignatureElements);
		    }
		}

	    } catch (final Exception e) {
	    	format = ISignatureFormatDetector.FORMAT_UNRECOGNIZED;
	    }
	}
	return format;
    }

    /**
     * Method that obtains the format of the most advanced XML signature format of Baseline form contained inside of a signed XML document.
     * @param temporalFormat Parameter that represents the current most advanced XML signature format contained inside of a signed XML document. This parameter only allows one of these values:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures contained inside of the XML document.
     * @return the most advanced XML signature format of Baseline form contained inside of the signed XML document, or the current most advanced XML signature format contained inside of a signed XML document
     * if any of the signatures contained inside of the XML document has a Baseline form. The value to return will be on of these:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LTA_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LT_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_B_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * </ul>
     */
    private static String resolveXAdESBaselineFormat(final String temporalFormat, final List<Element> listSignatureElements) {
	String format = temporalFormat;
	// Comprobamos si alguna de las firmas del documento XML tiene
	// el formato XAdES B-Level
	if (isXAdESBLevel(listSignatureElements)) {
	    // Indicamos que el formato por ahora es XAdES B-Level
	    format = ISignatureFormatDetector.FORMAT_XADES_B_LEVEL;

	    // Comprobamos si alguna de las firmas del documento XML
	    // tiene
	    // el formato XAdES T-Level
	    if (isXAdEST(listSignatureElements)) {
			// Indicamos que el formato por ahora es XAdES T-Level
			format = ISignatureFormatDetector.FORMAT_XADES_T_LEVEL;

			// Comprobamos si alguna de las firmas del documento XML
			// tiene
			// el formato XAdES LT-Level
			if (isXAdESLTLevel(listSignatureElements)) {
			    // Indicamos que el formato por ahora es XAdES
			    // LT-Level
			    format = ISignatureFormatDetector.FORMAT_XADES_LT_LEVEL;

			    // Comprobamos si alguna de las firmas del documento XML
			    // tiene
			    // el formato XAdES LTA-Level
			    if (isXAdESLTALevel(listSignatureElements)) {
					// Indicamos que el formato por ahora es XAdES
					// LTA-Level
					format = ISignatureFormatDetector.FORMAT_XADES_LTA_LEVEL;

			    }
			} else {
			    resolveXAdESNoBaselineFormat(format, listSignatureElements);
			}

	    }
	}
	return format;
    }

    /**
     * Method that obtains the format of the most advanced XML signature format without Baseline form contained inside of a signed XML document.
     * @param temporalFormat Parameter that represents the current most advanced XML signature format contained inside of a signed XML document. This parameter only allows one of these values:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures contained inside of the XML document.
     * @return the most advanced XML signature format without Baseline form contained inside of the signed XML document. The value to return will be on of these:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_A}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_C}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * </ul>
     */
    private static String resolveXAdESNoBaselineFormat(final String temporalFormat, final List<Element> listSignatureElements) {
	String format = temporalFormat;

	// Comprobamos si alguna de las firmas del documento XML
	// tiene
	// el formato XAdES-A
	if (isXAdESA(listSignatureElements)) {
	    // Indicamos que el formato es XAdES-A
	    format = ISignatureFormatDetector.FORMAT_XADES_A;
	}
	// Comprobamos si alguna de las firmas del documento XML
	// tiene
	// el formato XAdES-XL1
	else if (isXAdESXL1(listSignatureElements)) {
	    // Indicamos que el formato es XAdES-XL1
	    format = ISignatureFormatDetector.FORMAT_XADES_XL1;
	}
	// Comprobamos si alguna de las firmas del documento XML
	// tiene
	// el formato XAdES-XL2
	else if (isXAdESXL2(listSignatureElements)) {
	    // Indicamos que el formato es XAdES-XL2
	    format = ISignatureFormatDetector.FORMAT_XADES_XL2;
	}
	// Comprobamos si alguna de las firmas del documento XML
	// tiene
	// el formato XAdES-X1
	else if (isXAdESX1(listSignatureElements)) {
	    // Indicamos que el formato es XAdES-X1
	    format = ISignatureFormatDetector.FORMAT_XADES_X1;
	}
	// Comprobamos si alguna de las firmas del documento XML
	// tiene
	// el formato XAdES-X2
	else if (isXAdESX2(listSignatureElements)) {
	    // Indicamos que el formato es XAdES-X2
	    format = ISignatureFormatDetector.FORMAT_XADES_X2;
	}
	// Comprobamos si alguna de las firmas del documento XML
	// tiene
	// el formato XAdES-C
	else if (isXAdESC(listSignatureElements)) {
	    // Indicamos que el formato es XAdES-C
	    format = ISignatureFormatDetector.FORMAT_XADES_C;
	}
	// Comprobamos si alguna de las firmas del documento XML
	// tiene
	// el formato XAdES-T
	else if (isXAdEST(listSignatureElements) && !format.equals(ISignatureFormatDetector.FORMAT_XADES_T_LEVEL)) {
	    // Indicamos que el formato es XAdES-T
	    format = ISignatureFormatDetector.FORMAT_XADES_T;
	}
	return format;
    }

    /**
     * Method that obtains a child element of a parent element, both elements contained inside of a XML document.
     * @param parentElement Parameter that represents the parent element.
     * @param namespace Parameter that represents the namespace of the element to obtain.
     * @param elementName Parameter that represents the name of the element to obtain.
     * @return the found element, or <code>null</code> if the element hasn't been found.
     */
    private static Element getXMLElement(final Element parentElement, final String namespace, final String elementName) {
	Element result = (Element) parentElement.getElementsByTagNameNS(namespace, elementName).item(0);
	if (result == null) {
	    result = (Element) parentElement.getElementsByTagName(elementName).item(0);
	}

	return result;
    }

    /**
     * Method that indicates if a XML signature contains the <code>xades:SignaturePolicyIdentifier</code> element.
     * @param dsSignature Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the XML signature contains the <code>xades:SignaturePolicyIdentifier</code> element (true) or not (false).
     */
    public static boolean hasSignaturePolicyIdentifier(final Element dsSignature) {
	// Accedemos al elemento SignedProperties
	final Element signedProperties = getXMLElement(dsSignature, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_PROPERTIES);
	if (signedProperties != null) {
	    // Accedemos al elemento SignedSignatureProperties
	    final Element signedSignatureProperties = getXMLElement(signedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_SIGNATURE_PROPERTIES);
	    if (signedSignatureProperties != null) {
			// Se considera una firma con formato XAdES-EPES si
			// posee el
			// elemento SignaturePolicyIdentifier
			final Element signaturePolicyIdentifier = getXMLElement(signedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNATURE_POLICY_IDENTIFIER);
			if (signaturePolicyIdentifier != null) {
			    return true;
			}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:ArchiveTimeStamp</code> element</li>
     * or
     * <li><code>xadesv141:ArchiveTimeStamp</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of the list of XML signatures contains:
     * <ul>
     * <li>One <code>xades:ArchiveTimeStamp</code> element</li>
     * or
     * <li>One <code>xadesv141:ArchiveTimeStamp</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESLTALevel(final List<Element> listSignatureElements) {
    	return isXAdESA(listSignatureElements);
    }

    /**
     * Method that indicates if a signer has XAdES LTA-Level format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES LTA-Level format.
     */
    private static boolean isXAdESLTALevel(final Element signatureElement) {
    	return isXAdESA(signatureElement);
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains:
     * <ul>
     * <li>One <code>xades:CertificateValues</code> element and one <code>xades:RevocationValues</code> element</li>
     * or at least one
     * <li><code>xadesv141:TimeStampValidationData</code> element.</li>
     * </ul>
     * And it doesn't contain:
     * <ul>
     * <li>Any <code>xades:CompleteCertificateRefs</code> element</li>
     * and
     * <li>Any <code>xades:CompleteRevocationRefs</code> element</li>
     * and
     * <li>Any <code>xades:AttributeCertificateRefs</code> element</li>
     * and
     * <li>Any <code>xades:AttributeRevocationRefs</code> element</li>
     * and
     * <li>Any <code>xades:SigAndRefsTimeStamp</code> element</li>
     * and
     * <li>Any <code>xades:RefsOnlyTimeStamp</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of the list of XML signatures contains:
     * <ul>
     * <li>One <code>xades:CertificateValues</code> element and one <code>xades:RevocationValues</code> element</li>
     * or at least one
     * <li><code>xadesv141:TimeStampValidationData</code> element.</li>
     * </ul>
     * And it doesn't contain:
     * <ul>
     * <li>Any <code>xades:CompleteCertificateRefs</code> element</li>
     * and
     * <li>Any <code>xades:CompleteRevocationRefs</code> element</li>
     * and
     * <li>Any <code>xades:AttributeCertificateRefs</code> element</li>
     * and
     * <li>Any <code>xades:AttributeRevocationRefs</code> element</li>
     * and
     * <li>Any <code>xades:SigAndRefsTimeStamp</code> element</li>
     * and
     * <li>Any <code>xades:RefsOnlyTimeStamp</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESLTLevel(final List<Element> listSignatureElements) {
	/* Una firma se considerarÃ¡ XAdES LT-Level si posee los elementos:
	 * > xades:CertificateValues y xades:RevocationValues
	 * > o xadesv141:TimeStampValidationData
	 * Y si no posee los elementos:
	 * > xades:CompleteCertificateRefs
	 * > xades:CompleteRevocationRefs
	 * > xades:AttributeCertificateRefs
	 * > xades:AttributeRevocationRefs
	 * > xades:SigAndRefsTimeStamp
	 * > xades:RefsOnlyTimeStamp
	 */
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
			if (isXAdESLTLevel(signatureElement)) {
			    return true;
			}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES LT-Level format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES LT-Level format.
     */
    private static boolean isXAdESLTLevel(final Element signatureElement) {
	/* Una firma se considerarÃ¡ XAdES LT-Level si posee los elementos:
	 * > xades:CertificateValues y xades:RevocationValues
	 * > o xadesv141:TimeStampValidationData
	 * Y si no posee los elementos:
	 * > xades:CompleteCertificateRefs
	 * > xades:CompleteRevocationRefs
	 * > xades:AttributeCertificateRefs
	 * > xades:AttributeRevocationRefs
	 * > xades:SigAndRefsTimeStamp
	 * > xades:RefsOnlyTimeStamp
	 */
	boolean hasCertificateValues = false;
	final boolean hasRevocationValues = false;
	boolean hashTimeStampValidationData = false;
	boolean hasCompleteCertificateRefs = false;
	boolean hasCompleteRevocationRefs = false;
	boolean hasAttributeCertificateRefs = false;
	boolean hasAttributeRevocationRefs = false;
	boolean hasSigAndRefsTimeStamp = false;
	boolean hasRefsOnlyTimeStamp = false;

	// Accedemos al elemento xades:UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento xades:UnsignedSignatureProperties
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null) {

		// Comprobamos si contiene el elemento
		// xades:CertificateValues
		hasCertificateValues = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_CERTIFICATE_VALUES) != null;

		// Comprobamos si contiene el elemento
		// xades:RevocationValues
		hasCertificateValues = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_REVOCATION_VALUES) != null;

		// Comprobamos si contiene el elemento
		// xadesv141:TimeStampValidationData
		hashTimeStampValidationData = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_4_1, XAdESConstants.TAG_TIME_STAMP_VALIDATION_DATA) != null;

		// Comprobamos si contiene el elemento
		// xades:CompleteCertificateRefs
		hasCompleteCertificateRefs = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_COMPLETE_CERTIFICATE_REFS) != null;

		// Comprobamos si contiene el elemento
		// xades:CompleteRevocationRefs
		hasCompleteRevocationRefs = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_COMPLETE_REVOCATION_REFS) != null;

		// Comprobamos si contiene el elemento
		// xades:AttributeCertificateRefs
		hasAttributeCertificateRefs = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_ATTRIBUTE_CERTIFICATE_REFS) != null;

		// Comprobamos si contiene el elemento
		// xades:AttributeRevocationRefs
		hasAttributeRevocationRefs = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_ATTRIBUTE_REVOCATION_REFS) != null;

		// Comprobamos si contiene el elemento
		// xades:SigAndRefsTimeStamp
		hasSigAndRefsTimeStamp = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIG_AND_REFS_TIMESTAMP) != null;

		// Comprobamos si contiene el elemento
		// xades:RefsOnlyTimeStamp
		hasRefsOnlyTimeStamp = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_REFS_ONLY_TIMESTAMP) != null;

		if (checkLTLevel(hasCertificateValues, hasRevocationValues, hashTimeStampValidationData, hasCompleteCertificateRefs, hasCompleteRevocationRefs, hasAttributeCertificateRefs, hasAttributeRevocationRefs, hasSigAndRefsTimeStamp, hasRefsOnlyTimeStamp)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if the input parameters has the values associated to a XAdES LT-Level signature.
     * @param hasCertificateValues Parameter that indicates if a XML signature contains at least one <code>xades:CertificateValues</code> element.
     * @param hasRevocationValues Parameter that indicates if a XML signature contains at least one <code>xades:RevocationValues</code> element.
     * @param hashTimeStampValidationData Parameter that indicates if a XML signature contains at least one <code>xades:RevocationValues</code> element.
     * @param hasCompleteCertificateRefs Parameter that indicates if a XML signature contains at least one <code>xades:CompleteCertificateRefs</code> element.
     * @param hasCompleteRevocationRefs Parameter that indicates if a XML signature contains at least one <code>xades:CompleteRevocationRefs</code> element.
     * @param hasAttributeCertificateRefs Parameter that indicates if a XML signature contains at least one <code>xades:AttributeCertificateRefs</code> element.
     * @param hasAttributeRevocationRefs Parameter that indicates if a XML signature contains at least one <code>xades:AttributeRevocationRefs</code> element.
     * @param hasSigAndRefsTimeStamp Parameter that indicates if a XML signature contains at least one <code>xades:SigAndRefsTimeStamp</code> element.
     * @param hasRefsOnlyTimeStamp Parameter that indicates if a XML signature contains at least one <code>xades:RefsOnlyTimeStamp</code> element.
     * @return a boolean that indicates if a XML signature:
     * <ul>
     * <li>Contains the <code>xades:CertificateValues</code> element and the <code>xades:RevocationValues</code> element</li>
     * or
     * <li>contains the <code>xadesv141:TimeStampValidationData</code> element</li>
     * and doesn't contain
     * <li>the <code>xades:CompleteCertificateRefs</code> element</li>
     * and
     * <li>the <code>xades:CompleteRevocationRefs</code> element</li>
     * and
     * <li>the <code>xades:AttributeCertificateRefs</code> element</li>
     * and
     * <li>the <code>xades:AttributeRevocationRefs</code> element</li>
     * and
     * <li>the <code>xades:SigAndRefsTimeStamp</code> element</li>
     * and
     * <li>the <code>xades:RefsOnlyTimeStamp</code> element.</li>
     * </ul>
     */
    private static boolean checkLTLevel(final boolean hasCertificateValues, final boolean hasRevocationValues, final boolean hashTimeStampValidationData, final boolean hasCompleteCertificateRefs, final boolean hasCompleteRevocationRefs, final boolean hasAttributeCertificateRefs, final boolean hasAttributeRevocationRefs, final boolean hasSigAndRefsTimeStamp, final boolean hasRefsOnlyTimeStamp) {
	// CHECKSTYLE:OFF expresion complexity needed
	if ((hasCertificateValues && hasRevocationValues || hashTimeStampValidationData) && !hasCompleteCertificateRefs && !hasCompleteRevocationRefs && !hasAttributeCertificateRefs && !hasAttributeRevocationRefs && !hasSigAndRefsTimeStamp && !hasRefsOnlyTimeStamp) {
	    // CHECKSTYLE:ON
	    return true;
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li>One <code>xades:SigningCertificate</code> element</li>
     * and
     * <li>One <code>xades:SigningTime</code> element</li>
     * and
     * <li>One <code>xades:DataObjectFormat</code> element</li>
     * and it doesn't contain any:
     * <li>One <code>xades:QualifyingPropertiesReference</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of the list of XML signatures contains:
     * <ul>
     * <li>One <code>xades:SigningCertificate</code> element</li>
     * and
     * <li>One <code>xades:SigningTime</code> element</li>
     * and
     * <li>One <code>xades:DataObjectFormat</code> element</li>
     * and it doesn't contain any:
     * <li>One <code>xades:QualifyingPropertiesReference</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESBLevel(final List<Element> listSignatureElements) {
	/* Una firma se considerarÃ¡ XAdES B-Level si posee los elementos:
	 * > xades:SigningCertificate
	 * > xades:SigningTime
	 * > xades:DataObjectFormat
	 * Y si no posee el elemento:
	 * > xades:QualifyingPropertiesReference
	 */
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		return isXAdESBLevel(signatureElement);
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES B-Level format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES B-Level format.
     */
    private static boolean isXAdESBLevel(final Element signatureElement) {
	/* Un firmante se considerarÃ¡ XAdES B-Level si posee los elementos:
	 * > xades:SigningCertificate
	 * > xades:SigningTime
	 * > xades:DataObjectFormat (incluyendo el elemento xades:MimeType)
	 * Y si no posee el elemento:
	 * > xades:QualifyingPropertiesReference
	 */
	boolean hasSigningCertificate = false;
	boolean hasSigningTime = false;
	boolean hasDataObjectFormat = false;
	boolean hasMimeType = false;

	// Comprobamos que la firma no contiene el elemento
	// xades:QualifyingPropertiesReference
	if (signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_QUALIFYING_PROPERTIES_REFERENCE).getLength() == 0) {
	    // Accedemos al elemento xades:SignedProperties
	    Element signedPropertiesElement = null;
	    if (signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_PROPERTIES).getLength() > 0) {
		signedPropertiesElement = (Element) signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_PROPERTIES).item(0);

		// Accedemos al elemento xades:SignedSignatureProperties
		Element signedSignaturePropertiesElement = null;
		if (signedPropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_SIGNATURE_PROPERTIES).getLength() > 0) {
		    signedSignaturePropertiesElement = (Element) signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_SIGNATURE_PROPERTIES).item(0);

		    // Comprobamos si el elemento
		    // xades:SignedSignatureProperties tiene
		    // xades:SigningCertificate como uno de sus hijos
		    hasSigningCertificate = signedSignaturePropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNING_CERTIFICATE).getLength() > 0;

		    // Comprobamos si el elemento
		    // xades:SignedSignatureProperties tiene
		    // xades:SigningTime como uno de sus hijos
		    hasSigningTime = signedSignaturePropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNING_TIME).getLength() > 0;
		}
		// Accedemos al elemento
		// xades:SignedDataObjectProperties
		Element signedDataObjectPropertiesElement = null;
		if (signedPropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_DATA_OBJECT_PROPERTIES).getLength() > 0) {
		    signedDataObjectPropertiesElement = (Element) signedPropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_DATA_OBJECT_PROPERTIES).item(0);

		    // Comprobamos si el elemento
		    // xades:SignedDataObjectProperties tiene
		    // xades:DataObjectFormat como uno de sus hijos
		    final Element dataObjectFormatElement = getXMLElement(signedDataObjectPropertiesElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_DATA_OBJECT_FORMAT);

		    hasDataObjectFormat = dataObjectFormatElement != null;
		    if (dataObjectFormatElement != null) {
			hasDataObjectFormat = true;

			// Comprobamos si el elemento
			// xades:DataObjectFormat tiene
			// xades:MimeType como uno de sus hijos
			hasMimeType = dataObjectFormatElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_MIME_TYPE).getLength() > 0;
		    }
		}
		if (hasSigningTime && hasSigningCertificate && hasDataObjectFormat && hasMimeType) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES B-B-Level format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES B-Level format.
     */
    private static boolean isXAdESBBLevel(final Element signatureElement) {
	/* Un firmante se considerara XAdES B-Level si posee los elementos:
	 * > xades:SigningCertificateV2
	 * > xades:SigningTime
	 * > xades:DataObjectFormat (incluyendo el elemento xades:MimeType)
	 * Y si no posee el elemento:
	 * > xades:QualifyingPropertiesReference
	 */
	boolean hasSigningCertificateV2 = false;
	boolean hasSigningTime = false;
	boolean hasDataObjectFormat = false;
	boolean hasMimeType = false;

	// Comprobamos que la firma no contiene el elemento
	// xades:QualifyingPropertiesReference
	if (signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_QUALIFYING_PROPERTIES_REFERENCE).getLength() == 0) {
	    // Accedemos al elemento xades:SignedProperties
	    Element signedPropertiesElement = null;
	    if (signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_PROPERTIES).getLength() > 0) {
		signedPropertiesElement = (Element) signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_PROPERTIES).item(0);

		// Accedemos al elemento xades:SignedSignatureProperties
		Element signedSignaturePropertiesElement = null;
		if (signedPropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_SIGNATURE_PROPERTIES).getLength() > 0) {
		    signedSignaturePropertiesElement = (Element) signatureElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_SIGNATURE_PROPERTIES).item(0);

		    // Comprobamos si el elemento
		    // xades:SignedSignatureProperties tiene
		    // xades:SigningCertificate como uno de sus hijos
		    hasSigningCertificateV2 = signedSignaturePropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNING_CERTIFICATE_V2).getLength() > 0;

		    // Comprobamos si el elemento
		    // xades:SignedSignatureProperties tiene
		    // xades:SigningTime como uno de sus hijos
		    hasSigningTime = signedSignaturePropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNING_TIME).getLength() > 0;
		}
		// Accedemos al elemento
		// xades:SignedDataObjectProperties
		Element signedDataObjectPropertiesElement = null;
		if (signedPropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_DATA_OBJECT_PROPERTIES).getLength() > 0) {
		    signedDataObjectPropertiesElement = (Element) signedPropertiesElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNED_DATA_OBJECT_PROPERTIES).item(0);

		    // Comprobamos si el elemento
		    // xades:SignedDataObjectProperties tiene
		    // xades:DataObjectFormat como uno de sus hijos
		    final Element dataObjectFormatElement = getXMLElement(signedDataObjectPropertiesElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_DATA_OBJECT_FORMAT);

		    hasDataObjectFormat = dataObjectFormatElement != null;
		    if (dataObjectFormatElement != null) {
			hasDataObjectFormat = true;

			// Comprobamos si el elemento
			// xades:DataObjectFormat tiene
			// xades:MimeType como uno de sus hijos
			hasMimeType = dataObjectFormatElement.getElementsByTagNameNS(XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_MIME_TYPE).getLength() > 0;
		    }
		}
		if (hasSigningTime && hasSigningCertificateV2 && hasDataObjectFormat && hasMimeType) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li>One <code>xades:ArchiveTimeStamp</code> element</li>
     * or
     * <li>One <code>xadesv141:ArchiveTimeStamp</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of the list of XML signatures contains:
     * <ul>
     * <li>One <code>xades:ArchiveTimeStamp</code> element</li>
     * or
     * <li>One <code>xadesv141:ArchiveTimeStamp</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESA(final List<Element> listSignatureElements) {
	/* Una firma se considerarÃ¡ XAdES LTA-Level si posee al menos uno de los siguientes elementos:
	 * > xades:ArchiveTimeStamp
	 * > xadesv141:ArchiveTimeStamp
	 */
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		if (isXAdESA(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES-A format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES-A format.
     */
    private static boolean isXAdESA(final Element signatureElement) {
	/* Una firma se considerarÃ¡ XAdES LTA-Level si posee al menos uno de los siguientes elementos:
	 * > xades:ArchiveTimeStamp
	 * > xadesv141:ArchiveTimeStamp
	 */
	// Accedemos al elemento xades:UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento xades:UnsignedSignatureProperties
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null) {
		// Obtenemos el conjunto de hijos del elemento
		// xades:UnsignedSignatureProperties
		final NodeList nl = unsignedSignatureProperties.getChildNodes();

		// Recorremos el conjunto de elementos hijos
		for (int i = 0; i < nl.getLength(); i++) {
		    if (nl.item(i).getNodeType() == Node.ELEMENT_NODE && nl.item(i).getLocalName().equals(XAdESConstants.TAG_ARCHIVE_TIMESTAMP)) {
			return true;
		    }
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SigAndRefsTimeStamp</code> element</li>
     * and
     * <li>One <code>xades:CertificateValues</code> element</li>
     * and
     * <li>One <code>xades:RevocationValues</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SigAndRefsTimeStamp</code> element</li>
     * and
     * <li>One <code>xades:CertificateValues</code> element</li>
     * and
     * <li>One <code>xades:RevocationValues</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESXL1(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		if (isXAdESXL1(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES-XL1 format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES-XL1 format.
     */
    private static boolean isXAdESXL1(final Element signatureElement) {
	// Accedemos al elemento UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento UnsignedSignatureProperties
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null) {
		// Se considera una firma con formato XAdES-XL1 si posee
		// los
		// elementos SigAndRefsTimeStamp, CertificateValues y
		// RevocationValues
		final Element sigAndRefsTimeStamp = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIG_AND_REFS_TIMESTAMP);
		final Element certificateValues = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_CERTIFICATE_VALUES);
		final Element revocationValues = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_REVOCATION_VALUES);
		if (sigAndRefsTimeStamp != null && certificateValues != null && revocationValues != null) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:RefsOnlyTimeStamp</code> element</li>
     * and
     * <li>One <code>xades:CertificateValues</code> element</li>
     * and
     * <li>One <code>xades:RevocationValues</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:RefsOnlyTimeStamp</code> element</li>
     * and
     * <li>One <code>xades:CertificateValues</code> element</li>
     * and
     * <li>One <code>xades:RevocationValues</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESXL2(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		if (isXAdESXL2(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES-XL2 format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES-XL2 format.
     */
    private static boolean isXAdESXL2(final Element signatureElement) {
	// Accedemos al elemento UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento UnsignedSignatureProperties
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null) {
		// Se considera una firma con formato XAdES-XL2 si posee
		// los
		// elementos RefsOnlyTimeStamp, CertificateValues y
		// RevocationValues
		final Element refsOnlyTimeStamp = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_REFS_ONLY_TIMESTAMP);
		final Element certificateValues = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_CERTIFICATE_VALUES);
		final Element revocationValues = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_REVOCATION_VALUES);
		if (refsOnlyTimeStamp != null && certificateValues != null && revocationValues != null) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SigAndRefsTimeStamp</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SigAndRefsTimeStamp</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESX1(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		if (isXAdESX1(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES-X1 format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES-X1 format.
     */
    private static boolean isXAdESX1(final Element signatureElement) {
	// Accedemos al elemento UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento UnsignedSignatureProperties
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null) {
		// Se considera una firma con formato XAdES-X1 si posee
		// el
		// elemento SigAndRefsTimeStamp
		final Element sigAndRefsTimeStamp = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIG_AND_REFS_TIMESTAMP);
		if (sigAndRefsTimeStamp != null) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:RefsOnlyTimeStamp</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:RefsOnlyTimeStamp</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESX2(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		if (isXAdESX2(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES-X2 format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES-X2 format.
     */
    private static boolean isXAdESX2(final Element signatureElement) {
	// Accedemos al elemento UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento UnsignedSignatureProperties
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null) {
		// Se considera una firma con formato XAdES-X2 si posee
		// el
		// elemento RefsOnlyTimeStamp
		final Element refsOnlyTimeStamp = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_REFS_ONLY_TIMESTAMP);
		if (refsOnlyTimeStamp != null) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:CompleteCertificateRefs</code> element</li>
     * and
     * <li>One <code>xades:CompleteRevocationRefs</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:CompleteCertificateRefs</code> element</li>
     * and
     * <li>One <code>xades:CompleteRevocationRefs</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESC(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		if (isXAdESC(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES-C format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES-C format.
     */
    private static boolean isXAdESC(final Element signatureElement) {
	// Accedemos al elemento UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento UnsignedSignatureProperties
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null) {
		// Se considera una firma con formato XAdES-C si posee
		// los
		// elementos CompleteCertificateRefs y
		// CompleteRevocationRefs
		final Element completeCertificateRefs = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_COMPLETE_CERTIFICATE_REFS);
		final Element completeRevocationRefs = getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_COMPLETE_REVOCATION_REFS);
		if (completeCertificateRefs != null && completeRevocationRefs != null) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SignaturePolicyIdentifier</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SignaturePolicyIdentifier</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESEPES(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		// Comprobamos si el firmante posee el elemento
		// xades:SignaturePolicyIdentifier
		if (hasSignaturePolicyIdentifier(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:QualifyingProperties</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:QualifyingProperties</code> element.</li>
     * </ul>
     */
    private static boolean isXAdESBES(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		// Se considera una firma con formato XAdES-BES si posee el
		// elemento QualifyingProperties
		if (getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_QUALIFYING_PROPERTIES) != null) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES B-Level format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES B-Level format.
     */
    private static boolean isXAdESBES(final Element signatureElement) {
	// Se considera una firma con formato XAdES-BES si posee el
	// elemento QualifyingProperties
	if (getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_QUALIFYING_PROPERTIES) != null) {
	    return true;
	}
	return false;
    }

    /**
     * Method that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SignatureTimeStamp</code> element.</li>
     * </ul>
     * @param listSignatureElements Parameter that represents the list of signatures.
     * @return a boolean that indicates if at least one of a list of XML signatures contains at least one:
     * <ul>
     * <li><code>xades:SignatureTimeStamp</code> element.</li>
     * </ul>
     */
    private static boolean isXAdEST(final List<Element> listSignatureElements) {
	// Si el documento XML posee elementos ds:Signature
	if (!listSignatureElements.isEmpty()) {
	    // Recorremos la lista de elementos ds:Signature
	    for (final Element signatureElement: listSignatureElements) {
		if (isXAdEST(signatureElement)) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Method that indicates if a signer has XAdES T-Level format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return a boolean that indicates if the signer has XAdES T-Level format.
     */
    private static boolean isXAdEST(final Element signatureElement) {
	// Accedemos al elemento UnsignedProperties
	final Element unsignedProperties = getXMLElement(signatureElement, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_PROPERTIES);
	if (unsignedProperties != null) {
	    // Accedemos al elemento UnsignedSignatureProperties. Se
	    // considera una firma con formato XAdES-T si posee
	    // el
	    // elemento SignatureTimeStamp
	    final Element unsignedSignatureProperties = getXMLElement(unsignedProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES);
	    if (unsignedSignatureProperties != null && getXMLElement(unsignedSignatureProperties, XAdESConstants.NAMESPACE_XADES_1_3_2, XAdESConstants.TAG_SIGNATURE_TIMESTAMP) != null) {
		return true;
	    }
	}
	return false;
    }

    /**
     *  Method that obtains the format associated to a signer of a XML document.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return the signature format. The value to return will be on of these:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LTA_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LT_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_B_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_A}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_C}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_UNRECOGNIZED}.</li>
     * </ul>
     */
    public static String resolveSignerXAdESFormat(final Element signatureElement) {
	// Inicialmente definidos que el formato no estÃ¡ reconocido
	String format = FORMAT_UNRECOGNIZED;

	// Si se ha indicado firmante
	if (signatureElement != null) {
	    // Comprobamos si el firmante posee formato XAdES-EPES
	    if (isXAdESEPES(signatureElement)) {
		// Comprobamos si el firmante tiene
		// un formato Baseline, estableciendo que, al menos, el
		// formato
		// es XAdES-EPES
		format = resolveXAdESBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_EPES, signatureElement);

		// Si el firmante no tiene un
		// formato
		// Baseline, es decir, el formato por ahora es XAdES-EPES,
		// comprobamos si posee un formato mÃ¡s avanzado no Baseline
		if (format.equals(ISignatureFormatDetector.FORMAT_XADES_EPES)) {
		    format = resolveXAdESNoBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_EPES, signatureElement);
		}
	    }
	    // Comprobamos si el firmante tiene formato XAdES-BES
	    else if (isXAdESBES(signatureElement)) {
		// Comprobamos si el firmante tiene
		// un formato Baseline, estableciendo que, al menos, el
		// formato
		// es XAdES-BES
		format = resolveXAdESBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_BES, signatureElement);

		// Si el firmante no tiene un
		// formato
		// Baseline, es decir, el formato por ahora es XAdES-BES,
		// comprobamos si posee un formato mÃ¡s avanzado no Baseline
		if (format.equals(ISignatureFormatDetector.FORMAT_XADES_BES)) {
		    format = resolveXAdESNoBaselineFormat(ISignatureFormatDetector.FORMAT_XADES_BES, signatureElement);
		}
	    }
	}
	return format;
    }

    /**
     * Method that checks if a signer has XAdES-EPES format.
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element associated to the signer.
     * @return a boolean that indicates if the signer has XAdES-EPES format.
     */
    private static boolean isXAdESEPES(final Element signatureElement) {
	// Comprobamos si el firmante posee el elemento
	// xades:SignaturePolicyIdentifier
	if (hasSignaturePolicyIdentifier(signatureElement)) {
	    return true;
	}
	return false;
    }

    /**
     * Method that obtains the format associated to a signer.
     * @param temporalFormat Parameter that represents the current format defined for the signer. This parameter only allows one of these values:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * </ul>
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return one of these values:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LTA_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_LT_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_B_LEVEL}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_C}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_A}.</li>
     * </ul>
     */
    private static String resolveXAdESBaselineFormat(final String temporalFormat, final Element signatureElement) {
	String format = temporalFormat;
	// Comprobamos si el firmante tiene
	// el formato XAdES B-Level
	if (isXAdESBLevel(signatureElement)) {
	    // Indicamos que el formato por ahora es XAdES B-Level
	    format = ISignatureFormatDetector.FORMAT_XADES_B_LEVEL;

	    // Comprobamos si el firmante
	    // tiene
	    // el formato XAdES T-Level
	    if (isXAdEST(signatureElement)) {
		// Indicamos que el formato por ahora es XAdES T-Level
		format = ISignatureFormatDetector.FORMAT_XADES_T_LEVEL;

		// Comprobamos si el firmante
		// tiene
		// el formato XAdES LT-Level
		if (isXAdESLTLevel(signatureElement)) {
		    // Indicamos que el formato por ahora es XAdES
		    // LT-Level
		    format = ISignatureFormatDetector.FORMAT_XADES_LT_LEVEL;

		    // Comprobamos si el firmante
		    // tiene
		    // el formato XAdES LTA-Level
		    if (isXAdESLTALevel(signatureElement)) {
			// Indicamos que el formato por ahora es XAdES
			// LTA-Level
			format = ISignatureFormatDetector.FORMAT_XADES_LTA_LEVEL;

		    }
		} else {
		    resolveXAdESNoBaselineFormat(format, signatureElement);
		}

	    }
	} else if (isXAdESBBLevel(signatureElement)) {
		format = ISignatureFormatDetector.FORMAT_XADES_B_B_LEVEL;
	}
	return format;
    }

    /**
     * Method that obtains the format no Baseline of a signer.
     * @param temporalFormat Parameter that represents the current format associated to the signer. This parameter only allows one of these values:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * </ul>
     * @param signatureElement Parameter that represents the <code>ds:Signature</code> element.
     * @return the format associated to the signer. The value to return will be on of these:
     * <ul>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_A}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_XL1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X2}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_X1}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_T}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_C}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_BES}.</li>
     * <li>{@link ISignatureFormatDetector#FORMAT_XADES_EPES}.</li>
     * </ul>
     */
    private static String resolveXAdESNoBaselineFormat(final String temporalFormat, final Element signatureElement) {
	String format = temporalFormat;

	// Comprobamos si el firmante
	// tiene
	// el formato XAdES-A
	if (isXAdESA(signatureElement)) {
	    // Indicamos que el formato es XAdES-A
	    format = ISignatureFormatDetector.FORMAT_XADES_A;
	}
	// Comprobamos si el firmante
	// tiene
	// el formato XAdES-XL1
	else if (isXAdESXL1(signatureElement)) {
	    // Indicamos que el formato es XAdES-XL1
	    format = ISignatureFormatDetector.FORMAT_XADES_XL1;
	}
	// Comprobamos si el firmante
	// tiene
	// el formato XAdES-XL2
	else if (isXAdESXL2(signatureElement)) {
	    // Indicamos que el formato es XAdES-XL2
	    format = ISignatureFormatDetector.FORMAT_XADES_XL2;
	}
	// Comprobamos si el firmante
	// tiene
	// el formato XAdES-X1
	else if (isXAdESX1(signatureElement)) {
	    // Indicamos que el formato es XAdES-X1
	    format = ISignatureFormatDetector.FORMAT_XADES_X1;
	}
	// Comprobamos si el firmante
	// tiene
	// el formato XAdES-X2
	else if (isXAdESX2(signatureElement)) {
	    // Indicamos que el formato es XAdES-X2
	    format = ISignatureFormatDetector.FORMAT_XADES_X2;
	}
	// Comprobamos si el firmante
	// tiene
	// el formato XAdES-C
	else if (isXAdESC(signatureElement)) {
	    // Indicamos que el formato es XAdES-C
	    format = ISignatureFormatDetector.FORMAT_XADES_C;
	}
	// Comprobamos si el firmante
	// tiene
	// el formato XAdES-T
	else if (isXAdEST(signatureElement) && !format.equals(ISignatureFormatDetector.FORMAT_XADES_T_LEVEL)) {
	    // Indicamos que el formato es XAdES-T
	    format = ISignatureFormatDetector.FORMAT_XADES_T;
	}
	return format;
    }

    /**
     * Method that indicates if a signature format is associated to Baseline form.
     * @param signatureFormat Parameter that represents the signature format.
     * @return a boolean that indicates if a signature format is associated to Baseline form.
     */
    public static boolean isXAdESBaseline(final String signatureFormat) {
	return signatureFormat.equals(ISignatureFormatDetector.FORMAT_XADES_B_LEVEL) || signatureFormat.equals(ISignatureFormatDetector.FORMAT_XADES_T_LEVEL) || signatureFormat.equals(ISignatureFormatDetector.FORMAT_XADES_LT_LEVEL) || signatureFormat.equals(ISignatureFormatDetector.FORMAT_XADES_LTA_LEVEL);
    }

    /**
     * Method that obtains the list of <code>ds:Signature</code> elements, as signatures, contained inside of a XML document.
     * @param doc Parameter that represents the XML document.
     * @return a list with the <code>ds:Signature</code> elements.
     */
    public static List<Element> getListSignatures(final Document doc) {

		// Instanciamos la lista a devolver
		final List<Element> listSignatureElements = new ArrayList<Element>();

		// Obtenemos la lista de elementos ds:Signature contenidos en el
		// documento XML
		final NodeList nlSignature = doc.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
		if (nlSignature != null) {
			// Recorremos la lista de elementos ds:Signature
			for (int i = 0; i < nlSignature.getLength(); i++) {
				final Element signatureNode = (Element) nlSignature.item(i);
				// Comprobamos que el elemento ds:Signature no haga
				// referencia a un sello de tiempo XML
				if (signatureNode.getParentNode() == null || !XAdESConstants.TAG_XML_TIMESTAMP.equals(signatureNode.getParentNode().getLocalName()) && !XAdESConstants.TAG_TIMESTAMP.equals(signatureNode.getParentNode().getLocalName())) {
					// Añadimos el elemento a la lista que devolver
					listSignatureElements.add(signatureNode);
				}
			}
		}
		return listSignatureElements;
    }
}
