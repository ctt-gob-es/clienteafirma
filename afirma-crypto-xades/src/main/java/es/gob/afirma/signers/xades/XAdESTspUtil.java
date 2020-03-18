package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;
import es.gob.afirma.signers.xml.Utils;
import nu.xom.converters.DOMConverter;

/** Estampador de sellos de tiempo en firmas XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class XAdESTspUtil {

	private static final String DEFAULT_CANONICAL_ALGO = nu.xom.canonical.Canonicalizer.CANONICAL_XML;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static DocumentBuilderFactory D_FACTORY = DocumentBuilderFactory.newInstance();
    static {
	    try {
			D_FACTORY.setFeature(
				javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING,
				Boolean.TRUE.booleanValue()
			);
		}
	    catch (final ParserConfigurationException e) {
			LOGGER.warning(
				"No se ha podido habilitar el proceso seguro en la factoria DOM: " + e //$NON-NLS-1$
			);
		}
	    D_FACTORY.setValidating(false);
	    D_FACTORY.setNamespaceAware(true);
    }

	private XAdESTspUtil() {
		// No instanciable
	}

	/** Estampa un sello de tiempo a una firma XAdES.
	 * @param xml XAdES de entrada.
	 * @param extraParams Configuraci&oacute;n de la TSA.
	 * @return Firmas XAdES con el sello de tiempo estampado.
	 * @throws AOException Si ocurre cualquier problema durante el proceso. */
	public static byte[] timestampXAdES(final byte[] xml,
                                        final Properties extraParams) throws AOException {
		if (extraParams == null) {
			return xml;
		}

		final TsaParams tsaParams;
		try {
			tsaParams = new TsaParams(extraParams);
		}
		catch (final Exception e) {
			return xml;
		}

		final Document doc;
		try {
			doc = D_FACTORY.newDocumentBuilder().parse(
				new ByteArrayInputStream(
					xml
				)
			);
		}
		catch (SAXException | IOException | ParserConfigurationException e) {
			throw new AOException(
				"No se puede analizar la firma para agregar el sello de tiempo: " + e, e //$NON-NLS-1$
			);
		}

		final NodeList nl = doc.getElementsByTagNameNS(
			"http://uri.etsi.org/01903/v1.3.2#", //$NON-NLS-1$
			"QualifyingProperties" //$NON-NLS-1$
		);
		if (nl.getLength() < 1) {
			throw new AOException(
				"La firma no tiene 'QualifyingProperties', no se puede aplicar el sello de tiempo" //$NON-NLS-1$
			);
		}
		else if (nl.getLength() > 1) {
			LOGGER.warning(
				"El XML tiene mas de una firma, solo se sellara la primera" //$NON-NLS-1$
			);
		}
		final Node qualifyingPropertiesNode = nl.item(0);

		final String tspTokenB64;
		try {
			tspTokenB64 = getBase64XAdESTimestampToken(
				getSigningTime(
					qualifyingPropertiesNode
				),
				doc,
				tsaParams
			);
		}
		catch (NoSuchAlgorithmException | IOException e) {
			throw new AOException(
				"Error obteniendo el sello de la TSA: " + e, e //$NON-NLS-1$
			);
		}

		final Element tspNode;
		try {
			tspNode = D_FACTORY.newDocumentBuilder().parse(
				new ByteArrayInputStream(
					(
						"<xades:UnsignedProperties xmlns:xades=\"http://uri.etsi.org/01903/v1.3.2#\">\n" + //$NON-NLS-1$
					    " <xades:UnsignedSignatureProperties>\n" + //$NON-NLS-1$
						"  <xades:SignatureTimeStamp>\n" + //$NON-NLS-1$
					    "   <ds:CanonicalizationMethod xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Algorithm=\"" + DEFAULT_CANONICAL_ALGO + "\"/>\n" + //$NON-NLS-1$ //$NON-NLS-2$
						"   <xades:EncapsulatedTimeStamp>\n" + tspTokenB64 + "</xades:EncapsulatedTimeStamp>\n" +  //$NON-NLS-1$//$NON-NLS-2$
					    "  </xades:SignatureTimeStamp>\n" + //$NON-NLS-1$
						" </xades:UnsignedSignatureProperties>\n" + //$NON-NLS-1$
					    "</xades:UnsignedProperties>\n" //$NON-NLS-1$
				    ).getBytes(StandardCharsets.UTF_8)
				)
			) .getDocumentElement();
		}
		catch (SAXException | IOException | ParserConfigurationException e) {
			throw new AOException(
				"Error creando el nodo XML de sello de tiempo: " + e, e //$NON-NLS-1$
			);
		}

		final Node tspAdoptedNode = doc.importNode(tspNode, true);
		qualifyingPropertiesNode.appendChild(tspAdoptedNode);

		return Utils.writeXML(
			doc.getDocumentElement(),
			getOriginalXMLProperties(
				doc,
				null // Codificacion de salida
			),
			null,
			null
		);
	}

	private static String getBase64XAdESTimestampToken(final Calendar time,
			                                           final Document doc,
			                                           final TsaParams tsaParams) throws NoSuchAlgorithmException,
	                                                                                     IOException,
	                                                                                     AOException {
		return Base64.encode(
			new CMSTimestamper(tsaParams).getTimeStampToken(
				getSignatureNodeDigest(
					doc,
					tsaParams.getTsaHashAlgorithm()
				),
				tsaParams.getTsaHashAlgorithm(),
				time
			)
		);
	}

	private static Calendar getSigningTime(final Node qualifyingPropertiesNode) {
		if (!(qualifyingPropertiesNode instanceof Element)) {
			return new GregorianCalendar();
		}
		final Element e = (Element) qualifyingPropertiesNode;
		final NodeList nl = e.getElementsByTagNameNS(
			"http://uri.etsi.org/01903/v1.3.2#", //$NON-NLS-1$
			"SigningTime" //$NON-NLS-1$
		);
		if (nl.getLength() > 1) {
			return new GregorianCalendar();
		}
		final Node signingTimeNode = nl.item(0);

		GregorianCalendar calendar;
		try {
			calendar = DatatypeFactory.newInstance().newXMLGregorianCalendar(
						signingTimeNode.getTextContent().trim()
					).toGregorianCalendar();
		} catch (final Exception ex) {
			throw new RuntimeException("No se pudo instanciar la factoria para le parseo de una fecha del XML", ex); //$NON-NLS-1$
		}

		return calendar;
//		return DatatypeConverter.parseDateTime(
//			signingTimeNode.getTextContent()
//		);
	}

	private static byte[] getSignatureNodeDigest(final Document doc,
			                                           final String algorithm) throws NoSuchAlgorithmException,
	                                                                                  IOException {
		final NodeList nl = doc.getElementsByTagNameNS(
			"http://www.w3.org/2000/09/xmldsig#", //$NON-NLS-1$
			"SignatureValue" //$NON-NLS-1$
		);
		if (nl.getLength() < 1) {
			throw new IOException(
				"El XML no tiene nodo de firma" //$NON-NLS-1$
			);
		}
		if (nl.getLength() > 1) {
			LOGGER.warning("El XML tiene mas de un nodo de firma, se tratara solo el primero"); //$NON-NLS-1$
		}

		return MessageDigest.getInstance(algorithm).digest(
			canonicalizeXml((Element) nl.item(0), DEFAULT_CANONICAL_ALGO)
		);

	}

	private static byte[] canonicalizeXml(final org.w3c.dom.Element element,
			                              final String algorithm) throws IOException {
		final nu.xom.Element xomElement = DOMConverter.convert(element);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final nu.xom.canonical.Canonicalizer canonicalizer = new nu.xom.canonical.Canonicalizer(baos, algorithm);
		canonicalizer.write(xomElement);
		return baos.toByteArray();
	}

	private static Map<String, String> getOriginalXMLProperties(final Document docum,
			                                                   final String outputXmlEncoding) {

		final Map<String, String> originalXMLProperties = new Hashtable<>();
		if (docum != null) {
			if (outputXmlEncoding != null) {
				originalXMLProperties.put(OutputKeys.ENCODING, outputXmlEncoding);
			}
			else if (docum.getXmlEncoding() != null) {
				originalXMLProperties.put(OutputKeys.ENCODING, docum.getXmlEncoding());
			}

			String tmpXmlProp = docum.getXmlVersion();
			if (tmpXmlProp != null) {
				originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
			}

			final DocumentType dt = docum.getDoctype();
			if (dt != null) {
				tmpXmlProp = dt.getSystemId();
				if (tmpXmlProp != null) {
					originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
				}
			}
		}
		return originalXMLProperties;
	}

}
