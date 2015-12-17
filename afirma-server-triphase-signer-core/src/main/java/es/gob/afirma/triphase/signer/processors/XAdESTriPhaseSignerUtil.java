package es.gob.afirma.triphase.signer.processors;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.signers.xml.CustomUriDereferencer;
import es.gob.afirma.signers.xml.Utils;

final class XAdESTriPhaseSignerUtil {

	private XAdESTriPhaseSignerUtil() {
		// No instanciable
	}

	private static final String DS_NAMESPACE_URL = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$
	private static final String SA_NAMESPACE_URL = "http://uri.etsi.org/01903#SignedProperties"; //$NON-NLS-1$

	private static final String USE_MANIFEST = "useManifest"; //$NON-NLS-1$

	static byte[] insertCommonParts(final byte[] xmlBase,
			                        final byte[] xmlSource,
			                        final Properties extraParams) throws SAXException,
	                                                                     IOException,
	                                                                     ParserConfigurationException {

		if (extraParams != null && Boolean.parseBoolean(extraParams.getProperty(USE_MANIFEST))) {
			return xmlBase;
		}

		final Document docBase = getDocumentFromBytes(xmlBase);
		final List<List<String>> elDeliBase = getCommonContentDelimiters(
			getInmutableReferences(
				docBase
			),
			docBase
		);

		final Document docSource = getDocumentFromBytes(xmlSource);
		final List<List<String>> elDeliSource = getCommonContentDelimiters(
			getInmutableReferences(
				docSource
			),
			docSource
		);

		if (elDeliBase.size() != elDeliSource.size()) {
			throw new IllegalArgumentException(
				"El documento base no tiene las mismas partes comunes que el documento fuente" //$NON-NLS-1$
			);
		}

		String base = new String(xmlBase);
		final String source = new String(xmlSource);

		for (int i=0; i<elDeliBase.size(); i++) {
			base = base.replace(
				base.substring(
					base.indexOf(elDeliBase.get(i).get(0)) + elDeliBase.get(i).get(0).length(),
					base.indexOf(elDeliBase.get(i).get(1), base.indexOf(elDeliBase.get(i).get(0)) + elDeliBase.get(i).get(0).length())
				),
				source.substring(
					source.indexOf(elDeliSource.get(i).get(0)) + elDeliSource.get(i).get(0).length(),
					source.indexOf(elDeliSource.get(i).get(1), source.indexOf(elDeliSource.get(i).get(0)) + elDeliSource.get(i).get(0).length())
				)
			);
		}

		return base.getBytes();

	}

	static String removeCommonParts(final byte[] xml, final Properties extraParams) {

		if (xml == null || xml.length < 1) {
			throw new IllegalArgumentException("El XML de entrada no puede ser nulo ni vacio"); //$NON-NLS-1$
		}

		String ret = new String(xml);

		if (extraParams != null && Boolean.parseBoolean(extraParams.getProperty(USE_MANIFEST))) {
			return ret;
		}

		final org.w3c.dom.Document doc;
		try {
			doc = XAdESTriPhaseSignerUtil.getDocumentFromBytes(xml);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No ha podido tratarse la entrada como XML, no se eliminaran las partes comunes: " + e //$NON-NLS-1$
			);
			return ret;
		}
		final List<List<String>> delits = XAdESTriPhaseSignerUtil.getCommonContentDelimiters(
			XAdESTriPhaseSignerUtil.getInmutableReferences(doc),
			doc
		);

		for (final List<String> delPair : delits) {
			ret = ret.replace(
				ret.substring(
					ret.indexOf(delPair.get(0)) + delPair.get(0).length(),
					ret.indexOf(delPair.get(1), ret.indexOf(delPair.get(0)) + delPair.get(0).length())
				),
				TriPhaseUtil.getSignatureId(extraParams)
			);
		}

		return ret;
	}

	private static List<List<String>> getCommonContentDelimiters(final List<String> uris, final Document doc) {
		final List<List<String>> ret = new ArrayList<List<String>>();
		for (final String uriValue : uris) {
			final Node node = CustomUriDereferencer.getNodeByInternalUriReference(uriValue, doc);
			if (node != null) {
				ret.add(
					getFirstTagPair(
						removeXmlHeader(
							new String(
								Utils.writeXML(node, null, null, null)
							)
						)
					)
				);
			}
		}
		return ret;
	}

	private static Document getDocumentFromBytes(final byte[] data) throws SAXException,
	                                                               IOException,
	                                                               ParserConfigurationException {
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        return dbf.newDocumentBuilder().parse(
    		new ByteArrayInputStream(data)
		);
	}

	private static List<String> getInmutableReferences(final Document doc) {

		final Element signDoc = doc.getDocumentElement();

        // Obtenemos las firmas del documento
        final NodeList nl = signDoc.getElementsByTagNameNS(DS_NAMESPACE_URL, "Signature"); //$NON-NLS-1$

        // Por cada firma buscamos sus referencias
        final List<String> unmutableReferences = new ArrayList<String>();
        for(int i=0; i<nl.getLength(); i++) {
        	final Element sigs = (Element) nl.item(i);
        	final NodeList rf = sigs.getElementsByTagNameNS(DS_NAMESPACE_URL, "Reference"); //$NON-NLS-1$
        	for(int j=0; j<rf.getLength(); j++) {
        		final Node node = rf.item(j);
        		if (!SA_NAMESPACE_URL.equals(((Element)node).getAttribute("Type"))) { //$NON-NLS-1$
        			final String uri = ((Element)node).getAttribute("URI"); //$NON-NLS-1$
        			if (uri != null) {
        				unmutableReferences.add(uri);
        			}
        		}
        	}
        }

        return unmutableReferences;
	}

	private static String removeXmlHeader(final String xml) {
		if (xml == null) {
			throw new IllegalArgumentException("La entrada no puede ser nula"); //$NON-NLS-1$
		}
		if (!xml.startsWith("<?xml")) { //$NON-NLS-1$
			return xml;
		}
		return xml.substring(xml.indexOf("?>") + "?>".length(), xml.length()).trim(); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static List<String> getFirstTagPair(final String xml) {
		if (xml == null) {
			throw new IllegalArgumentException("La entrada no puede ser nula"); //$NON-NLS-1$
		}
		if (!xml.contains("<") || !xml.contains(">")) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new IllegalArgumentException("La entrada no tiene ninguna etiqueta XML"); //$NON-NLS-1$return xml;
		}
		final List<String> ret = new ArrayList<String>(2);
		ret.add(xml.substring(0, xml.indexOf(">") + 1).trim()); //$NON-NLS-1$
		ret.add(xml.substring(xml.lastIndexOf("<"), xml.length()).trim()); //$NON-NLS-1$
		return ret;
	}

}
