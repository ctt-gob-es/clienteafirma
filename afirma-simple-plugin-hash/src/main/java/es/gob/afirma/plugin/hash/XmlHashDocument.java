package es.gob.afirma.plugin.hash;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/**
 * Documento XML con los hashes de los ficheros de un directorio. El documento seguir&aacute;
 * el esquema:
 * <pre>
 * &lt;xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema"&gt;
 *	&lt;xs:element name="entries"&gt;
 *		&lt;xs:complexType&gt;
 * 			&lt;xs:sequence&gt;
 *   			&lt;xs:element name="entry" maxOccurs="unbounded" minOccurs="0"&gt;
 *     				&lt;xs:complexType&gt;
 *       				&lt;xs:simpleContent&gt;
 *         					&lt;xs:extension base="xs:string"&gt;
 *           					&lt;xs:attribute type="xs:string" name="hash" use="required"/&gt;
 *           					&lt;xs:attribute type="xs:string" name="name" use="required"/&gt;
 *           					&lt;xs:attribute type="xs:string" name="hexhash"/&gt;
 *         					&lt;\xs:extension&gt;
 *       				&lt;\xs:simpleContent&gt;
 *				     &lt;\xs:complexType&gt;
 *   			&lt;\xs:element&gt;
 *		 	&lt;\xs:sequence&gt;
 *		 &lt;xs:attribute name="hashAlgorithm"&gt;
 * 			&lt;xs:simpleType&gt;
 *				&lt;xs:restriction base="xs:string"&gt;
 * 					&lt;xs:enumeration value="SHA-1"/&gt;
 *					&lt;xs:enumeration value="SHA-256"/&gt;
 *					&lt;xs:enumeration value="SHA-384"/&gt;
 *					&lt;xs:enumeration value="SHA-512"/&gt;
 *				&lt;\xs:restriction&gt;
 *			&lt;\xs:simpleType&gt;
 * 		  &lt;\xs:attribute&gt;
 * 	     &lt;xs:attribute type="xs:boolean" name="recursive" use="required"/&gt;
 * 	   &lt;\xs:complexType&gt;
 *	 &lt;\xs:element&gt;
 * &lt;\xs:schema&gt;
 * </pre>
 */
public class XmlHashDocument extends HashDocument {

	public XmlHashDocument() {
		super();
	}

	@Override
	public byte[] generate() throws DocumentException {

		final DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		final DocumentBuilder docBuilder;
		try {
			docBuilder = docFactory.newDocumentBuilder();
		}
		catch (final Exception e) {
			throw new DocumentException("No se puede componer el documento XML", e); //$NON-NLS-1$
		}

		// Elemento raiz
		final Document doc = docBuilder.newDocument();
		final Element rootElement = doc.createElement("entries"); //$NON-NLS-1$
		doc.appendChild(rootElement);
		final Attr hashAlg = doc.createAttribute("hashAlgorithm"); //$NON-NLS-1$
		hashAlg.setValue(getAlgorithm());
		rootElement.setAttributeNode(hashAlg);
		final Attr recursive = doc.createAttribute("recursive"); //$NON-NLS-1$
		recursive.setValue(String.valueOf(isRecursive()));
		rootElement.setAttributeNode(recursive);

		final Map<String, byte[]> hashes = getHashes();
		final Set<String> paths = hashes.keySet();
		for (final String path : paths) {
			final byte[] hash = hashes.get(path);

			// Elemento entry
			final Element entry = doc.createElement("entry"); //$NON-NLS-1$
			rootElement.appendChild(entry);

			// Se inicializa el atributo name
			final Attr name = doc.createAttribute("name"); //$NON-NLS-1$
			name.setValue(path);
			entry.setAttributeNode(name);

			// Se inicializa el atributo hash
			final Attr hashAttribute = doc.createAttribute("hash"); //$NON-NLS-1$
			hashAttribute.setValue(Base64.encode(hash, true));
			entry.setAttributeNode(hashAttribute);

			// Se inicializa el atributo hexhash
			final Attr hexHashAttribute = doc.createAttribute("hexhash"); //$NON-NLS-1$
			hexHashAttribute.setValue(AOUtil.hexify(hash, false) + "h"); //$NON-NLS-1$
			entry.setAttributeNode(hexHashAttribute);
		}
		final StringWriter sw = new StringWriter();
		try {
			final TransformerFactory tf = TransformerFactory.newInstance();
			final Transformer transformer = tf.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.ENCODING, getCharset().name());

			transformer.transform(
					new DOMSource(doc), new StreamResult(sw)
					);
		}
		catch (final Exception e) {
			throw new DocumentException("Error al codificar el XML", e); //$NON-NLS-1$
		}

		return sw.toString().getBytes(getCharset());
	}

	@Override
	void load(final byte[] document) throws DocumentException, IOException, CorruptedDocumentException {

		Document doc;
		try {
			final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			final DocumentBuilder builder = factory.newDocumentBuilder();

			try (InputStream is = new ByteArrayInputStream(document)) {
				validateAgainstXSD(is, CheckHashDirDialog.class.getResourceAsStream("/schemas/folderhashes.xsd")); //$NON-NLS-1$
			}
			doc = builder.parse(new ByteArrayInputStream(document));
			final String encoding = doc.getXmlEncoding();
			if (encoding != null) {
				setCharset(Charset.forName(encoding));
			}
			doc.getDocumentElement().normalize();
		}
		catch (final IOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new DocumentException("El documento no es un XML valido",  e); //$NON-NLS-1$
		}

		final Map<String, byte[]> dirEntries = new HashMap<>();

		try {
			final NodeList nodeListEntries = doc.getElementsByTagName("entries"); //$NON-NLS-1$
			final Node nodeEntries = nodeListEntries.item(0);
			setAlgorithm(nodeEntries.getAttributes().getNamedItem("hashAlgorithm").getNodeValue()); //$NON-NLS-1$
			setRecursive(
					Boolean.parseBoolean(nodeEntries.getAttributes().getNamedItem("recursive").getNodeValue()) //$NON-NLS-1$
					);

			final NodeList nodeList = doc.getElementsByTagName("entry"); //$NON-NLS-1$
			for (int i = 0; i < nodeList.getLength(); i++) {
				final Node node = nodeList.item(i);

				final String name = node.getAttributes().getNamedItem("name").getNodeValue(); //$NON-NLS-1$

				final byte[] hashFromB64 = Base64.decode(
						node.getAttributes().getNamedItem("hash").getNodeValue(), true //$NON-NLS-1$
						);

				final byte[] hashFromHex = HexUtils.hexStringToByteArray(
						node.getAttributes().getNamedItem("hexhash").getNodeValue() //$NON-NLS-1$
						);

				if (!Arrays.equals(hashFromB64, hashFromHex)) {
					throw new CorruptedDocumentException("Se han encontrado que se han declarado dos hashes distintos para un fichero. Uno en hexadecimal y otro en Base 64"); //$NON-NLS-1$
				}

				dirEntries.put(name, hashFromB64);
			}
		}
		catch (final CorruptedDocumentException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new DocumentException("El formato del documento no es el esperado", e); //$NON-NLS-1$
		}

		setHashes(dirEntries);
	}

	private static void validateAgainstXSD(final InputStream isxml, final InputStream xsd) throws SAXException, IOException {

		final SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		final Schema schema = factory.newSchema(new StreamSource(xsd));
		final Validator validator = schema.newValidator();
		validator.validate(new StreamSource(isxml));
	}
}
