package es.gob.afirma.test.pades;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.InputSource;

import com.aowagie.text.DocumentException;
import com.aowagie.text.pdf.PdfDate;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.AOUtil;

/** Pruebas de operaciones sobre XMP.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestMetadata {

    private static final String TAG_UUID = "$$UUID$$"; //$NON-NLS-1$
    private static final String TAG_DATE = "$$DATE$$"; //$NON-NLS-1$

    private static final String PROCESSING_INSTRUCTION_SUFFIX = "?>"; //$NON-NLS-1$

    private static final String DEFAULT_ENCODING = "UTF-8"; //$NON-NLS-1$

	private static final String NEW_HISTORY_LIST_ITEM = "<rdf:li rdf:parseType=\"Resource\">\n" + //$NON-NLS-1$
			"  <stEvt:action>signed</stEvt:action>\n" + //$NON-NLS-1$
			"  <stEvt:instanceID>uuid:" + TAG_UUID + "</stEvt:instanceID>\n" + //$NON-NLS-1$ //$NON-NLS-2$
			"  <stEvt:parameters>Firmado por el Cliente @firma</stEvt:parameters>\n" + //$NON-NLS-1$
			"  <stEvt:softwareAgent>Cliente @firma</stEvt:softwareAgent>\n" + //$NON-NLS-1$
			"  <stEvt:when>" + TAG_DATE + "</stEvt:when>\n" + //$NON-NLS-1$ //$NON-NLS-2$
			"</rdf:li>"; //$NON-NLS-1$

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws Exception en cuauqluier error. */
	public static void main(final String[] args) throws Exception {

		// PDF de ejemplo
		final PdfReader reader = new PdfReader(
			AOUtil.getDataFromInputStream(
				ClassLoader.getSystemResourceAsStream("PDF-A1B.pdf") //$NON-NLS-1$
			)
		);

		final byte[] xmpBytes = reader.getMetadata();

		final String originalXmp = new String(xmpBytes, DEFAULT_ENCODING);
		final String originalXmpHeader = originalXmp.substring(
			0,
			originalXmp.indexOf(PROCESSING_INSTRUCTION_SUFFIX) + PROCESSING_INSTRUCTION_SUFFIX.length()
		).replace(PROCESSING_INSTRUCTION_SUFFIX, " encoding=\"" + DEFAULT_ENCODING.toLowerCase() + "\"" + PROCESSING_INSTRUCTION_SUFFIX); //$NON-NLS-1$ //$NON-NLS-2$
		final String originalXmpFooter = originalXmp.substring(
			originalXmp.lastIndexOf("<?"), //$NON-NLS-1$
			originalXmp.length()
		);

		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		final DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		final Document doc = db.parse(new ByteArrayInputStream(xmpBytes));
		NodeList nl = doc.getElementsByTagName("xmpMM:History"); //$NON-NLS-1$
		if (nl.getLength() != 1) {
			throw new IllegalStateException();
		}

		final Element e = (Element) nl.item(0);

		nl = e.getElementsByTagName("rdf:Seq"); //$NON-NLS-1$
		if (nl.getLength() != 1) {
			throw new IllegalStateException();
		}

		final Node n = nl.item(0);

		final Node node =  doc.importNode(
			db.parse(
				new InputSource(
					new StringReader(
						NEW_HISTORY_LIST_ITEM.replace(
							TAG_UUID,
							UUID.randomUUID().toString()
						).replace(
							TAG_DATE,
							new PdfDate(new GregorianCalendar()).getW3CDate()
						)
					)
				)
			).getDocumentElement(),
			true
		);

		n.appendChild(node);

		final Map<String, String> props = new ConcurrentHashMap<String, String>(1);
		props.put("encoding", "utf-8"); //$NON-NLS-1$ //$NON-NLS-2$

		String xmlString = new String(
			writeXml(doc.getDocumentElement()),
			DEFAULT_ENCODING
		);
		xmlString = xmlString.replace(
			xmlString.substring(
				0,
				xmlString.indexOf(PROCESSING_INSTRUCTION_SUFFIX) + PROCESSING_INSTRUCTION_SUFFIX.length()
			),
			originalXmpHeader
		).concat(originalXmpFooter);

		System.out.println(xmlString);


		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final Calendar globalDate = new GregorianCalendar();

		try {
			final PdfStamper stamper = new PdfStamper(reader, baos, globalDate);
			stamper.setXmpMetadata(xmlString.getBytes(DEFAULT_ENCODING));
			stamper.close(globalDate);
		}
		catch(final DocumentException ex) {
			throw new IOException(ex);
		}

		reader.close();

		final OutputStream fos = new FileOutputStream(File.createTempFile("PDFXMP_", ".txt")); //$NON-NLS-1$ //$NON-NLS-2$
		fos.write(baos.toByteArray());
		fos.close();

	}

    /** Escribe un XML como texto.
     * @param node Nodo XML que queremos pasar a texto.
     * @return Cadena de texto con el XML en forma de array de octetos */
	private static byte[] writeXml(final Node node) {
		final DOMImplementationLS domImpl = (DOMImplementationLS) node.getOwnerDocument().getImplementation();
        final LSSerializer lsSerializer = domImpl.createLSSerializer();
        final DOMConfiguration domConfiguration = lsSerializer.getDomConfig();
        if (domConfiguration.canSetParameter("namespaces", Boolean.FALSE)) { //$NON-NLS-1$
        	domConfiguration.setParameter("namespaces", Boolean.FALSE); //$NON-NLS-1$
        }
        if (domConfiguration.canSetParameter("canonical-form", Boolean.TRUE)) { //$NON-NLS-1$
            lsSerializer.getDomConfig().setParameter("canonical-form", Boolean.TRUE); //$NON-NLS-1$
        }
        final LSOutput lsOutput = domImpl.createLSOutput();
        lsOutput.setEncoding(DEFAULT_ENCODING);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        lsOutput.setByteStream(baos);
        lsSerializer.write(node, lsOutput);
        return baos.toByteArray();
    }

}
