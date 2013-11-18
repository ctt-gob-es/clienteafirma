package es.gob.afirma.signers.pades;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Calendar;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.Base64;


/** Resultado de una pre-firma (como primera parte de un firma trif&aacute;sica) o una firma completa PAdES. */
/** <i>JavaBean</i> que encapsula los resultados de la pre-firma o firma completa PDF. */
public final class PdfSignResult implements Serializable {

	private static final long serialVersionUID = 2L;

	private final String fileID;
    private final byte[] sign;
    private final Calendar signTime;
    private final Properties extraParams;

    PdfSignResult(final String pdfFileId,
    		         final byte[] preSignature,
    		         final Calendar signingTime,
    		         final Properties xParams) {
        if (signingTime == null || pdfFileId == null || preSignature == null || "".equals(pdfFileId) || preSignature.length < 1) { //$NON-NLS-1$
            throw new IllegalArgumentException("Es obligatorio proporcionar un MAC, una pre-firma y un momento de firmado"); //$NON-NLS-1$
        }
        this.fileID = pdfFileId;
        this.sign = preSignature.clone();
        this.signTime = signingTime;
        this.extraParams = xParams != null ? xParams : new Properties();
    }

    /** Obtiene las opciones adicionales de la firma.
     * @return Opciones adicionales de la firma */
    public Properties getExtraParams() {
    	return this.extraParams;
    }

    /** Obtiene el FileID (<i>/ID</i>) del diccionario PDF generado.
     * @return FileID del diccionario PDF generado */
    public String getFileID() {
        return this.fileID;
    }

    /** Obtiene los atributos CAdES a firmar.
     * @return Atributos CAdES a firmar (pre-firma) */
    public byte[] getSign() {
        return this.sign;
    }

    /** Obtiene el momento en el que se realiz&oacute; la firma.
     * @return Momento en el que se realiz&oacute; la firma */
    public Calendar getSignTime() {
    	return this.signTime;
    }

    private static String properties2Base64(final Properties p) throws IOException {
    	if (p == null) {
    		return ""; //$NON-NLS-1$
    	}
    	final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    	p.store(baos, "PAdES"); //$NON-NLS-1$
    	return Base64.encode(baos.toByteArray());
    }

    private static Properties base642Properties(final String base64) throws IOException {
    	final Properties p = new Properties();
    	if (base64 == null) {
    		return p;
    	}
    	p.load(
			new ByteArrayInputStream(
				Base64.decode(base64)
			)
		);
    	return p;
    }

    private void writeObject(final ObjectOutputStream out) throws IOException {
    	final StringBuilder sb = new StringBuilder()
    		.append("<signResult>\n") //$NON-NLS-1$
    		.append(" <extraParams>\n") //$NON-NLS-1$
    		.append(properties2Base64(getExtraParams())).append('\n')
    		.append(" </extraParams>\n") //$NON-NLS-1$
    		.append(" <pdfId>\n") //$NON-NLS-1$
    		.append(getFileID()).append('\n')
    		.append(" </pdfIf>") //$NON-NLS-1$
    		.append(" <sign>\n") //$NON-NLS-1$
    		.append(Base64.encode(getSign())).append('\n')
    		.append(" </sign>\n") //$NON-NLS-1$
    		.append(" <signTime>") //$NON-NLS-1$
    		.append(Long.toString(getSignTime().getTimeInMillis())).append('\n')
    		.append(" </signTime>") //$NON-NLS-1$
    		.append("</signResult>") //$NON-NLS-1$
		;
    	out.write(sb.toString().getBytes());
    }

    @SuppressWarnings({ "static-method", "unused" })
	private void readObject(final ObjectInputStream in) throws IOException, ClassNotFoundException {
    	final Document xmlSign;
    	try {
    		xmlSign = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
		}
    	catch (final SAXException e) {
			throw new IOException(e);
		}
    	catch (final ParserConfigurationException e) {
    		throw new IOException(e);
		}
    	//TODO: Implementar la deserializacion XML
    	throw new UnsupportedOperationException();
    }

}