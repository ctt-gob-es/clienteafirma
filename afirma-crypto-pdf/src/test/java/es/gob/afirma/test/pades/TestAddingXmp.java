package es.gob.afirma.test.pades;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;

import org.junit.Test;

import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Prueba de adici&oacute;n de XMP a un PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestAddingXmp {

	@SuppressWarnings("static-method")
	@Test
	public void test() throws Exception {

		// PDF de ejemplo
		final PdfReader reader = new PdfReader(
			AOUtil.getDataFromInputStream(
				ClassLoader.getSystemResourceAsStream("TEST_PDF.pdf") //$NON-NLS-1$
			)
		);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final Calendar globalDate = new GregorianCalendar();
		final PdfStamper stamper = new PdfStamper(reader, baos, globalDate);

//		// Datos a insertar como XMP
//		final BioMetadataSchema schema = new BioMetadataSchema();
//		schema.addIso197947Data("HOLA".getBytes()); //$NON-NLS-1$
//
//		// Insertamos los datos en el XMP
//		final ByteArrayOutputStream os = new ByteArrayOutputStream();
//        final XmpWriter xmp = new XmpWriter(os);
//        xmp.addRdfDescription(schema);
//        xmp.close();
//
//        // Insertamos el XMP en el PDF
//        stamper.setXmpMetadata(os.toByteArray());

		final String sigDataBase64 = Base64.encode(AOUtil.getDataFromInputStream(
			ClassLoader.getSystemResourceAsStream("4df6ec6b6b5c7.jpg") //$NON-NLS-1$
		));
		final HashMap<String, String> moreInfo = new HashMap<>(1);
		moreInfo.put("SignerBiometricSignatureData", sigDataBase64); //$NON-NLS-1$
		moreInfo.put("SignerBiometricSignatureFormat", "ISO 19795-7"); //$NON-NLS-1$ //$NON-NLS-2$
		moreInfo.put("SignerName", "Tom\u00E1s Garc\u00EDa-Mer\u00E1s"); //$NON-NLS-1$ //$NON-NLS-2$
		stamper.setMoreInfo(moreInfo);

		stamper.close(globalDate);
		reader.close();

        // Guardamos el resultado
        final File tmpFile = File.createTempFile("TESTXMP_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream fos = new FileOutputStream(tmpFile);
		) {
	        fos.write(baos.toByteArray());
	        fos.flush();
        }
	}

//	private static final class BioMetadataSchema extends XmpSchema {
//
//		private static final long serialVersionUID = 4086239408037761846L;
//
//		/** Datos biom&eacute;tricos de firma manuscrita seg&uacute;n ISO 19795-7. */
//		public static final String ISO197947DATA = "ls:Iso197947Data"; //$NON-NLS-1$
//
//		public static final String DEFAULT_XPATH_ID = "ls"; //$NON-NLS-1$
//	    public static final String DEFAULT_XPATH_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"; //$NON-NLS-1$
//
//		BioMetadataSchema() {
//			super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
//		}

//		private void addIso197947Data(final byte[] iso197947Asn1Data) {
//			if (iso197947Asn1Data == null) {
//				throw new IllegalArgumentException("Los datos ISO 19795-7 no pueden ser nulos"); //$NON-NLS-1$
//			}
//			final String base64Data = Base64.encode(iso197947Asn1Data);
//			final XmpArray<String> array = new XmpArray<String>(XmpArray.UNORDERED);
//			array.add(base64Data);
//			setProperty(ISO197947DATA, array);
//		}

//	}

}
