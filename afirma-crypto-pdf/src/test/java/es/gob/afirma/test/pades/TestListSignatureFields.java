package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.junit.Test;

import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PRAcroForm;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de tratamiento de campos de firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestListSignatureFields {

    private static final String CERT_PATH = "Cert Valido hasta 2021 - PruebaEmpleado4Activo_Giss2016.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "Giss2016"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$

	private static final String TEST_FILE = "/empty_signature_field.pdf"; //$NON-NLS-1$

	/** Pruebas de listado de campos de firma vac&iacute;ios.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testListEmptySignatureFields() throws Exception {
		final PdfReader reader = new PdfReader(
			TestListSignatureFields.class.getResourceAsStream(
				TEST_FILE
			)
		);
		final AcroFields fields = reader.getAcroFields();
		final List<String> emptySignatureFields = fields.getBlankSignatureNames();
		System.out.println(emptySignatureFields);
	}

	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testAnalizeSignatureFields() throws Exception {
		PdfReader reader;
		try (InputStream fis = new FileInputStream("C:\\Users\\carlos.gamuci\\Desktop\\test\\descargar_signed_campo.pdf")) { //$NON-NLS-1$
			reader = new PdfReader(fis);
		}

		System.out.println("Campos de formulario que empiezan por 'Signature': "); //$NON-NLS-1$
		for (final Object formField : reader.getAcroForm().getFields()) {
			final PRAcroForm.FieldInformation fieldInfo = (PRAcroForm.FieldInformation) formField;
			if (fieldInfo.getName().startsWith("Signature")) { //$NON-NLS-1$
				System.out.println("\t" + fieldInfo.getName()); //$NON-NLS-1$
				System.out.println("\t\tDetalle:"); //$NON-NLS-1$
				for (final PdfName pdfName : fieldInfo.getInfo().getKeys().toArray(new PdfName[0])) {
					System.out.println("\t\t\t" + pdfName + ": " + fieldInfo.getInfo().get(pdfName).toString()); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}

		final AcroFields fields = reader.getAcroFields();

		final Map<String, AcroFields.Item> fieldsMap = fields.getFields();


		System.out.println("Campos de firma: "); //$NON-NLS-1$
		for (final String fieldName : fieldsMap.keySet().toArray(new String[0])) {
			System.out.println("\t" + fieldName); //$NON-NLS-1$
		}

		System.out.println("Firmas: "); //$NON-NLS-1$
		for (final String signatureName : fields.getSignatureNames()) {
			System.out.println("\t" + signatureName); //$NON-NLS-1$
		}

		System.out.println("Campos de firma vacios: "); //$NON-NLS-1$
		final List<String> emptySignatureFields = fields.getBlankSignatureNames();
		System.out.println("\t" + emptySignatureFields); //$NON-NLS-1$

		System.out.println("Revisiones: " + fields.getTotalRevisions()); //$NON-NLS-1$
	}

	/** Prueba de firma usando un campo de un PDF.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSignEmptySignatureField() throws Exception {
		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Prueba de firma de un campo preexistente de un PDF" //$NON-NLS-1$
			);

	        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
	        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
	        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

	        // Configuracion de firma
			final Properties extraParams = new Properties();
			extraParams.setProperty("applySystemDate", "false"); //$NON-NLS-1$ //$NON-NLS-2$
			extraParams.setProperty("signatureRotation", "90"); //$NON-NLS-1$ //$NON-NLS-2$
			extraParams.setProperty("layer2Text", "Firmado electrnicamente por FIDEL MARTINEZ FERNANDEZ el da $$SIGNDATE=dd/MM/yyyy a las HH:mm:ss$$."); //$NON-NLS-1$ //$NON-NLS-2$
			extraParams.setProperty("layer2FontFamily", "1"); //$NON-NLS-1$ //$NON-NLS-2$
			extraParams.setProperty("layer2FontSize", "6"); //$NON-NLS-1$ //$NON-NLS-2$
			extraParams.setProperty("signatureField", "EmptySignatureField"); //$NON-NLS-1$ //$NON-NLS-2$

			final byte[] testPdf = AOUtil.getDataFromInputStream(TestListSignatureFields.class.getResourceAsStream(
					TEST_FILE
				));


			final AOPDFSigner signer = new AOPDFSigner();
			final byte[] signedPdf = signer.sign(
				testPdf,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				extraParams
			);

			// Guardamos el PDF para comprobaciones manuales
			final File tempFile = File.createTempFile("afirmaPDF-IMAGEN-TODAS_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
			try (
				final FileOutputStream fos = new FileOutputStream(tempFile);
			) {
				fos.write(signedPdf);
			}

			Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Fichero temporal para la comprobacion manual del resultado: " + //$NON-NLS-1$
					tempFile.getAbsolutePath()
			);
	}

}
