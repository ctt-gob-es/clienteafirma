package es.gob.afirma.signers.pades;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.aowagie.text.DocumentException;
import com.aowagie.text.Rectangle;
import com.aowagie.text.pdf.PdfAnnotation;
import com.aowagie.text.pdf.PdfFormField;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.test.pades.TestListSignatureFields;

/**
 * Ejemplos de creacion de campos de firma con iText.
 */
public class TestCreateSignFields {

	private static final String TEST_EMPTY_FILE = "/pdf_without_signatures.pdf"; //$NON-NLS-1$

	private static final String SIGNATURE_FIELD = "CampoFirma"; //$NON-NLS-1$

	private static final String SIGNATURE_FIELD_WITH_SYMBOLS = "Campo[Firm#]"; //$NON-NLS-1$

	private final static String DEFAULT_SIGNATURE_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    private PrivateKeyEntry pke = null;

    /**
     * Carga las claves para firmar.
     * @throws Exception Cuando falla la carga de la clave.
     */
    @Before
    public void loadKeyStore() throws Exception {
        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
    }

	/**
	 * Crea un campo de firma con presentaci&oacute;n en dos p&aacute;ginas
	 * del documento.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testCreateSignatureFieldsOnMultiPages() throws Exception {

		System.out.println("Prueba de generacion de campos de firma en multiples paginas"); //$NON-NLS-1$

		byte[] inPdf;
		try (InputStream is = TestListSignatureFields.class.getResourceAsStream(TEST_EMPTY_FILE)) {
			inPdf = AOUtil.getDataFromInputStream(is);
		}

		final File outFile = File.createTempFile("signedPdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Creamos el campo de firma
		createSignatureField(inPdf, outFile, SIGNATURE_FIELD, new int[] { 1, 2 });

		System.out.println("Documento guardado para comprobacion manual: " + outFile.getAbsolutePath()); //$NON-NLS-1$

		final byte[] outPdf = readFile(outFile);
		final List<String> unsignedFields = getEmptySignatureFields(outPdf);
		Assert.assertEquals("No se ha creado el campo de firma", 1, unsignedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo no es el esperado", SIGNATURE_FIELD, unsignedFields.get(0)); //$NON-NLS-1$
	}

	/**
	 * Crea un campo de firma con presentaci&oacute;n en dos p&aacute;ginas
	 * del documento y lo firma.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testSignFieldsOnMultiPages() throws Exception {

		System.out.println("Prueba de firma de un campo de firma en multiples paginas"); //$NON-NLS-1$

		byte[] inPdf;
		try (InputStream is = TestListSignatureFields.class.getResourceAsStream(TEST_EMPTY_FILE)) {
			inPdf = AOUtil.getDataFromInputStream(is);
		}

		// Fichero de salida con los campos de firma
		final File outFile = File.createTempFile("pdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Creamos el campo de firma
		createSignatureField(inPdf, outFile, SIGNATURE_FIELD, new int[] { 1, 2 });

		// Leemos el nuevo fichero
		final byte[] pdfWithFields = readFile(outFile);

		final List<String> unsignedFields = getEmptySignatureFields(pdfWithFields);
		Assert.assertEquals("No se ha creado el campo de firma", 1, unsignedFields.size()); //$NON-NLS-1$

		// Fichero de salida con las firmas
		final File signedFile = File.createTempFile("signedPdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Firmamos el campo de firma
		signSignatureField(pdfWithFields, signedFile, SIGNATURE_FIELD);

		System.out.println("Documento guardado para comprobacion manual: " + signedFile.getAbsolutePath()); //$NON-NLS-1$

		final byte[] outPdf = readFile(signedFile);
		final List<String> signedFields = getSignedSignatureFields(outPdf);
		Assert.assertEquals("No se ha firmado el campo de firma", 1, signedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo firmado no es el esperado", SIGNATURE_FIELD, signedFields.get(0)); //$NON-NLS-1$
	}

	/**
	 * Crea un campo de firma invisible en un documento PDF.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testCreateInvisibleSignatureField() throws Exception {

		System.out.println("Prueba de generacion de campos de firma invisibles"); //$NON-NLS-1$

		byte[] inPdf;
		try (InputStream is = TestListSignatureFields.class.getResourceAsStream(TEST_EMPTY_FILE)) {
			inPdf = AOUtil.getDataFromInputStream(is);
		}

		final File outFile = File.createTempFile("signedPdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Creamos el campo de firma
		createSignatureField(inPdf, outFile, SIGNATURE_FIELD, null);

		System.out.println("Documento guardado para comprobacion manual: " + outFile.getAbsolutePath()); //$NON-NLS-1$

		final byte[] outPdf = readFile(outFile);
		final List<String> unsignedFields = getEmptySignatureFields(outPdf);
		Assert.assertEquals("No se ha creado el campo de firma", 1, unsignedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo no es el esperado", SIGNATURE_FIELD, unsignedFields.get(0)); //$NON-NLS-1$
	}

	/**
	 * Crea un campo de firma invisible y lo firma.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testSignInvisibleSignatureField() throws Exception {

		System.out.println("Prueba de firma de un campo de firma invisible"); //$NON-NLS-1$

		byte[] inPdf;
		try (InputStream is = TestListSignatureFields.class.getResourceAsStream(TEST_EMPTY_FILE)) {
			inPdf = AOUtil.getDataFromInputStream(is);
		}

		// Fichero de salida con los campos de firma
		final File intermediateFile = File.createTempFile("pdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Creamos el campo de firma
		createSignatureField(inPdf, intermediateFile, SIGNATURE_FIELD, null);

		// Leemos el nuevo fichero
		final byte[] pdfWithFields = readFile(intermediateFile);

		final List<String> unsignedFields = getEmptySignatureFields(pdfWithFields);
		Assert.assertEquals("No se ha creado el campo de firma", 1, unsignedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo no es el esperado", SIGNATURE_FIELD, unsignedFields.get(0)); //$NON-NLS-1$

		// Fichero de salida con las firmas
		final File signedFile = File.createTempFile("signedPdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Firmamos el campo de firma
		signSignatureField(pdfWithFields, signedFile, SIGNATURE_FIELD);

		System.out.println("Documento guardado para comprobacion manual: " + signedFile.getAbsolutePath()); //$NON-NLS-1$

		final byte[] outPdf = readFile(signedFile);
		final List<String> signedFields = getSignedSignatureFields(outPdf);
		Assert.assertEquals("No se ha firmado el campo de firma", 1, signedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo firmado no es el esperado", SIGNATURE_FIELD, signedFields.get(0)); //$NON-NLS-1$
	}

	/**
	 * Crea un campo de firma  usando un nombre con s&iacute;bolos en un
	 * documento PDF.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testCreateSignatureFieldWithSymbols() throws Exception {

		System.out.println("Prueba de generacion de un campo de firma cuyo nombre contiene simbolos"); //$NON-NLS-1$

		byte[] inPdf;
		try (InputStream is = TestListSignatureFields.class.getResourceAsStream(TEST_EMPTY_FILE)) {
			inPdf = AOUtil.getDataFromInputStream(is);
		}

		final File outFile = File.createTempFile("signedPdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Creamos el campo de firma
		createSignatureField(inPdf, outFile, SIGNATURE_FIELD_WITH_SYMBOLS, null);

		System.out.println("Documento guardado para comprobacion manual: " + outFile.getAbsolutePath()); //$NON-NLS-1$

		final byte[] outPdf = readFile(outFile);
		final List<String> unsignedFields = getEmptySignatureFields(outPdf);
		Assert.assertEquals("No se ha creado el campo de firma", 1, unsignedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo no es el esperado", SIGNATURE_FIELD_WITH_SYMBOLS, unsignedFields.get(0)); //$NON-NLS-1$
	}

	/**
	 * Crea un campo de firma  usando un nombre con s&iacute;bolos en un
	 * documento PDF y lo firma.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testSignSignatureFieldWithSymbols() throws Exception {

		System.out.println("Prueba de firma de un campo de firma con nombre con simbolos"); //$NON-NLS-1$

		byte[] inPdf;
		try (InputStream is = TestListSignatureFields.class.getResourceAsStream(TEST_EMPTY_FILE)) {
			inPdf = AOUtil.getDataFromInputStream(is);
		}

		// Fichero de salida con los campos de firma
		final File intermediateFile = File.createTempFile("pdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Creamos el campo de firma
		createSignatureField(inPdf, intermediateFile, SIGNATURE_FIELD_WITH_SYMBOLS, null);

		// Leemos el nuevo fichero
		final byte[] pdfWithFields = readFile(intermediateFile);

		final List<String> unsignedFields = getEmptySignatureFields(pdfWithFields);
		Assert.assertEquals("No se ha creado el campo de firma", 1, unsignedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo no es el esperado", SIGNATURE_FIELD_WITH_SYMBOLS, unsignedFields.get(0)); //$NON-NLS-1$

		// Fichero de salida con las firmas
		final File signedFile = File.createTempFile("signedPdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		// Firmamos el campo de firma
		signSignatureField(pdfWithFields, signedFile, SIGNATURE_FIELD_WITH_SYMBOLS);

		System.out.println("Documento guardado para comprobacion manual: " + signedFile.getAbsolutePath()); //$NON-NLS-1$

		final byte[] outPdf = readFile(signedFile);
		final List<String> signedFields = getSignedSignatureFields(outPdf);
		Assert.assertEquals("No se ha firmado el campo de firma", 1, signedFields.size()); //$NON-NLS-1$
		Assert.assertEquals("El nombre del campo firmado no es el esperado", SIGNATURE_FIELD_WITH_SYMBOLS, signedFields.get(0)); //$NON-NLS-1$
	}

	/**
	 * Firma un campo de firma.
	 * @param pdfWithFields Documento PDF con los campos de firma.
	 * @param signedFile Fichero en el que almacenar el PDF con las firmas.
	 * @param signatureField Nombre del campo de firma a firmar.
	 * @throws IOException Error durante la lectura o guardado de los datos.
	 * @throws AOException Error al firmar el documento.
	 */
	private void signSignatureField(final byte[] pdfWithFields, final File signedFile, final String signatureField) throws AOException, IOException {

		// Configuramos la firma
		final Properties extraParams = new Properties();
		extraParams.setProperty("signatureField", signatureField); //$NON-NLS-1$

		// Firmamos el PDF
		final AOPDFSigner signer = new AOPDFSigner();
		final byte[] signedPdf = signer.sign(pdfWithFields, DEFAULT_SIGNATURE_ALGORITHM, this.pke.getPrivateKey(),
				this.pke.getCertificateChain(), extraParams);

		// Guardamos el PDF firmado
		try (OutputStream os = new FileOutputStream(signedFile)) {
			os.write(signedPdf);
		}
	}

	/**
	 * Crea un campo de firma vac&iacute;o en un PDF. Este campo puede presentarse en m&aacute;s
	 * de una p&aacute;gina y ser visible o invisible.
	 * @param pdf Documento PDF de entrada.
	 * @param outFile Fichero con el documento PDF de salida.
	 * @param fieldName Nombre del campo de firma.
	 * @param pages N&uacute;mero de p&aacute;ginas en los que presentar el campo o {@code null}
	 * si no debe ser visible.
	 * @throws FileNotFoundException Cuando no el fichero de salida no es v&aacute;lido.
	 * @throws IOException Cuando no se pueden leer los datos de entrada.
	 * @throws DocumentException Cuando los datos de entrada no son un PDF v&aacute;lido.
	 */
	private static void createSignatureField(final byte[] pdf, final File outFile,
			final String fieldName, final int[] pages)
			throws FileNotFoundException, IOException, DocumentException {

		final Rectangle visibleArea = pages != null ?
				new Rectangle(100, 500, 300, 600) : new Rectangle(0, 0, 0, 0);

		createSignatureField(pdf, outFile, fieldName, pages, visibleArea);
	}

	/**
	 * Crea un campo de firma vac&iacute;o en un PDF. Este campo puede presentarse en m&aacute;s
	 * de una p&aacute;gina.
	 * @param pdf Documento PDF de entrada.
	 * @param outFile Fichero con el documento PDF de salida.
	 * @param fieldName Nombre del campo de firma.
	 * @param pages N&uacute;mero de p&aacute;ginas en los que presentar el campo.
	 * @param rectangle &Aacute;rea visible del campo (con un area de 0, la firma no es visible).
	 * @throws FileNotFoundException Cuando no el fichero de salida no es v&aacute;lido.
	 * @throws IOException Cuando no se pueden leer los datos de entrada.
	 * @throws DocumentException Cuando los datos de entrada no son un PDF v&aacute;lido.
	 */
	private static void createSignatureField(final byte[] pdf, final File outFile, final String fieldName,
			final int[] pages, final Rectangle rectangle)
			throws FileNotFoundException, IOException, DocumentException {

		final PdfReader reader = new PdfReader(pdf);
		try (OutputStream os = new FileOutputStream(outFile)) {

			final PdfStamper stp = new PdfStamper(reader, os);

			final PdfFormField sig = PdfFormField.createSignature(stp.getWriter());
			sig.setWidget(rectangle, PdfName.HIGHLIGHT);
			sig.setFlags(PdfAnnotation.FLAGS_PRINT);
			sig.setFieldName(fieldName);
			sig.setPage(1);

			// Presentamos el campo en las paginas listadas
			if (pages != null) {
				for (final int page : pages) {
					stp.addAnnotation(sig, page);
				}
			}
			// Incluso los campos invisibles deben referenciarse desde alguna pagina
			else {
				stp.addAnnotation(sig, 1);
			}

			stp.close();
		}
	}

	/**
	 * Carga el contenido de un fichero.
	 * @param file Fichero que se desea cargar.
	 * @return Contenido del documento.
	 * @throws IOException Cuando falla la lectura del fichero.
	 */
	private static final byte[] readFile(final File file) throws IOException {

		byte[] data;
		try (InputStream is = new FileInputStream(file)) {
			data = AOUtil.getDataFromInputStream(is);
		}
		return data;
	}

	/**
	 * Recupera el listado de los campos de firma sin firmar del documento.
	 * @param pdf Documento PDF.
	 * @return Listado con los nombres de los campos de firma sin firmar.
	 * @throws IOException Cuando falla la carga del PDF.
	 */
	private static List<String> getEmptySignatureFields(final byte[] pdf)
			throws IOException {

		final PdfReader reader = new PdfReader(pdf);

		return reader.getAcroFields().getBlankSignatureNames();
	}

	/**
	 * Recupera el listado de los campos de firma firmados del documento.
	 * @param pdf Documento PDF.
	 * @return Listado con los nombres de los campos firmados.
	 * @throws IOException Cuando falla la carga del PDF.
	 */
	private static List<String> getSignedSignatureFields(final byte[] pdf)
			throws IOException {

		final PdfReader reader = new PdfReader(pdf);

		return reader.getAcroFields().getSignatureNames();
	}

}
