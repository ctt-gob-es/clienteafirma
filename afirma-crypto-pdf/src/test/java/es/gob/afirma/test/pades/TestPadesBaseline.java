package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.common.PdfExtraParams;

public class TestPadesBaseline {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String CERT_PATH_TO_SIGN = "00_empleado_publico-hsm.p12"; //$NON-NLS-1$
    private static final String CERT_PASS_TO_SIGN = "12345"; //$NON-NLS-1$
    private static final String CERT_ALIAS_TO_SIGN = "nombre apellido1 apellido2 - dni 12345678z"; //$NON-NLS-1$

	private static final String DATA_FILE = "aaa.pdf"; //$NON-NLS-1$

	private byte[] data;

	private Properties baselineParams;

	private Properties metadataParams;

	private Properties claimedRolesParams;

	private Properties policyParams;

	private Properties commitmentTypeIndicationsParams;

	private Properties contentHintParams;

	private PrivateKeyEntry pke = null;

	private AOPDFSigner signer = null;

	/** Antes de ejecutar cualquier prueba se ejecutar&aacute; este m&eacute;todo que cargar6aacute;
	 * todos los objetos que se vaya a necesitar en las distintas pruebas.
	 * @throws Exception En cualquier error. */
	@Before
	public void loadParams() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		this.baselineParams = new Properties();
		this.baselineParams.setProperty(PdfExtraParams.PROFILE, AOSignConstants.SIGN_PROFILE_BASELINE);

		this.metadataParams = new Properties();
		this.metadataParams.setProperty(PdfExtraParams.SIGN_REASON, "Raz\u00F3n de firma"); //$NON-NLS-1$
		this.metadataParams.setProperty(PdfExtraParams.SIGNATURE_PRODUCTION_CITY, "Plaza Espa\u00F1a, Madrid"); //$NON-NLS-1$
		this.metadataParams.setProperty(PdfExtraParams.SIGNER_CONTACT, "Raz\u00F3n de firma"); //$NON-NLS-1$

		this.claimedRolesParams = new Properties();
		this.claimedRolesParams.setProperty(PdfExtraParams.SIGNER_CLAIMED_ROLES, "Rol firmante|Rol validador"); //$NON-NLS-1$

		this.policyParams = new Properties();
		this.policyParams.setProperty(PdfExtraParams.POLICY_IDENTIFIER, "urn:oid:2.16.724.1.3.1.1.2.1.9"); //$NON-NLS-1$
		this.policyParams.setProperty(PdfExtraParams.POLICY_IDENTIFIER_HASH_ALGORITHM, "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$
		this.policyParams.setProperty(PdfExtraParams.POLICY_IDENTIFIER_HASH, "G7roucf600+f03r/o0bAOQ6WAs0="); //$NON-NLS-1$
		this.policyParams.setProperty(PdfExtraParams.POLICY_QUALIFIER, "https://sede.060.gob.es/politica_de_firma_anexo_1.pdf"); //$NON-NLS-1$

		this.commitmentTypeIndicationsParams = new Properties();
		this.commitmentTypeIndicationsParams.setProperty(PdfExtraParams.COMMITMENT_TYPE_INDICATIONS, "2"); //$NON-NLS-1$
		this.commitmentTypeIndicationsParams.setProperty(PdfExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + "0" + PdfExtraParams.COMMITMENT_TYPE_INDICATION_IDENTIFIER, "1"); //$NON-NLS-1$ //$NON-NLS-2$
		this.commitmentTypeIndicationsParams.setProperty(PdfExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + "0" + PdfExtraParams.COMMITMENT_TYPE_INDICATION_QUALIFIERS, "1.2.3.4|1.56.23.1"); //$NON-NLS-1$ //$NON-NLS-2$
		this.commitmentTypeIndicationsParams.setProperty(PdfExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + "1" + PdfExtraParams.COMMITMENT_TYPE_INDICATION_IDENTIFIER, "6"); //$NON-NLS-1$ //$NON-NLS-2$
		this.commitmentTypeIndicationsParams.setProperty(PdfExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + "1" + PdfExtraParams.COMMITMENT_TYPE_INDICATION_QUALIFIERS, "1.56.23.2"); //$NON-NLS-1$ //$NON-NLS-2$

		// Tipos erroneos que no deberian declararse
		this.contentHintParams = new Properties();
		this.contentHintParams.setProperty(CAdESExtraParams.CONTENT_TYPE_OID, "1.2.840.10008.1.2.4.50"); //$NON-NLS-1$
		this.contentHintParams.setProperty(CAdESExtraParams.CONTENT_DESCRIPTION, "JPEG Image"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH_TO_SIGN), CERT_PASS_TO_SIGN.toCharArray());
		this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS_TO_SIGN, new KeyStore.PasswordProtection(CERT_PASS_TO_SIGN.toCharArray()));

		try {
			this.data = AOUtil.getDataFromInputStream(TestPadesBaseline.class.getResourceAsStream("/" + DATA_FILE)); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "No se ha podido cargar el fichero de pruebas: " + DATA_FILE, e);  //$NON-NLS-1$
		}

		this.signer = new AOPDFSigner();
	}

	/** Firma baseline.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaseline() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				this.baselineParams);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con algoritmo y metadatos en el diccionario: raz&oacute;n de firma,
	 * localizacion e informacion de contacto.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConMetadatos() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.metadataParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con commitment-type-indications.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConCommitmentTypeIndications() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.commitmentTypeIndicationsParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con roles declarados.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConRoles() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.claimedRolesParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con pol&iacute;tica de firma.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConPolitica() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.policyParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con Content-Hint.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConContentHint() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.metadataParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con pol&iacute;tica de firma y raz&oacute;n de firma.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConPoliticaYReason() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.policyParams, this.metadataParams, this.claimedRolesParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con pol&iacute;tica de firma y commitment type indications.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConPoliticaYCommitmentTypeIndications() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.policyParams, this.commitmentTypeIndicationsParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline sin pol&iacute;tica de firma y commitment type indications. Esta
	 * ser&iacute;a una combinaci&oacute;n no v&aacute;lida para las firmas BES y Baseline EN
	 * ya que estas requieren que se declare una pol&iacute;tica de firma cuando se incluya
	 * el atributo CommitmentTypeIndications.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineSinPoliticaYConCommitmentTypeIndications() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.commitmentTypeIndicationsParams, this.claimedRolesParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con raz&oacute;n de firma y commitment type indications.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConReasonYCommitmentTypeIndications() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.metadataParams, this.commitmentTypeIndicationsParams, this.claimedRolesParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/** Firma baseline con pol&iacute;tica, raz&oacute;n de firma y commitment type indications.
	 * @throws Exception en cualquier error. */
	@Test
	public void testFirmaBaselineConPoliticaReasonYCommitmentTypeIndications() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;


		final byte[] result = this.signer.sign(
				this.data, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				add(this.baselineParams, this.policyParams, this.metadataParams, this.commitmentTypeIndicationsParams, this.claimedRolesParams));

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestPadesBaseline() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkSign(result, algorithm);
	}

	/**
	 * Unifica todas las propiedades de un numero indeterminado de objetos.
	 * @param configs Listado de objetos de propiedades
	 * @return Objetos con las propiedades de todos los objetos de entrada.
	 */
	private static Properties add(final Properties... configs) {

		final Properties result = new Properties();

		for (final Properties config : configs) {
			for (final String key : config.keySet().toArray(new String[0])) {
				result.setProperty(key, config.getProperty(key));
			}
		}
		return result;
	}

	/**
	 * Guarda datos temporales en disco.
	 * @param data Datos a guardar.
	 * @return Fichero temporal generado.
	 * @throws IOException Cuando ocurre un error en el guardado del fichero en disco.
	 */
	private static File saveTempFile(final byte[] data) throws IOException {
		final File saveFile = File.createTempFile("testSign-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try ( final OutputStream os = new FileOutputStream(saveFile) ) {
			os.write(data);
			os.flush();
		}
		return saveFile;
	}

	/**
	 * Realiza las comprobaciones necesarias sobre la firma generada.
	 * @param signature Firma generada.
	 * @param algorithm Algoritmo de firma empleado en la nueva firma.
	 * @throws IOException Cuando ocurre un error al leer la firma.
	 * @throws AOException Cuando ocurre un error al extraer los datos originalmente firmados.
	 */
	private void checkSign(final byte[] signature, final String algorithm) throws IOException, AOException {

		Assert.assertNotNull("Se ha obtenido una firma nula", signature); //$NON-NLS-1$
		Assert.assertTrue("La firma no se reconoce como firma PAdES", this.signer.isSign(signature)); //$NON-NLS-1$


		final AOTreeModel tree = this.signer.getSignersStructure(signature, true);

		final AOTreeNode rootNode = (AOTreeNode) tree.getRoot();
		Assert.assertEquals("No se interpreto correctamente la estructura de la firma", "Datos", rootNode.getUserObject()); //$NON-NLS-1$ //$NON-NLS-2$

		AOSimpleSignInfo signInfo = null;

		for (int i = 0; i < rootNode.getChildCount(); i++) {
			final AOSimpleSignInfo currrentSignInfo = (AOSimpleSignInfo) rootNode.getChildAt(i).getUserObject();
			if (this.pke.getCertificate().equals(currrentSignInfo.getCerts()[0])) {
				signInfo = currrentSignInfo;
				break;
			}
		}

		Assert.assertNotNull("No se encontro la cofirma generada en la nueva estructura de firma", signInfo); //$NON-NLS-1$
		Assert.assertEquals("El algoritmo de firma no es el esperado", algorithm, signInfo.getSignAlgorithm()); //$NON-NLS-1$
		Assert.assertNotNull("No se encontro la hora de firma", signInfo.getSigningTime()); //$NON-NLS-1$
	}
}
