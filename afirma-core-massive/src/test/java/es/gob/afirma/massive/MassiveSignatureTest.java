package es.gob.afirma.massive;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignerFactory;

/**
 * Clase para probar todas funciones de firma y multifirma disponibles para
 * la firma masiva program&aacute;tica.
 */
public class MassiveSignatureTest {

	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$

	/**
	 * Formatos a usarse en las pruebas.
	 * Campo 1: Identificador del formato
	 * Campo 2: Nombre para la generaci&oacute;n del nombre de fichero
	 * Campo 3: Extensi&oacute;n de firma
	 * Campo 4: Soporta contrafirma
	 */
	private static final String[][]	FORMATS = {
			{ AOSignConstants.SIGN_FORMAT_CMS, "CMS", "csig", "true", "true" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_CADES, "CADES", "csig", "true", "true" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_XADES_DETACHED, "XADES_DETACHED", "xsig", "true", "true" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, "XADES_ENVELOPING", "xsig", "true", "true" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, "XADES_ENVELOPED", "xsig", "true", "false" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, "XMLDSIG_DETACHED", "xsig", "true", "true" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "XMLDSIG_ENVELOPING", "xsig", "true", "true" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED, "XMLDSIG_ENVELOPED", "xsig", "true", "false" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_ODF, "ODF", "odt", "false", "false" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_OOXML, "OOXML", "docx", "false", "false" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			{ AOSignConstants.SIGN_FORMAT_PDF, "PDF", "pdf", "false", "false" } //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	};

	private static final String[][]	FORMATS_FILES	= {
			{ AOSignConstants.SIGN_FORMAT_CMS, "bin" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_CADES, "bin" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XADES_DETACHED, "bin" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XADES_DETACHED, "xml" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, "bin" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, "xml" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, "xml" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, "bin" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, "xml" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "bin" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "xml" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED, "xml" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_PDF, "pdf" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_ODF, "odt" }, //$NON-NLS-1$
			{ AOSignConstants.SIGN_FORMAT_OOXML, "docx" } //$NON-NLS-1$
													};

	// private static final boolean[] ORIGINAL_FORMAT = new boolean[] {true,
	// false};

	private static final String[][]	FORMATS_MODES	= {
			{ AOSignConstants.SIGN_FORMAT_CMS, AOSignConstants.SIGN_MODE_EXPLICIT },
			{ AOSignConstants.SIGN_FORMAT_CMS, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_CADES, AOSignConstants.SIGN_MODE_EXPLICIT },
			{ AOSignConstants.SIGN_FORMAT_CADES, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XADES_DETACHED, AOSignConstants.SIGN_MODE_EXPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XADES_DETACHED, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, AOSignConstants.SIGN_MODE_EXPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, AOSignConstants.SIGN_MODE_EXPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, AOSignConstants.SIGN_MODE_EXPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_PDF, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_ODF, AOSignConstants.SIGN_MODE_IMPLICIT },
			{ AOSignConstants.SIGN_FORMAT_OOXML, AOSignConstants.SIGN_MODE_IMPLICIT }
	};

	private static final boolean MANUAL_DEBUG = false;
	private final static String path = new File("").getAbsolutePath();	//$NON-NLS-1$

	//    private static final String CERT_PATH2 = "ANF_PJ_Activo.pfx"; //$NON-NLS-1$
	//    private static final String CERT_PASS2 = "12341234"; //$NON-NLS-1$
	//    private static final String CERT_ALIAS2 = "anf usuario activo"; //$NON-NLS-1$
	//
	//    private static final String CERT_PATH3 = "CAMERFIRMA_PF_SW_Clave_usuario_Activo.p12"; //$NON-NLS-1$
	//    private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
	//    private static final String CERT_ALIAS3 = "1"; //$NON-NLS-1$

	private static byte[] getDigestData(final byte[] data) throws Exception {
		return MessageDigest.getInstance("SHA-1").digest(data); //$NON-NLS-1$
	}

	private static String getResourcePath(final String filename) {
		return (System.getProperty("os.name").contains("indows") ? "" : File.separator) + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				MassiveSignatureTest.class
						.getResource("/" + filename).toString().replace("%20", " ").replace("file:/", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
	}

	private static void saveData(final byte[] data, final String filename) {
		if (MassiveSignatureTest.MANUAL_DEBUG) {
			try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(
					MassiveSignatureTest.path + File.separator + filename
				);
			) {
				Logger.getLogger("es.gob.afirma").info("Guardamos el fichero: " + //$NON-NLS-1$ //$NON-NLS-2$
				MassiveSignatureTest.path + File.separator + filename);
				fos.write(data);
				fos.flush();
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"Error al guardar el fichero '" + MassiveSignatureTest.path + File.separator + filename + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}

	/**
	 * Genera todo tipo de firmas y multifirmas masivas.
	 * @throws Exception Cuando se produce cualquier error durante la ejecuci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void pruebaTodasLasCombinacionesDeFirmaProgramatica() throws Exception {

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(MassiveSignatureTest.CERT_PATH)
		) {
			ks.load(
				is,
				MassiveSignatureTest.CERT_PASS.toCharArray()
			);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
				MassiveSignatureTest.CERT_ALIAS,
				new KeyStore.PasswordProtection(MassiveSignatureTest.CERT_PASS
						.toCharArray()));

		final MassiveSignConfiguration config = new MassiveSignConfiguration(pke);
		for (final String[] format : MassiveSignatureTest.FORMATS) {
			config.setDefaultFormat(format[0]);
			// for (final boolean originalFormat : ORIGINAL_FORMAT) {
			// config.setOriginalFormat(originalFormat);
			for (final String[] mode : MassiveSignatureTest.FORMATS_MODES) {
				if (format[0].equals(mode[0])) {
					config.setMode(mode[1]);
					for (final String[] file : MassiveSignatureTest.FORMATS_FILES) {
						if (format[0].equals(file[0])) {

							final String fullpath = MassiveSignatureTest.getResourcePath(file[1]);
							final byte[] data;
							try (
								final InputStream fis = new FileInputStream(fullpath);
							) {
								data = AOUtil.getDataFromInputStream(fis);
							}

							final MassiveSignatureHelper massive = new MassiveSignatureHelper(config);
							byte[] signature = massive.signFile(fullpath);
							String signatureName = "Firma_file_" + file[1] + "_" + format[1] + "_" + /*originalFormat + "_" +*/mode[1] + "." + format[2]; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
							Assert.assertNotNull("Se ha obtenido una firma nula al firmar: " + signatureName, signature); //$NON-NLS-1$
							MassiveSignatureTest.saveData(signature, signatureName);
							signature = massive.signData(data);
							signatureName = "Firma_data_" + file[1] + "_" + format[1] + "_" + /*originalFormat + "_" +*/mode[1] + "." + format[2]; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
							Assert.assertNotNull("Se ha obtenido una firma nula al firmar: " + signatureName, signature); //$NON-NLS-1$
							MassiveSignatureTest.saveData(signature, signatureName);

							if (Boolean.parseBoolean(format[4]) && AOSignConstants.SIGN_MODE_IMPLICIT .equals(mode[1])) {
								signature = massive.signHash(MassiveSignatureTest.getDigestData(data));
								signatureName = "Firma_hash_" + file[1] + "_" + format[1] + "_" + /*originalFormat + "_" +*/mode[1] + "." + format[2]; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
								Assert.assertNotNull("Se ha obtenido una firma nula al firmar: " + signatureName, signature); //$NON-NLS-1$
								MassiveSignatureTest.saveData(signature, signatureName);
							}
						}
					}
				}
			}
			// }
		}
	}

	/**
	 * Configura un formato de firma al inicio y lo cambia a lo largo de la ejecuci&oacute;n para firmas en otros formatos.
	 * @throws Exception Cuando se produce cualquier error durante la ejecuci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void pruebaCambioDeFormatoEnCaliente() throws Exception {

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(MassiveSignatureTest.CERT_PATH)
		) {
			ks.load(
				is,
				MassiveSignatureTest.CERT_PASS.toCharArray()
			);
		}
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
				MassiveSignatureTest.CERT_ALIAS,
				new KeyStore.PasswordProtection(MassiveSignatureTest.CERT_PASS.toCharArray()));

		final String fullpath = MassiveSignatureTest.getResourcePath("pdf"); //$NON-NLS-1$

		final MassiveSignConfiguration config = new MassiveSignConfiguration(pke);
		config.setAlgorithm(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA);
		config.setDefaultFormat(AOSignConstants.SIGN_FORMAT_PDF);
		config.setMode(AOSignConstants.SIGN_MODE_IMPLICIT);
		config.setOriginalFormat(true);

		byte[] result;

		final MassiveSignatureHelper massive = new MassiveSignatureHelper(config);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("La firma con el formato por defecto da problemas, siendo el formato: " + config.getDefaultFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getDefaultFormat(), AOSignerFactory.getSigner(config.getDefaultFormat()).isSign(result)); //$NON-NLS-1$


		massive.setSignatureFormat(AOSignConstants.SIGN_FORMAT_CADES);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("Error en la firma con el formato modificado en caliente: " + config.getSignatureFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getSignatureFormat(), AOSignerFactory.getSigner(config.getSignatureFormat()).isSign(result)); //$NON-NLS-1$

		massive.setSignatureFormat(AOSignConstants.SIGN_FORMAT_CMS);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("Error en la firma con el formato modificado en caliente: " + config.getSignatureFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getSignatureFormat(), AOSignerFactory.getSigner(config.getSignatureFormat()).isSign(result)); //$NON-NLS-1$

		massive.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("Error en la firma con el formato modificado en caliente: " + config.getSignatureFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getSignatureFormat(), AOSignerFactory.getSigner(config.getSignatureFormat()).isSign(result)); //$NON-NLS-1$

		massive.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("Error en la firma con el formato modificado en caliente: " + config.getSignatureFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getSignatureFormat(), AOSignerFactory.getSigner(config.getSignatureFormat()).isSign(result)); //$NON-NLS-1$

		massive.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("Error en la firma con el formato modificado en caliente: " + config.getSignatureFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getSignatureFormat(), AOSignerFactory.getSigner(config.getSignatureFormat()).isSign(result)); //$NON-NLS-1$

		massive.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("Error en la firma con el formato modificado en caliente: " + config.getSignatureFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getSignatureFormat(), AOSignerFactory.getSigner(config.getSignatureFormat()).isSign(result)); //$NON-NLS-1$

		massive.setSignatureFormat(AOSignConstants.SIGN_FORMAT_PDF);
		result = massive.signFile(fullpath);
		Assert.assertNotNull("Error en la firma con el formato modificado en caliente: " + config.getSignatureFormat(), result); //$NON-NLS-1$
		Assert.assertTrue("No se genero una firma en formato " + config.getSignatureFormat(), AOSignerFactory.getSigner(config.getSignatureFormat()).isSign(result)); //$NON-NLS-1$
	}
}
