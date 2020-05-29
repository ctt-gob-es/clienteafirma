package es.gob.afirma.signers.xades;

import java.io.InputStream;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;

/**
 * Conjunto de pruebas de extracci&oacute;n de los datos firmados de los distintos tipos de firma.
 */
public class TestExtractData {

	private static final String FILE_SIGNATURE_EXTERNALLY_DETACHED = "externally_detached.xsig"; //$NON-NLS-1$

	private static final String FILE_SIGNATURE_MANIFEST = "xades_con_manifest.xsig"; //$NON-NLS-1$

	private AOXAdESSigner signer;

	@Before
	public void beforeTest() {
		this.signer = new AOXAdESSigner();
	}

	/**
	 * Comprueba que no se extraigan los datos firmados de las firmas Externally Detached.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testExtractDataFromExternallyDetached() {

		final String signatureFile = FILE_SIGNATURE_EXTERNALLY_DETACHED;

		final byte[] signature = loadResource(signatureFile);

		Assert.assertNotNull("No se ha podido cargar el fichero " + signatureFile, signature); //$NON-NLS-1$
		Assert.assertFalse("El nombre de recurso no es valido", signature.length == 0); //$NON-NLS-1$

		byte[] data;
		try {
			data = this.signer.getData(signature);
		} catch (final AOInvalidFormatException e) {
			Assert.fail("El fichero de pruebas no es una firma: " + e); //$NON-NLS-1$
			return;
		}

		Assert.assertNull("La firma no debera permitir extraer datos", data); //$NON-NLS-1$
	}

	/**
	 * Comprueba que no se extraigan los datos firmados de las firmas Externally Detached.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testExtractDataFromXadesWithManifest() {

		final String signatureFile = FILE_SIGNATURE_MANIFEST;

		final byte[] signature = loadResource(signatureFile);

		Assert.assertNotNull("No se ha podido cargar el fichero " + signatureFile, signature); //$NON-NLS-1$
		Assert.assertFalse("El nombre de recurso no es valido", signature.length == 0); //$NON-NLS-1$

		byte[] data;
		try {
			data = this.signer.getData(signature);
		} catch (final AOInvalidFormatException e) {
			Assert.fail("El fichero de pruebas no es una firma: " + e); //$NON-NLS-1$
			return;
		}

		Assert.assertNull("La firma no debera permitir extraer datos", data); //$NON-NLS-1$
	}

	private static byte[] loadResource(final String resource) {
		final byte[] data;
		try (InputStream is = TestExtractData.class.getResourceAsStream("/" + resource)) { //$NON-NLS-1$
			data = AOUtil.getDataFromInputStream(is);
		}
		catch (final Exception e) {
			Assert.fail("Error en la carga del fichero " + resource + ":" + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
		return data;
	}
}
