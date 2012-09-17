package es.gob.afirma.test.cades;

import java.io.InputStream;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.CAdESValidator;

/**
 * Prueba las funciones de validaci&oacute;n de firmas CAdES.
 * @author Carlos Gamuci
 */
public class TestCadesValidator {

	private static final String CADES_EXPLICIT_FILE = "cades_explicit.csig"; //$NON-NLS-1$

	/**
	 * Prueba la correcci&oacute;n del m&eacute;todo de validaci&oacute;n de firmas CAdES con
	 * una firma CAdES ecpl&iacute;cita.
	 * @throws Exception Cuando se produce cualquier error.
	 */
	@Test
	public void testComprobarFirmaCades() throws Exception {

		final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_EXPLICIT_FILE);

		final byte[] cades = AOUtil.getDataFromInputStream(is);

		try {
			Assert.assertTrue("La firma CAdES no es valida", //$NON-NLS-1$
					CAdESValidator.isCAdESValid(cades, AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA));
		} finally {
			try {
				is.close();
			} catch (final Exception e) {
				// Ignoramos el error
			}
		}
	}
}
