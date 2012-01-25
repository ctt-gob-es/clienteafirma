package es.gob.afirma.applet.pkcs7;

import java.io.IOException;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.applet.CMSInformation;
import es.gob.afirma.applet.GenerateAllSigns;
import es.gob.afirma.core.misc.AOUtil;

/**
 * Prueba la clase para extraer informaci&oacute;n de los envoltorios CMS.
 */
public class CMSinformationTest {

	private static final String[] FILES = {
		"cms_signed_data", //$NON-NLS-1$
//		"cms_encrypted_data", //$NON-NLS-1$
		"cms_enveloped_data", //$NON-NLS-1$
		"cms_signed_enveloped_data", //$NON-NLS-1$
//		"cms_authenticated_data", //$NON-NLS-1$
		"cms_auth_enveloped_data" //$NON-NLS-1$
	};

	/**
	 * Extrae informacion de una firma CMS.
	 */
	@Test
	public void cmsSignedDataTest() {

		for (final String filename : FILES) {

			final byte[] cmsStructure = getResource(filename);
			Assert.assertNotNull("No se ha encontrado el fichero de recursos " + filename, cmsStructure); //$NON-NLS-1$

			final String info;
			try {
				info = CMSInformation.getInformation(cmsStructure);
				Assert.assertNotNull(info);
			} catch (final Exception e) {
				Assert.fail("No se ha podido recuperar la informacion del envoltorio: " + e); //$NON-NLS-1$
				return;
			}

			System.out.println(info);
			System.out.println("---"); //$NON-NLS-1$
		}
	}

    private static byte[] getResource(final String filename) {
    	try {
			return AOUtil.getDataFromInputStream(GenerateAllSigns.class.getResourceAsStream("/" + filename)); //$NON-NLS-1$
		} catch (final IOException e) {
			return null;
		}
    }
}
