package es.gob.afirma.signers.cades.asic;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import org.junit.Test;

import es.gob.afirma.core.signers.asic.ASiCUtil;

/** Pruebas de firmas ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class ASiCSTests {

	/** prueba de creaci&oacute;n de contenedor ASiC-S.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testContainerCreation() throws Exception {
		final byte[] zipFile = ASiCUtil.createSContainer(
			"FIRMA".getBytes(), //$NON-NLS-1$
			"DATOS".getBytes(), //$NON-NLS-1$
			null,
			null
		);

		try (
			final OutputStream fos = new FileOutputStream(File.createTempFile("ASIC-", ".zip")); //$NON-NLS-1$ //$NON-NLS-2$
		) {
			fos.write(zipFile);
		}
	}

}
