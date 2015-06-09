package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.util.List;

import org.junit.Test;

import es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName;

/** Pruebas de la base de datos de m&oacute;dulos PKCS#11.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestSecMod {

	/** Prueba el an&aacute;lisis de "secmod.db".
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSecmodParsing() throws Exception {
		final String path = new File(new File(
			TestSecMod.class.getResource("/Profiles/profdir/secmod.db").toURI() //$NON-NLS-1$
		).getParent()).getAbsolutePath();
		final List<ModuleName> mods = AOSecMod.getModules(path);
		for (final ModuleName mn : mods) {
			System.out.println(mn);
		}
	}

}
