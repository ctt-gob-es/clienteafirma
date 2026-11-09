package es.gob.afirma.standalone;

import org.junit.Assert;
import org.junit.Test;

/**
 * Valida la lista blanca de binarios para la detecci&oacute;n de tema (#42).
 * No hay di&aacute;logo nuevo: el fallo de tema sigue en silencio (visual-review: N/A UI).
 */
public class LookAndFeelManagerThemeCommandTest {

	@Test
	public void allowsLiteralGsettingsAndPs() {
		Assert.assertTrue(LookAndFeelManager.isAllowedThemeBinary("gsettings"));
		Assert.assertTrue(LookAndFeelManager.isAllowedThemeBinary("ps"));
	}

	@Test
	public void rejectsForeignOrPathBinaries() {
		Assert.assertFalse(LookAndFeelManager.isAllowedThemeBinary("bash"));
		Assert.assertFalse(LookAndFeelManager.isAllowedThemeBinary("/usr/bin/gsettings"));
		Assert.assertFalse(LookAndFeelManager.isAllowedThemeBinary("C:\\Windows\\System32\\ps.exe"));
		Assert.assertFalse(LookAndFeelManager.isAllowedThemeBinary(null));
		Assert.assertFalse(LookAndFeelManager.isAllowedThemeBinary(""));
	}
}
