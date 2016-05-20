package es.gob.afirma.keystores.mozilla;

import java.util.List;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptEngineManager;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.Platform;


/** Pruebas de AppleScript.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestAppleScriptEngine {

	/** Prueba de obtenci&oacute;n de motor AppleScript. */
	@SuppressWarnings("static-method")
	@Test
	public void testEngineInit() {
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			final ScriptEngine se = MozillaKeyStoreUtilitiesOsX.getAppleScriptEngine();
			Assert.assertNotNull(se);
		}
	}

	/** Main para listar los motores del JRE actual.
	 * @param args No se usa. */
	public static void main(final String[] args) {
        final ScriptEngineManager mgr = new ScriptEngineManager();
        final List<ScriptEngineFactory> factories = mgr.getEngineFactories();

        for (final ScriptEngineFactory factory : factories) {

            System.out.println("ScriptEngineFactory Info"); //$NON-NLS-1$

            final String engName = factory.getEngineName();
            final String engVersion = factory.getEngineVersion();
            final String langName = factory.getLanguageName();
            final String langVersion = factory.getLanguageVersion();

            System.out.printf("\tScript Engine: %s (%s)%n", engName, engVersion); //$NON-NLS-1$

            final List<String> engNames = factory.getNames();
            for(final String name : engNames) {
                System.out.printf("\tEngine Alias: %s%n", name); //$NON-NLS-1$
            }

            System.out.printf("\tLanguage: %s (%s)%n", langName, langVersion); //$NON-NLS-1$

        }
	}

}
