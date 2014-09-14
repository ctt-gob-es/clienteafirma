package es.gob.afirma.applet;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

import junit.framework.Assert;

import org.junit.Test;

/**
 * Valida que los ficheros de idioma contengan en todas los idiomas las mismas
 * entradas que en espa&ntilde;ol.
 */
public class BundleResourceTest {

	private final static String MAIN_LANGUAGE_FILE = "appletmessages.properties"; //$NON-NLS-1$

	private final static String[] OTHER_LANGUAGES_FILES = {
		"appletmessages_de.properties", //$NON-NLS-1$
		"appletmessages_en.properties", //$NON-NLS-1$
		"appletmessages_fr.properties", //$NON-NLS-1$
		"appletmessages_it.properties", //$NON-NLS-1$
		"appletmessages_lt.properties", //$NON-NLS-1$
	};

	/**
	 * Comprueba que todos los recursos secundarios declarados tengan definidas
	 * las mismas claves que el fichero principal.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void missingResourcesTest() {

		final Properties mainResource = new Properties();
		try {
			mainResource.load(getResourceAsStream(MAIN_LANGUAGE_FILE));
		} catch (final IOException e) {
			Assert.fail("No se pudo cargar el fichero principal de recursos"); //$NON-NLS-1$
		}

		for (final String file : OTHER_LANGUAGES_FILES) {
			final Properties secondaryResource = new Properties();
			try {
				secondaryResource.load(getResourceAsStream(file));
			} catch (final IOException e) {
				Assert.fail("No se pudo cargar el recurso " + file + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}

			String missingKeys = "\n"; //$NON-NLS-1$
			for (final String key : mainResource.keySet().toArray(new String[0])) {
				if (!secondaryResource.containsKey(key)) {
					missingKeys += key + "\n"; //$NON-NLS-1$
				}
			}
			Assert.assertTrue("En el fichero " + file + " faltan las entradas:" + missingKeys, //$NON-NLS-1$ //$NON-NLS-2$
					missingKeys.trim().length() == 0);
		}
	}

	/**
	 * Comprueba que los recursos secundarios no tengan definidas entradas adicionales
	 * con respecto al recurso principal.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void extraResourcesTest() {
		final Properties mainResource = new Properties();
		try {
			mainResource.load(getResourceAsStream(MAIN_LANGUAGE_FILE));
		} catch (final IOException e) {
			Assert.fail("No se pudo cargar el fichero principal de recursos"); //$NON-NLS-1$
		}

		for (final String file : OTHER_LANGUAGES_FILES) {
			final Properties secondaryResource = new Properties();
			try {
				secondaryResource.load(getResourceAsStream(file));
			} catch (final IOException e) {
				Assert.fail("No se pudo cargar el recurso " + file + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}

			String missingKeys = "\n"; //$NON-NLS-1$
			for (final String key : secondaryResource.keySet().toArray(new String[0])) {
				if (!mainResource.containsKey(key)) {
					missingKeys += key + "\n"; //$NON-NLS-1$
				}
			}
			Assert.assertTrue("El fichero " + file + //$NON-NLS-1$
					" contiene las siguientes entradas no existentes en el idioma principal:" + missingKeys, //$NON-NLS-1$
					missingKeys.trim().length() == 0);
		}
	}


	/** Comprueba que no existan cadenas duplicadas dentro del fichero principal. */
	@SuppressWarnings("static-method")
	@Test
	public void duplicateResourcesTest() {
		final Properties mainResource = new Properties();
		try {
			mainResource.load(getResourceAsStream(MAIN_LANGUAGE_FILE));
		} catch (final IOException e) {
			Assert.fail("No se pudo cargar el fichero principal de recursos"); //$NON-NLS-1$
		}

		String duplicateStrings = "\n"; //$NON-NLS-1$
		final Map<String, String> resourceKeyRel = new ConcurrentHashMap<String, String>();
		final String[] keys = mainResource.keySet().toArray(new String[0]);
		for (final String key : keys) {
			final String text = mainResource.getProperty(key);
			if (!resourceKeyRel.containsKey(text)) {
				resourceKeyRel.put(text, key);
			}
			else {
				duplicateStrings += resourceKeyRel.get(text) + " --> " + key + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		Assert.assertTrue("En el fichero principal (" + MAIN_LANGUAGE_FILE + //$NON-NLS-1$
				") se han encontrado recursos cuplicados:" + duplicateStrings, //$NON-NLS-1$
				duplicateStrings.trim().length() == 0);
	}

    private static InputStream getResourceAsStream(final String filename) {
    	return GenerateAllSigns.class.getResourceAsStream("/" + filename); //$NON-NLS-1$
    }
}
