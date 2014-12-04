package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;

final class MozillaKeyStoreUtilitiesOsX {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private MozillaKeyStoreUtilitiesOsX() {
		// No dejamos instanciar
	}

	static void configureMacNSS(final String binDir) throws AOException {

		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			return;
		}

		if (binDir == null) {
			LOGGER.severe("El directorio de NSS para configurar proporcionado es nulo, no se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		final String nssBinDir = binDir.endsWith("/") ? binDir : binDir + "/"; //$NON-NLS-1$ //$NON-NLS-2$

		// Intentamos la carga, para ver si es necesaria la reconfiguracion
		try {
			System.load(nssBinDir + "libsoftokn3.dylib"); //$NON-NLS-1$
			return; // Si funciona salimos sin hacer nada
		}
		catch (final Exception e) {
			// Se ignora el error
		}
		catch(final UnsatisfiedLinkError e) {
			// Se ignora el error
		}

		final String[] libs = new String[] {
				"libmozglue.dylib", // Firefox 11 y superiores //$NON-NLS-1$
				"libmozutils.dylib", // Firefox 9 y 10 //$NON-NLS-1$
				"libnspr4.dylib", //$NON-NLS-1$
				"libplds4.dylib", //$NON-NLS-1$
				"libplc4.dylib", //$NON-NLS-1$
				"libnssutil3.dylib", //$NON-NLS-1$
				"libmozsqlite3.dylib", //$NON-NLS-1$
				"libnss3.dylib" // Detectado en Firefox 30, quizas se introdujo en versiones anteriores //$NON-NLS-1$
		};

		// Creamos enlaces simbolicos via AppleScript
		final StringBuilder sb = new StringBuilder();
		for (final String lib : libs) {
			if (new File(nssBinDir + lib).exists()) {
				sb.append("ln -s "); //$NON-NLS-1$
				sb.append(nssBinDir);
				sb.append(lib);
				sb.append(" /usr/lib/"); //$NON-NLS-1$
				sb.append(lib);
				sb.append("; "); //$NON-NLS-1$
			}
		}
		try {
			// Probamos las dos formas de instanciar el motor AppleScript

			// Nuevo nombre desde OS X Yosemite: AppleScriptEngine
			ScriptEngine se = new ScriptEngineManager().getEngineByName("AppleScriptEngine"); //$NON-NLS-1$
			if (se == null) {
				// Nombre en versiones antiguas de OS X
				se = new ScriptEngineManager().getEngineByName("AppleScript"); //$NON-NLS-1$
			}
			if (se != null) {
				new ScriptEngineManager().getEngineByName("AppleScript").eval("do shell script \"" + sb.toString() + "\" with administrator privileges"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
			else {
				LOGGER.severe(
					"No se ha podido instanciar el motor AppleCript para crear los enlaces simbolicos para NSS" //$NON-NLS-1$
				);
			}
		}
		catch(final Exception e) {
			LOGGER.severe("No se ha podido crear los enlaces simbolicos para NSS: " + e); //$NON-NLS-1$
		}

		// Y reintentamos la carga, para ver si ha surtido efecto
		try {
			System.load(nssBinDir + "libsoftokn3.dylib"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException("La configuracion de NSS para Mac OS X ha fallado por motivos de seguridad: " + e); //$NON-NLS-1$
		}
		catch(final UnsatisfiedLinkError e) {
			throw new AOException("La configuracion de NSS para Mac OS X ha fallado: " + e); //$NON-NLS-1$
		}
	}

	static String getSystemNSSLibDirMacOsX() throws FileNotFoundException {

		final String[] paths =
			new String[] {
				"/Applications/Firefox.app/Contents/MacOS", //$NON-NLS-1$
				"/lib", //$NON-NLS-1$
				"/usr/lib", //$NON-NLS-1$
				"/usr/lib/nss", //$NON-NLS-1$
				"/Applications/Minefield.app/Contents/MacOS" //$NON-NLS-1$
		};

		String nssLibDir = null;

		for (final String path : paths) {
			if (new File(path + "/libsoftokn3.dylib").exists()) { //$NON-NLS-1$
				nssLibDir = path;
			}
		}

		if (nssLibDir == null) {
			throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en Mac OS X"); //$NON-NLS-1$
		}

		return nssLibDir;
	}

}
