package es.gob.afirma.standalone;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import es.gob.afirma.core.misc.AOUtil;

/** Gestor de los recursos de las diferentes formas de ayuda de la aplicaci&oacute;n,
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class HelpResourceManager {

	private HelpResourceManager() {
		// No permitimos instanciar
	}

    /** Crea el directorio de usuario del programa si no existe, */
    private static void createApplicationDataDir() {
    	final File apDir = new File(SimpleAfirma.APPLICATION_HOME);
    	if (!apDir.exists()) {
    		apDir.mkdirs();
    	}
    }

	static void createWindowsHelpResources(final File helpFile) throws IOException {
		extractResource(
			"help/WinHelp/AutoFirmaV2.chm", //$NON-NLS-1$
			helpFile
		);
	}

	/** Crea los recursos necesarios para mostrar la ayuda de la aplicaci&oacute;n en formato Apple OS X.
	 * En concreto, copia una biblioteca JNI nativa de OS X que hace de puente entre Java y el subsistema
	 * de ayuda de OS X. Esta biblioteca, llamada <code>libJavaHelpHook.jnilib</code> se crea a partir del
	 * siguiente c&oacute;digo fuente Objective-C (<code>JavaHelpHook.m</code>):
	 * <pre>
	 * #import &lt;JavaVM/jni.h&gt;
     * #import &lt;Cocoa/Cocoa.h&gt;
     *
     * JNIEXPORT void JNICALL Java_es_gob_afirma_standalone_ui_MacHelpHooker_showHelp(JNIEnv *env, jclass clazz)
     * {
	 *   [[NSApplication sharedApplication] performSelectorOnMainThread:@selector(showHelp:) withObject:NULL waitUntilDone:NO];
     * }
	 * </pre>
	 * @throws IOException Si no se pueden crear los recursos de ayuda. */
	public static void createOsxHelpResources() throws IOException {
		final File appleHelpFile = new File(SimpleAfirma.APPLICATION_HOME + "/libJavaHelpHook.jnilib"); //$NON-NLS-1$
		if (!appleHelpFile.exists()) {
			extractResource(
				"help/AppleHelp/libJavaHelpHook.jnilib", //$NON-NLS-1$
				appleHelpFile
			);
		}

	}

	private static void extractResource(final String source, final File destination) throws IOException {

		// Creamos el directorio de la aplicacion
		createApplicationDataDir();

		// Copiamos el recurso desde el JAR hasta el destino especificado
    	final byte[] helpDocument = AOUtil.getDataFromInputStream(
			ClassLoader.getSystemResourceAsStream(source)
		);
    	if (helpDocument == null || helpDocument.length == 0) {
    		throw new IOException(
				"No se ha encontrado la biblioteca JNI de carga de la ayuda de OS X (JavaHelpHook.jnilib)" //$NON-NLS-1$
			);
    	}

        try (final FileOutputStream fos = new FileOutputStream(destination); ) {
        	fos.write(helpDocument);
        }
	}
}
