package es.gob.afirma.standalone.ui.restoreconfig;

import es.gob.afirma.core.misc.LoggerUtil;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * Utilidades para la restauraci&oacute;n de configuraci&oacute;n en macOS.
 */
public class RestoreConfigMacOSXUtils {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private RestoreConfigMacOSXUtils() {}

    /** Escribe un <i>script</i> en un fichero dado.
     * @param scriptFile Fichero donde se escribir&aacute; el <i>script</i>.
     * @param command Datos a escribir.
     * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
     * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
    static void writeScriptFile(final File scriptFile, final String command, final boolean append) throws IOException{
        LOGGER.info("Se escribira en el fichero (" + LoggerUtil.getCleanUserHomePath(scriptFile.getAbsolutePath()) + ") el siguiente comando:\n" + command); //$NON-NLS-1$ //$NON-NLS-2$
        try (final FileOutputStream fout = new FileOutputStream(scriptFile, append)) {
            fout.write((command + "\n").getBytes()); //$NON-NLS-1$
        }
    }


    /** Escribe un <i>script</i> en un fichero dado.
     * @param scriptFile Fichero donde se escribir&aacute; el <i>script</i>.
     * @param command Datos a escribir.
     * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
     * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
    static void writeScriptFile(final File scriptFile, final StringBuilder command, final boolean append) throws IOException{
        writeScriptFile(scriptFile, command.toString(), append);
    }
}
