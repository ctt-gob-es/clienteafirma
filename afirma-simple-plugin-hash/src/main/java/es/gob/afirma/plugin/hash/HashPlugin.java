package es.gob.afirma.plugin.hash;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.PluginControlledException;

/**
 * Clase principal del plugin de huella digital.
 */
public class HashPlugin extends AfirmaPlugin {

	private static final String REPLACE_PATH_EXE = "$$PATH_EXE$$"; //$NON-NLS-1$

	private static final String REPLACE_INSTALL_DIR = "$$INSTALL_DIR$$"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger(HashPlugin.class.getName());

	@Override
	public void install() throws PluginControlledException {
		// Si el SO que se esta usando es Windows, procedemos a instalar los registros correspondientes
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			execRegistryScript("installHashPluginRegs.exe", "installHashPluginRegs.bat");  //$NON-NLS-1$//$NON-NLS-2$
		}
	}

	@Override
	public void uninstall() throws PluginControlledException {
		// Si el SO que se esta usando es Windows, procedemos a desinstalar los registros correspondientes
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			execRegistryScript("uninstallHashPluginRegs.exe", "uninstallHashPluginRegs.bat");  //$NON-NLS-1$//$NON-NLS-2$
		}
	}

	/**
	 * M&eacute;todo encargado de ejecutar el archivo bat, que a su vez ejecutar&aacute; su archivo exe correspondiente
	 * encargado de agregar o eliminar los registros correspondientes del sistema.
	 * @param exeFileName Nombre del archivo .exe
	 * @param batFileName Nombre del archivo .bat
	 * @throws PluginControlledException Error en una de las operaciones a realizar
	 */
	private static void execRegistryScript(final String exeFileName, final String batFileName) throws PluginControlledException {
		final File appDir = HashUtil.getApplicationDirectory();
		File workingDir;

		if (Files.isWritable(appDir.toPath())) {
			workingDir = appDir;
		} else {
			workingDir = HashUtil.getWindowsAlternativeAppDir();
		}

		// Copiamos al directorio el recurso a instalar
		final File exeFile = new File(workingDir, exeFileName);
		try (final FileOutputStream os = new FileOutputStream(exeFile);
				final InputStream is = HashPlugin.class.getResourceAsStream("/es/gob/afirma/plugin/hash/registryInstallation/" + exeFileName);) { //$NON-NLS-1$
			os.write(AOUtil.getDataFromInputStream(is));
			os.flush();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo copiar a disco uno de los recursos del plugin. Se abortara su ejecucion", e); //$NON-NLS-1$
			throw new PluginControlledException("No se pudo copiar a disco uno de los recursos del plugin. Se abortara su ejecucion", e); //$NON-NLS-1$
		}

		// Copiamos a disco y completamos el script para ejecutar el recurso con permisos de administrador
		final File batFile = new File(workingDir, batFileName);
		try (final FileOutputStream os = new FileOutputStream(batFile);
				final InputStream is = HashPlugin.class.getResourceAsStream("/es/gob/afirma/plugin/hash/registryInstallation/" + batFileName);) { //$NON-NLS-1$
			String batchScript = new String(AOUtil.getDataFromInputStream(is));
			batchScript = batchScript.replace(REPLACE_PATH_EXE, exeFile.getAbsolutePath().replace("\\", "\\\\")) //$NON-NLS-1$ //$NON-NLS-2$
										.replace(REPLACE_INSTALL_DIR, appDir.getAbsolutePath().replace("\\", "\\\\")); //$NON-NLS-1$ //$NON-NLS-2$
			os.write(batchScript.getBytes());
			os.flush();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo copiar a disco la aplicacion para instalar el plugin. Se abortara su ejecucion", e); //$NON-NLS-1$
			throw new PluginControlledException("No se pudo copiar a disco la aplicacion para instalar el plugin. Se abortara su ejecucion", e); //$NON-NLS-1$
		}

		// Ejecumos el script
		try {
			final Process process = Runtime.getRuntime().exec(new String[] { batFile.getAbsolutePath() });
			process.waitFor();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING,"Error durante la ejecucion del proceso de instalacion de los recursos para el plugin", e); //$NON-NLS-1$
			throw new PluginControlledException("Error durante la ejecucion del proceso de instalacion de los recursos para el plugin", e); //$NON-NLS-1$
		}

		// Esperamos 1 segundo para poder eliminar los ficheros
		try {
			Thread.sleep(1000);
		} catch (final InterruptedException e) {
			// No hacemos nada
		}

		// Eliminamos los ficheros
		try {
			Files.delete(exeFile.toPath());
			Files.delete(batFile.toPath());
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "No se pudo eliminar el ejecutable con los recursos del plugin", e); //$NON-NLS-1$
			throw new PluginControlledException("No se pudo eliminar el ejecutable con los recursos del plugin", e); //$NON-NLS-1$
		}
	}

}
