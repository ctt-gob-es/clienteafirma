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

	private static final String REPLACE_PATH_BAT = "$$PATH_BAT$$"; //$NON-NLS-1$

	private static final String REPLACE_AUTOFIRMA_EXE = "$$AUTOFIRMA_EXE$$"; //$NON-NLS-1$

	private static final String EXECUTOR_BAT_FILENAME = "execute.bat"; //$NON-NLS-1$

	private static final String APP_EXE_FILENAME = "Autofirma.exe"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger(HashPlugin.class.getName());


	@Override
	public void install() throws PluginControlledException {
		// Si el SO que se esta usando es Windows, procedemos a instalar los registros correspondientes
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			execRegistryScript("register.bat");  //$NON-NLS-1$
		}
	}

	@Override
	public void uninstall() throws PluginControlledException {
		// Si el SO que se esta usando es Windows, procedemos a desinstalar los registros correspondientes
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			execRegistryScript("unregister.bat");  //$NON-NLS-1$
		}
	}

	/**
	 * M&eacute;todo encargado de ejecutar el archivo bat, que a su vez ejecutar&aacute; su archivo exe correspondiente
	 * encargado de agregar o eliminar los registros correspondientes del sistema.
	 * @param exeFileName Nombre del archivo .exe
	 * @param batFileName Nombre del archivo .bat
	 * @throws PluginControlledException Error en una de las operaciones a realizar
	 */
	private static void execRegistryScript(final String batFileName) throws PluginControlledException {
		final File appDir = HashUtil.getApplicationDirectory();
		File workingDir;

		if (Files.isWritable(appDir.toPath())) {
			workingDir = appDir;
		} else {
			workingDir = HashUtil.getWindowsAlternativeAppDir();
		}

		// Definimos un subdirectorio en el que copiar los recursos
		final File pluginTempDir = new File(workingDir, "hashplugin"); //$NON-NLS-1$
		pluginTempDir.mkdirs();

		// Copiamos a disco el bat crear o eliminar las entradas de registro
		final File batFile = new File(pluginTempDir, batFileName);
		try (final FileOutputStream os = new FileOutputStream(batFile);
				final InputStream is = HashPlugin.class.getResourceAsStream("/es/gob/afirma/plugin/hash/registryInstallation/" + batFileName);) { //$NON-NLS-1$
			final byte[] content = AOUtil.getDataFromInputStream(is);
			os.write(content);
			os.flush();
		} catch (final Exception e) {
			throw new PluginControlledException("No se pudo copiar a disco el script de modificacion del registro. Se abortara su ejecucion", e); //$NON-NLS-1$
		}

		// Copiamos a disco el bat de ejecucion en modo administrador
		final File executorBatFile = new File(pluginTempDir, EXECUTOR_BAT_FILENAME);
		try (final FileOutputStream os = new FileOutputStream(executorBatFile);
				final InputStream is = HashPlugin.class.getResourceAsStream("/es/gob/afirma/plugin/hash/registryInstallation/" + EXECUTOR_BAT_FILENAME);) { //$NON-NLS-1$
			final byte[] content = AOUtil.getDataFromInputStream(is);
			String batchScript = new String(content);
			final File autofirmaFile = new File(appDir, APP_EXE_FILENAME);
			batchScript = batchScript.replace(REPLACE_PATH_BAT, batFile.getAbsolutePath().replace("\\", "\\\\")) //$NON-NLS-1$ //$NON-NLS-2$
										.replace(REPLACE_AUTOFIRMA_EXE, autofirmaFile.getAbsolutePath().replace("\\", "\\\\")); //$NON-NLS-1$ //$NON-NLS-2$
			os.write(batchScript.getBytes());
			os.flush();
		} catch (final Exception e) {
			throw new PluginControlledException("No se pudo copiar a disco el script para la ejecucion de los cambios en el registro con permisos de administrador. Se abortara su ejecucion", e); //$NON-NLS-1$
		}

		// Ejecumos el script
		try {
			final Process process = new ProcessBuilder(executorBatFile.getAbsolutePath()).start();
			process.waitFor();
			process.destroyForcibly();
		} catch (final Exception e) {
			throw new PluginControlledException("Error durante la ejecucion del proceso de instalacion de los recursos para el plugin", e); //$NON-NLS-1$
		}

		// El waitFor del Process no funciona en este caso, asi que forzamos una espera para dar tiempo a terminar la ejecucion del script
		try {
			Thread.sleep(5000);
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Se interrumpio la espera a la finalizacion del script", e); //$NON-NLS-1$
		}

		// Eliminamos los ficheros
		try {
			deleteFile(pluginTempDir);
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "No se pudo eliminar el directorio con los recursos del plugin", e); //$NON-NLS-1$
		}
	}

	/**
	 * Elimina un fichero o un directorio con todo su contenido.
	 * @param file Fichero o directorio a eliminar.
	 * @throws IOException Cuando ocurre un error al eliminar el elemento.
	 */
	private static void deleteFile(final File file) throws IOException {
		if (file.isDirectory()) {
			for (final File subFile : file.listFiles()) {
				deleteFile(subFile);
			}
		}
		Files.delete(file.toPath());
	}

}
