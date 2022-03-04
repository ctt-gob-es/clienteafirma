package es.gob.afirma.standalone.so.macos;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.LoggerUtil;

public class UnixUtils {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Da permisos de ejecuci&oacute;n a todos los ficheros de un directorio dado.
	 * @param dir Directorio al que dar permiso.
	 */
	public static void addExexPermissionsToAllFilesOnDirectory(final File dir) {

		for (final File fileEntry : dir.listFiles()) {
			addAllPermissionsToFile(fileEntry);
		}
	}

	/**
	 * Concede permisos POSIX completos a un fichero para todos los usuarios.
	 * @param f Fichero al que se le conceden permisos.
	 */
	public static void addAllPermissionsToFile(final File f) {
		final Set<PosixFilePermission> perms = new HashSet<>();
		perms.add(PosixFilePermission.OWNER_EXECUTE);
		perms.add(PosixFilePermission.GROUP_EXECUTE);
		perms.add(PosixFilePermission.OTHERS_EXECUTE);
		perms.add(PosixFilePermission.OWNER_READ);
		perms.add(PosixFilePermission.GROUP_READ);
		perms.add(PosixFilePermission.OTHERS_READ);
		perms.add(PosixFilePermission.OWNER_WRITE);
		perms.add(PosixFilePermission.GROUP_WRITE);
		perms.add(PosixFilePermission.OTHERS_WRITE);
		try {
			Files.setPosixFilePermissions(
				Paths.get(f.getAbsolutePath()),
				perms
			);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se ha podido dar permiso de ejecucion a '" + LoggerUtil.getCleanUserHomePath(f.getAbsolutePath()) + "': " + e//$NON-NLS-1$ //$NON-NLS-2$
			);
		}
	}
}
