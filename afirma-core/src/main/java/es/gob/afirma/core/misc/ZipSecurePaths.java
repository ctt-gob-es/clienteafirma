package es.gob.afirma.core.misc;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Resuelve con seguridad rutas de entradas de ZIP dentro del directorio de destino,
 * evitando Zip Slip (incluyendo rutas absolutas y enlaces simb&oacute;licos).
 */
public final class ZipSecurePaths {

	private ZipSecurePaths() {
		// No instanciable
	}

	/**
	 * Resuelve con seguridad la ruta final de una entrada de ZIP dentro del directorio
	 * de destino, evitando Zip Slip (incluyendo rutas absolutas y enlaces simb&oacute;licos).
	 * @param destDir Directorio de extracci&oacute;n permitido.
	 * @param entryName Nombre de la entrada del ZIP.
	 * @return Fichero resultante, garantizado dentro de {@code destDir}.
	 * @throws IOException Si la ruta queda fuera del destino.
	 */
	public static File safeResolve(final File destDir, final String entryName) throws IOException {
		final Path dir = destDir.toPath();
		final Path base = Files.exists(dir) ? dir.toRealPath().normalize()
											: dir.toAbsolutePath().normalize();
		final Path target = base.resolve(entryName).normalize();
		if (!target.startsWith(base)) {
			throw new IOException("Entrada de zip fuera del directorio destino: " + entryName); //$NON-NLS-1$
		}
		// Comprobaci&oacute;n adicional contra enlaces simb&oacute;licos del padre real.
		if (Files.exists(target.getParent())) {
			final Path realParent = target.getParent().toRealPath();
			if (!realParent.startsWith(base)) {
				throw new IOException("Entrada de zip fuera del directorio destino: " + entryName); //$NON-NLS-1$
			}
		}
		return target.toFile();
	}
}
