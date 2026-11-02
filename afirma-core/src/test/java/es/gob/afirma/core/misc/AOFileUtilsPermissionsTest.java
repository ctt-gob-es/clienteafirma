package es.gob.afirma.core.misc;

import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.Test;

/** Pruebas de los permisos que otorga AOFileUtils.
 * Regresi&oacute;n: no deben concederse permisos de escritura a grupo ni a otros usuarios. */
public final class AOFileUtilsPermissionsTest {

	/** Comprueba que setAllPermissions deja el fichero con permisos 0755 como m&aacute;ximo.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testNoConcedeEscrituraAGrupoNiOtros() throws Exception {

		// Solo tiene sentido donde hay atributos POSIX (Linux/macOS)
		Assume.assumeTrue(
			"Requiere sistema de ficheros POSIX",
			FileSystems.getDefault().supportedFileAttributeViews().contains("posix") //$NON-NLS-1$
		);

		final File f = File.createTempFile("perm_test_", ".tmp"); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertTrue(f.delete());

		try {
			final boolean created = f.createNewFile();
			Assert.assertTrue("No se pudo crear el fichero de pruebas", created); //$NON-NLS-1$

			AOFileUtils.setAllPermissions(f);

			final Set<PosixFilePermission> perms = Files.getPosixFilePermissions(f.toPath());

			Assert.assertFalse(
				"El fichero queda escribible por el grupo", //$NON-NLS-1$
				perms.contains(PosixFilePermission.GROUP_WRITE)
			);
			Assert.assertFalse(
				"El fichero queda escribible por otros usuarios", //$NON-NLS-1$
				perms.contains(PosixFilePermission.OTHERS_WRITE)
			);
			Assert.assertTrue(
				"El duenio debe poder leer, escribir y ejecutar", //$NON-NLS-1$
				perms.contains(PosixFilePermission.OWNER_READ)
					&& perms.contains(PosixFilePermission.OWNER_WRITE)
					&& perms.contains(PosixFilePermission.OWNER_EXECUTE)
			);
		}
		finally {
			Assert.assertTrue(f.delete());
		}
	}
}
