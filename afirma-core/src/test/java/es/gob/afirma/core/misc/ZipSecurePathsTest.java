package es.gob.afirma.core.misc;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Zip Slip (#61): entradas fuera del destino deben fallar con mensaje claro.
 */
public class ZipSecurePathsTest {

	@Rule
	public TemporaryFolder tmp = new TemporaryFolder();

	@Test
	public void safeResolveAcceptsNormalEntry() throws Exception {
		final File dest = this.tmp.newFolder("dest");
		final File out = ZipSecurePaths.safeResolve(dest, "a/b.txt");
		Assert.assertTrue(out.getPath().startsWith(dest.getCanonicalPath()));
	}

	@Test(expected = IOException.class)
	public void safeResolveRejectsTraversal() throws Exception {
		final File dest = this.tmp.newFolder("dest");
		ZipSecurePaths.safeResolve(dest, "../evil.txt");
	}

	@Test(expected = IOException.class)
	public void safeResolveRejectsDoubleTraversal() throws Exception {
		final File dest = this.tmp.newFolder("dest");
		ZipSecurePaths.safeResolve(dest, "a/../../evil.txt");
	}

	@Test(expected = IOException.class)
	public void safeResolveRejectsAbsolutePath() throws Exception {
		final File dest = this.tmp.newFolder("dest");
		ZipSecurePaths.safeResolve(dest, "/tmp/evil");
	}

	@Test
	public void safeResolveAcceptsNonExistentDestination() throws Exception {
		final File dest = new File(this.tmp.getRoot(), "noexiste");
		final File out = ZipSecurePaths.safeResolve(dest, "a/b.txt");
		Assert.assertTrue(out.getPath().startsWith(dest.getAbsolutePath()));
	}

	@Test
	public void safeResolveRejectsSymlinkEscape() throws Exception {
		final File dest = this.tmp.newFolder("dest");
		final File link = new File(dest, "link");
		boolean created = false;
		try {
			Files.createSymbolicLink(link.toPath(), new File("/tmp").toPath());
			created = true;
		}
		catch (final Exception e) {
			Assume.assumeTrue("No se pudo crear enlace simbólico (permisos)", false);
		}
		Assume.assumeTrue(created);
		try {
			ZipSecurePaths.safeResolve(dest, "link/evil.txt");
			Assert.fail("Debía rechazar la entrada que escapa por enlace simbólico");
		}
		catch (final IOException e) {
			// OK
		}
	}

	@Test
	public void zipSlipEntryDoesNotCreateFile() throws Exception {
		final File dest = this.tmp.newFolder("out");
		final File zip = this.tmp.newFile("slip.zip");
		try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zip))) {
			zos.putNextEntry(new ZipEntry("../evil.txt"));
			zos.write("x".getBytes(StandardCharsets.UTF_8));
			zos.closeEntry();
		}
		boolean failed = false;
		try (java.util.zip.ZipInputStream zis = new java.util.zip.ZipInputStream(new java.io.FileInputStream(zip))) {
			ZipEntry e;
			while ((e = zis.getNextEntry()) != null) {
				ZipSecurePaths.safeResolve(dest, e.getName());
			}
		}
		catch (final IOException ex) {
			failed = true;
		}
		Assert.assertTrue(failed);
		Assert.assertFalse(new File(this.tmp.getRoot(), "evil.txt").exists());
	}
}
