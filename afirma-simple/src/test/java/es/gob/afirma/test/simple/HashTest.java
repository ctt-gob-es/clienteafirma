package es.gob.afirma.test.simple;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.security.DigestInputStream;
import java.security.MessageDigest;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;

/** Pruebas gen&eacute;ricas de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HashTest {

    private static final int BUFFER_SIZE = 4096;

	private static final String TEST_FILE = "D:\\Downloads\\ws_anbiT_pal_wbfs_CANABIS.part2.rar"; //$NON-NLS-1$
	//private static final String TEST_FILE = "D:\\Downloads\\425_DS3AV_Updater.zip"; //$NON-NLS-1$
	//private static final String TEST_FILE = "D:\\Downloads\\alcatel-androidmanager-2-2-1209-2563.zip"; //$NON-NLS-1$
	//private static final String TEST_FILE = "D:\\Downloads\\ASF_DOC_ASF.zip"; //$NON-NLS-1$

	/** Prueba de rendimiento de un procedimiento cl&aacute;sico de huella.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testHashPerformanceClassic() throws Exception {
		final long time = System.currentTimeMillis();
		final MessageDigest md = MessageDigest.getInstance("SHA-512"); //$NON-NLS-1$
		try (
			final InputStream fis = new FileInputStream(TEST_FILE);
			final InputStream bis = new BufferedInputStream(fis);
		) {
			final byte[] data = AOUtil.getDataFromInputStream(bis);
			System.out.println(
				AOUtil.hexify(
					md.digest(data),
					false
				)
			);
		}
		System.out.println("Tiempo: " + Long.toString(System.currentTimeMillis() - time)); //$NON-NLS-1$
	}

	/** Prueba de rendimiento de un procedimiento de huella con flujos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testHashPerformanceStreamed() throws Exception {
		final long time = System.currentTimeMillis();
		final MessageDigest md = MessageDigest.getInstance("SHA-512"); //$NON-NLS-1$
		final File file = new File(TEST_FILE);
		final byte[] buffer = new byte[BUFFER_SIZE];
		try (
			final RandomAccessFile raf = new RandomAccessFile(file, "r"); //$NON-NLS-1$
			final FileChannel channel = raf.getChannel();
			final InputStream fis = Channels.newInputStream(channel);
			final InputStream dis = new DigestInputStream(fis, md);
		) {
			while (dis.read(buffer) != -1) { /* Vacio */ }
		}
		System.out.println(
			AOUtil.hexify(
				md.digest(),
				false
			)
		);
		System.out.println("Tiempo: " + Long.toString(System.currentTimeMillis() - time)); //$NON-NLS-1$
	}


}
