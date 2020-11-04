package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.mozilla.bintutil.ElfParser;
import es.gob.afirma.keystores.mozilla.bintutil.MsPortableExecutable;

/** Pruebas de analizado de ficheros PE/COFF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s Capote. */
public final class TestBinParser {

	private static final String FILE = "D:\\Program Files (x86)\\Mozilla Firefox\\softokn3.dll"; //$NON-NLS-1$

	/** Prueba b&aacute;sica de an&aacute;lisis de fichero PE/COFF y ELF.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {
		final File dllFile = new File(FILE);
		if (dllFile.isFile()) {
			final byte[] peBytes;
			try (
				final InputStream fis = new FileInputStream(dllFile);
			) {
				peBytes = AOUtil.getDataFromInputStream(fis);
			}
			final MsPortableExecutable pe = new MsPortableExecutable(peBytes);
			System.out.println(pe);
		}

		System.out.println(
			ElfParser.getMachineType(
				"C:\\Users\\tgarciameras\\workspace\\afirma-keystores-mozilla\\src\\test\\resources\\elf_arm64" //$NON-NLS-1$
			)
		);
		System.out.println(
			ElfParser.getMachineType(
				"C:\\Users\\tgarciameras\\workspace\\afirma-keystores-mozilla\\src\\test\\resources\\elf_x64" //$NON-NLS-1$
			)
		);
		System.out.println(
			ElfParser.getMachineType(
				"C:\\Users\\tgarciameras\\workspace\\afirma-keystores-mozilla\\src\\test\\resources\\elf_x86" //$NON-NLS-1$
			)
		);
	}

}
