package es.gob.afirma.keystores.mozilla.bintutil;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

/** Analizador de ficheros ELF (Executable and Linkable Format).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s Capote. */
public final class ElfParser {

	private ElfParser() {
		// Vacio
	}

	/** Obtiene el tipo de m&aacute;quina requerido para ejecutar este ELF.
	 * @param f Ruta ELF del cual se quiere saber el tipo de m&aacute;quina.
	 * @return Tipo de m&aacute;quina requerido para ejecutar este ELF.
	 * @throws IOException Si no se puede leer el fichero. */
	public static ElfMachineType getMachineType(final File f) throws IOException {
		if (f == null) {
			throw new IllegalArgumentException(
				"El fichero no puede ser nulo" //$NON-NLS-1$
			);
		}
		if (!f.isFile()) {
			throw new IllegalArgumentException(
				"El fichero no existe: " + f.getAbsolutePath() //$NON-NLS-1$
			);
		}
		try (
			final InputStream fis = new FileInputStream(f)
		) {
			return getMachineType(
				AOUtil.getDataFromInputStream(fis)
			);
		}
	}

	/** Obtiene el tipo de m&aacute;quina requerido para ejecutar este ELF.
	 * @param elfFilePath Ruta completa hacia el ELF del cual se quiere saber el tipo de m&aacute;quina.
	 * @return Tipo de m&aacute;quina requerido para ejecutar este ELF.
	 * @throws IOException Si no se puede leer el fichero. */
	public static ElfMachineType getMachineType(final String elfFilePath) throws IOException {
		if (elfFilePath == null) {
			throw new IllegalArgumentException(
				"El nombre de fichero no puede ser nulo" //$NON-NLS-1$
			);
		}
		return getMachineType(new File(elfFilePath));
	}

	/** Obtiene el tipo de m&aacute;quina requerido para ejecutar este ELF.
	 * @param elfFile ELF del cual se quiere saber el tipo de m&aacute;quina.
	 * @return Tipo de m&aacute;quina requerido para ejecutar este ELF. */
	public static ElfMachineType getMachineType(final byte[] elfFile) {
		if (elfFile == null || elfFile.length < 0x12 + 1) {
			throw new IllegalArgumentException("Fichero ELF nulo o invalido"); //$NON-NLS-1$
		}
		switch(elfFile[0x12]) {
			case (byte) 0x00:
				return ElfMachineType.UNSPECIFIED;
			case (byte) 0x01:
				return ElfMachineType.BELLMAC_32;
			case (byte) 0x02:
				return ElfMachineType.SPARC;
			case (byte) 0x03:
				return ElfMachineType.X86;
			case (byte) 0x04:
				return ElfMachineType.M68K;
			case (byte) 0x05:
				return ElfMachineType.M88K;
			case (byte) 0x06:
				return ElfMachineType.IMCU;
			case (byte) 0x07:
				return ElfMachineType.I860;
			case (byte) 0x08:
				return ElfMachineType.MIPS;
			case (byte) 0x09:
				return ElfMachineType.IBM_370;
			case (byte) 0x0A:
				return ElfMachineType.MIPS_R3K_LE;
			case (byte) 0x0B:
			case (byte) 0x0C:
			case (byte) 0x0D:
			case (byte) 0x0F:
				return ElfMachineType.RESERVED;
			case (byte) 0x0E:
				return ElfMachineType.HP_PARISC;
			case (byte) 0x13:
				return ElfMachineType.I960;
			case (byte) 0x14:
				return ElfMachineType.POWERPC;
			case (byte) 0x15:
				return ElfMachineType.POWERPC64;
			case (byte) 0x16:
				return ElfMachineType.IBM_390;
			case (byte) 0x28:
				return ElfMachineType.ARM;
			case (byte) 0x2A:
				return ElfMachineType.SUPERH;
			case (byte) 0x32:
				return ElfMachineType.IA64;
			case (byte) 0x3E:
				return ElfMachineType.AMD64;
			case (byte) 0x8C:
				return ElfMachineType.TMS320C6000;
			case (byte) 0xB7:
				return ElfMachineType.ARM64;
			case (byte) 0xF3:
				return ElfMachineType.RISCV;

			default:
				return ElfMachineType.UNKNOWN;

		}
	}

	/** Comprueba si la arquitectura de un fichero ELF corresponde con la del JRE actual.
	 * @param elf Fichero ELF a comprobar.
	 * @return <code>true</code> si la arquitectura de un ELF corresponde con la del JRE actual,
	 *         <code>false</code> en caso contrario. */
	public static boolean archMatches(final File elf) {
		if (elf == null) {
			return false;
		}
		final ElfMachineType a;
		try {
			a = getMachineType(elf);
		}
		catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No se puede comprobar la arquitectura del fichero '" + elf.getAbsolutePath() + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return false;
		}
		return "64".equals(Platform.getJavaArch()) && //$NON-NLS-1$
					Platform.MACHINE.AMD64.equals(Platform.getMachineType()) &&
						ElfMachineType.AMD64.equals(a) ||
			   "32".equals(Platform.getJavaArch()) && //$NON-NLS-1$
			   		(Platform.MACHINE.X86.equals(Platform.getMachineType()) || Platform.MACHINE.AMD64.equals(Platform.getMachineType())) && // 32 puede estar en maquina de 32 o de 64 bits
		   				ElfMachineType.X86.equals(a);
	}

}
