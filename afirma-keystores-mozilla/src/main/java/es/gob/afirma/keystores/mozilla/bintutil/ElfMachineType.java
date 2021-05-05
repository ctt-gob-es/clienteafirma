package es.gob.afirma.keystores.mozilla.bintutil;

/** Tipos de m&aacute;quina de binarios MS-PE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public enum ElfMachineType {

	/** No especificado. */
	UNSPECIFIED("No especificado"), //$NON-NLS-1$

	/** AT&amp;T WE 32100. */
	BELLMAC_32("AT&T WE 32100"), //$NON-NLS-1$

	/** SPARC. */
	SPARC("SPARC"), //$NON-NLS-1$

	/** x86. */
	X86("x86"), //$NON-NLS-1$

	/** Motorola 68000. */
	M68K("Motorola 68000"), //$NON-NLS-1$

	/** Motorola 88000. */
	M88K("Motorola 88000"), //$NON-NLS-1$

	/** Intel MCU. */
	IMCU("Intel MCU"), //$NON-NLS-1$

	/** Intel 80860. */
	I860("Intel 80860"), //$NON-NLS-1$

	/** MIPS. */
	MIPS("MIPS"), //$NON-NLS-1$

	/** IBM System/370. */
	IBM_370("IBM System/370"), //$NON-NLS-1$

	/** MIPS RS3000 Little-endian. */
	MIPS_R3K_LE("MIPS RS3000 Little-endian"), //$NON-NLS-1$

	/** Reservado. */
	RESERVED("Reservado para uso futuro"), //$NON-NLS-1$

	/** Hewlett-Packard PA-RISC. */
	HP_PARISC("Hewlett-Packard PA-RISC"), //$NON-NLS-1$

	/** Intel 80960. */
	I960("Intel 80960"), //$NON-NLS-1$

	/** PowerPC. */
	POWERPC("PowerPC"), //$NON-NLS-1$

	/** PowerPC 64. */
	POWERPC64("PowerPC 64"), //$NON-NLS-1$

	/** IBM System/390. */
	IBM_390("IBM System/390"), //$NON-NLS-1$

	/** ARM. */
	ARM("ARM"), //$NON-NLS-1$

	/** Hitachi SuperH. */
	SUPERH("Hitachi SuperH"), //$NON-NLS-1$

	/** IA-64. */
	IA64("IA-64"), //$NON-NLS-1$

	/** AMD64. */
	AMD64("AMD64"), //$NON-NLS-1$

	/** TI TMS320C6000 Family. */
	TMS320C6000("TMS320C6000 Family"), //$NON-NLS-1$

	/** ARM 64-bits (ARMv8/Aarch64). */
	ARM64("ARM 64-bits (ARMv8/Aarch64)"), //$NON-NLS-1$

	/** RISC-V. */
	RISCV("RISC-V"), //$NON-NLS-1$

	/** WDC 65C816. */
	WDC_65C816("WDC 65C816"), //$NON-NLS-1$

	/** Desconocido. */
	UNKNOWN("Desconocido"); //$NON-NLS-1$


	private final String description;

	private ElfMachineType(final String desc) {
		this.description = desc;
	}

	@Override
	public String toString() {
		return this.description;
	}

}
