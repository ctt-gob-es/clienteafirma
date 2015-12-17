package es.gob.afirma.keystores.mozilla.bintutil;

import java.util.Hashtable;

/** Tipos de m&aacute;quina de binarios MS-PE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public enum PeMachineType {

	/** Desconocido. */
	UNKNOWN("Desconocida"), //$NON-NLS-1$

	/** Matsushita AM33. */
	MATSUSHITA_AM33("Matsushita AM33"), //$NON-NLS-1$

	/** x64. */
	X64("x64"), //$NON-NLS-1$

	/** ARM little endian.*/
	ARM_LITTLE_ENDIAN("ARM little endian"), //$NON-NLS-1$

	/** EFI bytecode.*/
	EFI_BYTECODE("EFI bytecode"), //$NON-NLS-1$

	/** Intel 386 o posteriores y compatibles.*/
	INTEL_386("Intel 386 o posteriores y compatibles"), //$NON-NLS-1$

	/** Familia de procesadores Intel Itanium.*/
	INTEL_ITANIUM("Familia de procesadores Intel Itanium"), //$NON-NLS-1$

	/** Mitsubishi M32R little endian.*/
	MITSUBISHI_M32R_LITTLE_ENDIAN("Mitsubishi M32R little endian"), //$NON-NLS-1$

	/** MIPS 16.*/
	MIPS16("MIPS 16"), //$NON-NLS-1$

	/** MIPS con FPU.*/
	MIPS_FPU("MIPS con FPU"); //$NON-NLS-1$

	private final String description;

	private PeMachineType(final String des) {
		this.description = des;
	}

	@Override
	public String toString() {
		return this.description;
	}

	private static final Hashtable<String, PeMachineType> machineTypes;

	static {
		machineTypes = new Hashtable<String, PeMachineType>(10);
		machineTypes.put("00-00", UNKNOWN); //$NON-NLS-1$
		machineTypes.put("01-D3", MATSUSHITA_AM33); //$NON-NLS-1$
		machineTypes.put("86-64", X64); //$NON-NLS-1$
		machineTypes.put("01-C0", ARM_LITTLE_ENDIAN); //$NON-NLS-1$
		machineTypes.put("0E-BC", EFI_BYTECODE); //$NON-NLS-1$
		machineTypes.put("01-4C", INTEL_386); //$NON-NLS-1$
		machineTypes.put("02-00", INTEL_ITANIUM); //$NON-NLS-1$
		machineTypes.put("90-41", MITSUBISHI_M32R_LITTLE_ENDIAN); //$NON-NLS-1$
		machineTypes.put("02-66", MIPS16); //$NON-NLS-1$
		machineTypes.put("03-66", MIPS_FPU); //$NON-NLS-1$
	}

	static PeMachineType getPeMachineType(final String id) {
		return machineTypes.get(id);
	}

}
