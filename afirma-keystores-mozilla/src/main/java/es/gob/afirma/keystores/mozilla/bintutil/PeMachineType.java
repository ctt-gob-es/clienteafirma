/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

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

	private static final Hashtable<String, PeMachineType> MACHINE_TYPES;

	static {
		MACHINE_TYPES = new Hashtable<>(10);
		MACHINE_TYPES.put("00-00", UNKNOWN); //$NON-NLS-1$
		MACHINE_TYPES.put("01-D3", MATSUSHITA_AM33); //$NON-NLS-1$
		MACHINE_TYPES.put("86-64", X64); //$NON-NLS-1$
		MACHINE_TYPES.put("01-C0", ARM_LITTLE_ENDIAN); //$NON-NLS-1$
		MACHINE_TYPES.put("0E-BC", EFI_BYTECODE); //$NON-NLS-1$
		MACHINE_TYPES.put("01-4C", INTEL_386); //$NON-NLS-1$
		MACHINE_TYPES.put("02-00", INTEL_ITANIUM); //$NON-NLS-1$
		MACHINE_TYPES.put("90-41", MITSUBISHI_M32R_LITTLE_ENDIAN); //$NON-NLS-1$
		MACHINE_TYPES.put("02-66", MIPS16); //$NON-NLS-1$
		MACHINE_TYPES.put("03-66", MIPS_FPU); //$NON-NLS-1$
	}

	static PeMachineType getPeMachineType(final String id) {
		return MACHINE_TYPES.get(id);
	}

}
