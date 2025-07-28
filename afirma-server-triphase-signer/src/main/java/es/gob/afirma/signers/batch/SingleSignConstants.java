/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch;

import es.gob.afirma.core.signers.AOSignConstants;

/** Constantes para la definici&oacute;n de una firma independiente.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SingleSignConstants {

	/** Tipo de operaci&oacute;n de firma. */
	public enum SignSubOperation {

		/** Firma. */
		SIGN("sign"), //$NON-NLS-1$

		/** Cofirma. */
		COSIGN("cosign"), //$NON-NLS-1$

		/** Contrafirma. */
		COUNTERSIGN("countersign"); //$NON-NLS-1$

		private final String name;

		SignSubOperation(final String n) {
			this.name = n;
		}

		@Override
		public String toString() {
			return this.name;
		}

		/** Obtiene el tipo de operaci&oacute;n de firma a partir de su nombre.
		 * @param name Nombre del tipo de operaci&oacute;n de firma.
		 * @return Tipo de operaci&oacute;n de firma. */
		public static SignSubOperation getSubOperation(final String name) {
			if (SIGN.toString().equalsIgnoreCase(name)) {
				return SIGN;
			}
			if (COSIGN.toString().equalsIgnoreCase(name)) {
				return COSIGN;
			}
			if (COUNTERSIGN.toString().equalsIgnoreCase(name)) {
				return COUNTERSIGN;
			}
			throw new IllegalArgumentException(
				"Tipo de operacion (suboperation) de firma no soportado: " + name //$NON-NLS-1$
			);
		}
	}

	/** Formato de firma. */
	public enum SignFormat {

		/** CAdES. */
		CADES(AOSignConstants.SIGN_FORMAT_CADES),

		/** CAdES ASiC. */
		CADES_ASIC(AOSignConstants.SIGN_FORMAT_CADES_ASIC_S),

		/** XAdES. */
		XADES(AOSignConstants.SIGN_FORMAT_XADES),

		/** XAdES ASiC. */
		XADES_ASIC(AOSignConstants.SIGN_FORMAT_XADES_ASIC_S),

		/** PAdES. */
		PADES(AOSignConstants.SIGN_FORMAT_PADES),

		/** FacturaE. */
		FACTURAE(AOSignConstants.SIGN_FORMAT_FACTURAE),

		/** PKCS#1. */
		PKCS1(AOSignConstants.SIGN_FORMAT_PKCS1);

		private final String name;

		SignFormat(final String n) {
			this.name = n;
		}

		@Override
		public String toString() {
			return this.name;
		}

		/** Obtiene el formato de firma a partir de su nombre.
		 * @param name Nombre del formato de firma.
		 * @return Formato firma. */
		public static SignFormat getFormat(final String name) {
			if (name != null) {
				if (CADES.toString().equalsIgnoreCase(name.trim())) {
					return CADES;
				}
				if (XADES.toString().equalsIgnoreCase(name.trim())) {
					return XADES;
				}
				if (PADES.toString().equalsIgnoreCase(name.trim())) {
					return PADES;
				}
				if (FACTURAE.toString().equalsIgnoreCase(name.trim())) {
					return FACTURAE;
				}
				if (PKCS1.toString().equalsIgnoreCase(name.trim())) {
					return PKCS1;
				}
				if (CADES_ASIC.toString().equalsIgnoreCase(name.trim())) {
					return CADES_ASIC;
				}
				if (XADES_ASIC.toString().equalsIgnoreCase(name.trim())) {
					return XADES_ASIC;
				}
			}
			throw new IllegalArgumentException(
				"Tipo de formato de firma no soportado: " + name //$NON-NLS-1$
			);
		}
	}

	/** Algoritmo de firma. */
	public enum DigestAlgorithm {

		/** SHA1. */
		SHA1(AOSignConstants.DIGEST_ALGORITHM_SHA1),

		/** SHA256. */
		SHA256(AOSignConstants.DIGEST_ALGORITHM_SHA256),

		/** SHA284. */
		SHA384(AOSignConstants.DIGEST_ALGORITHM_SHA384),

		/** SHA512. */
		SHA512(AOSignConstants.DIGEST_ALGORITHM_SHA512);

		private final String name;

		DigestAlgorithm(final String n) {
			this.name = n;
		}

		/**
		 * Obtiene el nombre del algoritmo..
		 * @return Nombre del algoritmo.
		 */
		public String getName() {
			return this.name;
		}

		@Override
		public String toString() {
			return getName();
		}

		/**
		 * Obtiene el algoritmo de huella a partir de su nombre.
		 * @param name Nombre del algoritmo de huella.
		 * @return Algoritmo de huella o de firma RSA/ECDSA.
		 */
		public static DigestAlgorithm getAlgorithm(final String name) {
			// Ademas del algoritmo de huella, comparamos si nos pasan un algoritmo de firma RSA
			// por retrocompatibilidad (v1.9 y anteriores)
			if (SHA1.getName().equalsIgnoreCase(name)
					|| "SHA1withRSA".equalsIgnoreCase(name) //$NON-NLS-1$
					|| "SHA1withECDSA".equalsIgnoreCase(name)) { //$NON-NLS-1$
				return SHA1;
			}
			if (SHA256.getName().equalsIgnoreCase(name)
					|| "SHA256withRSA".equalsIgnoreCase(name) //$NON-NLS-1$
					|| "SHA256withECDSA".equalsIgnoreCase(name)) { //$NON-NLS-1$) {
				return SHA256;
			}
			if (SHA384.getName().equalsIgnoreCase(name)
					|| "SHA384withRSA".equalsIgnoreCase(name) //$NON-NLS-1$
					|| "SHA284withECDSA".equalsIgnoreCase(name)) { //$NON-NLS-1$) {
				return SHA384;
			}
			if (SHA512.getName().equalsIgnoreCase(name)
					|| "SHA512withRSA".equalsIgnoreCase(name) //$NON-NLS-1$
					|| "SHA512withECDSA".equalsIgnoreCase(name)) { //$NON-NLS-1$) {
				return SHA512;
			}
			throw new IllegalArgumentException(
				"Tipo de algoritmo de firma no soportado: " + name //$NON-NLS-1$
			);
		}
	}
}
