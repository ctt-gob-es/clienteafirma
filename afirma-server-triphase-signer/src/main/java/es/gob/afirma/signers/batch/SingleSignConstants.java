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
	public enum SignAlgorithm {

		/** SHA1withRSA. */
		SHA1WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA),

		/** SHA256withRSA. */
		SHA256WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA),

		/** SHA284withRSA. */
		SHA384WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA),

		/** SHA512withRSA. */
		SHA512WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA),

		/** SHA1. */
		SHA1(AOSignConstants.DIGEST_ALGORITHM_SHA1),

		/** SHA256. */
		SHA256(AOSignConstants.DIGEST_ALGORITHM_SHA256),

		/** SHA284. */
		SHA384(AOSignConstants.DIGEST_ALGORITHM_SHA384),

		/** SHA512. */
		SHA512(AOSignConstants.DIGEST_ALGORITHM_SHA512);

		private final String name;

		SignAlgorithm(final String n) {
			this.name = n;
		}

		@Override
		public String toString() {
			return this.name;
		}

		/** Obtiene el algoritmo de firma a partir de su nombre.
		 * @param name Nombre del algoritmo de firma.
		 * @return Algoritmo firma. */
		public static SignAlgorithm getAlgorithm(final String name) {
			if (SHA1.toString().equalsIgnoreCase(name)) {
				return SHA1;
			}
			if (SHA256.toString().equalsIgnoreCase(name)) {
				return SHA256;
			}
			if (SHA384.toString().equalsIgnoreCase(name)) {
				return SHA384;
			}
			if (SHA512.toString().equalsIgnoreCase(name)) {
				return SHA512;
			}
			if (SHA1WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA1WITHRSA;
			}
			if (SHA256WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA256WITHRSA;
			}
			if (SHA384WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA384WITHRSA;
			}
			if (SHA512WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA512WITHRSA;
			}
			throw new IllegalArgumentException(
				"Tipo de algoritmo de firma no soportado: " + name //$NON-NLS-1$
			);
		}
	}
}
