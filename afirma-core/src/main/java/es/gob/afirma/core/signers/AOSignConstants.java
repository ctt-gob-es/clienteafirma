/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.util.Locale;
import java.util.logging.Logger;

/** Constantes relativas a las firmas digitales. */
public final class AOSignConstants {

	// ************************************************************
	// ************* FORMATOS DE FIRMA*****************************
	// ************************************************************

	/** Identificador del formato de firma seleccionado autom&aacute;ticamente. */
	public static final String SIGN_FORMAT_AUTO = "auto"; //$NON-NLS-1$

	/** Identificador de la firma CMS. */
	public static final String SIGN_FORMAT_CMS = "CMS/PKCS#7"; //$NON-NLS-1$

	/** Identificador de la firma CAdES ASiC-S. */
	public static final String SIGN_FORMAT_CADES_ASIC_S = "CAdES-ASiC-S"; //$NON-NLS-1$

	/** Identificador de la firma CAdES ASiC-S trif&aacute;sica. */
	public static final String SIGN_FORMAT_CADES_ASIC_S_TRI = "CAdES-ASiC-S-tri"; //$NON-NLS-1$

	/** Identificador de la firma CAdES. */
	public static final String SIGN_FORMAT_CADES = "CAdES"; //$NON-NLS-1$

	/** Identificador de la firma CAdES trif&aacute;sica. */
	public static final String SIGN_FORMAT_CADES_TRI = "CAdEStri"; //$NON-NLS-1$

	/** Identificador de la firma PKCS1 (RAW). */
	public static final String SIGN_FORMAT_PKCS1 = "NONE"; //$NON-NLS-1$

	/** Identificador (alternativo 1) de la firma PKCS1 (RAW). */
	public static final String SIGN_FORMAT_PKCS1_ALT1 = "PKCS1"; //$NON-NLS-1$

	/** Identificador (alternativo 2) de la firma PKCS1 (RAW). */
	public static final String SIGN_FORMAT_PKCS1_ALT2 = "PKCS#1"; //$NON-NLS-1$

	/** Identificador de la firma PKCS#1 (RAW) para su uso en modo trif&aacute;sico
	 * (la parte servidora no har&aacute; nada. */
	public static final String SIGN_FORMAT_PKCS1_TRI = "NONEtri"; //$NON-NLS-1$

	/** Identificador de la firma XAdES-ASiC-S. */
	public static final String SIGN_FORMAT_XADES_ASIC_S = "XAdES-ASiC-S"; //$NON-NLS-1$

	/** Identificador de la firma XAdES-ASiC-S trif&aacute;sica. */
	public static final String SIGN_FORMAT_XADES_ASIC_S_TRI = "XAdES-ASiC-S-tri"; //$NON-NLS-1$

	/** Identificador de la firma XAdES Internally Detached. */
	public static final String SIGN_FORMAT_XADES_DETACHED = "XAdES Detached"; //$NON-NLS-1$

	/** Identificador de la firma XAdES Externally Detached. */
	public static final String SIGN_FORMAT_XADES_EXTERNALLY_DETACHED = "XAdES Externally Detached"; //$NON-NLS-1$

	/** Identificador de la firma XAdES Enveloped. */
	public static final String SIGN_FORMAT_XADES_ENVELOPED = "XAdES Enveloped"; //$NON-NLS-1$

	/** Identificador de la firma XAdES Enveloping. */
	public static final String SIGN_FORMAT_XADES_ENVELOPING = "XAdES Enveloping"; //$NON-NLS-1$

	/** Identificador de la firma XAdES por defecto. */
	public static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	/** Identificador de la firma XAdES trif&aacute;sica. */
	public static final String SIGN_FORMAT_XADES_TRI = "XAdEStri"; //$NON-NLS-1$

	/** Identificador de la firma XMLDsig Detached. */
	public static final String SIGN_FORMAT_XMLDSIG_DETACHED = "XMLDSig Detached"; //$NON-NLS-1$

	/** Identificador de la firma XMLdSig Externally Detached. */
	public static final String SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED = "XMLDSig Externally Detached"; //$NON-NLS-1$

	/** Identificador de la firma XMLDsig Enveloped. */
	public static final String SIGN_FORMAT_XMLDSIG_ENVELOPED = "XMLDSig Enveloped"; //$NON-NLS-1$

	/** Identificador de la firma XMLDsig Enveloping. */
	public static final String SIGN_FORMAT_XMLDSIG_ENVELOPING = "XMLDSig Enveloping"; //$NON-NLS-1$

	/** Identificador de la firma XMLDSig (<i>XML Digital Signature</i>). */
	public static final String SIGN_FORMAT_XMLDSIG = "XMLDSig"; //$NON-NLS-1$

	/** Identificador de la firma OOXML (<i>Office Open XML</i>). */
	public static final String SIGN_FORMAT_OOXML = "OOXML (Office Open XML)"; //$NON-NLS-1$

	/** Identificador alternativo n&uacute;mero 1 para el formato OOXML. */
	public static final String SIGN_FORMAT_OOXML_ALT1 = "OOXML"; //$NON-NLS-1$

	/** Identificador de la firma ODF (<i>Open Document Format</i>). */
	public static final String SIGN_FORMAT_ODF = "ODF (Open Document Format)"; //$NON-NLS-1$

	/** Identificador alternativo n&uacute;mero 1 para el formato ODF. */
	public static final String SIGN_FORMAT_ODF_ALT1 = "ODF"; //$NON-NLS-1$

	/** Identificador de la firma Adobe PDF. */
	public static final String SIGN_FORMAT_PDF = "Adobe PDF"; //$NON-NLS-1$

	/** Identificador de la firma Adobe PDF trif&aacute;sica. */
	public static final String SIGN_FORMAT_PDF_TRI = "Adobe PDF TriPhase"; //$NON-NLS-1$

	/** Identificador de la firma PAdES. */
	public static final String SIGN_FORMAT_PADES = "PAdES"; //$NON-NLS-1$

	/** Identificador de la firma PAdES trif&aacute;sica. */
	public static final String SIGN_FORMAT_PADES_TRI = "PAdEStri"; //$NON-NLS-1$

	/** Identificador de la firma SOAP. */
	public static final String SIGN_FORMAT_SOAP = "SOAP"; //$NON-NLS-1$

	/** Identificador de la firma Factura-e (derivado de XAdES-EPES). */
	public static final String SIGN_FORMAT_FACTURAE = "FacturaE"; //$NON-NLS-1$

	/** Identificador de la firma Factura-e (derivado de XAdES-EPES) trif&aacute;sica. */
	public static final String SIGN_FORMAT_FACTURAE_TRI = "FacturaEtri"; //$NON-NLS-1$

	/** Identificador alternativo de la firma Factura-e (derivado de XAdES-EPES). */
	public static final String SIGN_FORMAT_FACTURAE_ALT1 = "Factura-e"; //$NON-NLS-1$

	/** Formato de firma por defecto. */
	public static final String DEFAULT_SIGN_FORMAT = SIGN_FORMAT_CADES;

	// ************************************************************
	// ************* OPERACIONES **********************************
	// ************************************************************

	/** Identificador de la operaci&oacute;n de firma masiva. */
	public static final String MASSIVE_OPERATION_SIGN = "FIRMAR"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de cofirma masiva. */
	public static final String MASSIVE_OPERATION_COSIGN = "COFIRMAR"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de contrafirma masiva de todo el
	 * &aacute;rbol de firma. */
	public static final String MASSIVE_OPERATION_COUNTERSIGN_TREE = "CONTRAFIRMAR_ARBOL"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de contrafirma masiva de nodos hoja
	 * de firma. */
	public static final String MASSIVE_OPERATION_COUNTERSIGN_LEAFS = "CONTRAFIRMAR_HOJAS"; //$NON-NLS-1$

	/** Operaci&oacute;n masiva por defecto. */
	public static final String DEFAULT_MASSIVE_OPERATION = MASSIVE_OPERATION_SIGN;

	// ************************************************************
	// ****************** PERFILES DE FIRMA ***********************
	// ************************************************************

	/** Perfil de firma avanzada corriente (BES, EPES, T, etc.). */
	public static final String SIGN_PROFILE_ADVANCED = "advanced"; //$NON-NLS-1$

	/** Perfil de firma Baseline (B-Level, T-Level, etc.). */
	public static final String SIGN_PROFILE_BASELINE = "baseline"; //$NON-NLS-1$

	/** Perfil de firma por defecto. */
	public static final String DEFAULT_SIGN_PROFILE = SIGN_PROFILE_ADVANCED;

	// ************************************************************
	// ****************** ATRIBUTOS DE CMS ************************
	// ************************************************************

	/** Envoltorio binario de tipo <i>Data</i> (datos envueltos en un envoltorio
	 * PKCS#7). */
	public static final String CMS_CONTENTTYPE_DATA = "Data"; //$NON-NLS-1$

	/** Firma binaria de tipo <i>Signed Data</i>. */
	public static final String CMS_CONTENTTYPE_SIGNEDDATA = "SignedData"; //$NON-NLS-1$

	/** Envoltorio binario de tipo <i>Digest</i>. */
	public static final String CMS_CONTENTTYPE_DIGESTEDDATA = "DigestedData"; //$NON-NLS-1$

	/** Envoltorio binario de tipo <i>AuthenticatedEnvelopedData</i>. */
	public static final String CMS_CONTENTTYPE_COMPRESSEDDATA = "CompressedData"; //$NON-NLS-1$

	/** Firma binaria de tipo <i>Encrypted Data</i> */
	public static final String CMS_CONTENTTYPE_ENCRYPTEDDATA = "EncryptedData"; //$NON-NLS-1$

	/** Envoltorio binario de tipo <i>Enveloped</i> (sobre digital). */
	public static final String CMS_CONTENTTYPE_ENVELOPEDDATA = "EnvelopedData"; //$NON-NLS-1$

	/** Envoltorio binario de tipo <i>Signed and Enveloped</i>. */
	public static final String CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA = "SignedAndEnvelopedData"; //$NON-NLS-1$

	/** Envoltorio binario de tipo <i>AuthenticatedData</i>. */
	public static final String CMS_CONTENTTYPE_AUTHENTICATEDDATA = "AuthenticatedData"; //$NON-NLS-1$

	/** Envoltorio binario de tipo <i>AuthenticatedEnvelopedData</i>. */
	public static final String CMS_CONTENTTYPE_AUTHENVELOPEDDATA = "AuthEnvelopedData"; //$NON-NLS-1$

	/** Envoltorio binario por defecto. */
	public static final String DEFAULT_CMS_CONTENTTYPE = CMS_CONTENTTYPE_ENVELOPEDDATA;

	/** OID por defecto para los datos firmados. */
	public static final String DEFAULT_OID_TO_SIGN = "1.3.6.1.4.1.1466.115.121.1.40"; // Octect //$NON-NLS-1$

	// ************************************************************
	// ******************** SUBFILTROS PDF ************************
	// ************************************************************

	/** Filtro para firma PAdES-B&aacute;sico. */
	public static final String PADES_SUBFILTER_BASIC = "adbe.pkcs7.detached"; //$NON-NLS-1$

	/** Filtro para firma PAdES-BES. */
	public static final String PADES_SUBFILTER_BES = "ETSI.CAdES.detached"; //$NON-NLS-1$

	// ************************************************************
	// ************* ALGORITMOS DE FIRMA **************************
	// ************************************************************

	/** Algoritmo de huella digital SHA1. */
	public static final String DIGEST_ALGORITHM_SHA1 = "SHA1"; //$NON-NLS-1$

	/** Algoritmo de huella digital SHA256. */
	public static final String DIGEST_ALGORITHM_SHA256 = "SHA256"; //$NON-NLS-1$

	/** Algoritmo de huella digital SHA384. */
	public static final String DIGEST_ALGORITHM_SHA384 = "SHA384"; //$NON-NLS-1$

	/** Algoritmo de huella digital SHA512. */
	public static final String DIGEST_ALGORITHM_SHA512 = "SHA512"; //$NON-NLS-1$

	/** Algoritmo de firma SHA1withRSA. */
	public static final String SIGN_ALGORITHM_SHA1WITHRSA = "SHA1withRSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA256withRSA. */
	public static final String SIGN_ALGORITHM_SHA256WITHRSA = "SHA256withRSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA384withRSA. */
	public static final String SIGN_ALGORITHM_SHA384WITHRSA = "SHA384withRSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA512withRSA. */
	public static final String SIGN_ALGORITHM_SHA512WITHRSA = "SHA512withRSA"; //$NON-NLS-1$

	/** Algoritmo de firma RSA que no incluye la generaci&oacute;n de la huella
	 * digital (NONEwithRSA). */
	public static final String SIGN_ALGORITHM_NONEWITHRSA = "NONEwithRSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA1withDSA. */
	public static final String SIGN_ALGORITHM_SHA1WITHDSA = "SHA1withDSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA1withECDSA. */
	public static final String SIGN_ALGORITHM_SHA1WITHECDSA = "SHA1withECDSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA224withECDSA. */
	public static final String SIGN_ALGORITHM_SHA224WITHECDSA = "SHA224withECDSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA256withECDSA. */
	public static final String SIGN_ALGORITHM_SHA256WITHECDSA = "SHA256withECDSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA384withECDSA. */
	public static final String SIGN_ALGORITHM_SHA384WITHECDSA = "SHA384withECDSA"; //$NON-NLS-1$

	/** Algoritmo de firma SHA512withECDSA. */
	public static final String SIGN_ALGORITHM_SHA512WITHECDSA = "SHA512withECDSA"; //$NON-NLS-1$

	/** Algoritmo de firma ECDSA que no incluye la generaci&oacute;n de la huella
	 * digital (NONEwithEDSSA). */
	public static final String SIGN_ALGORITHM_NONEWITHECDSA = "NONEwithECDSA"; //$NON-NLS-1$

	/** Algoritmo de firma por defecto. */
	public static final String DEFAULT_SIGN_ALGO = SIGN_ALGORITHM_SHA512WITHRSA;

	// ************************************************************
	// ****************** MODOS DE FIRMA **************************
	// ************************************************************

	/** Identificador del modo de firma Expl&iacute;cita (Los datos NO se incluyen en la
	 * firma). */
	public static final String SIGN_MODE_EXPLICIT = "explicit"; //$NON-NLS-1$

	/** Identificador del modo de firma Impl&iacute;cita (Los datos SI se incluyen en la
	 * firma). */
	public static final String SIGN_MODE_IMPLICIT = "implicit"; //$NON-NLS-1$

	/** Modo de firma por defecto. */
	public static final String DEFAULT_SIGN_MODE = SIGN_MODE_EXPLICIT;

	private AOSignConstants() {
		// No permitimos la instanciacion
	}

	/** Obtiene el nombre de un algoritmo de huella digital a partir de una de
	 * las variantes de este.
	 * @param pseudoName Nombre o variante del nombre del algoritmo de huella digital.
	 * @return Nombre del algoritmo de huella digital. */
	public static String getDigestAlgorithmName(final String pseudoName) {
		if (pseudoName == null) {
			throw new IllegalArgumentException(
				"El nombre del algoritmo de huella digital no puede ser nulo" //$NON-NLS-1$
			);
		}
		final String upperPseudoName = pseudoName.toUpperCase(Locale.US);
		if ("SHA".equals(upperPseudoName)  //$NON-NLS-1$
				|| upperPseudoName.equals("http://www.w3.org/2000/09/xmldsig#sha1".toUpperCase(Locale.US)) //$NON-NLS-1$
				|| "1.3.14.3.2.26".equals(upperPseudoName) //$NON-NLS-1$
				|| upperPseudoName.startsWith("SHA1") //$NON-NLS-1$
				|| upperPseudoName.startsWith("SHA-1")) //$NON-NLS-1$
		{
			return "SHA1"; //$NON-NLS-1$
		}

		if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#sha256".toUpperCase(Locale.US))  //$NON-NLS-1$
				|| "2.16.840.1.101.3.4.2.1".equals(upperPseudoName) //$NON-NLS-1$
				|| upperPseudoName.startsWith("SHA256") //$NON-NLS-1$
				|| upperPseudoName.startsWith("SHA-256")) { //$NON-NLS-1$
			return "SHA-256"; //$NON-NLS-1$
		}

		if (upperPseudoName.startsWith("SHA384") //$NON-NLS-1$
				|| "2.16.840.1.101.3.4.2.2".equals(upperPseudoName) //$NON-NLS-1$
				|| upperPseudoName.startsWith("SHA-384")) { //$NON-NLS-1$
			return "SHA-384"; //$NON-NLS-1$
		}

		if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#sha512".toUpperCase(Locale.US))  //$NON-NLS-1$
				|| "2.16.840.1.101.3.4.2.3".equals(upperPseudoName) //$NON-NLS-1$
				|| upperPseudoName.startsWith("SHA512") //$NON-NLS-1$
				|| upperPseudoName.startsWith("SHA-512")) { //$NON-NLS-1$
			return "SHA-512"; //$NON-NLS-1$
		}

		if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#ripemd160".toUpperCase(Locale.US))  //$NON-NLS-1$
				|| upperPseudoName.startsWith("RIPEMD160") //$NON-NLS-1$
				|| upperPseudoName.startsWith("RIPEMD-160")) { //$NON-NLS-1$
			return "RIPEMD160"; //$NON-NLS-1$
		}

		// Comprobamos si el nombre del algoritmo tiene un formato conocido de algoritmo de firma
		// e intentamos obtener el nombre del algoritmo de huella del mismo

		// Nombre estandar de algoritmo de firma
		if (pseudoName.contains("with")) { //$NON-NLS-1$
			final String subname = pseudoName.substring(0, pseudoName.indexOf("with")); //$NON-NLS-1$
			return getDigestAlgorithmName(subname);
		}
		if (pseudoName.startsWith("http://www.w3.org/2001/04/xmldsig-more#") //$NON-NLS-1$
				|| pseudoName.startsWith("http://www.w3.org/2000/09/xmldsig#") //$NON-NLS-1$
				|| pseudoName.startsWith("http://www.w3.org/2009/xmldsig11#")) { //$NON-NLS-1$
			final String subname = pseudoName.substring(pseudoName.lastIndexOf('-') + 1);
			return getDigestAlgorithmName(subname);
		}

		// No se ha podido extraer el nombre del algoritmo
		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
			"Algoritmo de huella desconocido, no se normalizara su nombre: " + pseudoName //$NON-NLS-1$
		);

		return pseudoName;
	}

	/**
	 * Compone el nombre del algoritmo de firma que utiliza un algoritmo de huella concreto y emplea un determinado tipo
	 * de clave de cifrado. El algoritmo de huella se extrae del algoritmo proporcionado, que puede ser de huella, de firma
	 * o una referencia al algoritmo (OID o URL).
	 * @param algorithm Algoritmo de huella digital o firma.
	 * @param keyType Tipo de clave de firma.
	 * @return Nombre del algoritmo de firma. */
	public static String composeSignatureAlgorithmName(final String algorithm, final String keyType) {
		if (algorithm == null) {
			throw new IllegalArgumentException(
					"El nombre del algoritmo no puede ser nulo"); //$NON-NLS-1$
		}
		if (keyType == null) {
			throw new IllegalArgumentException(
					"El tipo de clave de certificado no puede ser nulo"); //$NON-NLS-1$
		}

		// Limpiamos el nombre del algorithm de huella
		final String digestAlgorithm = getDigestAlgorithmName(algorithm).replace("-", ""); //$NON-NLS-1$ //$NON-NLS-2$

		// Agregamos el algoritmo de cifrado correspondiente al tipo de clave del certificado
		String suffix;
		if ("RSA".equals(keyType)) { //$NON-NLS-1$
			suffix = "withRSA"; //$NON-NLS-1$
		} else if ("DSA".equals(keyType)) { //$NON-NLS-1$
			suffix = "withDSA"; //$NON-NLS-1$
		} else if (keyType.startsWith("EC")) { //$NON-NLS-1$
			suffix = "withECDSA"; //$NON-NLS-1$
		} else {
			throw new IllegalArgumentException("Tipo de clave de firma no soportado: " + keyType); //$NON-NLS-1$
		}
		return digestAlgorithm + suffix;
	}

	/**
	 * Comprueba si un algoritmo de firma utiliza un algoritmo de huella digital
	 * perteneciente a la familia de algoritmos SHA-2.
	 * @param algorithm Algoritmo de firma.
	 * @return {@code true} cuando el algoritmo es un SHA-2, {@code false} en caso contrario.
	 */
	public static boolean isSHA2SignatureAlgorithm(final String algorithm) {
		return SIGN_ALGORITHM_SHA256WITHRSA.equals(algorithm)   ||
			   SIGN_ALGORITHM_SHA384WITHRSA.equals(algorithm)   ||
			   SIGN_ALGORITHM_SHA512WITHRSA.equals(algorithm)   ||
			   SIGN_ALGORITHM_SHA256WITHECDSA.equals(algorithm) ||
			   SIGN_ALGORITHM_SHA384WITHECDSA.equals(algorithm) ||
			   SIGN_ALGORITHM_SHA512WITHECDSA.equals(algorithm);
	}

	/**
	 * Comprueba si un algoritmo de firma utiliza el algoritmo de huella digital
	 * SHA-1.
	 * @param algorithm Algoritmo de firma.
	 * @return {@code true} cuando el algoritmo es SHA-1, {@code false} en caso contrario.
	 */
	public static boolean isSHA1SignatureAlgorithm(final String algorithm) {
		return SIGN_ALGORITHM_SHA1WITHRSA.equals(algorithm)   ||
			   SIGN_ALGORITHM_SHA1WITHDSA.equals(algorithm)   ||
			   SIGN_ALGORITHM_SHA1WITHECDSA.equals(algorithm);
	}

	/**
	 * Comprueba si un algoritmo de firma utiliza el cifrade ECDSA o DSA.
	 * @param algorithm Algoritmo de firma.
	 * @return {@code true} cuando el algoritmo usa cidrado ECDSA o DSA,
	 * {@code false} en caso contrario.
	 */
	public static boolean isDSAorECDSASignatureAlgorithm(final String algorithm) {
		return algorithm != null &&
				(algorithm.endsWith("withECDSA") //$NON-NLS-1$
						|| algorithm.endsWith("withECDSAinP1363Format") //$NON-NLS-1$
						|| algorithm.endsWith("withDSA") //$NON-NLS-1$
						|| algorithm.endsWith("withDSAinP1363Format")); //$NON-NLS-1$
	}
}
