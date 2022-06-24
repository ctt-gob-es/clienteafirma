/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

import java.io.IOException;
import java.io.InputStream;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.misc.protocol.ConfirmationNeededException;
import es.gob.afirma.signers.pades.PdfExtraParams;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;

/** Validador de firmas PDF.
 * Se validan los certificados en local revisando si procede las fechas de validez de los certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class ValidatePdfSignature implements SignValider {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final PdfName PDFNAME_ETSI_RFC3161 = new PdfName("ETSI.RFC3161"); //$NON-NLS-1$
	private static final PdfName PDFNAME_DOCTIMESTAMP = new PdfName("DocTimeStamp"); //$NON-NLS-1$

	private static final String DEFAULT_PAGES_TO_CHECK_PSA = "10"; //$NON-NLS-1$

	/**
	 * Valida una firma PDF (PKCS#7/PAdES). En caso de validar los certificados de firma,
	 * s&oacute;lo se validar&aacute; el periodo de caducidad.
     * @param sign PDF firmado.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura del documento, si no se
     * encuentran firmas PDF en el documento o si se requiere m&aacute;s informacion para la validaci&oacute;n.
     */
	@Override
	public SignValidity validate(final byte[] sign) throws IOException {
		return validate(sign, true);
	}

	/**
	 * Valida una firma PDF (PKCS#7/PAdES). En caso de validar los certificados de firma,
	 * s&oacute;lo se validar&aacute; el periodo de caducidad.
     * @param sign PDF firmado.
     * @param checkCertificates Indica si debe comprobarse la caducidad de los certificados de firma.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura del documento, si no se
     * encuentran firmas PDF en el documento o si se requiere m&aacute;s informacion para la validaci&oacute;n.
     */
	@Override
	public SignValidity validate(final byte[] sign, final boolean checkCertificates) throws IOException {
		final Properties params = new Properties();
		params.setProperty(PdfExtraParams.CHECK_CERTIFICATES, Boolean.TRUE.toString());
		try {
			return validate(sign, params);
		} catch (final ConfirmationNeededException e) {
			throw new IOException("No se dispone de la informacion necesaria para completar la validacion", e); //$NON-NLS-1$
		}
	}

	/** Valida una firma PDF (PKCS#7/PAdES). En caso de validar los certificados de firma,
	 * s&oacute;lo se validar&aacute; el periodo de caducidad.
     * @param sign PDF firmado.
     * @param params Par&aacute;metros a tener en cuenta para la validaci&oacute;n.
     * @return Validez de la firma.
     * @throws ConfirmationNeededException Cuando para completar la validaci&oacute;n se necesita
     * que se proporcione m&aacute;s informaci&oacute;n.
     * @throws IOException Si ocurren problemas relacionados con la lectura del documento
     * o si no se encuentran firmas PDF en el documento. */
	@Override
	public SignValidity validate(final byte[] sign, final Properties params) throws ConfirmationNeededException, IOException {
		AcroFields af;
		try {
			final PdfReader reader = new PdfReader(sign);
			af = reader.getAcroFields();
		}
		catch (final Exception e) {
			return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_SIGN);
		}
		final List<String> signNames = af.getSignatureNames();


		// Si no hay firmas, no hay nada que comprobar
		if (signNames.size() == 0) {
			return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_SIGN);
		}

		for (final String name : signNames) {

			// Valimamos la firma
			final PdfPKCS7 pk = af.verifySignature(name);

			// Comprobamos que el algoritmo de hash este bien declarado, supliendo asi la flexibilidad de iText que permite
			// cargar firmas que usan algoritmos de firma como algoritmos de hash
			if (pk.getStrictHashAlgorithm() == null) {
				return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.ALGORITHM_NOT_SUPPORTED);
			}

    		// Comprobamos si es una firma o un sello
    		final PdfDictionary pdfDictionary = af.getSignatureDictionary(name);

    		// Si no es un sello, comprobamos el PKCS#1
    		if (!PDFNAME_ETSI_RFC3161.equals(pdfDictionary.get(PdfName.SUBFILTER)) && !PDFNAME_DOCTIMESTAMP.equals(pdfDictionary.get(PdfName.SUBFILTER))) {
				try {
					if (!pk.verify()) {
						return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_MATCH_DATA);
					}
				}
				catch (final Exception e) {
					LOGGER.warning("Error validando una de las firmas del PDF: " + e); //$NON-NLS-1$
					return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CORRUPTED_SIGN, e);
				}
    		}

    		final boolean checkCertificates = Boolean.parseBoolean(params.getProperty(PdfExtraParams.CHECK_CERTIFICATES, Boolean.TRUE.toString()));

    		if (checkCertificates) {
				final X509Certificate signCert = pk.getSigningCertificate();
				try {
					signCert.checkValidity();
				}
				catch (final CertificateExpiredException e) {
					// Certificado caducado
			        return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CERTIFICATE_EXPIRED, e);
				}
				catch (final CertificateNotYetValidException e) {
					// Certificado aun no valido
			        return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CERTIFICATE_NOT_VALID_YET, e);
				}
			}
		}

		final String allowShadowAttackProp = params.getProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK);
		final boolean allowPdfShadowAttack = Boolean.parseBoolean(allowShadowAttackProp);
		final String pagesToCheck =  params.getProperty(PdfExtraParams.PAGES_TO_CHECK_PSA, DEFAULT_PAGES_TO_CHECK_PSA);

		// Si se debe comprobar si se ha producido un PDF Shadow Attack
		// (modificacion de un documento tras la firma), se encuentran varias
		// revisiones en el documento y hay al menos una posterior a la ultima
		// firma (la de la posicion 0), se comprueba si el documento ha sufrido
		// un PSA.
		if (!allowPdfShadowAttack && af.getTotalRevisions() > 1 && af.getRevision(signNames.get(0)) < af.getTotalRevisions()) {
			// La revision firmada mas reciente se encuentra en el primer lugar de la lista, por ello se accede a la posicion 0
			try (final InputStream lastReviewStream = af.extractRevision(signNames.get(0))) {
				SignValidity validity = DataAnalizerUtil.checkPdfShadowAttack(sign, lastReviewStream, pagesToCheck);
				// Si se devolvio informacion de validez, la firma no es completamente valida
				if (validity != null) {
					// Se comprueba si se debe consultar al usuario y si se
					// cumplen los requisitos para ello
					if (validity.getValidity() == SignValidity.SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER
							&& allowShadowAttackProp == null) {
						throw new SuspectedPSAException("ProtocolInvocationError.PSA"); //$NON-NLS-1$
					}
					// Si habia que consultar y no se cumplen los requisitos,
					// se considera que la firma no es valida
					if (validity.getValidity() == SignValidity.SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER) {
						validity = new SignValidity(SIGN_DETAIL_TYPE.KO, validity.getError());
					}
					return validity;
				}
			}
		}

		return new SignValidity(SIGN_DETAIL_TYPE.OK, null);
	}

}
