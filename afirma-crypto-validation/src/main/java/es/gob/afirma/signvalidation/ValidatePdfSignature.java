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
import java.util.Map;
import java.util.logging.Logger;

import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.protocol.ConfirmationNeededException;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;

/** Validador de firmas PDF.
 * Se validan los certificados en local revisando si procede las fechas de validez de los certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class ValidatePdfSignature implements SignValider {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final PdfName PDFNAME_ETSI_RFC3161 = new PdfName("ETSI.RFC3161"); //$NON-NLS-1$
	private static final PdfName PDFNAME_DOCTIMESTAMP = new PdfName("DocTimeStamp"); //$NON-NLS-1$

	static final String PARAM_ALLOW_SHADOW_ATTACK = "allowShadowAttack"; //$NON-NLS-1$
	static final String PARAM_PAGES_TO_CHECK_SHADOW_ATTACK = "pagesToCheckShadowAttack"; //$NON-NLS-1$
	static final String PARAM_CHECK_CERTIFICATES = "checkCertificates"; //$NON-NLS-1$

	/** Valida una firma PDF (PKCS#7/PAdES).
	 * De los certificados de firma se revisan &uacute;nicamente las fechas de validez.
     * @param sign PDF firmado.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura del documento
     * o si no se encuentran firmas PDF en el documento. */
	@Override
	public SignValidity validate(final byte[] sign) throws IOException {
		return validate(sign, true);
	}

	/** Valida una firma PDF (PKCS#7/PAdES).
	 * De los certificados de firma se revisan &uacute;nicamente las fechas de validez.
     * @param sign PDF firmado.
     * @param checkCertificates Indica si debe comprobarse la caducidad de los certificados de firma.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura del documento
     * o si no se encuentran firmas PDF en el documento. */
	@Override
	public SignValidity validate(final byte[] sign, final boolean checkCertificates) throws IOException {
		return validate(sign, checkCertificates);
	}

	/** Valida una firma PDF (PKCS#7/PAdES).
	 * De los certificados de firma se revisan &uacute;nicamente las fechas de validez.
     * @param sign PDF firmado.
     * @param params Par&aacute;metros a tener en cuenta para la validaci&oacute;n.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura del documento
     * o si no se encuentran firmas PDF en el documento. */
	@Override
	public SignValidity validate(final byte[] sign, final Map<String, String> params) throws ConfirmationNeededException, IOException {
		AcroFields af;
		try {
			final PdfReader reader = new PdfReader(sign);
			af = reader.getAcroFields();
		}
		catch (final Exception e) {
			return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_SIGN);
		}
		final List<String> signNames = af.getSignatureNames();

		for (final String name : signNames) {

			final PdfPKCS7 pk = af.verifySignature(name);

    		// Comprobamos si es una firma o un sello
    		final PdfDictionary pdfDictionary = af.getSignatureDictionary(name);

    		// En los sellos no comprobamos el PKCS#1
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

    		final boolean checkCertificates = Boolean.parseBoolean(params.get(PARAM_CHECK_CERTIFICATES));

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

		final String allowShadowAttackProp = params.get(PARAM_ALLOW_SHADOW_ATTACK);
		final boolean allowPdfShadowAttack = Boolean.parseBoolean(allowShadowAttackProp);
		final String pagesToCheck = params.get(PARAM_PAGES_TO_CHECK_SHADOW_ATTACK);

		if (signNames.size() == 0) {
			return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_SIGN);
		}
		// Si se encuentran varias revisiones firmadas y no se permiten posibles PDF Shadow Attacks, comprobamos el documento firmado.
		else if (signNames.size() > 1 && !allowPdfShadowAttack && pagesToCheck != null) {
			// La revision firmada mas reciente se encuentra en el primer lugar de la lista, por ello se accede a la posicion 0
			try(final InputStream lastReviewStream = af.extractRevision(signNames.get(0))) {
				final byte [] lastReviewData = AOUtil.getDataFromInputStream(lastReviewStream);
				final SignValidity validity = DataAnalizerUtil.checkPdfShadowAttack(sign, lastReviewData, pagesToCheck);
				if (validity != null) {
					if (allowShadowAttackProp != null) {
						return validity;
					}
					throw new ConfirmationNeededException("ProtocolInvocationError.PSA"); //$NON-NLS-1$
				}
			}
		}

		return new SignValidity(SIGN_DETAIL_TYPE.OK, null);
	}

}
