/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.padestri.client;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Arrays;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pades.common.BadPdfPasswordException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pades.common.PdfFormModifiedException;
import es.gob.afirma.signers.pades.common.PdfHasUnregisteredSignaturesException;
import es.gob.afirma.signers.pades.common.PdfIsCertifiedException;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;
import es.gob.afirma.signers.pades.common.SuspectedPSAException;

/** Firmador PAdES en tres fases.
 * Las firmas que genera no se etiquetan como ETSI, sino como "Adobe PKCS#7 Detached".
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class AOPDFTriPhaseSigner implements AOSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	private static final String PDF_FILE_HEADER = "%PDF-"; //$NON-NLS-1$
	private static final String PDF_FILE_SUFFIX = ".pdf"; //$NON-NLS-1$

	/** Prefijo del mensaje de error del servicio de prefirma. */
	private static final String ERROR_PREFIX = "ERR-"; //$NON-NLS-1$
	/** Prefijo del mensaje de error cuando para completar la operaci&oacute;n se requiere intervenci&oacute;n del usuario. */
	private static final String CONFIG_NEEDED_ERROR_PREFIX = ERROR_PREFIX + "21:"; //$NON-NLS-1$

	/** Tama&ntilde;o m&iacute;nimo de un PDF.
	 * <a href="https://stackoverflow.com/questions/17279712/what-is-the-smallest-possible-valid-pdf">
	 * https://stackoverflow.com/questions/17279712/what-is-the-smallest-possible-valid-pdf
	 * </a>. */
	private static final int PDF_MIN_FILE_SIZE = 70;

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties xParams) throws AOException {

		if (xParams == null) {
			throw new IllegalArgumentException("Se necesitan parametros adicionales"); //$NON-NLS-1$
		}
		if (key == null) {
			throw new IllegalArgumentException("Es necesario proporcionar la clave privada de firma"); //$NON-NLS-1$
		}
		if (certChain == null || certChain.length == 0) {
			throw new IllegalArgumentException("Es necesario proporcionar el certificado de firma"); //$NON-NLS-1$
		}
		if (data == null) {
			throw new IllegalArgumentException("No se ha proporcionado el identificador de documento a firmar"); //$NON-NLS-1$
		}

		final Properties extraParams = getExtraParams(xParams);

		// Comprobamos la direccion del servidor
		final URL signServerUrl;
		try {
			signServerUrl = new URL(extraParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL));
		}
		catch (final Exception e) {
			throw new IllegalArgumentException("No se ha proporcionado una URL valida para el servidor de firma: " + extraParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL), e); //$NON-NLS-1$
		}

		// Decodificamos el identificador del documento
		final String documentId = Base64.encode(data, true);

		// ---------
		// PREFIRMA
		// ---------

		final byte[] preSignResult = PDFTriPhaseSignerUtil.doPresign(signServerUrl, algorithm, certChain, documentId, extraParams);

		// Comprobamos que no se trate de un error
		if (preSignResult.length > 8) {
			final String headMsg = new String(Arrays.copyOf(preSignResult, 8), StandardCharsets.UTF_8);
			if (headMsg.startsWith(ERROR_PREFIX)) {
				final String msg = new String(preSignResult, StandardCharsets.UTF_8);
				LOGGER.warning("Error durante la prefirma: " + msg); //$NON-NLS-1$
				throw buildInternalException(msg, extraParams);
			}
		}

		// ----------
		// FIRMA
		// ----------

		final String preResultAsBase64 = Base64.encode(
			PDFTriPhaseSignerUtil.doSign(
				preSignResult,
				algorithm,
				key,
				certChain,
				extraParams
			),
			true
		);

		// ---------
		// POSTFIRMA
		// ---------

		final byte[] postSignResult = PDFTriPhaseSignerUtil.doPostSign(
			preResultAsBase64,
			signServerUrl,
			algorithm,
			certChain,
			documentId,
			extraParams
		);

		if (postSignResult.length > 8) {
			final String headMsg = new String(Arrays.copyOf(postSignResult, 8), StandardCharsets.UTF_8);
			if (headMsg.startsWith(CONFIG_NEEDED_ERROR_PREFIX)) {
				final String msg = new String(postSignResult, StandardCharsets.UTF_8);
				LOGGER.warning("Error durante la postfirma: " + msg); //$NON-NLS-1$
				throw buildInternalException(msg, extraParams);
			}
		}

		return postSignResult;
	}

	@Override
	public byte[] cosign(final byte[] data,
			final byte[] sign,
			final String algorithm,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException {
		return sign(sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			final String algorithm,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException {
		return sign(sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			final String algorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException {
		throw new UnsupportedOperationException("No se soportan contrafirmas en PAdES"); //$NON-NLS-1$
	}
	
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo)
			throws AOInvalidFormatException, IOException {
		throw new UnsupportedOperationException("No soportado para firmas trifasicas"); //$NON-NLS-1$
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign,
			final boolean asSimpleSignInfo) {
		throw new UnsupportedOperationException("No soportado para firmas trifasicas"); //$NON-NLS-1$
	}
	
	@Override
	public boolean isSign(final byte[] sign, final Properties params){
		return false;
	}

	@Override
	public boolean isSign(final byte[] data) {
		return isSign(data, null);
	}

	@Override
	public boolean isValidDataFile(final byte[] data) {
		if (data == null) {
			LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
			return false;
		}
		return isPdfFile(data);
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		final String inTextInt = inText != null ? inText : ""; //$NON-NLS-1$
		if (originalName == null) {
			return "signed.pdf"; //$NON-NLS-1$
		}
		if (originalName.toLowerCase(Locale.ENGLISH).endsWith(PDF_FILE_SUFFIX)) {
			return originalName.substring(0, originalName.length() - PDF_FILE_SUFFIX.length()) + inTextInt + PDF_FILE_SUFFIX;
		}
		return originalName + inTextInt + PDF_FILE_SUFFIX;
	}
	
	@Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOInvalidFormatException, IOException, AOException {
		// Si no es una firma PDF valida, lanzamos una excepcion
		if (!isSign(sign)) {
			throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
		}
		return sign;
	}

	@Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException, IOException, AOException {
		return getData(sign, null);
	}
	
	@Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException, IOException {
		if (data == null) {
			throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
		}

		if (!isSign(data)) {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
		}

		return new AOSignInfo(AOSignConstants.SIGN_FORMAT_PDF);
		// Aqui podria venir el analisis de la firma buscando alguno de los
		// otros datos de relevancia que se almacenan en el objeto AOSignInfo
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException, IOException {
		return getSignInfo(data, null);
	}

	private static boolean isPdfFile(final byte[] data) {
		if (data == null || data.length < PDF_MIN_FILE_SIZE) {
			return false;
		}
		final byte[] buffer = new byte[PDF_FILE_HEADER.length()];
		try {
			new ByteArrayInputStream(data).read(buffer);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"El contenido parece corrupto o truncado: " + e //$NON-NLS-1$
			);
			return false;
		}
		// Comprobamos que cuente con una cabecera PDF
		if (!PDF_FILE_HEADER.equals(new String(buffer))) {
			return false;
		}
		return true;
	}

	/** Construye una excepci&oacute;n a partir del mensaje interno de error
	 * notificado por el servidor trif&aacute;sico.
	 * @param msg Mensaje de error devuelto por el servidor trif&aacute;sico.
	 * @param extraParams Configuraci&oacute;n aplicada en la operaci&oacute;n.
	 * @return Excepci&oacute;n construida.
	 */
	private static AOException buildInternalException(final String msg, final Properties extraParams) {

		AOException exception = null;
		final int separatorPos = msg.indexOf(":"); //$NON-NLS-1$
		if (msg.startsWith(CONFIG_NEEDED_ERROR_PREFIX)) {
			final int separatorPos2 = msg.indexOf(":", separatorPos + 1); //$NON-NLS-1$
			final String errorCode = msg.substring(separatorPos + 1, separatorPos2);
			final String errorMsg = msg.substring(separatorPos2 + 1);
			if (PdfIsCertifiedException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception = new PdfIsCertifiedException(errorMsg);
			}
			else if (PdfHasUnregisteredSignaturesException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception =  new PdfHasUnregisteredSignaturesException(errorMsg);
			}
			else if (PdfFormModifiedException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception =  new PdfFormModifiedException(errorMsg);
			}
			else if (SuspectedPSAException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception =  new SuspectedPSAException(errorMsg);
			}
			else if (PdfIsPasswordProtectedException.REQUESTOR_MSG_CODE.equals(errorCode)
					|| BadPdfPasswordException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				if (extraParams != null && (extraParams.containsKey(PdfExtraParams.OWNER_PASSWORD_STRING)
						|| extraParams.containsKey(PdfExtraParams.USER_PASSWORD_STRING))) {
					exception = new BadPdfPasswordException(errorMsg);
				}
				else {
					exception = new PdfIsPasswordProtectedException(errorMsg);
				}
			}
		}

		if (exception == null) {
			final int internalExceptionPos = msg.indexOf(":", separatorPos + 1); //$NON-NLS-1$
			if (internalExceptionPos > 0) {
				final String intMessage = msg.substring(internalExceptionPos + 1).trim();
				exception = AOTriphaseException.parseException(intMessage);
			}
			else {
				exception = new AOException(msg);
			}
		}

		return exception;
	}

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = (Properties) extraParams.clone();

    	return newExtraParams;
    }

}
