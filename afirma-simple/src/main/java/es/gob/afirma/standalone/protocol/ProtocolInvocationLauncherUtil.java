/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Logger;

import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.misc.protocol.UrlParameters;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.crypto.CypherDataManager;
import es.gob.afirma.standalone.plugins.SignOperation.Operation;

final class ProtocolInvocationLauncherUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherUtil() {
		// No instanciable
	}

	static final class InvalidEncryptedDataLengthException extends Exception {

		private static final long serialVersionUID = 1L;

		InvalidEncryptedDataLengthException(final String msg) {
			super(msg);
		}
	}

	static final class DecryptionException extends Exception {

		private static final long serialVersionUID = 1L;

		DecryptionException(final String msg, final Throwable e) {
			super(msg, e);
		}
	}

	static byte[] getDataFromRetrieveServlet(final UrlParameters params) throws DecryptionException,
	                                                                                    InvalidEncryptedDataLengthException,
	                                                                                    IOException {
		// Preparamos la URL
		final StringBuilder dataUrl = new StringBuilder(
				params.getRetrieveServletUrl().toString()).
					append("?") //$NON-NLS-1$
						.append("op=get&v=1_0&id=") //$NON-NLS-1$
							.append(params.getFileId());

		LOGGER.info("Intentamos recuperar los datos del servidor con la URL:\n" + dataUrl.toString()); //$NON-NLS-1$

		// Leemos los datos
		final byte[] recoveredData = IntermediateServerUtil.retrieveData(params.getRetrieveServletUrl().toString(), params.getFileId());

		// Si los datos recibidos representan un error, detenemos la ejecucion
		if (recoveredData.length > 8 && new String(Arrays.copyOf(recoveredData, 8)).toLowerCase().startsWith("err-")) { //$NON-NLS-1$
			LOGGER.severe("Se recupera un error desde el servidor intermedio: " + new String(recoveredData)); //$NON-NLS-1$
			throw new InvalidEncryptedDataLengthException("Se recupera un error desde el servidor intermedio: " + new String(recoveredData)); //$NON-NLS-1$
		}

		// Si no ha ocurrido un error, debemos haber recibido los datos cifrados
		final byte[] data;
		try {
			data = CypherDataManager.decipherData(recoveredData, params.getDesKey());
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el descifrado de los datos: " + e); //$NON-NLS-1$
			throw new DecryptionException("Error en el descifrado de los datos: " + e, e); //$NON-NLS-1$
		}

		return data;
	}

	/**
	 * Construye la excepci&oacute;n interna notificada por una excepci&oacute;n
	 * en el servicio de firma trif&aacute;sica.
	 * @param ex Excepci&oacute;n del servicio de firma trif&aacute;sica.
	 * @return Excepci&oacute;n interna o, si no se pudo obtener, la misma
	 * excepci&oacute;n recibida por par&aacute;metro.
	 */
	static Exception getInternalException(final AOTriphaseException ex) {

		Class<Exception> exceptionClass;
		try {
			exceptionClass = (Class<Exception>) Class.forName(ex.getServerExceptionClassname(), false, ProtocolInvocationLauncherUtil.class.getClassLoader());
		}
		catch (final Throwable e) {
			LOGGER.warning("No se pudo identificar la excepcion enviada por el servidor (" + ex.getServerExceptionClassname() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return ex;
		}

		if (!exceptionClass.isAssignableFrom(RuntimeConfigNeededException.class)) {
			LOGGER.warning("La clase indicada para pedir confirmacion no es de tipo RuntimeConfigNeededException (" + ex.getServerExceptionClassname() + "): " + ex); //$NON-NLS-1$ //$NON-NLS-2$
			return ex;
		}

		// Buscamos algun constructor que nos permita generar la excepcion
		try {
			return exceptionClass.
					getDeclaredConstructor(String.class).
					newInstance(ex.getMessage());
		}
		catch (final Exception e) { /* No hacemos nada */}
		try {
			return exceptionClass.
					getDeclaredConstructor(String.class, Throwable.class).
					newInstance(ex.getMessage(), null);
		}
		catch (final Exception e) { /* No hacemos nada */}
		try {
			return exceptionClass.
					getDeclaredConstructor(Throwable.class).
					newInstance((Throwable) null);
		}
		catch (final Exception e) { /* No hacemos nada */}
		try {
			return exceptionClass.
					getDeclaredConstructor().
					newInstance();
		}
		catch (final Exception e) { /* No hacemos nada */}

		LOGGER.warning("No se encontro un constructor para reconstruir la excepcion del servidor"); //$NON-NLS-1$
		return ex;
	}

	/**
	 * Identifica el formato firma que debe generar a partir de los datos y el
	 * tipo de operaci&oacute;n. En caso de configurarse la operacion de firma,
	 * habremos recibido simples datos y seleccionaremos segun su formato (por
	 * defecto, CAdES). En caso contrario, la operaci&oacute;n ser&aacute;
	 * cofirma o contrafirma, habremos recibido una firma y usaremos el mismo
	 * formato que tenga esta.
	 * @param data Datos a firmar o firma a multifirmar.
	 * @param cryptoOperation Operaci&oacute;n que debe realizarse (firma, cofirma o contrafirma).
	 * @return Formato de firma o {@code null} si no se determin&oacute; un formato.
	 */
	static String identifyFormatFromData(final byte[] data, final Operation cryptoOperation) {

		String format;
		if (Operation.SIGN == cryptoOperation) {
			if (DataAnalizerUtil.isPDF(data)) {
				format = AOSignConstants.SIGN_FORMAT_PADES;
			}
			else if (DataAnalizerUtil.isFacturae(data)) {
				format = AOSignConstants.SIGN_FORMAT_FACTURAE;
			}
			else if (DataAnalizerUtil.isXML(data)) {
				format = AOSignConstants.SIGN_FORMAT_XADES;
			}
			else {
				format = AOSignConstants.SIGN_FORMAT_CADES;
			}
		}
		else {
			try {
				final AOSigner signer = AOSignerFactory.getSigner(data);
				format = AOSignerFactory.getSignFormat(signer);
			}
			catch (final IOException e) {
				LOGGER.severe(
						"No se han podido analizar los datos para determinar si son una firma: " + e); //$NON-NLS-1$
				format = null;
			}
		}

		return format;
	}
}
