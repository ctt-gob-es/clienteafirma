/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades.common;

import java.util.Properties;

import es.gob.afirma.core.RuntimePasswordNeededException;

/**
 * Indica que el PDF no ha podido abrirse o firmarse por estar protegido por una contrase&ntilde;a distinta
 * a la proporcionada (si se proporcion&oacute; alguna).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class BadPdfPasswordException extends RuntimePasswordNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 8271382878128711677L;

	public static final String REQUESTOR_MSG_CODE = "pdfbadpassword"; //$NON-NLS-1$

	/**
	 * Crea una excepci&oacute;n que indica que el PDF no ha podido abrirse o firmarse
	 * por estar protegido por contrase&ntilde;a y la que se ha proporcionado no es correcta.
	 * @param msg Mesaje de error.
	 */
	public BadPdfPasswordException(final String msg) {
		super(msg, REQUESTOR_MSG_CODE, PdfExtraParams.USER_PASSWORD_STRING, null);
	}

	/**
	 * Crea una excepci&oacute;n que indica que el PDF no ha podido abrirse o firmarse por estar protegido por contrase&ntilde;a
     * y la que se ha proporcionado no es correcta.
	 * @param e Causa inicial de la excepci&oacute;n
	 */
	public BadPdfPasswordException(final Throwable e) {
		super("La contrasena del PDF no es correcta", REQUESTOR_MSG_CODE, PdfExtraParams.USER_PASSWORD_STRING, e); //$NON-NLS-1$
	}

	/**
	 * Crea una excepci&oacute;n que indica que el PDF no ha podido abrirse o firmarse por estar protegido por contrase&ntilde;a
     * y la que se ha proporcionado no es correcta.
     * @param msg Mensaje de error.
	 * @param e Causa inicial de la excepci&oacute;n
	 */
	public BadPdfPasswordException(final String msg, final Throwable e) {
		super(msg, REQUESTOR_MSG_CODE, PdfExtraParams.USER_PASSWORD_STRING, e);
	}

	@Override
	public void configure(final Properties config, final char[] password) {
		config.setProperty(PdfExtraParams.OWNER_PASSWORD_STRING, new String(password));
		config.remove(PdfExtraParams.USER_PASSWORD_STRING);
	}
}
