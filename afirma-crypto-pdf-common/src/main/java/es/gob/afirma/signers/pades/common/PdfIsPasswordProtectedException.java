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
 * Indica que el PDF no ha podido abrirse o firmarse por estar protegido por una contrase&ntilde;a
 * y no proporcionarse ninguna.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class PdfIsPasswordProtectedException extends RuntimePasswordNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = -4899395533584256907L;

	public static final String REQUESTOR_MSG_CODE = "pdfpasswordprotected"; //$NON-NLS-1$

	/**
	 * Crea una excepci&oacute;n que indica que el PDF no ha podido abrirse o firmarse por estar protegido por una contrase&ntilde;a
     * y no se ha proporciondo ninguna.
     * @param msg Mensaje de error.
	 */
	public PdfIsPasswordProtectedException(final String msg) {
		super(msg, REQUESTOR_MSG_CODE, PdfExtraParams.OWNER_PASSWORD_STRING, null);
	}

	/**
	 * Crea una excepci&oacute;n que indica que el PDF no ha podido abrirse o firmarse por estar protegido por una contrase&ntilde;a
     * y no se ha proporciondo ninguna.
	 * @param e Causa inicial de la excepci&oacute;n         
	 */
	public PdfIsPasswordProtectedException(final Throwable e) {
		super("El PDF se encuentra protegido con contrasena",  //$NON-NLS-1$
				REQUESTOR_MSG_CODE, PdfExtraParams.USER_PASSWORD_STRING, e);
	}

	/**
	 * Crea una excepci&oacute;n que indica que el PDF no ha podido abrirse o firmarse por estar protegido por una contrase&ntilde;a
     * y no se ha proporciondo ninguna.
     * @param msg Mensaje de error.
	 * @param e Causa inicial de la excepci&oacute;n
	 */
	public PdfIsPasswordProtectedException(final String msg, final Throwable e) {
		super(msg, REQUESTOR_MSG_CODE, PdfExtraParams.OWNER_PASSWORD_STRING, e);
	}

	/**
	 * Agrega a la configuracion las propiedades con la contrase&ntilde;a recibida del usuario.
	 * @param config Configuraci&oacute;n de firma.
	 * @param password Contrase&tilde;a del documento.
	 */
	@Override
	public void configure(final Properties config, final char[] password) {
		config.setProperty(PdfExtraParams.OWNER_PASSWORD_STRING, new String(password));
		config.remove(PdfExtraParams.USER_PASSWORD_STRING);
	}
}
