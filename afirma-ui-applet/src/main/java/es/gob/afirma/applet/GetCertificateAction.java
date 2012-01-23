/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.security.PrivilegedExceptionAction;
import java.security.cert.X509Certificate;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerException;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;

/**
 * Recupera un certificado de un almac&eacute;n de claves. Se deber&aacute; indicar
 * tanto el alias del certificado como la configuraci&oacute;n con el repositorio
 * activo.
 */
public class GetCertificateAction implements PrivilegedExceptionAction<X509Certificate> {

	private final String alias;

	private final KeyStoreConfigurationManager ksConfigManager;

	/**
	 * Construye la accui&oacute;n para la recuperaci&oacute;n del certificado de usuario.
	 * @param alias Alias del certificado.
	 * @param ksConfigManager Configuraci&oacute;n con el repositorio de certificados activo.
	 */
	public GetCertificateAction(final String alias, final KeyStoreConfigurationManager ksConfigManager) {
		this.alias = alias;
		this.ksConfigManager = ksConfigManager;
	}

	/** {@inheritDoc} */
	public X509Certificate run() throws AOKeyStoreManagerException, AOKeystoreAlternativeException {
		try {
            return (X509Certificate) this.ksConfigManager.getCertificate(this.alias);
        }
        catch (final AOCancelledOperationException e) {
            throw e;
        }
        catch (final AOKeyStoreManagerException e) {
        	throw e;
        }
        catch (final AOKeystoreAlternativeException e) {
        	throw e;
        }
	}
}
