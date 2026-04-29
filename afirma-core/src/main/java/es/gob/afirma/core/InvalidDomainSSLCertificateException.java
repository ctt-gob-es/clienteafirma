/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Excepci&oacute;n lanzada cuando se detecta que el certificado del servidor no esta expedido para su propio dominio
 */
public class InvalidDomainSSLCertificateException extends IOException {

    private static final long serialVersionUID = 825249824660706387L;

    private final String url;

    /**
     * Crea la excepci&oacute;n a partir de otra excepci&oacute;n.
     * @param e excepci&oacute;n.
     * @param url URL del dominio que se comprob&oacute;.
     */
    public InvalidDomainSSLCertificateException(final IOException e, final String url) {
        super(e);
        this.url = url;
    }

    /**
     * Obtiene el nombre del host que provoca el error.
     * @return Nombre del host.
     */
	public String getHost() {
		URL url = null;
		try {
			url = new URL(this.url);
		} catch (final MalformedURLException e) {
			return ""; //$NON-NLS-1$
		}
		return url.getHost();
	}

}
