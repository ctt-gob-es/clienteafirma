/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.security.cert.X509Certificate;
import java.util.Date;

import es.gob.afirma.core.misc.AOUtil;

/** Informaci&oacute;n sobre un sello de tiempo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOTimestampInfo {

	private final X509Certificate issuer;
	private final Date date;

	/** Construye la informaci&oacute;n sobre un sello de tiempo.
	 * @param tsIssuer Certificado del emisor del sello de tiempo.
	 * @param tsDate Fecha del sello de tiempo. */
	public AOTimestampInfo(final X509Certificate tsIssuer, final Date tsDate) {
		this.issuer = tsIssuer;
		this.date = (Date) tsDate.clone();
	}

	/** Obtiene la fecha del sello de tiempo.
	 * @return Fecha del sello de tiempo. */
	public Date getDate() {
		return (Date) this.date.clone();
	}

	/** Obtiene el certificado del emisor del sello de tiempo.
	 * @return Certificado del emisor del sello de tiempo. */
	public X509Certificate getIssuer() {
		return this.issuer;
	}

	@Override
	public String toString() {
		return
			"Sello de tiempo emitido por '" + //$NON-NLS-1$
			(this.issuer != null ? AOUtil.getCN(this.issuer) : "DESCONOCIDO") + //$NON-NLS-1$
			"' con fecha " + this.date //$NON-NLS-1$
		;
	}

}
