/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

import org.spongycastle.cert.X509CertificateHolder;
import org.spongycastle.cms.SignerId;
import org.spongycastle.util.Selector;

public final class CertHolderBySignerIdSelector implements Selector<X509CertificateHolder> {

	private final SignerId signerId;
	public CertHolderBySignerIdSelector(final SignerId sid) {
		if (sid == null) {
			throw new IllegalArgumentException("El ID del firmante no puede ser nulo"); //$NON-NLS-1$
		}
		this.signerId = sid;
	}

	/** {@inheritDoc} */
	@Override
	public boolean match(final X509CertificateHolder o) {
		return CertHolderBySignerIdSelector.this.signerId.getSerialNumber().equals(o.getSerialNumber());
	}

	/** {@inheritDoc} */
	@Override
	public Object clone() {
		throw new UnsupportedOperationException();
	}

}
