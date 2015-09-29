package es.gob.afirma.cert.signvalidation;

import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cms.SignerId;
import org.bouncycastle.util.Selector;

final class CertHolderBySignerIdSelector implements Selector<X509CertificateHolder> {

	private final SignerId signerId;
	CertHolderBySignerIdSelector(final SignerId sid) {
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
