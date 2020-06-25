package es.gob.afirma.ui.core.jse.certificateselection;

import java.security.cert.X509Certificate;

public class CertificateLineFactory {

	private final CertificateLineView view;

	private CertificateLineFactory(final CertificateLineView view) {
		this.view = view;
	}

	public static CertificateLineFactory newInstance(final CertificateLineView view) {
		return new CertificateLineFactory(view);
	}

	public CertificateLine buildCertificateLine(final String friendlyName, final X509Certificate certificate) {

		CertificateLine line;
		switch (this.view) {
		case REPRESENTATIVE:
			line = new RepresentativeCertificateLine(friendlyName, certificate);
			break;
		case PSEUDONYM:
			line = new PseudonymCertificateLine(friendlyName, certificate);
			break;
		case PERSONAL:
		default:
			line = new DefaultCertificateLine(friendlyName, certificate);
			break;
		}
		line.setFocusable(true);

		return line;
	}
}
