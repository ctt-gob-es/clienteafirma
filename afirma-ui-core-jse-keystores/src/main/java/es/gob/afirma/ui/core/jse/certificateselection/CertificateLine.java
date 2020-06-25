package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Rectangle;
import java.security.cert.X509Certificate;

import javax.swing.JPanel;

public abstract class CertificateLine extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 1L;

	private final X509Certificate certificate;
	private final String friendlyName;

	CertificateLine(final String friendlyName, final X509Certificate certificate) {
		this.friendlyName = friendlyName;
		this.certificate = certificate;
	}

	abstract Rectangle getCertificateLinkBounds();

	X509Certificate getCertificate() {
		return this.certificate;
	}

	String getFriendlyName() {
		return this.friendlyName;
	}

	@Override
	public String toString() {
		return this.friendlyName;
	}
}
