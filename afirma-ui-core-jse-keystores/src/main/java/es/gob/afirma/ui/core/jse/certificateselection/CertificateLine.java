package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Rectangle;
import java.security.cert.X509Certificate;

import javax.swing.JLabel;
import javax.swing.JPanel;

public abstract class CertificateLine extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 1L;

	private final X509Certificate certificate;
	private final JLabel friendlyName;
	private boolean highContrast;

	CertificateLine(final String friendlyName, final X509Certificate certificate, final boolean highContrast) {
		this.friendlyName = new JLabel(friendlyName);
		this.certificate = certificate;
		this.highContrast = highContrast;
	}

	abstract Rectangle getCertificateLinkBounds();

	X509Certificate getCertificate() {
		return this.certificate;
	}

	JLabel getFriendlyName() {
		return this.friendlyName;
	}

	public boolean isHighContrast() {
		return this.highContrast;
	}

	public void setHighContrast(final boolean highContrast) {
		this.highContrast = highContrast;
	}

	@Override
	public String toString() {
		return this.friendlyName.getText();
	}
}
