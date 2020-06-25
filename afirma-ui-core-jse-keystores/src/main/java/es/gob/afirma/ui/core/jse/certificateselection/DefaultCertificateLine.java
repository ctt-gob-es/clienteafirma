package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

import es.gob.afirma.core.keystores.KeyUsage;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

class DefaultCertificateLine extends CertificateLine {

	/** Serial Version */
	private static final long serialVersionUID = 5012625058876812352L;

	private static final String VERDANA_FONT_NAME = "Verdana"; //$NON-NLS-1$

	private static final Font SUBJECT_FONT = new Font(VERDANA_FONT_NAME, Font.BOLD, 14);
	private static final Font DETAILS_FONT = new Font(VERDANA_FONT_NAME, Font.PLAIN, 11);

	private static final long EXPIRITY_WARNING_LEVEL = 1000*60*60*25*7;

	private JLabel propertiesLink = null;

	/**
	 * Construye el panel con la informaci&oacute;n
	 * @param friendlyName Nombre legible con el que identificar el certificado.
	 * @param certificate Certificado del que se desea mostrar la informaci&oacute;n.
	 */
	DefaultCertificateLine(final String friendlyName, final X509Certificate certificate) {
		super(friendlyName, certificate);
		createUI();
	}

	void createUI() {

		setLayout(new GridBagLayout());

		setBackground(Color.WHITE);

		final GridBagConstraints c = new GridBagConstraints();
		c.gridx = 1;
		c.gridy = 1;
		c.gridheight = 4;

		final ImageIcon imageIcon = getIcon(getCertificate());
		final JLabel icon = new JLabel(imageIcon);

		c.insets = new Insets(2, 2, 2, 5);
		add(icon, c);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridx++;
		c.gridheight = 1;
		c.insets = new Insets(5, 0, 0, 5);

		final JLabel alias = new JLabel(getFriendlyName());
		alias.setFont(SUBJECT_FONT);
		add(alias, c);

		c.gridy++;
		c.insets = new Insets(0, 0, 0, 5);

		final JLabel issuer = new JLabel(
				CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.2", //$NON-NLS-1$
						AOUtil.getCN(getCertificate().getIssuerDN().toString()),
						new KeyUsage(getCertificate()).toString()
						)
		);
		issuer.setFont(DETAILS_FONT);
		add(issuer, c);

		c.gridy++;

		final JLabel dates = new JLabel(
			CertificateSelectionDialogMessages.getString(
					"CertificateSelectionPanel.3", //$NON-NLS-1$
					formatDate(getCertificate().getNotBefore()),
					formatDate(getCertificate().getNotAfter()))
		);
		dates.setFont(DETAILS_FONT);
		add(dates, c);

		c.gridy++;

		this.propertiesLink = new JLabel(
	        "<html><u>" + //$NON-NLS-1$
    		CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.5") + //$NON-NLS-1$
	        "</u></html>" //$NON-NLS-1$
        );
		// Omitimos la muestra de detalles de certificados en OS X porque el SO en vez de mostrar los detalles
		// inicia su importacion
		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			this.propertiesLink.setFont(DETAILS_FONT);
			add(this.propertiesLink, c);
		}

		final String subjectCn = AOUtil.getCN(getCertificate());
		final String validityText = CertificateUtils.isExpired(getCertificate()) ?
				CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.31") : //$NON-NLS-1$
					CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.30"); //$NON-NLS-1$
		final String toolTipText = CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.29", //$NON-NLS-1$
				subjectCn, validityText);
		setToolTipText(toolTipText);
	}

	/** Devuelve la fecha con formato.
	 * @param date Fecha.
	 * @return Texto que representativo de la fecha. */
	private static String formatDate(final Date date) {
		return new SimpleDateFormat("dd/MM/yyyy").format(date); //$NON-NLS-1$
	}

	/** Recupera el rect&aacute;ngulo ocupado por el enlace para la carga del certificado.
	 * @return Recuadro con el enlace. */
	@Override
	Rectangle getCertificateLinkBounds() {
		return this.propertiesLink.getBounds();
	}

	private static ImageIcon getIcon(final X509Certificate cert) {
		final long notAfter = cert.getNotAfter().getTime();
		final long currentDate = new Date().getTime();
		if (currentDate >= notAfter || currentDate <= cert.getNotBefore().getTime()) {
			return CertificateIconManager.getExpiredIcon(cert);
		}
		if (notAfter - currentDate < EXPIRITY_WARNING_LEVEL) {
			return CertificateIconManager.getWarningIcon(cert);
		}
		return CertificateIconManager.getNormalIcon(cert);
	}
}
