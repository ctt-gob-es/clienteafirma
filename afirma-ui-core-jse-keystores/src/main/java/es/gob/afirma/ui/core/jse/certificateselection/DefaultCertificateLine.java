package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.font.TextAttribute;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

import es.gob.afirma.core.keystores.KeyUsage;
import es.gob.afirma.core.misc.AOUtil;

class DefaultCertificateLine extends CertificateLine {

	/** Serial Version */
	private static final long serialVersionUID = 5012625058876812352L;

	private static final String VERDANA_FONT_NAME = "Verdana"; //$NON-NLS-1$

	private static final Font SUBJECT_FONT = new Font(VERDANA_FONT_NAME, Font.BOLD, 14);
	private static final Font DETAILS_FONT = new Font(VERDANA_FONT_NAME, Font.PLAIN, 11);

	private JLabel alias = null;

	private JLabel issuer = null;

	private JLabel dates = null;

	private JLabel propertiesLink = null;

	/**
	 * Construye el panel con la informaci&oacute;n
	 * @param friendlyName Nombre legible con el que identificar el certificado.
	 * @param certificate Certificado del que se desea mostrar la informaci&oacute;n.
	 * @param highContrast Indica si el modo de alto contraste est&aacute; activo en el SO.
	 */
	DefaultCertificateLine(final String friendlyName, final X509Certificate certificate, final boolean highContrast) {
		super(friendlyName, certificate, highContrast);
		createUI();
	}

	void createUI() {

		setLayout(new GridBagLayout());

		setBackground(Color.WHITE);

		final GridBagConstraints c = new GridBagConstraints();
		c.gridx = 1;
		c.gridy = 1;
		c.gridheight = 4;

		final ImageIcon imageIcon = CertificateIconManager.getIcon(getCertificate());
		final JLabel icon = new JLabel(imageIcon);

		c.insets = new Insets(2, 2, 2, 5);
		add(icon, c);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridx++;
		c.gridheight = 1;
		c.insets = new Insets(5, 0, 0, 5);

		this.alias = new JLabel(getFriendlyName().getText());
		if (isHighContrast()) {
			this.alias.setForeground(Color.WHITE);
		} else {
			this.alias.setForeground(Color.BLACK);
		}
		this.alias.setFont(SUBJECT_FONT);
		add(this.alias, c);

		c.gridy++;
		c.insets = new Insets(0, 0, 0, 5);

		this.issuer = new JLabel(
				CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.2", //$NON-NLS-1$
						AOUtil.getCN(getCertificate().getIssuerX500Principal().getName()),
						new KeyUsage(getCertificate()).toString()
						)
		);
		if (isHighContrast()) {
			this.issuer.setForeground(Color.WHITE);
		} else {
			this.issuer.setForeground(Color.BLACK);
		}
		this.issuer.setFont(DETAILS_FONT);
		add(this.issuer, c);

		c.gridy++;

		this.dates = new JLabel(
			CertificateSelectionDialogMessages.getString(
					"CertificateSelectionPanel.3", //$NON-NLS-1$
					formatDate(getCertificate().getNotBefore()),
					formatDate(getCertificate().getNotAfter()))
		);
		if (isHighContrast()) {
			this.dates.setForeground(Color.WHITE);
		} else {
			this.dates.setForeground(Color.BLACK);
		}
		this.dates.setFont(DETAILS_FONT);
		add(this.dates, c);

		c.gridy++;

		this.propertiesLink = new JLabel(
    		CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.5") //$NON-NLS-1$
        );
		if (isHighContrast()) {
			this.propertiesLink.setForeground(Color.YELLOW);
		} else {
			this.propertiesLink.setForeground(Color.BLACK);
		}
    	Font font = this.propertiesLink.getFont();
    	Map attributes = font.getAttributes();
    	attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
    	this.propertiesLink.setFont(font.deriveFont(attributes));

    	font = DETAILS_FONT;
    	attributes = font.getAttributes();
    	attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
    	this.propertiesLink.setFont(font.deriveFont(attributes));
    	add(this.propertiesLink, c);

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

	public JLabel getAlias() {
		return this.alias;
	}

	public JLabel getIssuer() {
		return this.issuer;
	}

	public JLabel getDates() {
		return this.dates;
	}

	public JLabel getPropertiesLink() {
		return this.propertiesLink;
	}

}
