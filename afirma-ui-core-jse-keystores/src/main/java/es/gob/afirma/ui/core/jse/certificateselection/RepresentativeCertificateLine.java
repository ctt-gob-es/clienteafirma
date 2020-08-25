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

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

class RepresentativeCertificateLine extends CertificateLine {

	/** Serial Version */
	private static final long serialVersionUID = 5012625058876812352L;

	private static final String VERDANA_FONT_NAME = "Verdana"; //$NON-NLS-1$

	private static final Font SUBJECT_FONT = new Font(VERDANA_FONT_NAME, Font.BOLD, 14);
	private static final Font DETAILS_FONT = new Font(VERDANA_FONT_NAME, Font.PLAIN, 11);

	private JLabel propertiesLink = null;

	/**
	 * Construye el panel con la informaci&oacute;n
	 * @param friendlyName Nombre legible con el que identificar el certificado.
	 * @param certificate Certificado del que se desea mostrar la informaci&oacute;n.
	 */
	RepresentativeCertificateLine(final String friendlyName, final X509Certificate certificate) {
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

		final ImageIcon imageIcon = CertificateIconManager.getIcon(getCertificate());
		final JLabel icon = new JLabel(imageIcon);

		c.insets = new Insets(2, 2, 2, 5);
		add(icon, c);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridx++;
		c.gridheight = 1;
		c.insets = new Insets(5, 0, 0, 5);


		final PrincipalStructure subjectPrincipal = new PrincipalStructure(getCertificate().getSubjectX500Principal());

		final String organization = subjectPrincipal.getRDNvalue(PrincipalStructure.O);
		final JLabel organizationLabel = new JLabel();
		if (organization != null) {
			organizationLabel.setText(organization);
		}
		else {
			organizationLabel.setText(getFriendlyName());
		}
		organizationLabel.setFont(SUBJECT_FONT);
		add(organizationLabel, c);

		c.gridy++;
		c.insets = new Insets(0, 0, 0, 5);

		final String subjectCN = AOUtil.getCN(getCertificate());
		final JLabel agentLabel = new JLabel(CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.28", subjectCN)); //$NON-NLS-1$
		agentLabel.setFont(DETAILS_FONT);
		add(agentLabel, c);

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

		final String validityText = CertificateUtils.isExpired(getCertificate()) ?
				CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.31") : //$NON-NLS-1$
					CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.30"); //$NON-NLS-1$
		final String toolTipText = CertificateSelectionDialogMessages.getString("CertificateSelectionPanel.32", //$NON-NLS-1$
				new String[] { organizationLabel.getText(), subjectCN, validityText });
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
}
