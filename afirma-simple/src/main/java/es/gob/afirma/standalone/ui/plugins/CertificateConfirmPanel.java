package es.gob.afirma.standalone.ui.plugins;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class CertificateConfirmPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = -1501351782356550463L;

	public CertificateConfirmPanel(final X509Certificate[][] certChains) {

		setLayout(new GridBagLayout());
		setAlignmentY(Component.TOP_ALIGNMENT);

		final JLabel signersTileLabel = new JLabel(SimpleAfirmaMessages.getString("CertificateConfirmPanel.0")); //$NON-NLS-1$

		final List<JLabel> cnLabels = new ArrayList<>();
		final List<JButton> downloadButtons = new ArrayList<>();
		for (final X509Certificate[] certChain : certChains) {
			final String cn = AOUtil.getCN(certChain[0]);
			final boolean expired = certChain[0].getNotAfter().before(new Date());
			final JLabel cnLabel = new JLabel();
			if (expired) {
				cnLabel.setText(" - " + cn + "(" + SimpleAfirmaMessages.getString("CertificateConfirmPanel.9") + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			} else {
				cnLabel.setText(" - " + cn); //$NON-NLS-1$
			}
			cnLabels.add(cnLabel);
			final JButton downloadButton = new JButton(SimpleAfirmaMessages.getString("CertificateConfirmPanel.1")); //$NON-NLS-1$
			downloadButton.setIcon(new ImageIcon(
					this.getClass().getResource("/resources/certificate_16.png")));  //$NON-NLS-1$
			if (expired) {
				downloadButton.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("CertificateConfirmPanel.10", cn)); //$NON-NLS-1$
			} else {
				downloadButton.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("CertificateConfirmPanel.2", cn)); //$NON-NLS-1$
			}
			downloadButton.addActionListener(new DownloadCertAction(certChain[0]));
			downloadButtons.add(downloadButton);
		}
		final JLabel questionLabel = new JLabel(SimpleAfirmaMessages.getString("CertificateConfirmPanel.3")); //$NON-NLS-1$

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridy = 0;
		add(signersTileLabel, c);
		c.insets = new Insets(5,  5,  5,  5);
		for (int i = 0; i < cnLabels.size(); i++) {
			c.gridx = 0;
			c.gridy++;
			c.weightx = 1.0;
			add(cnLabels.get(i), c);
			c.gridx = 1;
			c.weightx = 0.0;
			add(downloadButtons.get(i), c);
		}
		c.insets = new Insets(0,  0,  0,  0);
		c.gridx = 0;
		c.gridy++;
		add(questionLabel, c);
	}

	/**
	 * Acci&oacute;n que permite el guardado de un certificado.
	 */
	class DownloadCertAction implements ActionListener {

		private final X509Certificate cert;

		public DownloadCertAction(final X509Certificate cert) {
			this.cert = cert;
		}

		@Override
		public void actionPerformed(final ActionEvent e) {
			try {
				download();
			} catch (final AOCancelledOperationException ex) {
				return;
			} catch (final Exception ex) {
				JOptionPane.showMessageDialog(
						null,
						SimpleAfirmaMessages.getString("CertificateConfirmPanel.4"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CertificateConfirmPanel.5"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE);
			}
		}

		private void download()
				throws CertificateEncodingException, IOException {

	    	final String fDescription = SimpleAfirmaMessages.getString("CertificateConfirmPanel.6"); //$NON-NLS-1$
	    	final String[] fExtensions = new String[] { "cer" }; //$NON-NLS-1$

	    	AOUIFactory.getSaveDataToFile(
	    			this.cert.getEncoded(),
	    			SimpleAfirmaMessages.getString("CertificateConfirmPanel.7"), //$NON-NLS-1$
	    			null,
	    			SimpleAfirmaMessages.getString("CertificateConfirmPanel.8"), //$NON-NLS-1$
	    			Collections.singletonList(
	    					new GenericFileFilter(
	    							fExtensions,
	    							fDescription
	    							)
	    					),
	    			this);
		}
	}
}
