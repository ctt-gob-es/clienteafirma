/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLHandshakeException;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.UrlHttpManagerImpl;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class ImportCertificatesDialog extends JDialog {

	private static final long serialVersionUID = -3168095095548385291L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static final String TRUSTED_KS_PWD = "changeit"; //$NON-NLS-1$
	private KeyStore ks;

	ImportCertificatesDialog(final JFrame parent) {
		super(parent, true);
		createUI();
	}

	void createUI() {

		setTitle(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.6")); //$NON-NLS-1$
		setIconImages(AutoFirmaUtil.getIconImages());
		setResizable(false);

		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setLayout(new GridBagLayout());
		//setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridy = 0;
		c.insets = new Insets(11,  11,  0,  11);

		final JLabel importDescLbl = new JLabel(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.7")); //$NON-NLS-1$
		this.add(importDescLbl, c);

		final JPanel importLocalCertPanel = createImportLocalCertUI();
        c.gridy++;
        this.add(importLocalCertPanel, c);


        final JPanel importRemoteCertPanel = createImportRemoteCertUI();
        c.gridy++;
        this.add(importRemoteCertPanel, c);

		final JButton closeDialogButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.28")); //$NON-NLS-1$
		closeDialogButton.addActionListener(e -> dispose());
		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.SOUTHEAST;
		c.insets = new Insets(11,  11,  11,  11);
        c.gridy++;
        this.add(closeDialogButton, c);

        pack();

        final Window ancestor = SwingUtilities.getWindowAncestor(this);
		setLocationRelativeTo(ancestor);
	}

	private JPanel createImportLocalCertUI() {
		final JPanel importLocalCertPanel = new JPanel();
        importLocalCertPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("TrustedCertificatesDialog.6") //$NON-NLS-1$
			)
		);
        importLocalCertPanel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridx = 0;
		c.insets = new Insets(5,  5,  5,  5);

		final JLabel importLocalCertDesc = new JLabel(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.9")); //$NON-NLS-1$
		importLocalCertPanel.add(importLocalCertDesc, c);

		final JButton fileButton= new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.13")); //$NON-NLS-1$
		fileButton.addActionListener(
				ae -> downloadLocalCert(this)
			);
		c.weightx = 0.0;
		c.gridx++;
		importLocalCertPanel.add(fileButton, c);

		return importLocalCertPanel;
	}

	private JPanel createImportRemoteCertUI() {
		final JPanel importRemoteCertPanel = new JPanel();
        importRemoteCertPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("TrustedCertificatesDialog.10") //$NON-NLS-1$
			)
		);
        importRemoteCertPanel.setLayout(new GridBagLayout());
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridy = 0;
		c.gridx = 0;
		c.gridwidth = 2;
		c.insets = new Insets(5,  5,  0,  5);

		final JLabel importTrustedCertbl = new JLabel(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.11")); //$NON-NLS-1$
		importRemoteCertPanel.add(importTrustedCertbl, c);

		c.gridy++;
		c.gridwidth = 1;
		c.insets = new Insets(0,  5,  5,  5);

		final JTextField domainTxt = new JTextField();
		importRemoteCertPanel.add(domainTxt, c);

		c.weightx = 0.0;
		c.gridx++;

		final JButton obtainCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.12")); //$NON-NLS-1$
		obtainCertButton.addActionListener(
				ae -> downloadRemoteCert(this, domainTxt)
		);
		obtainCertButton.setEnabled(false);
		importRemoteCertPanel.add(obtainCertButton, c);

		domainTxt.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void insertUpdate(final DocumentEvent e) {
				if (domainTxt.getText().isEmpty()) {
					obtainCertButton.setEnabled(false);
				} else {
					obtainCertButton.setEnabled(true);
				}
			}
			@Override
			public void removeUpdate(final DocumentEvent e) {
				if (domainTxt.getText().isEmpty()) {
					obtainCertButton.setEnabled(false);
				} else {
					obtainCertButton.setEnabled(true);
				}
			}
			@Override
			public void changedUpdate(final DocumentEvent e) {
				if (domainTxt.getText().isEmpty()) {
					obtainCertButton.setEnabled(false);
				} else {
					obtainCertButton.setEnabled(true);
				}
			}
		});

		return importRemoteCertPanel;
	}

	private void downloadRemoteCert(final Container container, final JTextField domainTxt) {

		final File trustedKSFile = new File(getTrustedCertKSPath());
		if (!trustedKSFile.exists()) {
			try {
				createTrustedKeystore(trustedKSFile, this);
			} catch (final IOException e) {
				AOUIFactory.showErrorMessage(
						this,
						SimpleAfirmaMessages.getString("TrustedCertificatesDialog.26"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e);
				return;
			}
		}

		try {
			final X509Certificate [] x509certs = downloadFromRemoteServer(trustedKSFile, domainTxt.getText(), false);
	    	final ConfirmImportCertDialog confirmImportCertDialog = new ConfirmImportCertDialog(x509certs, this.ks, this, false);
	    	confirmImportCertDialog.setVisible(true);
	    	this.setVisible(false);
		} catch (final SSLHandshakeException sslhe) {
			try {
				final X509Certificate [] x509certs = downloadFromRemoteServer(trustedKSFile, domainTxt.getText(), true);
				final ConfirmImportCertDialog confirmImportCertDialog = new ConfirmImportCertDialog(x509certs, this.ks, this, false);
				confirmImportCertDialog.setVisible(true);
				this.setVisible(false);
			} catch (final Exception e) {
				AOUIFactory.showErrorMessage(
						container,
						SimpleAfirmaMessages.getString("TrustedCertificatesDialog.27"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e);
			}
		} catch (final Exception e) {
			AOUIFactory.showErrorMessage(
					container,
					SimpleAfirmaMessages.getString("TrustedCertificatesDialog.27"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}
	}

	private void downloadLocalCert(final Container container) {

		final File [] certFiles;
		try {
			certFiles = AOUIFactory.getLoadFiles(
				SimpleAfirmaMessages.getString("TrustedCertificatesDialog.20"), //$NON-NLS-1$
				null,
				null,
				new String[] { "cer", "cert", "crt" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				SimpleAfirmaMessages.getString("TrustedCertificatesDialog.21"), //$NON-NLS-1$
				false,
				true,
				AutoFirmaUtil.getDefaultDialogsIcon(),
				this
			);
		}
		catch(final AOCancelledOperationException e) {
			return;
		}

		final File trustedKSFile = new File(getTrustedCertKSPath());
		if (!trustedKSFile.exists()) {
			try {
				createTrustedKeystore(trustedKSFile, this);
			} catch (final IOException e) {
				AOUIFactory.showErrorMessage(
						container,
						SimpleAfirmaMessages.getString("TrustedCertificatesDialog.26"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e);
				return;
			}
		}

		try (InputStream trustedKSStream = new FileInputStream(trustedKSFile)) {
			if (this.ks == null) {
				this.ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
			}
			this.ks.load(trustedKSStream, TRUSTED_KS_PWD.toCharArray());

			final CertificateFactory certFactory = CertificateFactory.getInstance("X509"); //$NON-NLS-1$
			final X509Certificate [] certsToImport = new X509Certificate[certFiles.length];
			for (int i = 0 ; i < certFiles.length ; i++) {
				try (InputStream fis = new FileInputStream(certFiles[i])) {
					certsToImport[i] = (X509Certificate) certFactory.generateCertificate(fis);
				}
			}
	    	final ConfirmImportCertDialog comfirmImportCertDialog = new ConfirmImportCertDialog(certsToImport, this.ks, this, true);
	    	comfirmImportCertDialog.setVisible(true);
	    	this.setVisible(false);
		} catch (final Exception e) {
			AOUIFactory.showErrorMessage(
					container,
					SimpleAfirmaMessages.getString("TrustedCertificatesDialog.27"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}
	}

	static String getTrustedCertKSPath() {
		return Platform.getUserHome() + File.separator + ".afirma" + File.separator + "TrustedCertsKeystore.jks"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private void createTrustedKeystore(final File trustedKSFile, final Container parent) throws IOException {

		trustedKSFile.createNewFile();

		try (final OutputStream bos = new BufferedOutputStream(new FileOutputStream(trustedKSFile))) {

			this.ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
			this.ks.load(null, TRUSTED_KS_PWD.toCharArray());
    		this.ks.store(bos, TRUSTED_KS_PWD.toCharArray());

		} catch (final Exception e) {
			AOUIFactory.showErrorMessage(
					parent,
					SimpleAfirmaMessages.getString("TrustedCertificatesDialog.26"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}
	}

	private X509Certificate [] downloadFromRemoteServer(final File trustedKSFile, final String domainName, final boolean disableSSL) throws FileNotFoundException, IOException, KeyManagementException, NoSuchAlgorithmException, CertificateException, KeyStoreException {

		X509Certificate [] x509certs = null;

		try (InputStream trustedKSStream = new FileInputStream(trustedKSFile)) {

			final CertificateFactory certFactory = CertificateFactory.getInstance("X509"); //$NON-NLS-1$
			final URL url = new URL(domainName);
			if (disableSSL) {
				UrlHttpManagerImpl.disableSslChecks();
			}
			final HttpsURLConnection conn = (HttpsURLConnection) url.openConnection();
			conn.connect();
			final Certificate [] trustedServerCerts = conn.getServerCertificates();
			conn.disconnect();
			if (disableSSL) {
				UrlHttpManagerImpl.enableSslChecks();
			}
			if (this.ks == null) {
				this.ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
			}
			this.ks.load(trustedKSStream, TRUSTED_KS_PWD.toCharArray());
			if (trustedServerCerts.length > 1) {
				// Solo se obtienen los certificados emisores
				x509certs = new X509Certificate [trustedServerCerts.length - 1];
				for (int i = 1 ; i < trustedServerCerts.length ; i++) {
					final X509Certificate cert = (X509Certificate) certFactory.generateCertificate(
																	new ByteArrayInputStream(trustedServerCerts[i].getEncoded())
																	);
					x509certs[i - 1] = cert;
				}
			} else {
				x509certs = new X509Certificate [1];
				final X509Certificate cert = (X509Certificate) certFactory.generateCertificate(
						new ByteArrayInputStream(trustedServerCerts[0].getEncoded())
						);
				x509certs[0] = cert;
			}

		}


		return x509certs;
	}
}