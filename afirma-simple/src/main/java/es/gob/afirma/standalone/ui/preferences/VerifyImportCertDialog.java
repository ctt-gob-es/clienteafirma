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
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.WindowConstants;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateUtils;

final class VerifyImportCertDialog extends JDialog  {

	private static final long serialVersionUID = -3168095095548385291L;

	private static final int PREFERRED_WIDTH = 600;
	private static final int PREFERRED_HEIGHT = 300;

	private final X509Certificate [] certsToImport;
	private final KeyStore ks;
	private final String domain;

	VerifyImportCertDialog(final X509Certificate [] certsToImport, final KeyStore ks, final String domain, final Container parent) {
		this.certsToImport = certsToImport;
		this.domain = domain;
		this.ks = ks;
	    createUI(parent);
	}

	void createUI(final Container parent) {

		setModal(true);
		setTitle(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.22")); //$NON-NLS-1$
		setIconImages(AutoFirmaUtil.getIconImages());
		setResizable(false);
		pack();
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setLayout(new GridBagLayout());
		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 0.0;
		c.gridy = 0;

		final JLabel issuerDescLbl = new JLabel(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.23", this.domain)); //$NON-NLS-1$
		this.add(issuerDescLbl, c);

		c.gridy++;
		final JLabel verifyCertLbl = new JLabel(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.24")); //$NON-NLS-1$
		this.add(verifyCertLbl, c);

		c.gridx = 2;
		final JButton openCertBtn= new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.17")); //$NON-NLS-1$
		openCertBtn.addActionListener(
        		ae -> CertificateUtils.openCert(this, this.certsToImport[0])
		);
		this.add(openCertBtn, c);

        c.gridy++;
        c.gridx = 0;

		final JLabel importTrustedCertbl = new JLabel(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.25")); //$NON-NLS-1$
		this.add(importTrustedCertbl, c);

		c.gridx = 2;

		final JButton importCertBtn= new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.19")); //$NON-NLS-1$
		importCertBtn.addActionListener(
        		ae -> importCerts(this)
		);
		this.add(importCertBtn, c);
	}

	private void importCerts(final Container parent) {
		final String trustedCertKSPath = ImportCertificatesDialog.getTrustedCertKSPath();

		try (final OutputStream fos = new FileOutputStream(trustedCertKSPath)) {
			if (this.certsToImport.length == 1) {
				this.ks.setCertificateEntry(this.certsToImport[0].getSubjectDN().toString(), this.certsToImport[0]);
				this.ks.store(fos, ImportCertificatesDialog.TRUSTED_KS_PWD.toCharArray());
			} else {
				for (int i = 1; i < this.certsToImport.length; i++) {
					this.ks.setCertificateEntry(this.certsToImport[i].getSubjectDN().toString(), this.certsToImport[i]);
					this.ks.store(fos, ImportCertificatesDialog.TRUSTED_KS_PWD.toCharArray());
				}
			}
			this.setVisible(false);
		} catch (final Exception e) {
			AOUIFactory.showErrorMessage(
					parent,
					SimpleAfirmaMessages.getString("SignPanel.18"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}
	 }

}