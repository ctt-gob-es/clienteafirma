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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class TrustedCertificatesPanel extends JPanel  {

	private static final long serialVersionUID = -3168095095548385291L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int PREFERRED_WIDTH = 420;
	private static final int PREFERRED_HEIGHT = 150;

	private JTable table;
	private DefaultTableModel model;

	static final String TRUSTED_KS_PWD = "changeit"; //$NON-NLS-1$

	void createUI() {

		setLayout(new GridBagLayout());
		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 0.0;

		c.gridy = 0;

        final JPanel importedCertPanels = new JPanel();
        importedCertPanels.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("TrustedCertificatesDialog.1") //$NON-NLS-1$
			)
		);

        final JScrollPane certsScrollPane = createImportedCertsTable();
        importedCertPanels.add(certsScrollPane);

		c.gridy++;
		this.add(importedCertPanels, c);

		c.gridy++;
		this.add(createButtonsPanel(), c);
	}


	TrustedCertificatesPanel() {
	    createUI();
	}

	private JPanel createButtonsPanel() {

		final JPanel buttonsPanel = new JPanel();
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;

		final JButton importCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.2")); //$NON-NLS-1$
		importCertButton.addActionListener(
				ae -> importCertificatesDlg(this)
			);

		buttonsPanel.add(importCertButton, c);

		final JButton viewCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.3")); //$NON-NLS-1$

		c.gridx++;
		buttonsPanel.add(viewCertButton, c);

		final JButton deleteCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.5")); //$NON-NLS-1$

		c.gridx++;
		buttonsPanel.add(deleteCertButton, c);

		return buttonsPanel;

	}

	private JScrollPane createImportedCertsTable () {

		  final JScrollPane scrollPane = new JScrollPane();
		  scrollPane.setBounds(10, 11, 560, 227);

		  final String[] columnNames = { "Nombre", "Emitido por", "Fecha de expiracion" };  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		  this.model = new DefaultTableModel(null, columnNames);

		  final Object [] trustedCerts = obtainTrustedCerts(this);

		  if (trustedCerts != null) {
			  for (int i = 0; i < trustedCerts.length ; i++) {
				  this.model.addRow((Object[]) trustedCerts[i]);
			  }
		  }

		  this.table = new JTable(this.model);
		  scrollPane.setViewportView(this.table);

		  return scrollPane;
	}

	/** Di&aacute;logo para importar los certificados de confianza.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public static void importCertificatesDlg(final Container container) {

    	final ImportCertificatesDialog importCertDialog = new ImportCertificatesDialog(null);
    	importCertDialog.setVisible(true);
    }

    private static Object[] obtainTrustedCerts(final Container parent) {
    	final File trustedKSFile = new File(ImportCertificatesDialog.getTrustedCertKSPath());
    	Object [] result = null;
    	if (trustedKSFile.exists()) {
			try (InputStream trustedKSStream = new FileInputStream(trustedKSFile)) {
				final CertificateFactory certFactory = CertificateFactory.getInstance("X509"); //$NON-NLS-1$
				final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
				ks.load(trustedKSStream, TRUSTED_KS_PWD.toCharArray());
				result = new Object [ks.size()];
				final Enumeration<String> aliases = ks.aliases();
				for (int i = 0 ; i < result.length ; i++) {
					final Certificate cert = ks.getCertificate(aliases.nextElement());
					final X509Certificate x509cert = (X509Certificate) certFactory.generateCertificate(
																	new ByteArrayInputStream(cert.getEncoded())
																	);
					final Object [] auxArray = {
												x509cert.getSubjectDN(),
												x509cert.getIssuerDN(),
												new SimpleDateFormat("dd-MM-yyyy").format(x509cert.getNotAfter()).toString() //$NON-NLS-1$
												};
					result[i] = auxArray;
				}
			} catch (final Exception e) {
				AOUIFactory.showErrorMessage(
						parent,
						SimpleAfirmaMessages.getString("TrustedCertificatesDialog.27"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e);
			}
    	}

    	return result;
    }

}