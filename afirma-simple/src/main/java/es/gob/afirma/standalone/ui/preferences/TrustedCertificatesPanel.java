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
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.http.SslSecurityManager;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateUtils;

final class TrustedCertificatesPanel extends JPanel  {

	private static final long serialVersionUID = -3168095095548385291L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int PREFERRED_WIDTH = 420;
	private static final int PREFERRED_HEIGHT = 150;

	private JTable table;
	private DefaultTableModel model;
	private JButton importCertButton = null;
	private JButton viewCertButton = null;
	private JButton deleteCertButton = null;
	private JScrollPane certsScrollPane;
	private JPanel importedCertPanels;

	List<X509Certificate> savedCerts = new ArrayList<>();
	private boolean noCerts = true;
	private KeyStore ks;

	static final String TRUSTED_KS_PWD = "changeit"; //$NON-NLS-1$

	void createUI() {

		setLayout(new GridBagLayout());
		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 0.0;

		c.gridy = 0;

        this.importedCertPanels = new JPanel();
        this.importedCertPanels.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("TrustedCertificatesDialog.1") //$NON-NLS-1$
			)
		);

        createImportedCertsTable();
        this.importedCertPanels.add(this.certsScrollPane);

		c.gridy++;
		this.add(this.importedCertPanels, c);

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

		this.importCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.2")); //$NON-NLS-1$
		this.importCertButton.addActionListener(
				ae -> importCertificatesDlg(this)
			);

		buttonsPanel.add(this.importCertButton, c);

		this.viewCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.3")); //$NON-NLS-1$
		this.viewCertButton.addActionListener(
        		ae -> CertificateUtils.openCert(this, this.savedCerts.get(this.table.getSelectedRow()))
		);
		this.viewCertButton.setEnabled(false);

		c.gridx++;
		buttonsPanel.add(this.viewCertButton, c);

		this.deleteCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.5")); //$NON-NLS-1$
		this.deleteCertButton.addActionListener(
        		ae -> {
					deleteCert(this.savedCerts.get(this.table.getSelectedRow()));
				}
		);
		this.deleteCertButton.setEnabled(false);

		c.gridx++;
		buttonsPanel.add(this.deleteCertButton, c);

		if (this.noCerts == true) {
			this.viewCertButton.setEnabled(false);
			this.deleteCertButton.setEnabled(false);
		}

		return buttonsPanel;

	}

	private void createImportedCertsTable() {

		  final String[] columnNames = { "Nombre", "Emitido por", "Fecha de expiracion" };  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		  this.model = new DefaultTableModel(null, columnNames) {
			  /** Serial Id. */
			private static final long serialVersionUID = -3513927556747399446L;

			@Override
			  public boolean isCellEditable(final int row, final int column) {
				  return false;
			  }
		  };

		  final Object [] trustedCerts = obtainTrustedCerts(this);

		  this.model.setRowCount(0);

		  if (trustedCerts != null) {
			  for (int i = 0; i < trustedCerts.length ; i++) {
				  this.model.addRow((Object[]) trustedCerts[i]);
				  this.noCerts = false;
			  }
		  }

		  this.table = new JTable(this.model);
		  this.table.getSelectionModel().addListSelectionListener(event -> {
			TrustedCertificatesPanel.this.viewCertButton.setEnabled(true);
			TrustedCertificatesPanel.this.deleteCertButton.setEnabled(true);
		  });

		  this.certsScrollPane = new JScrollPane();
		  this.certsScrollPane.setBounds(10, 11, 560, 227);
		  this.certsScrollPane.setViewportView(this.table);
		  this.certsScrollPane.setVisible(true);
	}

	/** Di&aacute;logo para importar los certificados de confianza.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public void importCertificatesDlg(final Container container) {
    	final ImportCertificatesDialog importCertDialog = new ImportCertificatesDialog(null);
    	importCertDialog.setVisible(true);
    	updateTableInfo();
    }

    /**
     * Obtiene los certificados que se encuentran en el almac&eacute;n de confianza de AutoFirma.
     * @param parent Contenedor padre.
     * @return Array con la informaci&oacute;n de los certificados.
     */
    private Object[] obtainTrustedCerts(final Container parent) {
    	final File trustedKSFile = new File(ImportCertificatesDialog.getTrustedCertKSPath());
    	Object [] result = null;
    	if (trustedKSFile.exists()) {
			try (InputStream trustedKSStream = new FileInputStream(trustedKSFile)) {
				final CertificateFactory certFactory = CertificateFactory.getInstance("X509"); //$NON-NLS-1$
				this.ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
				this.ks.load(trustedKSStream, TRUSTED_KS_PWD.toCharArray());
				result = new Object [this.ks.size()];
				final Enumeration<String> aliases = this.ks.aliases();
				for (int i = 0 ; i < result.length ; i++) {
					final Certificate cert = this.ks.getCertificate(aliases.nextElement());
					final X509Certificate x509cert = (X509Certificate) certFactory.generateCertificate(
																	new ByteArrayInputStream(cert.getEncoded())
																	);
					this.savedCerts.add(x509cert);
					final Object [] auxArray = {
												AOUtil.getCN(x509cert.getSubjectX500Principal().toString()),
												AOUtil.getCN(x509cert.getIssuerX500Principal().toString()),
												new SimpleDateFormat("dd-MM-yyyy").format(x509cert.getNotAfter()).toString() //$NON-NLS-1$
												};
					result[i] = auxArray;
				}
			} catch (final Exception e) {
				AOUIFactory.showErrorMessage(
						parent,
						SimpleAfirmaMessages.getString("TrustedCertificatesDialog.27"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e);
			}
    	}

    	return result;
    }

    /**
     * Elimina el certificado indicado por par&aacute;metro del almac&eacute;n de confianza de AutoFirma
     * @param x509Certificate Certificado a eliminar
     */
	private void deleteCert(final X509Certificate x509Certificate) {
		if (AOUIFactory.showConfirmDialog(this, SimpleAfirmaMessages.getString("TrustedCertificatesDialog.29"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
				JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {

			final String trustedCertKSPath = ImportCertificatesDialog.getTrustedCertKSPath();

			try (final OutputStream fos = new FileOutputStream(trustedCertKSPath)) {
				final String certAlias = this.ks.getCertificateAlias(x509Certificate);
				this.ks.deleteEntry(certAlias);
				this.ks.store(fos, ImportCertificatesDialog.TRUSTED_KS_PWD.toCharArray());
				// Actualizamos los Trust Managers al eliminar un certificado
				SslSecurityManager.configureTrustManagers();
				for (int i = 0; i < this.savedCerts.size(); i++) {
					if (certAlias.equals(this.savedCerts.get(i).getSubjectX500Principal().getName())) {
						this.savedCerts.remove(i);
						break;
					}
				}
				updateTableInfo();
			} catch (final Exception e) {
				AOUIFactory.showErrorMessage(this, SimpleAfirmaMessages.getString("TrustedCertificatesDialog.30"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE, e);
			}
		}
	}

	/**
	 * Actualiza los campos de la tabla de certificados.
	 */
	private void updateTableInfo() {
		  final Object [] trustedCerts = obtainTrustedCerts(this);

		  this.model.setRowCount(0);

		  if (trustedCerts != null) {
			  for (int i = 0; i < trustedCerts.length ; i++) {
				  this.model.addRow((Object[]) trustedCerts[i]);
				  this.noCerts = false;
			  }
		  }

		  this.table = new JTable(this.model);
		  this.table.getSelectionModel().addListSelectionListener(event -> {
			TrustedCertificatesPanel.this.viewCertButton.setEnabled(true);
			TrustedCertificatesPanel.this.deleteCertButton.setEnabled(true);
		  });
		  this.certsScrollPane.setViewportView(this.table);
	}

}