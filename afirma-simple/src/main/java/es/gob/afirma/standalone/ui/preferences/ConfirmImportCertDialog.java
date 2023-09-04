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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;

import es.gob.afirma.core.misc.http.UrlHttpManagerImpl;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateUtils;

final class ConfirmImportCertDialog extends JDialog  {

	private static final long serialVersionUID = -3168095095548385291L;

	private static final int PREFERRED_WIDTH = 600;
	private static final int PREFERRED_HEIGHT = 600;

	private final X509Certificate [] certsToImport;
	private final KeyStore ks;

	JButton openCertBtn;
	Integer rowSelected = null;

	ConfirmImportCertDialog(final X509Certificate [] certsToImport, final KeyStore ks, final Container parent) {
		this.certsToImport = certsToImport;
		this.ks = ks;
	    createUI(parent);
	}

	void createUI(final Container parent) {

		setModal(true);
		setTitle(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.6")); //$NON-NLS-1$
		setIconImages(AutoFirmaUtil.getIconImages());
		setResizable(false);
		pack();
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setLayout(new GridBagLayout());
		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.gridwidth = 2;
		c.weightx = 0.0;
		c.gridy = 0;

		final JLabel issuerDescLbl = new JLabel(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.15")); //$NON-NLS-1$
		this.add(issuerDescLbl, c);

		c.gridy++;

		final JScrollPane certsScrollPane = createLoadedCertsTable();
		this.add(certsScrollPane, c);

		c.gridy++;
		this.openCertBtn= new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.17")); //$NON-NLS-1$
		this.openCertBtn.addActionListener(
        		ae -> CertificateUtils.openCert(this, this.certsToImport[this.rowSelected])
		);
		this.openCertBtn.setEnabled(false);
		this.add(this.openCertBtn, c);

		c.gridy++;

		final JButton importCertBtn= new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.19")); //$NON-NLS-1$
		importCertBtn.addActionListener(
        		ae -> {
        			importCerts(this);
				}
		);
		this.add(importCertBtn, c);

		final JButton closeDialogButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.28")); //$NON-NLS-1$
		closeDialogButton.addActionListener(new ActionListener() {
		    @Override
		    public void actionPerformed(final ActionEvent e) {
		        dispose();
		    }
		});

        c.gridy++;
        this.add(closeDialogButton, c);
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
			UrlHttpManagerImpl.configureTrustManagers();
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

	private JScrollPane createLoadedCertsTable() {

		  final JScrollPane scrollPane = new JScrollPane();
		  scrollPane.setBounds(10, 11, 560, 227);

		  final String[] columnNames = { "Nombre", "Emitido por", "Fecha de expiracion" };  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		  final DefaultTableModel model = new DefaultTableModel(null, columnNames) {
			  @Override
			  public boolean isCellEditable(final int row, final int column) {
				  return false;
			  }
		  };

		  final Object [] loadedCerts = new Object [this.certsToImport.length];
		  for (int i = 0 ; i < this.certsToImport.length ; i++) {
				final Object [] auxArray = {
						this.certsToImport[i].getSubjectDN(),
						this.certsToImport[i].getIssuerDN(),
						new SimpleDateFormat("dd-MM-yyyy").format(this.certsToImport[i].getNotAfter()).toString() //$NON-NLS-1$
						};
				loadedCerts[i] = auxArray;
		  }

		  for (int i = 0; i < loadedCerts.length ; i++) {
			  model.addRow((Object[]) loadedCerts[i]);
		  }

		  final JTable table = new JTable(model);
		  table.getSelectionModel().addListSelectionListener(new ListSelectionListener(){
		        @Override
				public void valueChanged(final ListSelectionEvent event) {
		        	ConfirmImportCertDialog.this.rowSelected = table.getSelectedRow();
		        	ConfirmImportCertDialog.this.openCertBtn.setEnabled(true);
		        }
		    });
		  scrollPane.setViewportView(table);

		  return scrollPane;
	}

}