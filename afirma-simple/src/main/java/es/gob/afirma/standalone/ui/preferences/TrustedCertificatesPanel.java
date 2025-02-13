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
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableModel;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.http.SslSecurityManager;
import es.gob.afirma.core.misc.http.TrustStoreManager;
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

		final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		this.importCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.2")); //$NON-NLS-1$
		this.importCertButton.addActionListener(
				ae -> importCertificatesDlg(this)
			);

		buttonsPanel.add(this.importCertButton);

		this.viewCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.3")); //$NON-NLS-1$
		this.viewCertButton.addActionListener(
        		ae -> CertificateUtils.openCert(this, this.savedCerts.get(this.table.getSelectedRow()))
		);
		this.viewCertButton.setEnabled(false);

		buttonsPanel.add(this.viewCertButton);

		this.deleteCertButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.5")); //$NON-NLS-1$
		this.deleteCertButton.addActionListener(
        		ae -> {
					deleteCert(this.table.getSelectedRow());
				}
		);
		this.deleteCertButton.setEnabled(false);

		buttonsPanel.add(this.deleteCertButton);

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
		  this.table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		  this.table.getSelectionModel().addListSelectionListener(event -> {
			TrustedCertificatesPanel.this.viewCertButton.setEnabled(true);
			TrustedCertificatesPanel.this.deleteCertButton.setEnabled(true);
		  });
		  this.table.addMouseListener(new java.awt.event.MouseAdapter() {
		      @Override
			public void mouseClicked(final java.awt.event.MouseEvent e) {
		          if(e.getClickCount()==2){
		        	  CertificateUtils.openCert(TrustedCertificatesPanel.this.table, TrustedCertificatesPanel.this.savedCerts.get(TrustedCertificatesPanel.this.table.getSelectedRow()));
		          }
		      }
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
     * Obtiene los certificados que se encuentran en el almac&eacute;n de confianza de Autofirma.
     * @param parent Contenedor padre.
     * @return Array con la informaci&oacute;n de los certificados o {@code null} si no se
     * pudieron cargar los certitifcados.
     */
    private Object[] obtainTrustedCerts(final Container parent) {

    	// Vaciamos el listado y volvemos a cargar los certificados
    	this.savedCerts.clear();

    	Object [] result;
    	try {
    		final TrustStoreManager ts = TrustStoreManager.getInstance(parent);
    		final X509Certificate[] certs = ts.getCertificates();

    		result = new Object[certs.length];

    		for (int i = 0; i < certs.length; i++) {

    			final X509Certificate cert = certs[i];
    			this.savedCerts.add(cert);

    			final Object [] auxArray = {
    					AOUtil.getCN(cert.getSubjectX500Principal().toString()),
    					AOUtil.getCN(cert.getIssuerX500Principal().toString()),
    					new SimpleDateFormat("dd-MM-yyyy").format(cert.getNotAfter()).toString() //$NON-NLS-1$
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
    		result = null;
    	}

    	return result;
    }

    /**
     * Elimina el certificado indicado por par&aacute;metro del almac&eacute;n de confianza de Autofirma
     * @param x509Certificate Certificado a eliminar
     */
    private void deleteCert(final int certIdx) {

    	final X509Certificate cert = this.savedCerts.get(certIdx);

    	if (AOUIFactory.showConfirmDialog(this, SimpleAfirmaMessages.getString("TrustedCertificatesDialog.29", AOUtil.getCN(cert.getSubjectX500Principal().toString())), //$NON-NLS-1$
    			SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
    			JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {

    		try {

    			LOGGER.info("Se elimina del almacen de confianza el certificado con el numero de serie: " + AOUtil.hexify(cert.getSerialNumber().toByteArray(), false)); //$NON-NLS-1$

    			TrustStoreManager.getInstance().deleteCert(cert);

    			// Actualizamos los Trust Managers al eliminar un certificado
    			SslSecurityManager.configureAfirmaTrustManagers();

    			this.savedCerts.remove(certIdx);

    		} catch (final Exception e) {
    			AOUIFactory.showErrorMessage(this, SimpleAfirmaMessages.getString("TrustedCertificatesDialog.30"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					JOptionPane.ERROR_MESSAGE, e);
    			return;
    		}

    		updateTableInfo();
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
			  final boolean elementsSelected = !this.table.getSelectionModel().isSelectionEmpty();
			  TrustedCertificatesPanel.this.viewCertButton.setEnabled(elementsSelected);
			  TrustedCertificatesPanel.this.deleteCertButton.setEnabled(elementsSelected);
		  });
		  this.certsScrollPane.setViewportView(this.table);
	}

}