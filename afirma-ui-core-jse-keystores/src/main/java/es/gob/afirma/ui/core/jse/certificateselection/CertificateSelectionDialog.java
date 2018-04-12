/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Color;
import java.awt.Component;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.Comparator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.NameCertificateBean;
import es.gob.afirma.core.ui.KeyStoreDialogManager;

/** Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica similar al de Windows 7.
 * @author Carlos Gamuci
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateSelectionDialog extends MouseAdapter {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private final CertificateSelectionPanel csd;
	private final JOptionPane optionPane;
	private final Component parent;
	private final KeyStoreDialogManager ksdm;

	private final boolean disableSelection;

	/** Construye el di&aacute;logo de selecci&oacute;n de certificados a partir del listado con
	 * sus nombres y los propios certificados.
	 * @param parent Componente sobre el que se mostrar&aacute; el di&aacute;logo.
	 * @param ksdm Gestor del almacen de claves. */
	public CertificateSelectionDialog(final Component parent,
			                          final KeyStoreDialogManager ksdm) {
		this(parent, ksdm, null, null, true, false);
	}

	/** Construye el di&aacute;logo de selecci&oacute;n de certificados a partir del listado con
	 * sus nombres y los propios certificados.
	 * @param parent Componente sobre el que se mostrar&aacute; el di&aacute;logo.
	 * @param ksdm Gestor del almacen de claves.
	 * @param dialogHeadline Texto de la cabecera superior del di&aacute;logo. Si se indica <code>null</code> se usa
	 *                       el por defecto.
	 * @param dialogSubHeadline Texto de la cabecera inferior del di&aacute;logo. Si se indica <code>null</code> se usa
	 *                          el por defecto.
	 * @param showControlButons Si se indica <code>true</code> se muestran los botones de <i>refrescar</i>,
	 *                          <i>abrir</i> y <i>ayuda</i>, si se indica <code>false</code> no se muestra
	 *                          ninguno de los tres botones.
	 * @param disableCertificateSelection Si se indica <code>true</code> se deshabilita la opci&oacute;n de
	 *                                    selecci&oacute;n de certificado, dejando el di&aacute;logo solo para
	 *                                    consulta. */
	public CertificateSelectionDialog(final Component parent,
			                          final KeyStoreDialogManager ksdm,
					                  final String dialogHeadline,
					                  final String dialogSubHeadline,
						              final boolean showControlButons,
						              final boolean disableCertificateSelection) {

		this.parent = parent;
	    this.ksdm = ksdm;

	    final NameCertificateBean[] certs = this.ksdm.getNameCertificates();

	    Arrays.sort(certs, CERT_NAME_COMPARATOR);
	    this.csd = new CertificateSelectionPanel(
    		certs,
    		this,
    		dialogHeadline,
    		dialogSubHeadline,
    		showControlButons,
    		ksdm.isExternalStoresOpeningAllowed()
		);
		this.optionPane = certs.length > 1 ?
			new CertOptionPane(this.csd) :
				new JOptionPane();

		this.csd.addCertificateListMouseListener(this);

		this.optionPane.setMessage(this.csd);
		this.optionPane.setMessageType(JOptionPane.PLAIN_MESSAGE);
		this.optionPane.setOptionType(
			disableCertificateSelection ?
				JOptionPane.DEFAULT_OPTION :
					JOptionPane.OK_CANCEL_OPTION
		);

		this.disableSelection = disableCertificateSelection;
	}

	JDialog certDialog = null;

	/** Muestra el di&aacute;logo de selecci&oacute;n de certificados.
	 * @return Alias del certificado seleccionado o {@code null} si el usuario
	 * cancela el di&aacute;logo o cierra sin seleccionar. */
	public String showDialog() {

		this.certDialog = this.optionPane.createDialog(
			this.parent,
			CertificateSelectionDialogMessages.getString("CertificateSelectionDialog.0") //$NON-NLS-1$
		);

		this.certDialog.setBackground(Color.WHITE);
		this.certDialog.setModal(true);
		this.certDialog.setAlwaysOnTop(true);

		final KeyEventDispatcher dispatcher = new CertificateSelectionDispatcherListener(
			this.optionPane,
			this
		);

		KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(dispatcher);

		this.certDialog.setVisible(true);

		KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventDispatcher(dispatcher);

		// Si el usuario cancelo el dialogo, lo cerramos
		if (this.optionPane.getValue() == null || ((Integer) this.optionPane.getValue()).intValue() != JOptionPane.OK_OPTION) {
			this.certDialog.dispose();
			return null;
		}

		final String selectedAlias = this.csd.getSelectedCertificateAlias();

		this.certDialog.dispose();
		this.certDialog = null;

		return selectedAlias;
	}

	/** {@inheritDoc} */
	@Override
	public void mouseClicked(final MouseEvent me) {
		if (me.getClickCount() == 2 && this.optionPane != null && !this.disableSelection) {
			this.optionPane.setValue(Integer.valueOf(JOptionPane.OK_OPTION));
		}
	}

	private static final class CertOptionPane extends JOptionPane {

		private static final long serialVersionUID = 1L;

		private final CertificateSelectionPanel selectionPanel;

		CertOptionPane(final CertificateSelectionPanel csd) {
			this.selectionPanel = csd;
		}

		/** {@inheritDoc} */
		@Override
        public void selectInitialValue() {
			this.selectionPanel.selectCertificateList();
        }
	}

	/** Refresca el almacen de certificados y el di&aacute;logo de selecci&oacute;n. */
	public void refresh() {

		try {
			this.ksdm.refresh();
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error en la orden de actualizacion del almacen: " + e, e); //$NON-NLS-1$
			return;
		}

		refreshDialog();
	}

	/** Refresca el apartado gr&aacute;fico del di&aacute;logo de selecci&oacute;n. */
	private void refreshDialog() {

		final NameCertificateBean[] certs = this.ksdm.getNameCertificates();
		Arrays.sort(certs, CERT_NAME_COMPARATOR);

		try {
			this.csd.refresh(certs);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo actualizar el dialogo de seleccion: " + e); //$NON-NLS-1$
		}

		this.certDialog.pack();
//		// Cerramos el dialogo mostrado
//		closeDialog();
//
//		// Volvemos a mostrar el dialogo, que ya se habra redimensionado
//		// segun el nuevo contenido
//		showDialog();
	}

	/**
	 * Oculta el di&aacute;logo.
	 */
	private void closeDialog() {
		if (this.certDialog != null) {
			this.certDialog.dispose();
		}
	}

	/** Cambia el almac&eacute;n de claves actual.
	 * @param ksm Gestor de almacenes de claves. */
	public void changeKeyStore(final KeyStoreManager ksm) {
		this.ksdm.setKeyStoreManager(ksm);
		refreshDialog();
	}

	private static final Comparator<NameCertificateBean> CERT_NAME_COMPARATOR = new Comparator<NameCertificateBean>() {
		/** {@inheritDoc} */
		@Override
		public int compare(final NameCertificateBean o1, final NameCertificateBean o2) {
			if (o1 == null && o2 == null) {
				return 0;
			}
			else if (o1 == null) {
				return 1;
			}
			else if (o2 == null) {
				return -1;
			}
			else{
				return o1.getName().compareToIgnoreCase(o2.getName());
			}
		}
	};
}
