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
import java.awt.Dimension;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.Comparator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

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
	private String currentKeyStoreTypeName;

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

	    this.currentKeyStoreTypeName = this.ksdm.getKeyStoreName();

	    final NameCertificateBean[] certs = this.ksdm.getNameCertificates();

	    Arrays.sort(certs, CERT_NAME_COMPARATOR);
	    this.csd = new CertificateSelectionPanel(
    		certs,
    		this,
    		dialogHeadline,
    		dialogSubHeadline,
    		showControlButons,
    		ksdm.isExternalStoresOpeningAllowed(),
    		this.ksdm.getAvailablesKeyStores()
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

	/**
	 * Muestra el di&aacute;logo de selecci&oacute;n de certificados.
	 * @return Alias del certificado seleccionado o {@code null} si el usuario
	 * cancela el di&aacute;logo o cierra sin seleccionar.
	 * @throws CertificatesNotFoundException Cuando no el usuario cierra el
	 * di&aacute;logo y no hab&iacute;a cargado ning&uacute;n certificado v&aacute;lido.
	 */
	public String showDialog() throws  CertificatesNotFoundException {
		return showDialog(true);
	}

	/**
	 * Muestra el di&aacute;logo de selecci&oacute;n de certificados.
	 * @param alwaysOnTop {@code true} si el di&aacute;logo se debe mantener por encima del resto
	 * de ventanas, {@code false} en caso contrario.
	 * @return Alias del certificado seleccionado o {@code null} si el usuario
	 * cancela el di&aacute;logo o cierra sin seleccionar.
	 * @throws CertificatesNotFoundException Cuando no el usuario cierra el
	 * di&aacute;logo y no hab&iacute;a cargado ning&uacute;n certificado v&aacute;lido.
	 */
	public String showDialog(final boolean alwaysOnTop) throws  CertificatesNotFoundException {

		String title = CertificateSelectionDialogMessages.getString(
				"CertificateSelectionDialog.0", this.currentKeyStoreTypeName); //$NON-NLS-1$

		LOGGER.info("Abrimos el dialogo de certificados para el almacen: " + this.currentKeyStoreTypeName); //$NON-NLS-1$

		if (this.ksdm.getLibName() != null) {
			title += " : " + this.ksdm.getLibName(); //$NON-NLS-1$
		}

		this.certDialog = this.optionPane.createDialog(
			this.parent,
			title
		);

		this.certDialog.setBackground(Color.WHITE);
		this.certDialog.setModal(true);
		this.certDialog.setAlwaysOnTop(alwaysOnTop);

		final KeyEventDispatcher dispatcher = new CertificateSelectionDispatcherListener(
			this.optionPane,
			this
		);

		KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(dispatcher);

		final Toolkit toolkit = Toolkit.getDefaultToolkit();
		if (toolkit != null) {
			final int screenHeight = (int) toolkit.getScreenSize().getHeight();
			if (this.certDialog.getSize().getHeight() > screenHeight) {
				this.certDialog.setSize(
						new Dimension((int) this.certDialog.getSize().getWidth(), screenHeight));
			}
		}
		this.certDialog.setVisible(true);

		KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventDispatcher(dispatcher);

		// Si en el dialogo mostrado, no habia ningun certificado, independientemente del modo de
		// cerrar el dialogo, consideramos que el problema es que el usuario no tiene certificados
		// validos
		if (this.csd.getShowedCertsCount() == 0) {
			throw new CertificatesNotFoundException("No habia certificados validos en el almacen del usuario"); //$NON-NLS-1$
		}

		// Si el usuario cancelo el dialogo, lo cerramos
		if (this.optionPane.getValue() == null || ((Integer) this.optionPane.getValue()).intValue() != JOptionPane.OK_OPTION) {
			this.certDialog.dispose();
			return null;
		}

		// Obtenemos el alias del certificado seleccionado
		final String selectedAlias = this.csd.getSelectedCertificateAlias();

		// Guardamos la vista seleccionada
		this.csd.savePreferredCertificateView();

		// Cerramos el dialogo
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

	/** Refresca el almacen de certificados y el di&aacute;logo de selecci&oacute;n. */
	public void refreshKeystore() {

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

		refreshDialog(certs);
	}

	/** Refresca el apartado gr&aacute;fico del di&aacute;logo de selecci&oacute;n
	 * mostrando los certificados indicados.
	 * @param certs Lista de certificados que se deben mostrar. */
	private void refreshDialog(final NameCertificateBean[] certs) {

		// Ya que al refrescarga el dialogo pueden aparecer otros nuevos (como alguno de solicitud de PIN),
		// dejamos de obligar a que este este siempre encima
		this.certDialog.setAlwaysOnTop(false);

		try {
			this.csd.refresh(certs);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo actualizar el dialogo de seleccion: " + e); //$NON-NLS-1$
		}
		this.certDialog.pack();
	}

	/** Refresca el apartado gr&aacute;fico del di&aacute;logo de selecci&oacute;n
	 * mostrando los certificados indicados con la vista seleccionada.
	 * @param certs Lista de certificados que se deben mostrar.
	 * @param view Vista de certificados que se debe aplicar. */
	public void refreshDialog(final NameCertificateBean[] certs, final CertificateLineView view) {

		this.csd.setCertLineView(view);

		refreshDialog(certs);
	}

	/** Cambia el almac&eacute;n de claves actual.
	 * @param ksType Tipo de almac&eacute;n de claves.
	 * @param ksName Nombre de almac&eacute;n de claves
	 * @param ksLibPath Librer&iacute;a de almac&eacute;n de claves.  */
	public void changeKeyStore(final int ksType, final String ksName, final String ksLibPath) {

		// Ya que el cambio de dialogo puede hacer aparecer otros nuevos (como alguno
		// de seleccion de fichero o de solicitud de PIN), dejamos de obligar a que
		// este este siempre encima
		this.certDialog.setAlwaysOnTop(false);

		final boolean changed;

		// Cambiamos de almacen
		if (ksLibPath == null || ksLibPath.isEmpty()) {
			changed = this.ksdm.changeKeyStoreManager(ksType, this.parent);
		} else {
			changed = this.ksdm.changeKeyStoreManagerToPKCS11(this.parent, ksName, ksLibPath);
		}

		// Si se ha completado el cambio de almacen, refrescamos el dialogo
		if (changed) {
			// Actualizamos el dialogo para cargar los nuevos certificados
			refreshDialog();

			// Cambiamos el titulo del dialogo
			this.currentKeyStoreTypeName = this.ksdm.getKeyStoreName();
			final String name = this.currentKeyStoreTypeName != null ? this.currentKeyStoreTypeName : ksName;
			this.certDialog.setTitle(CertificateSelectionDialogMessages.getString(
					"CertificateSelectionDialog.0", name)); //$NON-NLS-1$
		}
	}

	private static final Comparator<NameCertificateBean> CERT_NAME_COMPARATOR = new Comparator<NameCertificateBean>() {

		@Override
		public int compare(final NameCertificateBean o1, final NameCertificateBean o2) {
			if (o1 == null && o2 == null) {
				return 0;
			}
			if (o1 == null) {
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
}
