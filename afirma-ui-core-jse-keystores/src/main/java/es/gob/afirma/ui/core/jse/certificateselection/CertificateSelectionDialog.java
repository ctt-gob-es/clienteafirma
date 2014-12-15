/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
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
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.gob.afirma.core.AOException;
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


	/** Construye el di&aacute;logo de selecci&oacute;n de certificados a partir del listado con
	 * sus nombres y los propios certificados.
	 * @param parent Componente sobre el que se mostrar&aacute; el di&aacute;logo.
	 * @param ksdm Gestor del almacen de claves. */
	public CertificateSelectionDialog(final Component parent, final KeyStoreDialogManager ksdm) {

		this.parent = parent;
	    this.ksdm = ksdm;

	    final NameCertificateBean[] certs = this.ksdm.getNameCertificates();

	    Arrays.sort(certs, CERT_NAME_COMPARATOR);
	    this.csd = new CertificateSelectionPanel(certs, this);
		this.optionPane = certs.length > 1 ?
				new CertOptionPane(this.csd) : new JOptionPane();

		this.csd.addCertificateListMouseListener(this);

		this.optionPane.setMessage(this.csd);
		this.optionPane.setMessageType(JOptionPane.PLAIN_MESSAGE);
		this.optionPane.setOptionType(JOptionPane.OK_CANCEL_OPTION);

	}

	/** Muestra el di&aacute;logo de selecci&oacute;n de certificados.
	 * @return Alias del certificado seleccionado o {@code null} si el usuario
	 * cancela el di&aacute;logo o cierra sin seleccionar.
	 * @throws AOException Cuando ocurre cualquier error durante el proceso. */
	public String showDialog() throws AOException {

		final JDialog certDialog = this.optionPane.createDialog(
			this.parent,
			CertificateSelectionDialogMessages.getString("CertificateSelectionDialog.0") //$NON-NLS-1$
		);

		certDialog.setBackground(Color.WHITE);
		certDialog.setModal(true);

		final KeyEventDispatcher dispatcher = new CertificateSelectionDispatcherListener(
				this.optionPane,
				this
		);

		KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(dispatcher);

		certDialog.setVisible(true);

		if (this.optionPane.getValue() == null ||
				((Integer) this.optionPane.getValue()).intValue() != JOptionPane.OK_OPTION) {
			KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventDispatcher(dispatcher);
			certDialog.dispose();
			return null;
		}

		final String selectedAlias = this.csd.getSelectedCertificateAlias();

		KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventDispatcher(dispatcher);
		certDialog.dispose();

		return selectedAlias;
	}

	/** {@inheritDoc} */
	@Override
	public void mouseClicked(final MouseEvent me) {
		if (me.getClickCount() == 2 && this.optionPane != null) {
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
			LOGGER.warning("Error en la orden de actualizacion del almacen: " + e); //$NON-NLS-1$
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
