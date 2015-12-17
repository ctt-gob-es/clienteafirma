/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardsobres;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.CertificateDestiny;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Clase que contiene los elementos necesarios para crear un grupo de destinatarios
 * a partir de una seleccion de certificados de destinatarios. */
final class PanelDestinatarios extends JAccessibilityDialogWizard {

    /** Botonera con funciones para la pagina panel de multifirma - cofirma */
    private class Botonera extends BotoneraInferior {

        private static final long serialVersionUID = 1L;

        public Botonera(final List<JDialogWizard> ventanas, final int posicion) {
            super(ventanas, posicion);
        }

        @Override
        protected void siguienteActionPerformed(final JButton anterior, final JButton siguiente, final JButton finalizar) {

            // Cargamos el listado de certificados
            ((PanelRemitentes) getVentanas().get(2)).setListaCertificados(PanelDestinatarios.this.getListaCertificados());

            if (verificarCertificados()) {
                super.siguienteActionPerformed(anterior, siguiente, finalizar);
            }
        }
    }
    /** log. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Lista de certificados. */
    private final List<CertificateDestiny> listaCertificados = new ArrayList<>();
    List<CertificateDestiny> getListaCertificados() {
    	return this.listaCertificados;
    }

    /** Lista con los destinatarios. */
    final JList listaDestinatarios = new JList();
    JList getListaDestinatarios() {
    	return this.listaDestinatarios;
    }

    /** Constructor. */
    public PanelDestinatarios() {
        initComponents();
    }

    /** A&ntilde;ade un destinatario del origen seleccionado en el combo
     * @param listaModel Modelo de la lista de destinatarios */
    void anadirActionPerformed(final JComboBox comboDestinatarios, final DefaultListModel listaModel, final JButton eliminar) {
        AOKeyStoreManager keyStoreManager = null;
        final KeyStoreConfiguration kc = (KeyStoreConfiguration) comboDestinatarios.getSelectedItem();
        try {
            final AOKeyStore ao = kc.getType();
            String lib = null;
            if (ao == AOKeyStore.PKCS12 || ao == AOKeyStore.SINGLE) {
                ExtFilter filter;
                if (ao == AOKeyStore.PKCS12) {
                    filter = new ExtFilter(
                		new String[] {
                        	"p12", "pfx"  //$NON-NLS-1$//$NON-NLS-2$
                    	},
                        Messages.getString("Filtro.fichero.pkcs12.descripcion") //$NON-NLS-1$
                    );
                }
                else {
                    filter = new ExtFilter(new String[] {
                                                         "cer", "p7b", "p7s"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                                         Messages.getString("Filtro.fichero.certificado.descripcion")); //$NON-NLS-1$
                }
                final File keystorePath = SelectionDialog.showFileOpenDialog(this, Messages.getString("Ensobrado.dialogo.almacen.titulo"), Main.getPreferences().get("dialog.load.dir", null), filter); //$NON-NLS-1$ //$NON-NLS-2$
                if (keystorePath == null) {
                    throw new AOCancelledOperationException();
                }
                lib = keystorePath.getAbsolutePath();
            }
            else if (ao == AOKeyStore.PKCS11) {
                final ExtFilter filter = new ExtFilter(
                		new String[] {
                        	"dll", "so"  //$NON-NLS-1$//$NON-NLS-2$
                    	},
                        Messages.getString("Filtro.fichero.pkcs11.descripcion") //$NON-NLS-1$
                    );
                final File keystorePath = SelectionDialog.showFileOpenDialog(this, Messages.getString("Ensobrado.dialogo.almacen.titulo"), Main.getPreferences().get("dialog.load.repository.pkcs11", null), filter); //$NON-NLS-1$ //$NON-NLS-2$
                if (keystorePath == null) {
                    throw new AOCancelledOperationException();
                }
                lib = keystorePath.getAbsolutePath();
                Main.getPreferences().put("dialog.load.repository.pkcs11", lib); //$NON-NLS-1$
            }

            keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(ao, lib, null, getPreferredPCB(ao), this);
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
            return;
        }
        catch (final IOException e) {
            // Control de la excepcion generada al introducir mal la contrasena para el almacen
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.sobres.error.almacen.contrasenia"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return;
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido abrir el almacen de certificados: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.sobres.error.abrir.almacen"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return;
        }

        final CertificateDestiny certDest = new CertificateDestiny(keyStoreManager, this);

        // Comprobamos que el certificado es correcto
        if (certDest.getAlias() != null && !certDest.equals("")) { //$NON-NLS-1$
            boolean copiar = true;
            for (int i = 0; i < listaModel.getSize(); i++) {
                if (certDest.getAlias().equals(listaModel.getElementAt(i))) {
                    copiar = false;
                }
            }
            if (copiar) {
                listaModel.addElement(certDest.getAlias());
                this.listaCertificados.add(certDest);
                eliminar.setEnabled(true);
                eliminar.setMnemonic(KeyEvent.VK_E); // Se asigna un atajo al boton ya que ha sido habilitado
            }
            else {
                CustomDialog.showMessageDialog(
            		this,
                    true,
                    Messages.getString("Wizard.sobres.error.usuario.existe"), //$NON-NLS-1$
                    Messages.getString("error"), //$NON-NLS-1$
                    JOptionPane.WARNING_MESSAGE
                );
            }
        }
    }

    /** Carga el combo con las diferentes opciones de destinatarios
     * @param comboDestinatarios Combo a cargar con las diferentes opciones de destinatarios */
    private static void cargarCombo(final JComboBox comboDestinatarios) {
        comboDestinatarios.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToWrap()));
    }

    /** Elimina un destintatario de la lista
     * @param listaModel Modelo de la lista de destinatarios
     * @param eliminar Boton eliminar */
    void eliminarActionPerformed(final DefaultListModel listaModel, final JButton eliminar) {
        for (int i = 0; i < this.listaCertificados.size(); i++) {
            if (this.listaCertificados.get(i).getAlias().equals(this.listaDestinatarios.getSelectedValue())) {
                this.listaCertificados.remove(this.listaCertificados.get(i));
                listaModel.remove(this.listaDestinatarios.getSelectedIndex());
                break;
            }
        }

        if (listaModel.isEmpty()) {
            eliminar.setEnabled(false);
            eliminar.setMnemonic(0); // Se asigna un atajo vacio al boton ya que ha sido deshabilitado
        }
    }

    /** Relacion minima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 8;
    }

    /** Recupera el PasswordCallback que t&iacute;picamente se requiere para el acceso a un
     * almac&eacute;n de claves.
     * @param kStore Almac&eacuten de claves */
    private PasswordCallback getPreferredPCB(final AOKeyStore kStore) {
        if (kStore == null) {
            throw new IllegalArgumentException("No se ha indicado el KeyStore del que desea obtener el PasswordCallBack"); //$NON-NLS-1$
        }

        PasswordCallback pssCallback;
        if (kStore == AOKeyStore.WINDOWS ||
        		kStore == AOKeyStore.PKCS11 || kStore == AOKeyStore.SINGLE ||
        		kStore == AOKeyStore.WINADDRESSBOOK) {
            pssCallback =
                NullPasswordCallback.getInstance();
        }
        else if (kStore == AOKeyStore.DNIEJAVA) {
            pssCallback = null;
        }
        else {
            pssCallback =
                new UIPasswordCallbackAccessibility(
            		Messages.getString("Wizard.sobres.almacen.pass") + " " + kStore.getName(), //$NON-NLS-1$ //$NON-NLS-2$
                    this,
                    Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                    KeyEvent.VK_O,
                    Messages.getString("CustomDialog.showInputPasswordDialog.title") //$NON-NLS-1$
        		);
        }

        return pssCallback;
    }

    /** Inicializacion de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("Wizard.sobres.titulo")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior =
            new CabeceraAsistente(
        		"Wizard.sobres.pagina1.titulo", //$NON-NLS-1$
                "Wizard.sobres.pagina1.titulo.explicacion1", //$NON-NLS-1$
                "Wizard.sobres.pagina1.titulo.explicacion2", //$NON-NLS-1$
                null
            );
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);

        // Panel central
        final JPanel panelCentral = new JPanel();
        panelCentral.setMinimumSize(new Dimension(603, 289));
        panelCentral.setLayout(new GridBagLayout());

        // Configuramos el layout
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 2;
        c.weightx = 1.0;
        c.gridx = 0;

        // Etiqueta con el texto "Puede anadir uno o..."
        final InfoLabel labelText = new InfoLabel(Messages.getString("Wizard.sobres.pagina1.contenido.explicacion1"), false); //$NON-NLS-1$

        c.gridy = 1;
        c.gridwidth = 1;

        // Etiqueta con el texto "Anadir un destinatario..."
        final JLabel etiquetaAnadir = new JLabel();
        etiquetaAnadir.setText(Messages.getString("wizard.sobres.aniadir.destinatario")); //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAnadir);
        Utils.setFontBold(etiquetaAnadir);
        panelCentral.add(etiquetaAnadir, c);

        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = 1;
        c.gridy = 2;
        c.weightx = 1.0;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con las listas de destinatarios
        final JComboBox comboDestinatarios = new JComboBox();
        comboDestinatarios.setToolTipText(Messages.getString("Wizard.sobres.pagina1.comboDestinatarios.description")); //$NON-NLS-1$
        comboDestinatarios.getAccessibleContext()
        .setAccessibleName(etiquetaAnadir.getText() + " " + comboDestinatarios.getToolTipText() + "ALT + D."); //$NON-NLS-1$ //$NON-NLS-2$
        comboDestinatarios.getAccessibleContext().setAccessibleDescription(comboDestinatarios.getToolTipText());
        cargarCombo(comboDestinatarios);
        Utils.remarcar(comboDestinatarios);
        Utils.setContrastColor(comboDestinatarios);
        Utils.setFontBold(comboDestinatarios);
        panelCentral.add(comboDestinatarios, c);

        // Relacion entre etiqueta y combo
        etiquetaAnadir.setLabelFor(comboDestinatarios);
        // Asignacion de mnemonico
        etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D);

        c.insets = new Insets(0, 10, 0, 20);
        c.gridx = 1;
        c.weightx = 0.0;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        final JPanel panelAnadir = new JPanel(new GridLayout(1, 1));
        // Boton anadir destinatarios
        final JButton anadir = new JButton();
        final JButton eliminar = new JButton();
        anadir.setToolTipText(Messages.getString("wizard.aniadir.description")); //$NON-NLS-1$
        anadir.setText(Messages.getString("wizard.aniadir")); //$NON-NLS-1$
        anadir.setAutoscrolls(true);
        anadir.setMnemonic(KeyEvent.VK_I); // Se asigna un atajo al boton
        anadir.getAccessibleContext().setAccessibleName(anadir.getText() + " " + anadir.getToolTipText()); //$NON-NLS-1$
        anadir.getAccessibleContext().setAccessibleDescription(anadir.getToolTipText());
        anadir.addActionListener(new ActionListener() {
            /** Accion del boton anadir. */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                anadirActionPerformed(comboDestinatarios, (DefaultListModel) PanelDestinatarios.this.getListaDestinatarios().getModel(), eliminar);
            }
        });
        Utils.remarcar(anadir);
        Utils.setContrastColor(anadir);
        Utils.setFontBold(anadir);
        panelAnadir.add(anadir);
        panelCentral.add(panelAnadir, c);

        c.insets = new Insets(10, 20, 0, 20);
        c.gridx = 0;
        c.gridy = 3;
        c.weightx = 0.0;

        // Etiqueta con el texto "Destinatarios"
        final JLabel destLabel = new JLabel();
        destLabel.setText(Messages.getString("wizard.sobres.listaDestinatarios")); //$NON-NLS-1$
        Utils.setContrastColor(destLabel);
        Utils.setFontBold(destLabel);
        panelCentral.add(destLabel, c);

        c.insets = new Insets(0, 20, 0, 20);
        c.ipady = 80;
        c.gridwidth = 2;
        c.gridy = 4;
        c.gridx = 0;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.fill = GridBagConstraints.BOTH;

        // Panel que contiene a la lista de destintatarios
        final JScrollPane panelLista = new JScrollPane();
        panelCentral.add(panelLista, c);

        // Lista con los destinatarios
        this.listaDestinatarios.setToolTipText(Messages.getString("wizard.listaDestinatarios.description")); //$NON-NLS-1$
        this.listaDestinatarios.setModel(new DefaultListModel());
        this.listaDestinatarios.getAccessibleContext().setAccessibleName(destLabel.getText() + " " + this.listaDestinatarios.getToolTipText()); //$NON-NLS-1$
        this.listaDestinatarios.getAccessibleContext().setAccessibleDescription(this.listaDestinatarios.getToolTipText());
        Utils.remarcar(this.listaDestinatarios);
        Utils.setContrastColor(this.listaDestinatarios);
        Utils.setFontBold(this.listaDestinatarios);

        panelLista.setViewportView(this.listaDestinatarios);

        // Relacion entre etiqueta y lista
        destLabel.setLabelFor(this.listaDestinatarios);
        // Asignacion de mnemonico
        destLabel.setDisplayedMnemonic(KeyEvent.VK_T);

        c.ipady = 0;
        c.gridwidth = 1;
        c.gridy = 5;
        c.weightx = 1.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Espacio izdo del boton
        final Panel panelVacio = new Panel();
        panelCentral.add(panelVacio, c);

        c.insets = new Insets(10, 0, 10, 20);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelEliminar = new JPanel(new GridLayout(1, 1));
        // Boton eliminar destinatarios
        eliminar.setToolTipText(Messages.getString("wizard.eliminar.description")); //$NON-NLS-1$
        eliminar.setEnabled(false);
        eliminar.setText(Messages.getString("wizard.sobres.eliminar.destinatario")); //$NON-NLS-1$
        eliminar.getAccessibleContext().setAccessibleName(eliminar.getText() + " " + eliminar.getToolTipText()); //$NON-NLS-1$
        eliminar.getAccessibleContext().setAccessibleDescription(eliminar.getToolTipText());
        eliminar.addActionListener(new ActionListener() {
            /** Accion del boton eliminar. */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                eliminarActionPerformed((DefaultListModel) PanelDestinatarios.this.listaDestinatarios.getModel(), eliminar);
            }
        });
        Utils.remarcar(eliminar);
        Utils.setContrastColor(eliminar);
        Utils.setFontBold(eliminar);
        panelEliminar.add(eliminar);
        panelCentral.add(panelEliminar, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(comboDestinatarios, "ensobrado.wizard.repositorio.destinatario"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(labelText, "ensobrado.wizard.destinatarios"); //$NON-NLS-1$
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    public void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 1));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }

    /** Comprueba que se ha seleccionado algun certificado
     * @return True o false segun la verificacion */
    public boolean verificarCertificados() {
        final DefaultListModel listModel = (DefaultListModel) this.getListaDestinatarios().getModel();
        if (listModel.isEmpty()) {
            CustomDialog.showMessageDialog(
        		this,
                true,
                Messages.getString("WizardCifrado.error.destinatario"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return false;
        }
        return true;
    }
}
