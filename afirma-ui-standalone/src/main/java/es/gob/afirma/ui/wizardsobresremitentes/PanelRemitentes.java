/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardsobresremitentes;

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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
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
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.envelopers.cms.AOCMSEnveloper;
import es.gob.afirma.envelopers.cms.AOCMSMultiEnveloper;
import es.gob.afirma.envelopers.cms.CMSAuthenticatedEnvelopedData;
import es.gob.afirma.envelopers.cms.CMSEnvelopedData;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.KeyStoreConfiguration;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.SignFileUtils;
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.CertificateDestiny;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Clase que contiene los elementos necesarios para crear un grupo de Remitentes
 * a partir de una seleccion de certificados de remitentes. */
final class PanelRemitentes extends JAccessibilityDialogWizard {

    /** Botonera con funciones para la pagina panel de multifirma - cofirma */
    private class Botonera extends BotoneraInferior {
        /** UID. */
        private static final long serialVersionUID = 1L;

        /** Constructor.
         * @param ventanas Lista de ventanas que componen el wizard.
         * @param posicion posicion de la ventana donde se inserta esta botonera. */
        public Botonera(final List<JDialogWizard> ventanas, final int posicion) {
            super(ventanas, posicion);
        }

        /** Accion para el boton siguiente. */
        @Override
        protected void siguienteActionPerformed(final JButton anterior, final JButton siguiente, final JButton finalizar) {
            if (anadirRemitentes()) {
                super.siguienteActionPerformed(anterior, siguiente, finalizar);
            }
        }
    }

    /** Log. */
    private static Logger logger = Logger.getLogger(PanelRemitentes.class.getName());

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Etiqueta con el texto "Anadir remitente desde...". */
    private final JLabel etiquetaAnadir = new JLabel();

    /** Configuracion del KeyStore */
    private KeyStoreConfiguration kconf;

    /** Manager del KeyStore */
    private AOKeyStoreManager keyStoreManager;

    /** Lista con los remitentes */
    private final List<CertificateDestiny> listaCertificadosRe = new ArrayList<CertificateDestiny>();

    /** Lista de remitentes. */
    private final JList listaRemitentes = new JList();

    /** Ruta donde se encuentra el fichero a ensobrar */
    private final String rutafichero;

    /** Constructor.
     * @param rutafichero */
    public PanelRemitentes(final String rutafichero) {
        this.rutafichero = rutafichero;
        initComponents();
    }

    /** A&ntilde;ade un nuevo remitente desde el repositorio indicado
     * @param comboRepositorios combo con el listado de repositorios / almacenes
     * @param listModel Modelo de la lista de remitentes
     * @param eliminar Boton para eliminar un remitente del listado de repositorios
     * @param anadir Boton para anadir un remitente al listado de repositorios */
    void anadirActionPerformed(final JComboBox comboRepositorios, final JButton eliminar, final JButton anadir) {
        this.kconf = (KeyStoreConfiguration) comboRepositorios.getSelectedItem();

        try {
            final AOKeyStore ao = this.kconf.getType();
            String lib = null;
            if (ao == AOKeyStore.PKCS12 || ao == AOKeyStore.SINGLE) {
                ExtFilter filter;
                if (ao == AOKeyStore.PKCS12) {
                    filter = new ExtFilter(new String[] {
                                                         "p12", "pfx"}, //$NON-NLS-1$ //$NON-NLS-2$
                                                         Messages.getString("Filtro.fichero.pkcs12.descripcion")); //$NON-NLS-1$
                }
                else {
                    filter = new ExtFilter(new String[] {
                                                         "cer", "p7b", "p7s"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                                         Messages.getString("Filtro.fichero.certificado.descripcion")); //$NON-NLS-1$
                }
                final File keystorePath = SelectionDialog.showFileOpenDialog(this, Messages.getString("Ensobrado.dialogo.almacen.titulo"), filter); //$NON-NLS-1$
                if (keystorePath == null) {
                    throw new AOCancelledOperationException();
                }
                lib = keystorePath.getAbsolutePath();
            }
            this.keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(ao, lib, null, getPreferredPCB(ao), this);
        }
        catch (final AOCancelledOperationException e) {
            logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
            return;
        }
        catch (final Exception e) {
            logger.severe("No se ha podido abrir el almacen de certificados: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.certificados.almacen"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return;
        }

        // Obtenemos el certificado
        final CertificateDestiny certDest = new CertificateDestiny(this.keyStoreManager, this);

        // Comprobamos que el certificado es correcto
        if (certDest.getAlias() != null && !certDest.equals("")) { //$NON-NLS-1$
            final DefaultListModel listModel = (DefaultListModel) this.listaRemitentes.getModel();

            boolean copiar = true;
            for (int i = 0; i < listModel.getSize(); i++) {
                if (listModel.getElementAt(i).equals(certDest.getAlias())) {
                    copiar = false;
                    break;
                }
            }

            if (copiar) {
                listModel.addElement(certDest.getAlias());
                this.listaCertificadosRe.add(certDest);
                anadir.setEnabled(false);
                comboRepositorios.setEnabled(false);
                this.etiquetaAnadir.setDisplayedMnemonic(0); // Se asigna un atajo vacio puesto que se ha deshabilitado el combo asociado
                this.etiquetaAnadir.getAccessibleContext().setAccessibleName(this.etiquetaAnadir.getText() + " "
                                                                             + Messages.getString("wizard.sobres.etiquetaAnadir")); //$NON-NLS-1$
                this.etiquetaAnadir.setFocusable(true);
                eliminar.setEnabled(true);
                eliminar.setMnemonic(KeyEvent.VK_E); // Se asigna un atajo al boton ya que ha sido habilitado
            }
            else {
                CustomDialog.showMessageDialog(this,
                                               true,
                                               Messages.getString("Wizard.sobres.error.usuario"), //$NON-NLS-1$
                                               Messages.getString("error"), //$NON-NLS-1$
                                               JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    /** Ensobra el fichero dado
     * @return True o false si la operacion ha sido satisfactoria */
    public boolean anadirRemitentes() {
        try {
            final FileInputStream dataFis = new FileInputStream(this.rutafichero);
            byte[] envelopedData = AOUtil.getDataFromInputStream(dataFis);

            // Anadimos solo el ultimo
            final PrivateKeyEntry privateKey =
                getPrivateKeyEntry(this.keyStoreManager, this.listaCertificadosRe.get(this.listaCertificadosRe.size() - 1).getAlias(), this.kconf);
            final String contentType = comprobarTipo(envelopedData, privateKey);
            if (contentType == null) {
                return false;
            }

            envelopedData = doCoEnvelopOperation(envelopedData, contentType, privateKey);

            // Guardamos el sobre generado
            final File savedFile =
                SelectionDialog.saveDataToFile(Messages.getString("wizard.sobres.remitentes.filechooser.save.title"), //$NON-NLS-1$
                                               envelopedData,
                                               new File(this.rutafichero + ".csig").getName(), //$NON-NLS-1$
                                               SignFileUtils.getOutFileFilter(AOSignConstants.SIGN_FORMAT_CMS),
                                               this);
            // Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
            if (savedFile == null) {
                return false;
            }

            try {
                dataFis.close();
            }
            catch (final Exception e) {
                logger.warning("No se pudo cerrar el fichero de datos"); //$NON-NLS-1$
            }
        }
        catch (final AOCancelledOperationException e) {
            logger.info("La operacion ha sido cancelada por el usuario: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Ensobrado.msg.error.generacion"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }
        catch (final FileNotFoundException e) {
            logger.warning("No ha sido posible leer el fichero indicado: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.lectura.generico"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }
        catch (final Exception e) {
            logger.warning("Error durante el proceso de andir un nuevo remitente: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.sobres.almacen.anadir.remitentes"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }

    /** Carga un combo con los repositorios/almacenes disponibles
     * @param comboRepositorios Combo donde se deben cargar los repositorios */
    private static void cargarCombo(final JComboBox comboRepositorios) {
        comboRepositorios.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
    }

    /** Comprueba el tipo de todos los certificados
     * @param data Sobre electr&oacute;nico.
     * @param privateKey Certificado
     * @return Tipo de sobre */
    private String comprobarTipo(final byte[] data, final PrivateKeyEntry privateKey) {
        String tipo = null;
        if (AOCMSEnveloper.isCMSValid(data, AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
            tipo = AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA;
        }
        else if (AOCMSEnveloper.isCMSValid(data, AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
            tipo = AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA;
        }
        else if (AOCMSEnveloper.isCMSValid(data, AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
            tipo = AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA;
        }
        else {
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.sobres.almacen.sobre.soportado"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return null;
        }

        return tipo;
    }

    /** Anade un nuevo remitente al sobre
     * @param data Sobre electr&oacute;nico.
     * @param contentType Tipo del contenido
     * @param privateKey Clave privada
     * @return */
    private byte[] doCoEnvelopOperation(final byte[] data, final String contentType, final PrivateKeyEntry privateKey) {
        byte[] envelop;
        final X509Certificate[] originatorCertChain = (X509Certificate[]) privateKey.getCertificateChain();

        if (contentType.equals(AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
            try {
                envelop = CMSEnvelopedData.addOriginatorInfo(data, originatorCertChain);
            }
            catch (final AOException e) {
                logger.warning("Ocurrio al agregar el nuevo remitente al sobre electronico: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.almacen.anadir.remitentes"), //$NON-NLS-1$
                                               Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                return null;
            }
        }
        else if (contentType.equals(AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
            final AOCMSMultiEnveloper coEnveloper = new AOCMSMultiEnveloper();
            try {
                envelop = coEnveloper.cosign(data, AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, privateKey);
            }
            catch (final AOException e) {
                logger.warning("Error durante el proceso de agregar un nuevo remitente: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.almacen.anadir.remitentes"), //$NON-NLS-1$
                                               Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                return null;
            }
        }
        else if (contentType.equals(AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
            envelop = CMSAuthenticatedEnvelopedData.addOriginatorInfo(data, originatorCertChain);
        }
        else {
            CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.almacen.certificado.soportado"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return null;
        }

        return envelop;
    }

    /** Elimina un remitente de la lista de remitentes
     * @param comboRepositorios Combo con el repositorio / almacen
     * @param eliminar Boton para eliminar un remitente de la lista
     * @param anadir Boton para anadir un remitente a la lista */
    void eliminarActionPerformed(final JComboBox comboRepositorios, final JButton eliminar, final JButton anadir) {
        for (int i = 0; i < this.listaCertificadosRe.size(); i++) {
            if (this.listaCertificadosRe.get(i).getAlias().equals(this.listaRemitentes.getSelectedValue())) {
                this.listaCertificadosRe.remove(this.listaCertificadosRe.get(i));
                ((DefaultListModel) this.listaRemitentes.getModel()).remove(this.listaRemitentes.getSelectedIndex());

                eliminar.setEnabled(false);
                eliminar.setMnemonic(0); // Se asigna un atajo vacio al boton ya que ha sido deshabilitado
                anadir.setEnabled(true);
                anadir.setMnemonic(KeyEvent.VK_R); // Se asigna un atajo al boton puesto que se ha habilitado
                comboRepositorios.setEnabled(true);
                this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); // Se asigna un atajo puesto que se ha habilitado el combo asociado
                this.etiquetaAnadir.setFocusable(false);
                break;
            }
        }
    }

    /** Relacion minima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 9;
    }

    /** Recupera el PasswordCallback que t&iacute;picamente se requiere para el acceso a un
     * almac&eacute;n de claves.
     * @param kStore Almac&eacuten de claves */
    private PasswordCallback getPreferredPCB(final AOKeyStore kStore) {
        if (kStore == null) {
            throw new IllegalArgumentException("No se ha indicado el KeyStore del que desea obtener el PasswordCallBack"); //$NON-NLS-1$
        }

        PasswordCallback pssCallback;
        if (kStore == AOKeyStore.WINDOWS || kStore == AOKeyStore.WINROOT || kStore == AOKeyStore.PKCS11 || kStore == AOKeyStore.SINGLE) {
            pssCallback =
                new NullPasswordCallback();
        }
        else {
            // pssCallback = new UIPasswordCallback(Messages.getString("Wizard.sobres.almacen.pass")+" "+kStore.getDescription(), this);
            pssCallback =
                new UIPasswordCallbackAccessibility(Messages.getString("Wizard.sobres.almacen.pass") + " " + kStore.getDescription(), //$NON-NLS-1$
                                                    this,
                                                    Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                                                    KeyEvent.VK_O,
                                                    Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$
        }

        return pssCallback;
    }

    /** Obtiene la clave privada
     * @param keyStoreManager1 Manager del KeyStore
     * @param seleccionado Certificado seleccionado
     * @param kconf1 Configuracion del KeyStore
     * @return
     * @throws AOException */
    private PrivateKeyEntry getPrivateKeyEntry(final AOKeyStoreManager keyStoreManager1, final String seleccionado, final KeyStoreConfiguration kconf1) throws AOException {

        // Comprobamos si se ha cancelado la seleccion
        if (seleccionado == null)
        {
            throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$
        }

        // Recuperamos la clave del certificado
        PrivateKeyEntry privateKeyEntry = null;
        try {
            privateKeyEntry = keyStoreManager1.getKeyEntry(seleccionado, Utils.getCertificatePC(kconf1.getType(), this));
        }
        catch (final AOCancelledOperationException e) {
            // Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
            // Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
            // que las relanza en forma de AOException
            throw e;
        }
        catch (final Exception e) {
            logger.severe("No se ha podido obtener el certificado con el alias '" + seleccionado + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            throw new AOException(e.getMessage());
        }

        return privateKeyEntry;
    }

    /** Inicializacion de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("Wizard.sobres.titulo")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior =
            new CabeceraAsistente("Wizard.sobres.remitentes.titulo", "Wizard.sobres.remitentes.titulo.explicacion", null, true);  //$NON-NLS-1$//$NON-NLS-2$
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
        final InfoLabel labelText = new InfoLabel(Messages.getString("Wizard.sobres.remitentes.contenido.explicacion1"), false); //$NON-NLS-1$
        panelCentral.add(labelText, c);

        c.gridy = 1;
        c.gridwidth = 1;

        // Etiqueta con el texto "Anadir remitente desde..."
        this.etiquetaAnadir.setText(Messages.getString("Wizard.sobres.aniadir.originante")); //$NON-NLS-1$
        Utils.setContrastColor(this.etiquetaAnadir);
        Utils.setFontBold(this.etiquetaAnadir);
        panelCentral.add(this.etiquetaAnadir, c);

        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = 1;
        c.gridy = 2;
        c.weightx = 1.0;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con los repositorios / almacenes
        final JComboBox comboRepositorios = new JComboBox();
        comboRepositorios.setToolTipText(Messages.getString("wizard.comboRepositorios.description")); //$NON-NLS-1$
        comboRepositorios.getAccessibleContext().setAccessibleName(this.etiquetaAnadir.getText() + " "
                                                                   + comboRepositorios.getToolTipText()
                                                                   + " ALT + D.");
        comboRepositorios.getAccessibleContext().setAccessibleDescription(comboRepositorios.getToolTipText());
        comboRepositorios.addActionListener(new ActionListener() {
            /** Accion combo de repositorios. */
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                PanelRemitentes.this.listaCertificadosRe.clear();
                ((DefaultListModel) PanelRemitentes.this.listaRemitentes.getModel()).removeAllElements();
            }
        });
        cargarCombo(comboRepositorios);
        Utils.remarcar(comboRepositorios);
        Utils.setContrastColor(comboRepositorios);
        Utils.setFontBold(comboRepositorios);
        panelCentral.add(comboRepositorios, c);

        // Relacion entre etiqueta y combo
        this.etiquetaAnadir.setLabelFor(comboRepositorios);
        // Asignacion de mnemonico
        this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D);

        c.insets = new Insets(0, 10, 0, 20);
        c.gridx = 1;
        c.weightx = 0.0;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        final JPanel panelAnadir = new JPanel(new GridLayout(1, 1));
        // Boton Anadir
        final JButton anadir = new JButton();
        final JButton eliminar = new JButton();
        anadir.setToolTipText(Messages.getString("Wizard.sobres.aniadir.originante.description")); //$NON-NLS-1$
        anadir.setText(Messages.getString("wizard.aniadir")); //$NON-NLS-1$
        anadir.setAutoscrolls(true);
        anadir.setMnemonic(KeyEvent.VK_R); // Se asigna un atajo al boton
        anadir.getAccessibleContext().setAccessibleName(anadir.getText() + " " + anadir.getToolTipText());
        anadir.getAccessibleContext().setAccessibleDescription(anadir.getToolTipText());
        anadir.addActionListener(new ActionListener() {
            /** Accion boton anadir. */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                anadirActionPerformed(comboRepositorios, eliminar, anadir);
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

        // Etiqueta con el texto "Remitentes"
        final JLabel senderLabel = new JLabel();
        senderLabel.setText(Messages.getString("wizard.sobres.listaRemitentes")); //$NON-NLS-1$
        Utils.setContrastColor(senderLabel);
        Utils.setFontBold(senderLabel);
        panelCentral.add(senderLabel, c);

        c.insets = new Insets(10, 20, 0, 20);
        c.ipady = 80;
        c.gridwidth = 2;
        c.gridy = 4;
        c.gridx = 0;
        c.weightx = 1.0;
        c.weighty = 1.0;

        // Panel que contiene la lista de remitentes
        final JScrollPane panelLista = new JScrollPane();
        panelCentral.add(panelLista, c);

        // Listado de remitentes
        this.listaRemitentes.setToolTipText(Messages.getString("Wizard.sobres.listaRemitentes.description")); //$NON-NLS-1$
        this.listaRemitentes.setModel(new DefaultListModel());
        this.listaRemitentes.getAccessibleContext().setAccessibleName(senderLabel.getText() + " " + this.listaRemitentes.getToolTipText());
        this.listaRemitentes.getAccessibleContext().setAccessibleDescription(this.listaRemitentes.getToolTipText());
        Utils.remarcar(this.listaRemitentes);
        Utils.setContrastColor(this.listaRemitentes);
        Utils.setFontBold(this.listaRemitentes);
        panelLista.setViewportView(this.listaRemitentes);

        // Relacion entre etiqueta y lista
        senderLabel.setLabelFor(this.listaRemitentes);
        // Asignacion de mnemonico
        senderLabel.setDisplayedMnemonic(KeyEvent.VK_T);

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
        // Boton eliminar
        eliminar.setEnabled(false);
        eliminar.setToolTipText(Messages.getString("Wizard.sobres.eliminar.remitente.description")); //$NON-NLS-1$
        eliminar.setText(Messages.getString("wizard.sobres.eliminar.remitente")); //$NON-NLS-1$
        eliminar.getAccessibleContext().setAccessibleName(eliminar.getText() + " " + eliminar.getToolTipText());
        eliminar.getAccessibleContext().setAccessibleDescription(eliminar.getToolTipText());
        eliminar.addActionListener(new ActionListener() {
            /** Accion boton eliminar. */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                eliminarActionPerformed(comboRepositorios, eliminar, anadir);
            }
        });
        eliminar.getAccessibleContext().setAccessibleDescription(Messages.getString("wizard.sobres.eliminar.remitente")); //$NON-NLS-1$
        Utils.remarcar(eliminar);
        Utils.setContrastColor(eliminar);
        Utils.setFontBold(eliminar);
        panelEliminar.add(eliminar);
        panelCentral.add(panelEliminar, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(comboRepositorios, "ensobrado.wizard.repositorio.remitente"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(labelText, "ensobrado.wizard.remitentes"); //$NON-NLS-1$
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    public void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 1));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}
