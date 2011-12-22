/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.InvalidKeyException;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.text.Caret;


import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.main.common.KeyStoreConfiguration;
import es.gob.afirma.keystores.main.filters.CertificateFilter;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.SignedFileManager;
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;


/**
 * Clase que muestra los elementos necesarios para realizar una firma.
 */
final class Firma extends JPanel {

    private static final long serialVersionUID = 1L;

    private static Logger logger = Logger.getLogger(Firma.class.getName());

    // Nombres de los diferentes formatos de firmado
    private List<String> formatosL = new ArrayList<String>(Arrays.asList(
            "Firma est\u00E1ndar (XAdES Detached)",
            //			"XAdES Enveloping",
            //			"XAdES Enveloped",
            "CAdES",
            "PAdES"
    ));

    // Constantes de los diferentes formatos de firmado
    private List<String> formatosV = new ArrayList<String>(Arrays.asList(
            AOSignConstants.SIGN_FORMAT_XADES_DETACHED,
            //			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
            //			AOConstants.SIGN_FORMAT_XADES_ENVELOPED,
            AOSignConstants.SIGN_FORMAT_CADES,
            AOSignConstants.SIGN_FORMAT_PDF
    ));

    public Firma() {
        initComponents();
    }

    /**
     * Inicializacion de los componentes
     */
    private void initComponents() {
        setLayout(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;

        // Etiqueta fichero a firmar digitalmente
        final JLabel etiquetaFichero = new JLabel();
        etiquetaFichero.setText(Messages.getString("Firma.buscar")); // NOI18N //$NON-NLS-1$
        etiquetaFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Firma.buscar.description")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFichero);
        Utils.setFontBold(etiquetaFichero);
        add(etiquetaFichero, c);

        c.insets = new Insets(0, 13, 0, 0);
        c.gridwidth = 1;
        c.weightx = 1.0;
        c.gridy	= 1;

        // Campo donde se guarda el nombre del fichero a firmar
        final JTextField campoFichero = new JTextField();
        campoFichero.setToolTipText(Messages.getString("Firma.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Firma.buscar.caja.description.status")));
        campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Firma.buscar.caja.description.status")));
        campoFichero.getAccessibleContext().setAccessibleName(etiquetaFichero.getText()+" "+"ALT + G.");
        campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Firma.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        campoFichero.addAncestorListener(new RequestFocusListener(false));
        Utils.remarcar(campoFichero);
        if (GeneralConfig.isBigCaret()) {
            Caret caret = new ConfigureCaret();
            campoFichero.setCaret(caret);
        }
        Utils.setFontBold(campoFichero);
        add(campoFichero, c);

        //Relacion entre etiqueta y campo de texto
        etiquetaFichero.setLabelFor(campoFichero);
        //Asignacion de mnemonico
        etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_G);

        c.insets = new Insets(0, 10, 0, 13);
        c.weightx = 0.0;
        c.gridx = 1;

        JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); // NOI18N //$NON-NLS-1$
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$
        examinar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        examinar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        //examinar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.Examinar") + " " + Messages.getString("PrincipalGUI.Examinar.description.status"));
        examinar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                examinarActionPerformed(campoFichero);
            }
        });
        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);
        
        panelExaminar.add(examinar);
        add(panelExaminar, c);

        //Espacio en blanco
        JPanel emptyPanel01 = new JPanel();
        emptyPanel01.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 2;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel01, c);
        
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridwidth = 2;
        c.gridx = 0;
        c.gridy	= 3;

        // Etiqueta para el Almacen / repositorio
        JLabel etiquetaAlmacen = new JLabel();
        etiquetaAlmacen.setText(Messages.getString("Firma.almacen.certificados")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAlmacen);
        Utils.setFontBold(etiquetaAlmacen);
        add(etiquetaAlmacen, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.weightx = 1.0;
        c.gridy = 4;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con las opciones del almacen o repositorio
        final JComboBox comboAlmacen = new JComboBox();
        comboAlmacen.setToolTipText(Messages.getString("Firma.almacen.certificados.description")); // NOI18N //$NON-NLS-1$
        comboAlmacen.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Firma.almacen.certificados.description")));
        comboAlmacen.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Firma.almacen.certificados.description")));
        //comboAlmacen.getAccessibleContext().setAccessibleName(etiquetaAlmacen.getText()+" "+Messages.getString("Firma.almacen.certificados.description") + " ALT + A."); // NOI18N

        Utils.remarcar(comboAlmacen);
        cargarComboAlmacen(comboAlmacen);
        Utils.setContrastColor(comboAlmacen);
        Utils.setFontBold(comboAlmacen);
        add(comboAlmacen, c);

        //Relacion entre etiqueta y combo
        etiquetaAlmacen.setLabelFor(comboAlmacen);
        //Asignacion de mnemonico
        etiquetaAlmacen.setDisplayedMnemonic(KeyEvent.VK_A);

        //Espacio en blanco
        JPanel emptyPanel02 = new JPanel();
        emptyPanel02.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 5;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel02, c);
        
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridy = 6;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Etiqueta formato / formato
        JLabel etiquetaFormato = new JLabel();
        etiquetaFormato.setText(Messages.getString("Firma.formato")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFormato);
        Utils.setFontBold(etiquetaFormato);
        add(etiquetaFormato, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.weightx = 1.0;
        c.gridy = 7;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con los diferentes formatos de firma
        final JComboBox comboFormato = new JComboBox();
        comboFormato.setToolTipText(Messages.getString("Firma.formato.description")); // NOI18N //$NON-NLS-1$
        comboFormato.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Firma.formato.description.status")));
        comboFormato.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Firma.formato.description.status")));
        //comboFormato.getAccessibleContext().setAccessibleName(etiquetaFormato.getText()+" " + Messages.getString("Firma.formato.description") + " ALT + O."); // NOI18N

        if(GeneralConfig.isAvanzados()) {
            // XAdES Enveloping (Solo en la vista avanzada)
            this.formatosL.add("XAdES Enveloping");
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
            // XAdES Enveloped (Solo en la vista avanzada)
            this.formatosL.add("XAdES Enveloped");
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
            // OOXML (Solo en la vista avanzada)
            this.formatosL.add("OOXML");
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_OOXML);
            // ODF (Solo en la vista avanzada)
            this.formatosL.add("ODF");
            this.formatosV.add(AOSignConstants.SIGN_FORMAT_ODF);
        }
        comboFormato.setModel(new DefaultComboBoxModel(this.formatosL.toArray()));
        Utils.remarcar(comboFormato);
        Utils.setContrastColor(comboFormato);
        Utils.setFontBold(comboFormato);
        add(comboFormato, c);

        //Relacion entre etiqueta y combo
        etiquetaFormato.setLabelFor(comboFormato);
        //Asignacion de mnemonico
        etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_O);

        c.weighty = 1.0;
        c.gridy = 8;
        c.gridheight = 4;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
        JPanel emptyPanel = new JPanel();
        add(emptyPanel, c);

        // Panel con los botones
        JPanel panelBotones = new JPanel(new GridBagLayout());

        GridBagConstraints cons = new GridBagConstraints();
        cons.anchor = GridBagConstraints.FIRST_LINE_START; //control de la orientacion de componentes al redimensionar
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.ipadx = 15;
        cons.gridx = 0;

        // Etiqueta para rellenar a la izquierda
        JLabel label = new JLabel();
        panelBotones.add(label, cons);

        JPanel panelFirmar = new JPanel(new GridLayout(1, 1));
        // Boton firmar
        JButton firmar = new JButton();
        firmar.setMnemonic(KeyEvent.VK_R);
        firmar.setText(Messages.getString("PrincipalGUI.firmar")); // NOI18N //$NON-NLS-1$
        firmar.setToolTipText(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N //$NON-NLS-1$
        //firmar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.firmar") + " " + Messages.getString("PrincipalGUI.firmar.description.status"));
        firmar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.firmar.description.status")));
        firmar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.firmar.description.status")));
        firmar.addActionListener(new ActionListener() {
            @Override
			public void actionPerformed(ActionEvent evt) {
                firmarActionPerformed(comboAlmacen, comboFormato, campoFichero);
            }
        });
        firmar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(firmar);
        Utils.setContrastColor(firmar);
        Utils.setFontBold(firmar);

        cons.ipadx = 0;
        cons.gridx = 1;
        cons.weightx = 1.0;

        panelFirmar.add(firmar);
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelFirmar, BorderLayout.CENTER);
        panelBotones.add(buttonPanel, cons);

        cons.ipadx = 15;
        cons.weightx = 0.0;
        cons.gridx = 2;

        JPanel panelAyuda = new JPanel();
        // Boton ayuda
        JButton botonAyuda = HelpUtils.helpButton("firma");
        botonAyuda.setName("helpButton");
        panelAyuda.add(botonAyuda);
        panelBotones.add(panelAyuda, cons);

        c.gridwidth	= 2;
        c.insets = new Insets(13,13,13,13);
        c.weighty = 0.0;
        c.gridy = 12;

        add(panelBotones, c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoFichero, "firma.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(examinar, "firma.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboAlmacen, "firma.almacen"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboFormato, "firma.formato"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(firmar, "firma"); //$NON-NLS-1$
    }

    /**
     * Pulsar boton examinar: Muestra una ventana para seleccinar un archivo.
     * Modifica el valor de la caja con el nombre del archivo seleccionado
     * @param campoFichero	Campo en el que se escribe el nombre del fichero seleccionado
     */
    void examinarActionPerformed(JTextField campoFichero) {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("PrincipalGUI.chooser.title")); //$NON-NLS-1$
        if (selectedFile != null) {
            campoFichero.setText(selectedFile.getAbsolutePath());
        }
    }

    /**
     * Firma el fichero seleccionado
     * @param comboAlmacen		Combo con el almacen / repositorio de certificados
     * @param comboFormato		Combo con los formatos de firmado
     * @param campoFichero		Campo con el nombre del archivo a firmar
     */
    void firmarActionPerformed(JComboBox comboAlmacen, JComboBox comboFormato, JTextField campoFichero) {//GEN-FIRST:event_firmarActionPerformed
        // Obtenemos la constante del formato a utilizar
        String formato = this.formatosV.get(comboFormato.getSelectedIndex());

        // Keystore
        AOKeyStoreManager keyStoreManager = null;

        // Obtenemos la ruta del fichero a firmar
        if (campoFichero.getText() == null || campoFichero.getText().equals("")) {
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            campoFichero.requestFocusInWindow();
            return;
        }

      //Mensaje que indica que se va a realizar el proceso de firma y que puede llevar un tiempo
    	CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.info"), Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$
        
        try {
            PasswordCallback pssCallback;
            KeyStoreConfiguration kssc = (KeyStoreConfiguration)comboAlmacen.getSelectedItem();
            AOKeyStore store = kssc.getType();
            String lib = kssc.getLib();
            if (store == AOKeyStore.WINDOWS || 
                store == AOKeyStore.WINROOT ||
                store == AOKeyStore.SINGLE) { 
                pssCallback = new NullPasswordCallback();
            }
            else if(store==AOKeyStore.PKCS12){
            	pssCallback = new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription() + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", SwingUtilities.getRoot(this), //$NON-NLS-1$
            			Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$ //$NON-NLS-2$
            	File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Open.repository"), (ExtFilter)Utils.getRepositoryFileFilter()); //$NON-NLS-1$
                if (selectedFile != null) {
                	lib = selectedFile.getAbsolutePath();
                } 
                else {
                	return;
                }
            }
            else {
            	pssCallback = new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription() + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", SwingUtilities.getRoot(this), //$NON-NLS-1$
            			Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$ //$NON-NLS-2$
            }

            try {
                keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
                        store,
                        lib,
                        kssc.toString(),
                        pssCallback,
                        this
                );
            } 
            catch (InvalidKeyException e) {
            	//Control de la excepcion generada al introducir mal la contrasena para el almacén
            	CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                return;
            }  
            catch (AOKeystoreAlternativeException e) {
            	CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.almacen"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                 return;
            }

            // Seleccionamos un certificado
            String selectedcert = Utils.showCertSelectionDialog(keyStoreManager.getAliases(), keyStoreManager, SwingUtilities.getRoot(this), true, true, true, new Vector<CertificateFilter>(0),
                    false);

            // Comprobamos si se ha cancelado la seleccion
            if (selectedcert == null) 
                throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$

            // Recuperamos la clave del certificado
            PrivateKeyEntry privateKeyEntry = null;
            try {
                privateKeyEntry = keyStoreManager.getKeyEntry(selectedcert, Utils.getCertificatePC(store, SwingUtilities.getRoot(this)));
            }
            catch (KeyException e) {
            	//Control de la excepcion generada al introducir mal la contrasena para el certificado
            	CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                return;
            }
            catch (AOCancelledOperationException e) {
                // Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
                // Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
                // que las relanza en forma de AOException
                throw e;
            }
            catch (Exception e) {
                throw new AOException("No se ha podido recuperar el certificado seleccionado: " + selectedcert); //$NON-NLS-1$
            }

            if (privateKeyEntry == null) {
                throw new KeyException("No se pudo obtener la informacion del certificado, no se firmara el fichero"); //$NON-NLS-1$
            }

            // Firmamos los datos
            AOSigner signer = null;
            try {
                signer = AOSignerFactory.getSigner(formato);
            }
            catch (Exception e) {
                logger.warning("Formato de firma no soportado: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.formato"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$//$NON-NLS-2$
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                return;
            }

            URI uri = null;
            try {
                uri = AOUtil.createURI(campoFichero.getText());
            } 
            catch (Exception e) {
                logger.severe("La ruta del fichero de datos no es v\u00E1lida: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.ruta"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                campoFichero.requestFocusInWindow();
                return;
            }

            byte[] fileData;
            InputStream fileIn = null;
            try {
                fileIn = AOUtil.loadFile(uri);
                fileData = AOUtil.getDataFromInputStream(fileIn);					
            }
            catch (FileNotFoundException e) {
            	CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero.noencontrado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
                return;
            }
            catch (IOException e) {
            	CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero.leer"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
                return;
            }
            catch (AOException e) {
                throw e;
            } 
            finally {
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                if (fileIn != null) {
                    try { 
                        fileIn.close(); 
                    } 
                    catch (Exception e) {
                        /* Ignorada */ 
                    }
                }
            }

            // En el caso de firma CAdES, preguntamos al usuario si desea incluir el documento que
            // se firma en la propia firma. El documento se incluira en la firma, salvo que se indique
            // los contrario
            String modoFirma = AOSignConstants.SIGN_MODE_IMPLICIT;
            if (formato.equals(AOSignConstants.SIGN_FORMAT_CADES)){ 
                int incluir = CustomDialog.showConfirmDialog(SwingUtilities.getRoot(this), true,Messages.getString("Firma.incluir.original"), //$NON-NLS-1$
                        "Firma",
                        JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE);
                modoFirma = (incluir == JOptionPane.NO_OPTION ? AOSignConstants.SIGN_MODE_EXPLICIT : AOSignConstants.SIGN_MODE_IMPLICIT);
            }

            Properties prop = GeneralConfig.getSignConfig();

            prop.setProperty("format", formato); //$NON-NLS-1$
            prop.setProperty("mode", modoFirma); //$NON-NLS-1$
            prop.setProperty("uri", uri.toASCIIString()); //$NON-NLS-1$

            byte[] signedData = null;
            try {
                signedData = signer.sign(
                        fileData,
                        GeneralConfig.getSignAlgorithm(),
                        privateKeyEntry,
                        prop
                );
            } 
            catch (AOFormatFileException e) {
                logger.severe("Error al generar la firma electronica: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.generar.formato"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                return;
            } 
            catch (AOException e) {
                logger.severe("Error al generar la firma electronica: " + e); //$NON-NLS-1$
                e.printStackTrace();
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.generar.firma"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                return;
            } 
            catch (Exception e) {
                logger.severe("Error al generar la firma electronica: " + e); //$NON-NLS-1$
                CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.generar.firma"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
                return;
            }

            // Si el proceso de firma devuelve una firma nula o vacia, lanzamos una excepcion
            if (signedData == null || signedData.length == 0) {
                throw new AOException("La firma generada esta vacia"); //$NON-NLS-1$
            }

            // Guardamos la firma en fichero
            final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("Firma.filechooser.save.title"), signedData,
                    SignedFileManager.getOutFileName(this.getFilename(campoFichero.getText()), formato),
                    SignedFileManager.getOutFileFilter(formato),
                    this);

            if (savedFile!= null) {
            	CustomDialog.showMessageDialog(
            			SwingUtilities.getRoot(this), true,
                        Messages.getString("Firma.msg.ok"),  //$NON-NLS-1$
                        Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma"),  //$NON-NLS-1$
                        JOptionPane.INFORMATION_MESSAGE
                );
            }
        } 
        catch (AOCancelledOperationException e) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
        } 
        catch (AOException e) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            logger.severe("Error: " + e.getMessage()); //$NON-NLS-1$
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this), true, e.getMessage(), "Firma", JOptionPane.ERROR_MESSAGE);
        } 
        catch(Exception e) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            logger.severe("Error al generar la firma electronica: "+e); //$NON-NLS-1$
        }
    }

    /**
     * Obtiene el nombre del archivo desde el path
     * @param path	Path del archivo
     * @return		Nombre del archivo
     */
    private String getFilename(String path) {
        int i = path.lastIndexOf(System.getProperty("file.separator")); //$NON-NLS-1$
        if (i > 0 && i <path.length()-1) {
            return path.substring(i+1);
        }
        return path;
    }

    /**
     * Carga el combo de almacen y repositorios
     * @param comboAlmacen	Combo donde se guarda la lista
     */
    private void cargarComboAlmacen(JComboBox comboAlmacen){
        comboAlmacen.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
    }
}
