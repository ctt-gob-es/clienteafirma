/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileFilter;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.Caret;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.KeyStoreConfiguration;
import es.gob.afirma.keystores.main.filters.CertificateFilter;
import es.gob.afirma.massive.MassiveType;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.DirectorySignatureHelperAdv;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.OpenFileMessageDialog;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/** Panel multifirma masiva para el wizard de multifirma masiva.
 * @author inteco */
class PanelMultifirmaMasiva extends JAccessibilityDialogWizard {
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

            boolean continuar = true;
            continuar = multifirmarFicheros();

            if (continuar) {
                super.siguienteActionPerformed(anterior, siguiente, finalizar);
            }
        }
    }
    /** Clave para el filtrado de ficheros seg&uacute;n su extensi&oacute;n. */
    private class ExtensionsFileFilter implements java.io.FileFilter {

        private final String[] exts;

        ExtensionsFileFilter(final String[] extensions) {
            this.exts = extensions.clone();
        }

        /** Accion aceptar. */
        @Override
        public boolean accept(final File file) {
            if (file.isDirectory()) {
                return true;
            }
            // getExtension() pasa la extension a minusculas
            final String extension = getExtension(file);
            for (final String extension2 : this.exts) {
                if (extension2.equalsIgnoreCase(extension)) {
                    return true;
                }
            }
            return false;
        }

        /** Devuelve la extensi&oacute;n de un fichero.
         * @param f
         *        Fichero del cual queremos conocer la extensi&oacute;n
         * @return Extensi&oacute;n del fichero o cadena vac&iacute;a si este no
         *         tiene extensi&oacute;n */
        private final String getExtension(final File f) {
            final String s = f.getName();
            final int i = s.lastIndexOf('.');
            if (i > 0 && i < s.length() - 1) {
                return s.substring(i + 1).toLowerCase();
            }
            return ""; //$NON-NLS-1$
        }
    }

    /** Log. */
    private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Algoritmo utilizado para firmar */
    private String algoritmo;

    /** Indica si debe emitir un beep al finalizar */
    private final boolean beep;

    // Caja de texto donde se guarda el nombre del directorio de firmas
    private final JTextField campoDirectorio = new JTextField();

    // Caja de texto donde se guarda el nombre del fichero log
    private final JTextField campoFicheroLog = new JTextField();

    // Checkbox con el texto "Sobrescribir ficheros"
    private final JCheckBox checkSobrescribir = new JCheckBox();

    /** Ruta del directorio de entrada */
    private String directorioEntrada;

    // Etiqueta con el texto "Fichero de log"
    private final JLabel etiquetaFichero = new JLabel();

    // Boton para seleccionar el fichero de log
    private JButton examinarFichero = new JButton();

    /** Cadena con las extensiones de los ficheros a firmar */
    private String extensiones;

    /** Indica si se deben incluir los subdirectorio del directorio de entrada */
    private boolean incluir;

    /** Configuracion del KeyStore */
    private KeyStoreConfiguration kssc = null;

    /** Indica si se esta firmando un documento en CADES implicito o explicito
     * Si es true - implicito, si es false - explicito */
    private boolean modoFormato = true;

    /** Indica si debe respetar el formato original */
    private boolean respetar = true;

    /** Tipo de firma a desarrollar */
    private int tipo;

    /** Tipo de contrafirma (todos o ultimos) */
    private boolean tipoContrafirma = false;

    /** Carga la pagina con los parametros
     * @param kssc Configuracion del KeyStore
     * @param beep Indica si debe emitir un "beep" al finalizar */
    PanelMultifirmaMasiva(final KeyStoreConfiguration kssc, final boolean beep) {
        this.kssc = kssc;
        this.beep = beep;
        initComponents();
    }

    /** Comprueba que el archivo seleccionado es correcto y guarda su nombre en el campo de texto.
     * Tambien genera el nombre del fichero log y lo guarda en su respectivo campo. */
    void examinarDirectorioActionPerformed() {
        final File selectedFile = SelectionDialog.showDirOpenDialog(this, Messages.getString("PrincipalGUI.chooser.dir.outtitle")); //$NON-NLS-1$
        if (selectedFile != null) {
            this.campoDirectorio.setText(selectedFile.getAbsolutePath());
            this.campoFicheroLog.setText(new File(selectedFile.getAbsoluteFile().getParent(), "result.txt").getAbsolutePath());
        }

        // Asignacion de mnemonico
        this.etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_F);
        // Activamos el boton de examinar el directorio del log y el campo para introducirlo
        this.examinarFichero.setEnabled(true);
        this.examinarFichero.setMnemonic(KeyEvent.VK_X); // mnemonico asignado puesto que se habilita el boton
        this.campoFicheroLog.setEnabled(true);
        this.etiquetaFichero.setFocusable(false); // Ahora el elemento focusable sera el campo de texto, no la etiqueta
    }

    /** Comprueba que el archivo log seleccionado es correcto y guarda su nombre en el campo de texto */
    void examinarFicheroLogActionPerformed() {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Wizard.multifirma.chooserLog.tittle")); //$NON-NLS-1$
        if (selectedFile != null) {
            this.campoFicheroLog.setText(selectedFile.getAbsolutePath());
        }
    }

    /** Filtro para extensiones
     * @param extensiones1 extensiones recogidas del wizard
     * @return filtro de extensiones */
    private FileFilter getExtensionFileFilter(final String extensiones1) {
        if (extensiones1 == null || extensiones1.trim().equals("")) { //$NON-NLS-1$
            return null;
        }
        return new ExtensionsFileFilter(extensiones1.split(","));
    }

    /** Obtiene el tipo de multifirma que se est&aacute; realizando
     * @param tipo tipo de firma
     * @param hojas si se han de firmar las hojas
     * @return tipo de firma a realizar. */
    private static MassiveType getMassiveOperationType(final int tipo, final boolean hojas) {
        MassiveType operation = null;
        switch (tipo) {
            case 0:
                operation = MassiveType.SIGN;
                break;
            case 1:
                operation = MassiveType.COSIGN;
                break;
            case 2:
                operation = hojas ? MassiveType.COUNTERSIGN_LEAFS : MassiveType.COUNTERSIGN_ALL;
                break;
        }
        return operation;
    }

    /** Relacion minima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 9;
    }
    /** Inicializacion de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("Wizard.multifirma.titulo.ventana")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior =
            new CabeceraAsistente("Wizard.multifirma.ventana4.titulo", "Wizard.multifirma.ventana4.titulo.descripcion", null, true); //$NON-NLS-1$ //$NON-NLS-2$
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
        c.gridwidth = 2;
        c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;

        // Etiqueta con el texto "Directorio de firmas"
        final JLabel etiquetaFirma = new JLabel();
        etiquetaFirma.setText(Messages.getString("Wizard.multifirma.ventana4.directorio")); //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFirma);
        Utils.setFontBold(etiquetaFirma);
        panelCentral.add(etiquetaFirma, c);

        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = 1;
        c.gridy = 1;
        c.gridx = 0;

        // Caja de texto donde se guarda el nombre del directorio de firmas
        this.campoDirectorio.setToolTipText(Messages.getString("Wizard.multifirma.ventana4.directorio.description")); //$NON-NLS-1$
        this.campoDirectorio.getAccessibleContext().setAccessibleName(etiquetaFirma.getText() + " "
                                                                      + this.campoDirectorio.getToolTipText()
                                                                      + "ALT + D");
        this.campoDirectorio.getAccessibleContext().setAccessibleDescription(this.campoDirectorio.getToolTipText());
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoDirectorio.setCaret(caret);
        }
        Utils.remarcar(this.campoDirectorio);
        Utils.setContrastColor(this.campoDirectorio);
        Utils.setFontBold(this.campoDirectorio);
        panelCentral.add(this.campoDirectorio, c);

        // Relacion entre etiqueta y campo de texto
        etiquetaFirma.setLabelFor(this.campoDirectorio);
        // Asignacion de mnemonico
        etiquetaFirma.setDisplayedMnemonic(KeyEvent.VK_D);

        c.insets = new Insets(0, 10, 0, 20);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminarDirectorio = new JPanel(new GridLayout(1, 1));
        // Boton examinar directorio firmas
        final JButton examinarDirectorio = new JButton();
        examinarDirectorio.setMnemonic(KeyEvent.VK_E);
        examinarDirectorio.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        examinarDirectorio.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        examinarDirectorio.getAccessibleContext().setAccessibleName(examinarDirectorio.getText() + " " + examinarDirectorio.getToolTipText());
        examinarDirectorio.getAccessibleContext().setAccessibleDescription(examinarDirectorio.getToolTipText());
        examinarDirectorio.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarDirectorioActionPerformed();
            }
        });
        Utils.remarcar(examinarDirectorio);
        Utils.setContrastColor(examinarDirectorio);
        Utils.setFontBold(examinarDirectorio);
        panelExaminarDirectorio.add(examinarDirectorio);
        panelCentral.add(panelExaminarDirectorio, c);

        c.insets = new Insets(5, 20, 0, 20);
        c.weightx = 1.0;
        c.gridx = 0;
        c.gridy = 2;

        final JPanel panelCheckSobrescribir = new JPanel(new GridLayout(1, 1));
        // Checkbox con el texto "Sobrescribir ficheros"
        this.checkSobrescribir.setText(Messages.getString("Wizard.multifirma.ventana4.check.sobrescribir")); //$NON-NLS-1$
        this.checkSobrescribir.getAccessibleContext()
        .setAccessibleName(this.checkSobrescribir.getText() + " "
                           + Messages.getString("Wizard.multifirma.ventana4.check.sobrescribir.description")); //$NON-NLS-1$
        this.checkSobrescribir.getAccessibleContext()
        .setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana4.check.sobrescribir.description")); //$NON-NLS-1$
        this.checkSobrescribir.setMnemonic(KeyEvent.VK_O); // Se asigna un atajo al checkbox
        Utils.remarcar(this.checkSobrescribir);
        Utils.setContrastColor(this.checkSobrescribir);
        Utils.setFontBold(this.checkSobrescribir);
        panelCheckSobrescribir.add(this.checkSobrescribir);
        panelCentral.add(panelCheckSobrescribir, c);

        c.insets = new Insets(20, 20, 0, 20);
        c.gridy = 3;

        // Etiqueta con el texto "Fichero de log"
        this.etiquetaFichero.setText(Messages.getString("Wizard.multifirma.ventana4.log")); //$NON-NLS-1$
        this.etiquetaFichero.setFocusable(true); // Se hace focusable por temas de accesibilidad
        this.etiquetaFichero.getAccessibleContext()
        .setAccessibleName(this.etiquetaFichero.getText() + Messages.getString("Wizard.multifirma.ventana4.log.description") //$NON-NLS-1$
                           + " "
                           + Messages.getString("Wizard.multifirma.chooserLog.disabled")); //$NON-NLS-1$
        Utils.setContrastColor(this.etiquetaFichero);
        Utils.setFontBold(this.etiquetaFichero);
        panelCentral.add(this.etiquetaFichero, c);

        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = 1;
        c.gridy = 4;

        // Caja de texto donde se guarda el nombre del fichero log
        this.campoFicheroLog.setEnabled(false);
        this.campoFicheroLog.setToolTipText(Messages.getString("Wizard.multifirma.ventana4.log.description")); //$NON-NLS-1$
        this.campoFicheroLog.getAccessibleContext().setAccessibleName(this.etiquetaFichero.getText() + " "
                                                                      + this.campoFicheroLog.getToolTipText()
                                                                      + "ALT + F");
        this.campoFicheroLog.getAccessibleContext().setAccessibleDescription(this.etiquetaFichero.getToolTipText());
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoFicheroLog.setCaret(caret);
        }
        Utils.remarcar(this.campoFicheroLog);
        Utils.setContrastColor(this.campoFicheroLog);
        Utils.setFontBold(this.campoFicheroLog);
        panelCentral.add(this.campoFicheroLog, c);

        // Relacion entre etiqueta y campo de texto
        this.etiquetaFichero.setLabelFor(this.campoFicheroLog);

        c.insets = new Insets(0, 10, 0, 20);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminarFichero = new JPanel(new GridLayout(1, 1));
        // Boton examinar fichero log
        this.examinarFichero = new JButton();
        this.examinarFichero.setEnabled(false);
        this.examinarFichero.setMnemonic(0); // mnemonico vacio puesto que por defecto esta deshabilitado
        this.examinarFichero.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        this.examinarFichero.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        this.examinarFichero.getAccessibleContext().setAccessibleName(this.examinarFichero.getText() + " " + this.examinarFichero.getToolTipText());
        this.examinarFichero.getAccessibleContext().setAccessibleDescription(this.examinarFichero.getToolTipText());
        this.examinarFichero.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarFicheroLogActionPerformed();
            }
        });
        Utils.remarcar(this.examinarFichero);
        Utils.setContrastColor(this.examinarFichero);
        Utils.setFontBold(this.examinarFichero);
        panelExaminarFichero.add(this.examinarFichero);
        panelCentral.add(panelExaminarFichero, c);

        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 2;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 5;

        // Panel introducido para poder mantener la linea superior correcta
        final Panel panelVacio = new Panel();
        panelCentral.add(panelVacio, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoDirectorio, "multifirma.masiva.wizard.firma.directoriofirmas"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkSobrescribir, "multifirma.masiva.wizard.firma.sobrescribir"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.campoFicheroLog, "multifirma.masiva.wizard.firma.ficheroLog"); //$NON-NLS-1$
    }
    /** Comprueba si los archivos son correctos
     * @return */
    boolean multifirmarFicheros() {
        // Comprobamos rutas de los ficheros
        final String directorio = this.campoDirectorio.getText();
        if (directorio == null || directorio.equals("")) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.multifirma.error.directorio.destino"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        // Comprobacion de las extensiones.
        final String log = this.campoFicheroLog.getText();
        if (log == null || log.equals("")) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Wizard.multifirma.error.fichero.log"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }

        // Comienza la multifirma
        AOKeyStoreManager keyStoreManager = null;
        PrivateKeyEntry privateKeyEntry = null;

        boolean resultadoFirma = true;
        try {
            PasswordCallback pssCallback;

            final AOKeyStore store = this.kssc.getType();
            String lib = this.kssc.getLib();
            if (store == AOKeyStore.WINDOWS || store == AOKeyStore.WINROOT || store == AOKeyStore.SINGLE) {
                pssCallback = new NullPasswordCallback();
            }
            else if (store == AOKeyStore.PKCS12) {
                pssCallback =
                    new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription() + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", null, //$NON-NLS-1$
                                                        Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$
                final File selectedFile =
                    SelectionDialog.showFileOpenDialog(this, Messages.getString("Open.repository"), (ExtFilter) Utils.getRepositoryFileFilter()); //$NON-NLS-1$
                if (selectedFile != null) {
                    lib = selectedFile.getAbsolutePath();
                }
                else {
                    throw new AOCancelledOperationException("No se ha seleccionado el almac\u00E9n de certificados"); //$NON-NLS-1$
                }
            }
            else {
                pssCallback =
                    new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " //$NON-NLS-1$
                                                        + store.getDescription()
                                                        + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.",
                                                        null,
                                                        Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$ //$NON-NLS-2$
            }
            keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(store, lib, this.kssc.toString(), pssCallback, this);

            // Seleccionamos un certificado
            final String selectedcert =
                Utils.showCertSelectionDialog(keyStoreManager.getAliases(),
                                              keyStoreManager,
                                              this,
                                              true,
                                              true,
                                              true,
                                              new Vector<CertificateFilter>(0),
                                              false);

            // Comprobamos si se ha cancelado la seleccion
            if (selectedcert == null)
            {
                throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$
            }

            // Recuperamos la clave del certificado
            try {
                privateKeyEntry = keyStoreManager.getKeyEntry(selectedcert, Utils.getCertificatePC(store, this));
            }
            catch (final AOCancelledOperationException e) {
                // Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
                // Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
                // que las relanza en forma de AOException
                throw e;
            }
            catch (final Exception e) {
                throw new AOException("No se ha podido extraer el certificado seleccionado."); //$NON-NLS-1$
            }

            if (privateKeyEntry == null) {
                throw new KeyException("No se pudo obtener la informacion del certificado, no se firmara el fichero."); //$NON-NLS-1$
            }
        }
        catch (final AOException e) {
            LOGGER.severe(e.toString());
            // El pop-up muestra el mensaje de la excepcion
            CustomDialog.showMessageDialog(this, true, e.getMessage(), Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }
        catch (final Exception e) {
            LOGGER.severe(e.toString());
            // El pop-up muestra el mensaje de la excepcion
            CustomDialog.showMessageDialog(this, true, e.getMessage(), Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }

        try {
            final DirectorySignatureHelperAdv dSigner =
                new DirectorySignatureHelperAdv(GeneralConfig.getSignAlgorithm(), this.algoritmo, AOSignConstants.SIGN_MODE_IMPLICIT, this);

            // Establecemos el filtro de ficheros por extension
            dSigner.setFileFilter(getExtensionFileFilter(this.extensiones));

            // Indicamos si deseamos sobrescribir ficheros previos de firma que encontremos
            dSigner.setOverwritePreviuosFileSigns(this.checkSobrescribir.isSelected());

            final Properties config = GeneralConfig.getSignConfig();
            config.setProperty("mode", this.modoFormato ? AOSignConstants.SIGN_MODE_IMPLICIT : AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$
            config.setProperty("format", this.algoritmo); //$NON-NLS-1$
            config.setProperty("ignoreStyleSheets", "true"); //$NON-NLS-1$//$NON-NLS-2$

            // Seleccionamos el tipo de operacion
            final MassiveType operation = PanelMultifirmaMasiva.getMassiveOperationType(this.tipo, this.tipoContrafirma);

            // Creamos el archivo de log
            dSigner.setLogPath(this.campoFicheroLog.getText());

            // Ejecutamos la operacion masiva
            resultadoFirma =
                dSigner.massiveSign(operation,
                                    this.directorioEntrada,
                                    this.incluir,
                                    this.campoDirectorio.getText(),
                                    true,
                                    this.respetar,
                                    privateKeyEntry,
                                    config);

            // Hacemos el pitido si es necesario
            if (this.beep) {
                Toolkit.getDefaultToolkit().beep();
            }
        }
        catch (final Exception e) {
            LOGGER.severe(e.toString());
            resultadoFirma = false;
        }

        if (resultadoFirma) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.ok"), //$NON-NLS-1$
                                           Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$
        }
        else {
            OpenFileMessageDialog.show(this, Messages.getString("Wizard.multifirma.ko"), //$NON-NLS-1$
                                       Messages.getString("Wizard.multifirma.ok.titulo"), //$NON-NLS-1$
                                       new File(this.campoFicheroLog.getText()));
        }

        return true;
    }
    void setAlgoritmo(final String algoritmo) {
        this.algoritmo = algoritmo;
    }
    void setDirectorioEntrada(final String directorioEntrada) {
        this.directorioEntrada = directorioEntrada;
    }

    void setExtensiones(final String extensiones) {
        this.extensiones = extensiones;
    }

    void setIncluir(final boolean incluir) {
        this.incluir = incluir;
    }

    void setModoFormato(final boolean modoFormato) {
        this.modoFormato = modoFormato;
    }

    void setRespetar(final boolean respetar) {
        this.respetar = respetar;
    }

    void setTipo(final int tipo) {
        this.tipo = tipo;
    }

    void setTipoContrafirma(final boolean tipoContrafirma) {
        this.tipoContrafirma = tipoContrafirma;
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 4));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}
