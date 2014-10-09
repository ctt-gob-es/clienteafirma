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
import java.io.IOException;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.text.Caret;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.massive.MassiveType;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.CertificateManagerDialog;
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
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Panel multifirma masiva para el wizard de multifirma masiva.
 * @author inteco */
final class PanelMultifirmaMasiva extends JAccessibilityDialogWizard {
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
    private static final class ExtensionsFileFilter implements java.io.FileFilter {

        private final String[] exts;

        ExtensionsFileFilter(final String[] extensions) {
            this.exts = extensions.clone();
            for (int i=0;i<this.exts.length;i++) {
            	if (this.exts[i].startsWith("*.")) { //$NON-NLS-1$
            		this.exts[i] = this.exts[i].substring(2);
            	}
            	else if (this.exts[i].startsWith(".")) { //$NON-NLS-1$
            		this.exts[i] = this.exts[i].substring(1);
            	}
            }
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
        private static String getExtension(final File f) {
            final String s = f.getName();
            final int i = s.lastIndexOf('.');
            if (i > 0 && i < s.length() - 1) {
                return s.substring(i + 1).toLowerCase();
            }
            return ""; //$NON-NLS-1$
        }
    }

    /** Log. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Algoritmo utilizado para firmar */
    private String algoritmo;

    /** Indica si debe emitir un beep al finalizar */
    private final boolean beep;

    /** Caja de texto donde se guarda el nombre del directorio de firmas. */
    private final JTextField campoDirectorio = new JTextField();

    /** Caja de texto donde se guarda el nombre del fichero log. */
    private final JTextField campoFicheroLog = new JTextField();

    /** Checkbox con el texto "Sobrescribir ficheros". */
    private final JCheckBox checkSobrescribir = new JCheckBox();

    /** Ruta del directorio de entrada */
    private String directorioEntrada;

    /** Etiqueta con el texto "Fichero de log". */
    private final JLabel etiquetaFichero = new JLabel();

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

    /** Tipo de contrafirma (todos o &uacute;ltimos) */
    private boolean tipoContrafirma = false;

    /** Carga la p&aacute;gina con los parametros
     * @param kssc Configuraci&oacute;n del KeyStore
     * @param beep Indica si debe emitir un "beep" al finalizar */
    PanelMultifirmaMasiva(final KeyStoreConfiguration kssc, final boolean beep) {
        this.kssc = kssc;
        this.beep = beep;
        initComponents();
    }

    /** Comprueba que el archivo seleccionado es correcto y guarda su nombre en el campo de texto.
     * Tambien genera el nombre del fichero log y lo guarda en su respectivo campo. */
    void examinarDirectorioActionPerformed() {
        final File selectedFile = SelectionDialog.showDirOpenDialog(this, Messages.getString("PrincipalGUI.chooser.dir.outtitle"), Main.getPreferences().get("dialog.load.dir.massiveout", null)); //$NON-NLS-1$ //$NON-NLS-2$
        if (selectedFile != null) {
            this.campoDirectorio.setText(selectedFile.getAbsolutePath());
            Main.getPreferences().put("dialog.load.dir.massiveout", selectedFile.getAbsolutePath()); //$NON-NLS-1$
            if ("".equals(this.campoFicheroLog.getText().trim())) { //$NON-NLS-1$
            	this.campoFicheroLog.setText(new File(selectedFile.getAbsoluteFile().getParent(), "result.txt").getAbsolutePath()); //$NON-NLS-1$
            }
        }
    }

    /** Comprueba que el archivo log seleccionado es correcto y guarda su nombre en el campo de texto */
    void examinarFicheroLogActionPerformed() {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Wizard.multifirma.chooserLog.tittle"), null); //$NON-NLS-1$
        if (selectedFile != null) {
            this.campoFicheroLog.setText(selectedFile.getAbsolutePath());
        }
    }

    /** Filtro para extensiones
     * @param extensiones1 extensiones recogidas del wizard
     * @return filtro de extensiones */
    private static FileFilter getExtensionFileFilter(final String extensiones1) {
        if (extensiones1 == null || extensiones1.trim().equals("")) { //$NON-NLS-1$
            return null;
        }
        return new ExtensionsFileFilter(extensiones1.split(",")); //$NON-NLS-1$
    }

    /** Obtiene el tipo de multifirma que se est&aacute; realizando
     * @param tipo tipo de firma
     * @param hojas si se han de firmar las hojas
     * @return tipo de firma a realizar. */
    private static MassiveType getMassiveOperationType(final int tipo, final boolean hojas) {
        switch (tipo) {
            case 0:
                return MassiveType.SIGN;
            case 1:
                return MassiveType.COSIGN;
            case 2:
                return hojas ? MassiveType.COUNTERSIGN_LEAFS : MassiveType.COUNTERSIGN_ALL;
            default:
            	return null;
        }
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
                new CabeceraAsistente("Wizard.multifirma.ventana4.titulo", "Wizard.multifirma.ventana4.titulo.descripcion", null); //$NON-NLS-1$ //$NON-NLS-2$
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
        this.campoDirectorio.getAccessibleContext().setAccessibleName(etiquetaFirma.getText() + " " //$NON-NLS-1$
                                                                      + this.campoDirectorio.getToolTipText()
                                                                      + "ALT + D"); //$NON-NLS-1$
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
        examinarDirectorio.getAccessibleContext().setAccessibleName(examinarDirectorio.getText() + " " + examinarDirectorio.getToolTipText()); //$NON-NLS-1$
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
                              .setAccessibleName(this.checkSobrescribir.getText() + " " //$NON-NLS-1$
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
        this.etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_F);
        this.etiquetaFichero.setLabelFor(this.campoFicheroLog);
        this.etiquetaFichero.getAccessibleContext().setAccessibleName(
    		this.etiquetaFichero.getText() +
    			Messages.getString("Wizard.multifirma.ventana4.log.description") + //$NON-NLS-1$
    			" " + //$NON-NLS-1$
                Messages.getString("Wizard.multifirma.chooserLog.disabled") //$NON-NLS-1$
        );
        Utils.setContrastColor(this.etiquetaFichero);
        Utils.setFontBold(this.etiquetaFichero);
        panelCentral.add(this.etiquetaFichero, c);

        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = 1;
        c.gridy = 4;

        // Caja de texto donde se guarda el nombre del fichero log
        // Asignacion de mnemonico
        this.campoFicheroLog.setToolTipText(Messages.getString("Wizard.multifirma.ventana4.log.description")); //$NON-NLS-1$
        this.campoFicheroLog.getAccessibleContext().setAccessibleName(
    		this.etiquetaFichero.getText() + " " //$NON-NLS-1$
            	+ this.campoFicheroLog.getToolTipText()
                + "ALT + F" //$NON-NLS-1$
        );
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
        final JButton examinarFichero = new JButton();
        examinarFichero.setMnemonic(KeyEvent.VK_X); // mnemonico asignado puesto que se habilita el boton
        examinarFichero.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        examinarFichero.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        examinarFichero.getAccessibleContext().setAccessibleName(examinarFichero.getText() + " " + examinarFichero.getToolTipText()); //$NON-NLS-1$
        examinarFichero.getAccessibleContext().setAccessibleDescription(examinarFichero.getToolTipText());
        examinarFichero.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarFicheroLogActionPerformed();
            }
        });
        Utils.remarcar(examinarFichero);
        Utils.setContrastColor(examinarFichero);
        Utils.setFontBold(examinarFichero);
        panelExaminarFichero.add(examinarFichero);
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

    /** Multifirma los ficheros del directorio seleccionado.
     * @return {@code true} si la operaci&oacute;n finaliz&oacute; correctamente, {@code false} en caso contrario. */
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
            CustomDialog.showMessageDialog(
        		this,
                true,
                Messages.getString("Wizard.multifirma.error.fichero.log"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE //$NON-NLS-1$ //$NON-NLS-2$
    		);
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
            if (store == AOKeyStore.WINDOWS || store == AOKeyStore.SINGLE) {
                pssCallback = NullPasswordCallback.getInstance();
            }
            else if (store == AOKeyStore.DNIEJAVA) {
                pssCallback = null;
            }
            else if (store == AOKeyStore.PKCS12) {
                pssCallback =
                        new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " + store.getName() + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", null, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                                            Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$ //$NON-NLS-2$
                final File selectedFile =
                        SelectionDialog.showFileOpenDialog(this, Messages.getString("Open.repository.pkcs12"), Main.getPreferences().get("dialog.load.repository.pkcs12", null), (ExtFilter) Utils.getRepositoryFileFilterPkcs12()); //$NON-NLS-1$ //$NON-NLS-2$
                if (selectedFile != null) {
                    lib = selectedFile.getAbsolutePath();
                    Main.getPreferences().put("dialog.load.repository.pkcs12", lib); //$NON-NLS-1$
                }
                else {
                    throw new AOCancelledOperationException("No se ha seleccionado el almac\u00E9n de certificados"); //$NON-NLS-1$
                }
            }
            else {
                pssCallback =
                        new UIPasswordCallbackAccessibility(Messages.getString("Msg.pedir.contraenia") + " " //$NON-NLS-1$ //$NON-NLS-2$
                                                                    + store.getName()
                                                                    + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", //$NON-NLS-1$
                                                            null,
                                                            Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$ //$NON-NLS-2$
            }
            keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(store, lib, this.kssc.toString(), pssCallback, this);

            // Recuperamos la clave del certificado
            try {
            	privateKeyEntry = new CertificateManagerDialog().show(this, keyStoreManager);
            }
            catch (final AOCancelledOperationException e) {
                // Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
                // Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
                // que las relanza en forma de AOException
                throw e;
            }
            catch (final AOCertificatesNotFoundException e) {
                LOGGER.severe("No se han encontrado certificados validos en el almacen: " + e); //$NON-NLS-1$
                throw e;
            }
            catch (final Exception e) {
                throw new AOException("No se ha podido extraer el certificado seleccionado.", e); //$NON-NLS-1$
            }

            if (privateKeyEntry == null) {
                throw new KeyException("No se pudo obtener la informacion del certificado, no se firmara el fichero."); //$NON-NLS-1$
            }
        }
        catch (final java.security.ProviderException e) {
        	// Comprobacion especifica para el proveedor Java de DNIe
        	if (e.getCause() != null && e.getCause().getClass().getName().equals("es.gob.jmulticard.card.AuthenticationModeLockedException")) { //$NON-NLS-1$
        		CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                        true,
                        Messages.getString("Firma.msg.error.dnie.AuthenticationModeLockedException"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        		return false;
        	}
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                           true,
                                           Messages.getString("Firma.msg.error.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        catch (final AOException e) {
            LOGGER.severe(e.toString());
            // El pop-up muestra el mensaje de la excepcion
            CustomDialog.showMessageDialog(this, true, e.getMessage(), Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }
        catch (final IOException e) {
            LOGGER.severe(e.toString());

        	// Condiciones especificas para el proveedor Java de DNIe
        	String msg = e.getMessage();
        	if (e.getClass().getName().equals("BurnedDnieCardException")) { //$NON-NLS-1$
        		msg = Messages.getString("Firma.msg.error.dnie.BurnedDnieCardException"); //$NON-NLS-1$
        	} else if (e.getClass().getName().equals("es.gob.jmulticard.card.InvalidCardException")) { //$NON-NLS-1$
        		msg = Messages.getString("Firma.msg.error.dnie.InvalidCardException"); //$NON-NLS-1$
        	} else if (e.getClass().getName().equals("es.gob.jmulticard.apdu.connection.CardNotPresentException")) { //$NON-NLS-1$
        		msg = Messages.getString("Firma.msg.error.dnie.CardNotPresentException"); //$NON-NLS-1$
        	} else if (e.getClass().getName().equals("es.gob.jmulticard.apdu.connection.NoReadersFoundException")) { //$NON-NLS-1$
        		msg = Messages.getString("Firma.msg.error.dnie.NoReadersFoundException"); //$NON-NLS-1$
        	}
            CustomDialog.showMessageDialog(this, true, msg, Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
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
                    new DirectorySignatureHelperAdv(GeneralConfig.getSignAlgorithm(), this.algoritmo, AOSignConstants.SIGN_MODE_IMPLICIT);

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
        catch (final Throwable e) {
            LOGGER.severe("Error durante la operacion de firma: " + e.toString()); //$NON-NLS-1$
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