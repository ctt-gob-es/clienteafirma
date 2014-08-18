/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardcifradoclave;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.Key;
import java.security.KeyException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.Caret;

import es.gob.afirma.ciphers.AOCipherKeyStoreHelper;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.ui.utils.CipherConfig;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Dialogo con la pagina 2: Clave de cifrado */
final class PanelClaveCifrado extends JAccessibilityDialogWizard {
    /** Botonera con funciones para la pagina panel de cifrado */
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
            continuar = cifrarFichero();

            if (continuar) {
                super.siguienteActionPerformed(anterior, siguiente, finalizar);
            }
            else {
                // Si ha ocurrido algun error durante el proceso de cifrado mediante clave
                // el foco vuelve al campo de insercion de clave
                getCampoClave().requestFocusInWindow();
            }
        }
    }

    /** Log. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Campo donde se guarda la clave generada. */
    private final JTextField campoClave = new JTextField();

    /** Check que indica si se debe guardar la clave en el almacen. */
    private final JCheckBox checkGuardar = new JCheckBox();

    /** Cifrador configurado para un algoritmo dado */
    private final CipherConfig cipherConfig;

    /** Clave de cifrado */
    private Key cipherKey;

    /** Ruta donde se encuentra el archivo a cifrar */
    private final String rutaFichero;

    /** Constructor.
     * @param algoritmo
     * @param rutaFichero */
    public PanelClaveCifrado(final String algoritmo, final String rutaFichero) {
        this.cipherConfig = new CipherConfig(algoritmo);
        this.rutaFichero = rutaFichero;
        initComponents();
    }

    /** @return the checkGuardar */
    protected JCheckBox getCheckGuardar() {
        return this.checkGuardar;
    }

    /** Obtenemos una clase del almacen/repositorio */
    void almacenActionPerformed() {
        // Comprobamos que el almacen exista.
        if (!AOCipherKeyStoreHelper.storeExists()) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.almacen.noexiste"), //$NON-NLS-1$
                                           Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$
            return;
        }

        // Mostramos la clave de cifrado recuperada del almacen
        try {
            this.campoClave.setText(getKeyFromCipherKeyStore());
            this.checkGuardar.setEnabled(false);
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.warning("El usuario ha cancelado la recuperacion de claves de cifrado del almacen" //$NON-NLS-1$
            );
        }
        catch (final IOException e) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.msg.error.contrasenia"), //$NON-NLS-1$
                                           Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$
        }
        catch (final AOException e) {
            CustomDialog.showMessageDialog(this,
                                           true,
                                           e.getMessage(),
                                           Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$
        }
        catch (final Exception e) {
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("WizardCifrado.almacen.error.clave"), Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Genera la clave */
    void autogenerarActionPerformed() {
        try {
            generateKey(this.cipherConfig.getConfig());
        }
        catch (final Exception ex) {
            LOGGER.log(Level.SEVERE, null, ex);
        }

        this.campoClave.setText(Base64.encode(this.cipherKey.getEncoded()));
        this.checkGuardar.setEnabled(true);
    }

    /** Cifra un fichero dado
     * @return true o false indicando si se ha cifrado correctamente */
    boolean cifrarFichero() {
    	// Comprobamos si se ha indicado un fichero de datos
    	if (this.rutaFichero == null) {
            LOGGER.warning("No se ha indicado un fichero de datos"); //$NON-NLS-1$
            CustomDialog.showMessageDialog(
        		this,
                true,
                Messages.getString("Cifrado.msg.error.fichero"), //$NON-NLS-1$
                Messages.getString("Cifrado.msg.titulo"), //$NON-NLS-1$
                JOptionPane.WARNING_MESSAGE
            );
            return false;
    	}

        // Comprobamos si se ha generado alguna clave
        if (this.campoClave.getText() == null || this.campoClave.getText().equals("")) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(
        		this,
                true,
                Messages.getString("Cifrado.msg.clave"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return false;
        }
        // Generamos la clave necesaria para el cifrado
        try {
            this.cipherKey = this.cipherConfig.getCipher().decodeKey(Base64.decode(this.campoClave.getText()), this.cipherConfig.getConfig(), null);
        }
        catch (final Exception ex) {
            LOGGER.severe("Error durante el proceso de generacion de claves: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(
        		this,
        		true,
        		Messages.getString("Cifrado.msg.error.cifrado"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return false;
        }

        // Leemos el fichero de datos
        final byte[] fileContent;
        try {
            fileContent = getFileContent();
        }
        catch (final FileNotFoundException ex) {
            LOGGER.warning("No se encuentra el fichero: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(
        		this,
                true,
                Messages.getString("Cifrado.msg.error.lectura"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return false;
        }
        catch (final Exception ex) {
            LOGGER.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Cifrado.msg.error.lectura"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }

        // Ciframos los datos
        byte[] result = null;
        try {
            result = this.cipherConfig.getCipher().cipher(fileContent, this.cipherConfig.getConfig(), this.cipherKey);
        }
        catch (final KeyException e) {
            LOGGER.severe("Clave no valida: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.clave"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }
        catch (final Exception ex) {
            LOGGER.warning("Error al cifrar: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.operacion"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$

            return false;
        }

        // Guardamos los datos
        if (result == null) {
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Cifrado.msg.error.noresultado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$//$NON-NLS-2$
            return false;
        }
        // Almacenamos el fichero de salida de la operacion
        final File savedFile =
                SelectionDialog.saveDataToFile(Messages.getString("WizardCifrado.clave.filechooser.save.title"), result, "cifrado", null, this); //$NON-NLS-1$ //$NON-NLS-2$
        if (savedFile == null) {
            return false;
        }

        // Guardamos la clave de cifrado si se solicito
        if (this.checkGuardar.isSelected()) {
            guardarClaveCifrado();
        }

        return true;
    }

    /** Genera una clave v&aacute;lida para un algoritmo de cifrado y la almacena en el repoitorio de @firma si
     * as&iacute; lo indica el usuario.
     * @param algorithmConfig Configuraci&oacute;n de cifrado.
     * @throws NoSuchAlgorithmException Cuando no se reconoce el algoritmo de cifrado
     * @throws java.security.NoSuchProviderException Cuando no se reconoce el proveedor para la generaci&oacute;n de claves
     * @throws java.security.spec.InvalidKeySpecException Cuando la contrase&ntilde;a introducida no cumple los requisitos necesarios
     * @throws AOCipherAlgorithmException Cuando el algoritmo indicado no soporta el modo de generaci&oacute;n de clave
     * @throws AOException Cuando se produce un error al auto generar las claves.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de cifrado no esta soportado. */
    private void generateKey(final AOCipherConfig algorithmConfig) throws AOException, NoSuchAlgorithmException {
        this.cipherKey = this.cipherConfig.getCipher().generateKey(algorithmConfig);
    }

    /** Getter para el campo de la clave.
     * @return Campo de la clave. */
    public JTextField getCampoClave() {
        return this.campoClave;
    }

    /** Obtiene el contenido del fichero seleccionado por el usuario.
     * @return Contenido del fichero.
     * @throws FileNotFoundException Cuando no se encuentra o no puede leerse el fichero seleccionado.
     * @throws IOException Cuando ocurre un error durante la lectura de un fichero local.
     * @throws AOException Cuando ocurre un error al formar una ruta remota o al leer un fichero remoto.
     * @throws URISyntaxException Si la ruta del fichero no es v&aacute;lida */
    private byte[] getFileContent() throws IOException, AOException, URISyntaxException {
        if (this.rutaFichero == null) {
            throw new IllegalArgumentException("No se ha indicado un fichero de entrada"); //$NON-NLS-1$
        }
        return AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(this.rutaFichero)));
    }

    /** Obtiene una clave de cifrado en base 64 del almac&eacute;n de claves del usuario.
     * Se pedira al usuario que inserte la contrase&ntilde;a del almac&eacute;n de claves
     * y seleccione la clave que desea recuperar del mismo. Devuelve {@code null} si
     * ocurre un error durante la transformaci&oacute;n a base 64.
     * @return Clave en base 64 o {@code}
     * @throws AOException Ocurri&oacute; un error durate el proceso de configuraci&oacute;n.
     * @throws KeyStoreException
     * @throws CertificateException
     * @throws NoSuchAlgorithmException */
    private String getKeyFromCipherKeyStore() throws AOException, IOException, NoSuchAlgorithmException, CertificateException, KeyStoreException {
        // Abrimos el Almacen de claves de cifrado preguntandole al usuario la clave si no
        // la indico
        final AOCipherKeyStoreHelper cKs = new AOCipherKeyStoreHelper(
    		CustomDialog.showInputPasswordDialog(
				this,
				true,
                null,
                false,
                Messages.getString("WizardCifrado.almacen.claves.contrasenia"), //$NON-NLS-1$
                KeyEvent.VK_O,
                Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                JOptionPane.QUESTION_MESSAGE
            )
        );

        // Le pedimos el alias de laclave de cifrado al usuario
        final String alias = (String) CustomDialog.showInputDialog(
        		this,
        		true,
        		"Seleccione la clave de cifrado.",
        		KeyEvent.VK_S,
        		"Selecci\u00F3n de clave",
        		JOptionPane.NO_OPTION,
        		cKs.getAliases(),
        		cKs.getAliases()[0]);
        
        return Base64.encode(cKs.getKey(alias).getEncoded());
    }

    /** Relacion minima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 9;
    }

    /** Almacena la clave de cifrado configurada. */
    private void guardarClaveCifrado() {
        try {
            boolean gainedAccess;
            int tries = 0;
            AOCipherKeyStoreHelper cksh = null;
            do {
                gainedAccess = true;
                tries++;
                try {
                    if (!AOCipherKeyStoreHelper.storeExists()) {
                        cksh =
                                new AOCipherKeyStoreHelper(CustomDialog.showInputPasswordDialog(this,
                                                                                                true,
                                                                                                null,
                                                                                                false,
                                                                                                Messages.getString("Cifrado.introducir.pass"), //$NON-NLS-1$
                                                                                                KeyEvent.VK_O,
                                                                                                Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                                                                                                JOptionPane.QUESTION_MESSAGE));

                    }
                    else {
                        final PasswordCallback pssCallback =
                                new UIPasswordCallbackAccessibility(Messages.getString("Cifrado.introducir.pass.almacen"), this, //$NON-NLS-1$
                                                                    Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$ //$NON-NLS-2$
                        cksh = new AOCipherKeyStoreHelper(pssCallback.getPassword());
                    }
                }
                catch (final IOException e) {
                    if (tries < 3) {
                        CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.msg.error.contrasenia"), //$NON-NLS-1$
                                                       Messages.getString("Cifrado.msg.error.titulo"), //$NON-NLS-1$
                                                       JOptionPane.WARNING_MESSAGE);
                    }
                    else {
                        // Abortamos al tercer intento incorrecto de introducir la clave
                        CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.pass.incorrecto.almacenar"), //$NON-NLS-1$
                                                       Messages.getString("Cifrado.msg.error.titulo"), //$NON-NLS-1$
                                                       JOptionPane.WARNING_MESSAGE);
                        return;
                    }
                    gainedAccess = false;
                }
            } while (!gainedAccess);

            final String alias = CustomDialog.showInputDialog(this, true, Messages.getString("Cifrado.introducir.alias"), //$NON-NLS-1$
                                                              KeyEvent.VK_I,
                                                              Messages.getString("Cifrado.introducir.alias.titulo"), //$NON-NLS-1$
                                                              JOptionPane.QUESTION_MESSAGE);
            if (cksh != null) {
                cksh.storeKey(alias + " (" + this.cipherConfig.getConfig().getAlgorithm().getName() + ")", this.cipherKey); //$NON-NLS-1$ //$NON-NLS-2$
            }
            else {
                CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.clavecifrar"), //$NON-NLS-1$
                                               Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            }
        }
        catch (final AOCancelledOperationException e) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.cancelar"), //$NON-NLS-1$
                                           Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$
        }
        catch (final Exception e) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.clavecifrar"), //$NON-NLS-1$
                                           Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
    }

    /** Inicializacion de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("WizardCifrado.titulo")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardCifrado.explicacion.titulo", "WizardCifrado.explicacion", null); //$NON-NLS-1$ //$NON-NLS-2$
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);

        // Panel central
        final JPanel panelCentral = new JPanel();
        panelCentral.setMinimumSize(new Dimension(603, 289));
        panelCentral.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 20, 0, 20);
        c.weightx = 1.0;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 0;

        // Etiqueta que contiene el texto "Introduzca una clave de..."
        final InfoLabel insertLabel = new InfoLabel(Messages.getString("WizardCifrado.contenido.texto1"), false); //$NON-NLS-1$
        panelCentral.add(insertLabel, c);

        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(0, 20, 0, 20);
        c.gridy = 1;
        c.weighty = 1;

        // Etiqueta que contiene el texto "Adicionalmente, puede almacenar..."
        final InfoLabel lostLabel = new InfoLabel(Messages.getString("WizardCifrado.contenido.texto5"), false); //$NON-NLS-1$
        panelCentral.add(lostLabel, c);

        c.gridy = 2;
        c.gridwidth = 1;
        c.weighty = 0;
        c.insets = new Insets(0, 20, 0, 20);

        // Etiqueta con el texto Clave de cifrado
        final JLabel encodeKeyLabel = new JLabel(Messages.getString("WizardCifrado.claveCifrado")); //$NON-NLS-1$
        Utils.setContrastColor(encodeKeyLabel);
        Utils.setFontBold(encodeKeyLabel);
        panelCentral.add(encodeKeyLabel, c);

        c.gridx = 0;
        c.gridy = 3;
        c.insets = new Insets(0, 20, 0, 0);

        // Caja de texto donde se escribe la clave
        this.campoClave.setToolTipText(Messages.getString("WizardCifrado.campoClave.description")); // NOI18N //$NON-NLS-1$
        this.campoClave.getAccessibleContext().setAccessibleName(encodeKeyLabel.getText() + " " + "ALT + V."); //$NON-NLS-1$ //$NON-NLS-2$
        this.campoClave.getAccessibleContext().setAccessibleDescription(encodeKeyLabel.getToolTipText());
        this.campoClave.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent e) {
                getCheckGuardar().setEnabled(true);
            }
        });
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoClave.setCaret(caret);
        }
        Utils.remarcar(this.campoClave);
        Utils.setContrastColor(this.campoClave);
        Utils.setFontBold(this.campoClave);
        panelCentral.add(this.campoClave, c);

        // Relacion entre etiqueta y campo de texto
        encodeKeyLabel.setLabelFor(this.campoClave);
        // Asignacion de mnemonico
        encodeKeyLabel.setDisplayedMnemonic(KeyEvent.VK_V);

        c.weightx = 0.0;
        c.insets = new Insets(0, 10, 0, 0);
        c.gridx = 1;

        final JPanel panelAutogenerar = new JPanel(new GridLayout(1, 1));
        // Boton autogenerar
        final JButton autogenerar = new JButton();
        autogenerar.setMnemonic(KeyEvent.VK_U);
        autogenerar.setToolTipText(Messages.getString("WizardCifrado.autogenerar.description")); // NOI18N //$NON-NLS-1$
        autogenerar.setText(Messages.getString("WizardCifrado.autogenerar")); // NOI18N //$NON-NLS-1$
        autogenerar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                autogenerarActionPerformed();
            }
        });
        autogenerar.getAccessibleContext().setAccessibleName(autogenerar.getText() + " " + autogenerar.getToolTipText()); // NOI18N //$NON-NLS-1$
        autogenerar.getAccessibleContext().setAccessibleDescription(autogenerar.getToolTipText()); // NOI18N
        Utils.remarcar(autogenerar);
        Utils.setContrastColor(autogenerar);
        Utils.setFontBold(autogenerar);
        panelAutogenerar.add(autogenerar);
        panelCentral.add(panelAutogenerar, c);

        c.insets = new Insets(0, 10, 0, 20);
        c.weightx = 0.0;
        c.gridx = 2;

        final JPanel panelAlmacen = new JPanel(new GridLayout(1, 1));
        // Boton cargar clave del almacen
        final JButton almacen = new JButton();
        almacen.setMnemonic(KeyEvent.VK_L);
        almacen.setToolTipText(Messages.getString("WizardCifrado.almacen.description")); // NOI18N //$NON-NLS-1$
        almacen.setText(Messages.getString("WizardCifrado.almacen")); // NOI18N //$NON-NLS-1$
        almacen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                almacenActionPerformed();
            }
        });
        almacen.getAccessibleContext().setAccessibleName(almacen.getText() + " " + almacen.getToolTipText()); // NOI18N //$NON-NLS-1$
        almacen.getAccessibleContext().setAccessibleDescription(almacen.getToolTipText()); // NOI18N
        Utils.remarcar(almacen);
        Utils.setContrastColor(almacen);
        Utils.setFontBold(almacen);
        panelAlmacen.add(almacen);
        panelCentral.add(panelAlmacen, c);

        c.insets = new Insets(10, 20, 0, 20);
        c.weightx = 1.0;
        c.gridwidth = 3;
        c.gridy = 4;
        c.gridx = 0;

        final JPanel panelCheckGuardar = new JPanel(new GridLayout(1, 1));
        // Checkbox para guardar en el almacen
        this.checkGuardar.setText(Messages.getString("WizardCifrado.check")); // NOI18N //$NON-NLS-1$
        this.checkGuardar.setToolTipText(Messages.getString("WizardCifrado.check.description")); // NOI18N //$NON-NLS-1$
        this.checkGuardar.getAccessibleContext().setAccessibleDescription(this.checkGuardar.getToolTipText()); // NOI18N
        this.checkGuardar.setMnemonic(KeyEvent.VK_G);
        Utils.remarcar(this.checkGuardar);
        Utils.setContrastColor(this.checkGuardar);
        Utils.setFontBold(this.checkGuardar);
        panelCheckGuardar.add(this.checkGuardar);
        panelCentral.add(panelCheckGuardar, c);

        c.gridy = c.gridy + 1;
        c.weighty = 1.0;
        panelCentral.add(new JPanel(), c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // La botonera se carga desde el asistente

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoClave, "cifrado.wizard.autogenerar"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(autogenerar, "cifrado.wizard.autogenerar"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(almacen, "cifrado.wizard.almacen"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkGuardar, "cifrado.wizard.salvar"); //$NON-NLS-1$
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    public void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 1));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}