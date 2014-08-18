/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizarddescifradoclave;

import java.awt.BorderLayout;
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
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.Caret;

import es.gob.afirma.ciphers.AOCipherKeyStoreHelper;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
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
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Clase que muestra el contenido principal del descifrado de una clave. */
final class PanelClave extends JAccessibilityDialogWizard {
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
            continuar = descifrarFichero();

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
    private static final Logger LOGGER = Logger.getLogger(PanelClave.class.getName());

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Campo donde se guarda la contrasenia. */
    private final JTextField campoClave = new JTextField();

    /** Cifrador configurado para un algoritmo dado */
    private final CipherConfig cipherConfig;

    /** Ruta donde se encuentra el archivo a cifrar */
    private final String rutaFichero;

    /** Constructor.
     * @param algoritmo
     * @param rutaFichero */
    public PanelClave(final String algoritmo, final String rutaFichero) {
        this.cipherConfig = new CipherConfig(algoritmo);
        this.rutaFichero = rutaFichero;
        initComponents();
    }

    /** Descifra un fichero dado
     * @return true o false indicando si se ha descifrado correctamente */
    public boolean descifrarFichero() {
    	// Comprobamos si se ha indicado un fichero de datos
    	if (this.rutaFichero == null) {
            LOGGER.warning("No se ha indicado un fichero de datos"); //$NON-NLS-1$
            CustomDialog.showMessageDialog(
        		this,
                true,
                Messages.getString("Descifrado.msg.fichero"), //$NON-NLS-1$
                Messages.getString("Descifrado.btndescifrar"), //$NON-NLS-1$
                JOptionPane.WARNING_MESSAGE
            );
            return false;
    	}
        // Recuperamos la clave
        final String clave = this.campoClave.getText();

        if (clave == null || clave.equals("")) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.clave"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }
        final byte[] fileContent;
        try {
            fileContent = this.getFileContent();
        }
        catch (final FileNotFoundException ex) {
            LOGGER.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero2"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }
        catch (final Exception ex) {
            LOGGER.warning("Ocurri\u00F3 un error durante la lectura del fichero de datos: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero2"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }

        byte[] result = null;
        try {
            final Key tmpKey = this.cipherConfig.getCipher().decodeKey(Base64.decode(clave), this.cipherConfig.getConfig(), null);
            result = this.cipherConfig.getCipher().decipher(fileContent, this.cipherConfig.getConfig(), tmpKey);
        }
        catch (final InvalidKeyException e) {
            LOGGER.severe("Clave no valida: " + e); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.clave"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }
        catch (final KeyException ex) {
            LOGGER.severe("Error al descifrar, compruebe que el fichero esta cifrado con el algoritmo seleccionado: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.malcifrado"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
            return false;
        }
        catch (final Exception ex) {
            LOGGER.severe("Error al descifrar: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.operacion"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // Almacenamos el fichero de salida de la operacion
        final File savedFile =
                SelectionDialog.saveDataToFile(Messages.getString("WizardDescifrado.clave.filechooser.save.title"), result, "fichero", null, this); //$NON-NLS-1$ //$NON-NLS-2$
        if (savedFile == null) {
            return false;
        }

        return true;
    }

    /** Accede al almacen de claves para obtener una */
    void examinarActionPerformed() {
        // Comprobamos que el almacen exista.
        if (!AOCipherKeyStoreHelper.storeExists()) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("WizardDescifrado.msg.error.almacen"), //$NON-NLS-1$
                                           Messages.getString("WizardDescifrado.msg.error.titulo"), //$NON-NLS-1$
                                           JOptionPane.WARNING_MESSAGE);
            return;
        }

        // Mostramos la clave de cifrado recuperada del almacen
        try {
            this.campoClave.setText(getKeyFromCipherKeyStore());
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("El usuario ha cancelado la recuperacion de claves de cifrado del almacen"); //$NON-NLS-1$
        }
        catch (final IOException e) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.contrasenia"), //$NON-NLS-1$
                                           Messages.getString("WizardDescifrado.msg.error.titulo"), //$NON-NLS-1$
                                           JOptionPane.WARNING_MESSAGE);
        }
        catch (final Exception e) {
            CustomDialog.showMessageDialog(this, true, Messages.getString("WizardDescifrado.msg.error.clave"), //$NON-NLS-1$
                                           Messages.getString("WizardDescifrado.msg.error.titulo"), //$NON-NLS-1$
                                           JOptionPane.WARNING_MESSAGE);
        }
    }

    /** Getter para el campo de la clave.
     * @return Campo de la clave. */
    public JTextField getCampoClave() {
        return this.campoClave;
    }

    /** Obtiene el contenido del fichero seleccionado por el usuario.
     * @return Contenido del fichero.
     * @throws java.io.FileNotFoundException Cuando no se encuentra o no puede leerse el fichero seleccionado.
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
     * y seleccione la clave que desea recuperar del mismo.
     * @return Clave en base 64.
     * @throws AOException Ocurri&oacute; un error durate el proceso de configuraci&oacute;n.
     * @throws IOException Cuando no se indique la contrase&ntilde;a correcta del almacen.
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
                Messages.getString("WizardDescifrado.clave.pass"), //$NON-NLS-1$
                KeyEvent.VK_O,
                Messages.getString("CustomDialog.showInputPasswordDialog.title"), //$NON-NLS-1$
                JOptionPane.QUESTION_MESSAGE
            )
        );

        // Le pedimos el alias de la clave de cifrado al usuario
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

    /** Inicializacion de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("WizardDescifrado.titulo")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior =
                new CabeceraAsistente("WizardDescifrado.clave.explicacion.titulo", "WizardDescifrado.clave.explicacion", null); //$NON-NLS-1$ //$NON-NLS-2$
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
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;

        // Etiqueta que contiene el texto "Introduzca la clave con..."
        final InfoLabel insertLabel = new InfoLabel(Messages.getString("WizardDescifrado.clave.contenido.texto1"), false); //$NON-NLS-1$
        panelCentral.add(insertLabel, c);

        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 1;
        c.weightx = 1.0;
        c.gridy = 1;

        // Etiqueta con el texto Clave de descifrado
        final JLabel keyLabel = new JLabel(Messages.getString("WizardDescifrado.clave")); //$NON-NLS-1$
        Utils.setContrastColor(keyLabel);
        Utils.setFontBold(keyLabel);
        panelCentral.add(keyLabel, c);

        c.insets = new Insets(0, 20, 0, 20);
        c.gridwidth = 1;
        c.weightx = 1.0;
        c.gridy = 2;

        // Caja de texto donde se guarda la clave
        this.campoClave.setToolTipText(Messages.getString("WizardDescifrado.clave.contrasenia.description")); // NOI18N //$NON-NLS-1$
        this.campoClave.getAccessibleContext().setAccessibleName(keyLabel.getText() + " " + this.campoClave.getToolTipText() + "ALT + L."); //$NON-NLS-1$ //$NON-NLS-2$
        this.campoClave.getAccessibleContext().setAccessibleDescription(this.campoClave.getToolTipText());
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoClave.setCaret(caret);
        }
        Utils.remarcar(this.campoClave);
        Utils.setContrastColor(this.campoClave);
        Utils.setFontBold(this.campoClave);
        panelCentral.add(this.campoClave, c);

        // Relacion entre etiqueta y campo de texto
        keyLabel.setLabelFor(this.campoClave);
        // Asignacion de mnemonico
        keyLabel.setDisplayedMnemonic(KeyEvent.VK_L);

        c.insets = new Insets(0, 20, 0, 20);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton para examinar el almacen
        final JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setToolTipText(Messages.getString("WizardDescifrado.clave.boton.description")); // NOI18N //$NON-NLS-1$
        examinar.setText(Messages.getString("WizardDescifrado.clave.boton")); // NOI18N //$NON-NLS-1$
        examinar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarActionPerformed();
            }
        });
        examinar.getAccessibleContext().setAccessibleName(examinar.getText() + " " + examinar.getToolTipText()); // NOI18N //$NON-NLS-1$
        examinar.getAccessibleContext().setAccessibleDescription(examinar.getToolTipText()); // NOI18N
        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);
        panelExaminar.add(examinar);
        panelCentral.add(panelExaminar, c);

        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 2;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridy = 3;
        c.gridx = 0;

        // Etiqueta que contiene el texto "En caso de obtener..."
        final InfoLabel endLabel = new InfoLabel(Messages.getString("WizardDescifrado.clave.contenido.texto5"), false); //$NON-NLS-1$
        panelCentral.add(endLabel, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoClave, "descifrado.wizard.clave"); //$NON-NLS-1$
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    public void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 1));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}