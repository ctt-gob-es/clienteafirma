/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardcifradocontrasenia;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;
import javax.swing.text.Caret;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.ui.core.jse.JSEUIManager;
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

/** Di&aacute;logo con la pagina 2: Clave de cifrado */
final class PanelContrasenia extends JAccessibilityDialogWizard implements KeyListener {

    /** Botonera con funciones para la pagina panel de cifrado */
    private final class Botonera extends BotoneraInferior {

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
                getBotonera().getCancelar().doClick();
            }
        }
    }

    /** Log. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Campo donde se guarda la contrasenia. */
    private final JPasswordField campoContrasenia = new JPasswordField();

    /** Campo donde se guarda la contrasenia repetida. */
    private final JPasswordField campoContraseniaRep = new JPasswordField();

    /** Cifrador configurado para un algoritmo dado */
    private final CipherConfig cipherConfig;

    /** Clave de cifrado */
    private Key cipherKey;

    /** Ruta donde se encuentra el archivo a cifrar */
    private final String rutaFichero;

    /** Constructor.
     * @param algoritmo
     * @param rutaFichero */
    public PanelContrasenia(final String algoritmo, final String rutaFichero) {
        this.cipherConfig = new CipherConfig(algoritmo);
        this.rutaFichero = rutaFichero;
        initComponents();
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
        // Generamos la clave necesaria para el cifrado
        try {
            this.cipherKey = this.cipherConfig.getCipher().decodePassphrase(
        		this.campoContrasenia.getPassword(),
        		this.cipherConfig.getConfig(),
        		null
    		);
        }
        catch (final Exception ex) {
            LOGGER.severe("Error durante el proceso de generacion de claves: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.cifrado"), //$NON-NLS-1$
                                           Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$

            return false;
        }
        // Leemos el fichero de datos
        final byte[] fileContent;
        try {
            fileContent = this.getFileContent();
        }
        catch (final FileNotFoundException ex) {
            LOGGER.warning("No se encuentra el fichero: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Cifrado.msg.error.lectura"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        catch (final Exception ex) {
            LOGGER.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Cifrado.msg.error.lectura"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        catch(final OutOfMemoryError e) {
        	CustomDialog.showMessageDialog(
    			SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero.tamano"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
        }

        final byte[] result;
        try {
            result = this.cipherConfig.getCipher().cipher(fileContent, this.cipherConfig.getConfig(), this.cipherKey);
        }
        catch (final InvalidKeyException ex) {
            LOGGER.severe("No se cumplen con los requisitos de contrasena del algoritmo: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("WizardCifrado.contrasenia.error.requerimientos"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        catch (final Exception ex) {
            LOGGER.warning("Error al cifrar: " + ex); //$NON-NLS-1$
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Cifrado.msg.error.operacion"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$

            return false;
        }

        if (result == null) {
            CustomDialog.showMessageDialog(this,
                                           true,
                                           Messages.getString("Cifrado.msg.error.noresultado"), //$NON-NLS-1$
                                           Messages.getString("error"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
        }
        else {
            // Almacenamos el fichero de salida de la operacion
            final File savedFile =
                SelectionDialog.saveDataToFile(Messages.getString("WizardCifrado.contrasenia.filechooser.save.title"), //$NON-NLS-1$
                                               result,
                                               "cifrado", null, this); //$NON-NLS-1$
            if (savedFile == null) {
                return false;
            }
        }

        return true;
    }

    /** Getter para el campo de contrasenia.
     * @return Campo de contrasenia. */
    public JPasswordField getCampoContrasenia() {
        return this.campoContrasenia;
    }

    JPasswordField getCampoContraseniaRep() {
    	return this.campoContraseniaRep;
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

    /** Relacion minima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 8;
    }

    /** Inicializacion de componentes */
    private void initComponents() {

        // Titulo de la ventana
        setTitulo(Messages.getString("WizardCifrado.titulo")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior =
            new CabeceraAsistente("WizardCifrado.contrasenia.explicacion.titulo", "wizardCifrado.contrasenia.explicacion", null);  //$NON-NLS-1$//$NON-NLS-2$
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
        c.gridx = 0;

        // Etiqueta que contiene el texto "Introduzca una contrasenia de..."
        final InfoLabel insertLabel = new InfoLabel(Messages.getString("WizardCifrado.contrasenia.contenido.texto1"), false); //$NON-NLS-1$
        panelCentral.add(insertLabel, c);

        c.gridy = 1;
        c.insets = new Insets(20, 20, 0, 20);

        // Etiqueta con el texto "Introduzca una contrasenia"
        final JLabel etiquetaContrasenia = new JLabel();
        etiquetaContrasenia.setText(Messages.getString("WizardCifrado.contrasenia")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaContrasenia);
        Utils.setFontBold(etiquetaContrasenia);
        panelCentral.add(etiquetaContrasenia, c);

        c.gridy = 2;
        c.insets = new Insets(5, 20, 0, 20);

        // Caja de texto con la contrasenia
        this.campoContrasenia.addKeyListener(this);
        this.campoContrasenia.setToolTipText(Messages.getString("WizardCifrado.contrasenia.description")); // NOI18N //$NON-NLS-1$
        this.campoContrasenia.getAccessibleContext().setAccessibleName(etiquetaContrasenia.getText() + " "
                                                                       + this.campoContrasenia.getToolTipText()
                                                                       + "ALT + I.");
        this.campoContrasenia.getAccessibleContext().setAccessibleDescription(this.campoContrasenia.getToolTipText());
        this.campoContrasenia.setDocument(new JSEUIManager.JTextFieldASCIIFilter(true));
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoContrasenia.setCaret(caret);
        }
        Utils.remarcar(this.campoContrasenia);
        Utils.setContrastColor(this.campoContrasenia);
        Utils.setFontBold(this.campoContrasenia);
        panelCentral.add(this.campoContrasenia, c);

        // Relacion entre etiqueta y campo de contrasena
        etiquetaContrasenia.setLabelFor(this.campoContrasenia);
        // Asignacion de mnemonico
        etiquetaContrasenia.setDisplayedMnemonic(KeyEvent.VK_I);

        c.gridy = 3;
        c.insets = new Insets(20, 20, 0, 20);

        // Etiqueta con el texto "Introduzca de nuevo..."
        final JLabel etiquetaContraseniaRep = new JLabel();
        etiquetaContraseniaRep.setText(Messages.getString("WizardCifrado.recontrasenia")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaContraseniaRep);
        Utils.setFontBold(etiquetaContraseniaRep);
        panelCentral.add(etiquetaContraseniaRep, c);

        c.gridy = 4;
        c.insets = new Insets(5, 20, 0, 20);

        // Caja de texto con la contrasenia
        this.campoContraseniaRep.addKeyListener(this);
        this.campoContraseniaRep.setToolTipText(Messages.getString("WizardCifrado.recontrasenia.description")); // NOI18N //$NON-NLS-1$
        this.campoContraseniaRep.getAccessibleContext().setAccessibleName(etiquetaContraseniaRep.getText() + " "
                                                                          + this.campoContraseniaRep.getToolTipText()
                                                                          + "ALT+N.");
        this.campoContraseniaRep.getAccessibleContext().setAccessibleDescription(this.campoContraseniaRep.getToolTipText());
        this.campoContraseniaRep.setDocument(new JSEUIManager.JTextFieldASCIIFilter(true));
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoContraseniaRep.setCaret(caret);
        }
        Utils.remarcar(this.campoContraseniaRep);
        Utils.setContrastColor(this.campoContraseniaRep);
        Utils.setFontBold(this.campoContraseniaRep);
        panelCentral.add(this.campoContraseniaRep, c);

        // Relacion entre etiqueta y campo de contrasena
        etiquetaContraseniaRep.setLabelFor(this.campoContraseniaRep);
        // Asignacion de mnemonico
        etiquetaContraseniaRep.setDisplayedMnemonic(KeyEvent.VK_N);

        c.gridy = 5;
        c.insets = new Insets(5, 20, 0, 20);

        // Check de mostrar contrasena o no
        final JPanel panelCheckShowPass = new JPanel(new GridLayout(1, 1));
        final JCheckBox showPassCheckBox = new JCheckBox(Messages.getString("CustomDialog.showInputPasswordDialog.showPassCheckBox.text")); //$NON-NLS-1$
        showPassCheckBox.setToolTipText(Messages.getString("CustomDialog.showInputPasswordDialog.showPassCheckBox.tooltip")); //$NON-NLS-1$
        showPassCheckBox.getAccessibleContext().setAccessibleDescription(showPassCheckBox.getToolTipText());
        showPassCheckBox.setMnemonic(KeyEvent.VK_T);

        // Se almacena el caracter por defecto para ocultar la contrasena
        final char defaultChar = this.campoContrasenia.getEchoChar();
        showPassCheckBox.setSelected(false); // Check noseleccionado por defecto
        showPassCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent evt) {
                if (evt.getStateChange() == ItemEvent.SELECTED) {
                    // Se muestra la contrasena
                    PanelContrasenia.this.getCampoContrasenia().setEchoChar((char) 0);
                    PanelContrasenia.this.getCampoContraseniaRep().setEchoChar((char) 0);

                }
                else if (evt.getStateChange() == ItemEvent.DESELECTED) {
                    // Se oculta la contrasena
                    PanelContrasenia.this.getCampoContrasenia().setEchoChar(defaultChar);
                    PanelContrasenia.this.getCampoContraseniaRep().setEchoChar(defaultChar);
                }

                // Foco al input
                PanelContrasenia.this.getCampoContrasenia().requestFocus();
            }
        });
        Utils.remarcar(showPassCheckBox);
        Utils.setContrastColor(showPassCheckBox);
        Utils.setFontBold(showPassCheckBox);
        panelCheckShowPass.add(showPassCheckBox);

        panelCentral.add(panelCheckShowPass, c);

        c.fill = GridBagConstraints.BOTH;
        c.gridy = 6;
        c.weighty = 1.0;
        c.insets = new Insets(20, 20, 0, 20);

        // Etiqueta que contiene el texto "El olvido o perdida..."
        final InfoLabel lostLabel = new InfoLabel(Messages.getString("WizardCifrado.contrasenia.contenido.texto5"), false); //$NON-NLS-1$
        panelCentral.add(lostLabel, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // La botonera se carga desde el asistente

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoContrasenia, "cifrado.wizard.password"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.campoContraseniaRep, "cifrado.wizard.repeatpassword"); //$NON-NLS-1$

    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    public void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 1));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
        getBotonera().getSiguiente().setEnabled(false);
    }

	@Override
	public void keyPressed(final KeyEvent arg0) { /* Vacio */ }

	@Override
	public void keyReleased(final KeyEvent arg0) {
		if (!"".equals(getCampoContrasenia().getPassword()) && Arrays.equals(getCampoContrasenia().getPassword(), getCampoContraseniaRep().getPassword())) { //$NON-NLS-1$
			getBotonera().getSiguiente().setEnabled(true);
		}
		else {
			getBotonera().getSiguiente().setEnabled(false);
		}
	}

	@Override
	public void keyTyped(final KeyEvent arg0) {
		// TODO Auto-generated method stub

	}

}
