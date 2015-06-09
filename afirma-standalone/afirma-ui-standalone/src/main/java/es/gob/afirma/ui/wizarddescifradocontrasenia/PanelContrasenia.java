/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizarddescifradocontrasenia;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.security.Key;
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


/**
 * Clase que muestra el contenido principal del descifrado de una contrasenia.
 */
final class PanelContrasenia extends JAccessibilityDialogWizard {
	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Log.
	 */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Ruta donde se encuentra el archivo a cifrar
	 */
	private String rutaFichero;

	/**
	 * Cifrador configurado para un algoritmo dado
	 */
	private CipherConfig cipherConfig;

	/** Campo donde se guarda la contrase&ntilde;a. */
	private final JPasswordField campoContrasenia = new JPasswordField();

    /**
	 * Relacion minima para el redimensionado de componentes.
	 */
	@Override
	public int getMinimumRelation(){
		return 9;
	}

	/**
     * Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas	Listado con todas las paginas del asistente
     */
    public void setVentanas(final List<JDialogWizard> ventanas) {
    	this.setBotonera(new Botonera(ventanas, 1));
    	getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
	/**
	 * Constructor.
	 */
	public PanelContrasenia() {
        initComponents();
    }
	/** Constructor.
	 * @param algoritmo Algoritmo de cifrado.
	 * @param rutaFichero Ruta hacia el ficharo a cifrar. */
	public PanelContrasenia(final String algoritmo, final String rutaFichero) {
		this.cipherConfig = new CipherConfig(algoritmo);
		this.rutaFichero = rutaFichero;
        initComponents();
    }

    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("WizardDescifrado.titulo")); //$NON-NLS-1$

    	// Panel con la cabecera
        final CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardDescifrado.contrasenia.explicacion.titulo", "WizardDescifrado.contrasenia.explicacion", null); //$NON-NLS-1$ //$NON-NLS-2$
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
		final InfoLabel insertLabel = new InfoLabel(Messages.getString("WizardDescifrado.contrasenia.contenido.texto1"), false); //$NON-NLS-1$
		panelCentral.add(insertLabel, c);

		 c.insets = new Insets(20, 20, 0, 20);
		 c.weightx = 1.0;
		 c.gridx = 0;
		 c.gridy = 1;

		//Etiqueta con el texto Contrasenia de descifrado
    	final JLabel passwordLabel = new JLabel (Messages.getString("WizardDescifrado.contrasenia")); //$NON-NLS-1$
    	Utils.setContrastColor(passwordLabel);
    	Utils.setFontBold(passwordLabel);
    	panelCentral.add(passwordLabel, c);

    	 c.insets = new Insets(5, 20, 0, 20);
		 c.weightx = 1.0;
		 c.gridx = 0;
		 c.gridy = 2;


        // Caja de texto donde se guarda la contrasenia
		 this.campoContrasenia.setToolTipText(Messages.getString("WizardDescifrado.contrasenia.contrasenia.description")); // NOI18N //$NON-NLS-1$
		 this.campoContrasenia.getAccessibleContext().setAccessibleName(passwordLabel.getText() + " " + this.campoContrasenia.getToolTipText() + "ALT + O.");  //$NON-NLS-1$//$NON-NLS-2$
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

        //Relacion entre etiqueta y campo de texto
        passwordLabel.setLabelFor(this.campoContrasenia);
      	//Asignacion de mnemonico
        passwordLabel.setDisplayedMnemonic(KeyEvent.VK_O);

        c.gridy = 3;
        c.insets = new Insets(5, 20, 0, 20);

        //Check de mostrar contrasena o no
		final JPanel panelCheckShowPass = new JPanel(new GridLayout(1, 1));
		final JCheckBox showPassCheckBox= new JCheckBox(Messages.getString("CustomDialog.showInputPasswordDialog.showPassCheckBox.text")); //$NON-NLS-1$
		showPassCheckBox.setToolTipText(Messages.getString("CustomDialog.showInputPasswordDialog.showPassCheckBox.tooltip")); //$NON-NLS-1$
		showPassCheckBox.getAccessibleContext().setAccessibleDescription(showPassCheckBox.getToolTipText());
		showPassCheckBox.setMnemonic(KeyEvent.VK_T);

		//Se almacena el caracter por defecto para ocultar la contrasena
		final char defaultChar = this.campoContrasenia.getEchoChar();
		showPassCheckBox.setSelected(false); //Check noseleccionado por defecto
		showPassCheckBox.addItemListener(new ItemListener() {
			@Override
            public void itemStateChanged(final ItemEvent evt) {
				if (evt.getStateChange() == ItemEvent.SELECTED){
					//Se muestra la contrasena
					PanelContrasenia.this.getCampoContrasenia().setEchoChar((char)0);

				} else if (evt.getStateChange() == ItemEvent.DESELECTED){
					//Se oculta la contrasena
					PanelContrasenia.this.getCampoContrasenia().setEchoChar(defaultChar);
				}

				//Foco al input
				PanelContrasenia.this.getCampoContrasenia().requestFocus();
			}
		});
		Utils.remarcar(showPassCheckBox);
		Utils.setContrastColor(showPassCheckBox);
		Utils.setFontBold(showPassCheckBox);
		panelCheckShowPass.add(showPassCheckBox);

		panelCentral.add(panelCheckShowPass, c);

        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridy	= 4;
        c.weighty = 1.0;

        // Etiqueta que contiene el texto "Introduzca la contrasenia con..."
        final InfoLabel endLabel = new InfoLabel(Messages.getString("WizardDescifrado.contrasenia.contenido.texto5"), false); //$NON-NLS-1$
		panelCentral.add(endLabel, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoContrasenia, "descifrado.wizard.password"); //$NON-NLS-1$
    }

    /**
	 * Botonera con funciones para la pagina panel de cifrado
	 */
	private class Botonera extends BotoneraInferior {
		/**
		 * UID.
		 */
		private static final long serialVersionUID = 1L;
		/**
		 * Constructor.
		 * @param ventanas Lista de ventanas que componen el wizard.
		 * @param posicion posicion de la ventana donde se inserta esta botonera.
		 */
		public Botonera(final List<JDialogWizard> ventanas, final int posicion) {
			super(ventanas, posicion);
		}
		/**
		 * Accion para el boton siguiente.
		 */
		@Override
		protected void siguienteActionPerformed(final JButton anterior,
												final JButton siguiente,
												final JButton finalizar) {

			boolean continuar = false;
			try {
				continuar = descifrarFichero();
			}
			catch(final OutOfMemoryError e) {
	        	CustomDialog.showMessageDialog(
        			SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero.tamano"), //$NON-NLS-1$
                    Messages.getString("error"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
	        	getBotonera().getCancelar().doClick();
			}

			if (continuar) {
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			}
			else {
				//Si ha ocurrido algun error durante el proceso de descifrado mediante contrasenia
				//el foco vuelve al campo de insercion de contrasenia
				getCampoContrasenia().requestFocusInWindow();
			}
		}
	}

	/**
	 * Descifra un fichero dado
	 * @return	true o false indicando si se ha descifrado correctamente
	 */
	public boolean descifrarFichero() {
		if (this.rutaFichero == null) {
			LOGGER.warning("No se ha indicado un fichero de datos"); //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero"), //$NON-NLS-1$
					Messages.getString("Descifrado.btndescifrar"),JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$
			return false;
		}
		final char[] contrasenia = this.campoContrasenia.getPassword();
		if (contrasenia == null || new String(contrasenia).trim().equals("")){ //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$//$NON-NLS-2$
			return false;
		}
		final byte[] fileContent;
		try {
			fileContent = getFileContent();
		}
		catch (final FileNotFoundException ex) {
			LOGGER.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero2"),  //$NON-NLS-1$
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
			return false;
		}
		catch (final Exception ex) {
			LOGGER.warning("Error durante la lectura del fichero de datos: " + ex); //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero2"),  //$NON-NLS-1$
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
			return false;
		}

		final byte[] result;
		try {
			final Key tmpKey = this.cipherConfig.getCipher().decodePassphrase(contrasenia, this.cipherConfig.getConfig(), null);
			result = this.cipherConfig.getCipher().decipher(fileContent, this.cipherConfig.getConfig(), tmpKey);
		}
		catch (final InvalidKeyException e) {
			LOGGER.severe("Contrasena no valida: " + e); //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.contrasenia"),  //$NON-NLS-1$
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
			return false;
		}
		catch (final Exception ex) {
			LOGGER.severe("Error al descifrar: " + ex); //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true,
					Messages.getString("Descifrado.msg.error.operacion"), Messages.getString("error"), //$NON-NLS-1$ //$NON-NLS-2$
					JOptionPane.ERROR_MESSAGE);

			return false;
		}

		if (result == null) {
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.noresultado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		}

		// Almacenamos el fichero de salida de la operacion

		final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("WizardDescifrado.contrasenia.filechooser.save.title"), result, "fichero", null, this); //$NON-NLS-1$

		// Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
		if (savedFile == null) {
			return false;
		}

		return true;
	}

	/**
	 * Obtiene el contenido del fichero seleccionado por el usuario.
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

	/**
	 * Getter para el campo de contrasenia.
	 * @return Campo de contrasenia.
	 */
	public JPasswordField getCampoContrasenia() {
		return this.campoContrasenia;
	}
}
