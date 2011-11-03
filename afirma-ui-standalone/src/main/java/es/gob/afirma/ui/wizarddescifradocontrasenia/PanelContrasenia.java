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
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.text.Caret;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.jse.JSEUIManager;
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
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;


/**
 * Clase que muestra el contenido principal del descifrado de una contrasenia.
 */
public class PanelContrasenia extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelContrasenia.class.getName());
	
	/**
	 * Ruta donde se encuentra el archivo a cifrar
	 */
	private String rutaFichero = "";
	
	/**
	 * Cifrador configurado para un algoritmo dado
	 */
	private CipherConfig cipherConfig;
	
	// Campo donde se guarda la contrasenia
    private JPasswordField campoContrasenia = new JPasswordField();


	@Override
	public int getMinimumRelation(){
		return 9;
	}
	
	/**
     * Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas	Listado con todas las paginas del asistente
     */
    public void setVentanas(List<JDialogWizard> ventanas) {
    	Botonera botonera = new Botonera(ventanas, 1);
    	getContentPane().add(botonera, BorderLayout.PAGE_END);
    }
	
	public PanelContrasenia() {
        initComponents();
    }

	public PanelContrasenia(String algoritmo, String rutaFichero) {
		this.cipherConfig = new CipherConfig(algoritmo);
		this.rutaFichero = rutaFichero;
        initComponents();
    }
	
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("WizardDescifrado.titulo"));
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardDescifrado.contrasenia.explicacion.titulo", "WizardDescifrado.contrasenia.explicacion", null, true);
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);
        
    	// Panel central
    	JPanel panelCentral = new JPanel();
    	panelCentral.setMinimumSize(new Dimension(603, 289));
    	panelCentral.setLayout(new GridBagLayout());
        
    	// Configuramos el layout
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.gridx = 0;
    	
		// Etiqueta que contiene el texto "Introduzca una contrasenia de..."
		InfoLabel insertLabel = new InfoLabel(Messages.getString("WizardDescifrado.contrasenia.contenido.texto1"), false);
		panelCentral.add(insertLabel, c);

		 c.insets = new Insets(20, 20, 0, 20);
		 c.weightx = 1.0;
		 c.gridx = 0;
		 c.gridy	= 1;
		 
		//Etiqueta con el texto Contrasenia de descifrado
    	JLabel passwordLabel = new JLabel (Messages.getString("WizardDescifrado.contrasenia"));
    	Utils.setContrastColor(passwordLabel);
    	Utils.setFontBold(passwordLabel);
    	panelCentral.add(passwordLabel, c);
    	
    	 c.insets = new Insets(5, 20, 0, 20);
		 c.weightx = 1.0;
		 c.gridx = 0;
		 c.gridy = 2;
       
		
        // Caja de texto donde se guarda la contrasenia
		 this.campoContrasenia.setToolTipText(Messages.getString("WizardDescifrado.contrasenia.contrasenia.description")); // NOI18N //$NON-NLS-1$
		 campoContrasenia.getAccessibleContext().setAccessibleName(passwordLabel.getText() + " " + campoContrasenia.getToolTipText() + "ALT + O.");
	     campoContrasenia.getAccessibleContext().setAccessibleDescription(campoContrasenia.getToolTipText());
	     this.campoContrasenia.setDocument(new JSEUIManager.JTextFieldASCIIFilter(true));
	     if (GeneralConfig.isBigCaret()) {
				Caret caret = new ConfigureCaret();
				campoContrasenia.setCaret(caret);
			}
	     Utils.remarcar(campoContrasenia);
	     Utils.setContrastColor(campoContrasenia);
	     Utils.setFontBold(campoContrasenia);
	     panelCentral.add(this.campoContrasenia, c);
    	
        //Relación entre etiqueta y campo de texto
        passwordLabel.setLabelFor(campoContrasenia);
      	//Asignación de mnemónico
        passwordLabel.setDisplayedMnemonic(KeyEvent.VK_O);
        
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridy	= 3;
        c.weighty = 1.0;
        
        // Etiqueta que contiene el texto "Introduzca la contrasenia con..."
        InfoLabel endLabel = new InfoLabel(Messages.getString("WizardDescifrado.contrasenia.contenido.texto5"), false);
		panelCentral.add(endLabel, c);
      
        getContentPane().add(panelCentral, BorderLayout.CENTER);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoContrasenia, "descifrado.wizard.password");
    }

    /**
	 * Botonera con funciones para la pagina panel de cifrado
	 */
	private class Botonera extends BotoneraInferior {

		private static final long serialVersionUID = 1L;

		public Botonera(List<JDialogWizard> ventanas, Integer posicion) {
			super(ventanas, posicion);
		}

		@Override
		protected void siguienteActionPerformed(JButton anterior,
				JButton siguiente, JButton finalizar) {

			Boolean continuar = true;
			continuar = descifrarFichero();

			if (continuar.equals(true)) {
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			} else {
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
		char[] contrasenia = campoContrasenia.getPassword();
		
		if (contrasenia == null || new String(contrasenia).trim().equals("")){
			CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		}

		byte[] fileContent = null;
		try {
			fileContent = getFileContent();
		}
		catch (NullPointerException ex) {
			logger.warning("No se ha indicado un fichero de datos: " + ex);
			ex.printStackTrace();
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero"),
					Messages.getString("Descifrado.btndescifrar"),JOptionPane.WARNING_MESSAGE);
			return false;
		} catch (FileNotFoundException ex) {
			logger.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
			ex.printStackTrace();
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero2"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		} catch (Exception ex) {
			logger.warning("Ocurri\u00F3 un error durante la lectura del fichero de datos: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
			ex.printStackTrace();
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.fichero2"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		}

		byte[] result = null;
		try {
			Key tmpKey = cipherConfig.getCipher().decodePassphrase(contrasenia, cipherConfig.getConfig(), null);
			result = cipherConfig.getCipher().decipher(fileContent, cipherConfig.getConfig(), tmpKey);
		} catch (InvalidKeyException e) {
			logger.severe("Contrasena no valida: " + e);
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.contrasenia"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		} catch (Exception ex) {
			logger.severe("Error al descifrar: " + ex);
			ex.printStackTrace();
			CustomDialog.showMessageDialog(this, true,
					Messages.getString("Descifrado.msg.error.operacion"), Messages.getString("error"),
					JOptionPane.ERROR_MESSAGE);

			return false;
		}

		if (result == null) {
			CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.noresultado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
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
	 * @throws FileNotFoundException Cuando no se encuentra o no puede leerse el fichero seleccionado.
	 * @throws IOException Cuando ocurre un error durante la lectura de un fichero local.
	 * @throws AOException Cuando ocurre un error al formar una ruta remota o al leer un fichero remoto.
	 */
	private byte[] getFileContent() throws FileNotFoundException, IOException, AOException, NullPointerException {
		if (rutaFichero == null) 
			throw new NullPointerException("No se ha indicado un fichero de entrada");
		return AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(rutaFichero)));
	}

	/**
	 * Getter para el campo de contrasenia.
	 * @return Campo de contrasenia.
	 */
	public JPasswordField getCampoContrasenia() {
		return campoContrasenia;
	}
}
