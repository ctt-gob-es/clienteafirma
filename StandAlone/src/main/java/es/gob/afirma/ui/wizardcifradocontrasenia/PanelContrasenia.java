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
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.util.Arrays;
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
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.jse.JSEUIManager;
import es.gob.afirma.ui.utils.CipherConfig;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;

/**
 * Dialogo con la pagina 2: Clave de cifrado
 */
public class PanelContrasenia extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;
	
	static Logger logger = Logger.getLogger(PanelContrasenia.class.getName());
	
	/**
	 * Cifrador configurado para un algoritmo dado
	 */
	private CipherConfig cipherConfig;
    
	/**
	 * Clave de cifrado
	 */
	private Key cipherKey;
	
	@Override
	public int getMinimumRelation(){
		return 8;
	}
	
    /**
     * Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas	Listado con todas las paginas del asistente
     */
    public void setVentanas(List<JDialogWizard> ventanas) {
    	Botonera botonera = new Botonera(ventanas, 1);
    	getContentPane().add(botonera, BorderLayout.PAGE_END);
    }
	
	/**
	 * Ruta donde se encuentra el archivo a cifrar
	 */
	private String rutaFichero = "";
    
    public PanelContrasenia(String algoritmo, String rutaFichero) {
    	this.cipherConfig = new CipherConfig(algoritmo);
    	this.rutaFichero = rutaFichero;
        initComponents();
    }
    
    // Campo donde se guarda la contrasenia
    private JPasswordField campoContrasenia = new JPasswordField();
    // Campo donde se guarda la contrasenia repetida
    private JPasswordField campoContraseniaRep = new JPasswordField();
    
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {    	
    	// Titulo de la ventana
    	setTitulo(Messages.getString("WizardCifrado.titulo"));
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardCifrado.contrasenia.explicacion.titulo", "wizardCifrado.contrasenia.explicacion", null, true);
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
    	
        // Panel que contiene el texto "Introduzca una contrasenia de..."
		panelCentral.add(PanelesTexto.generarPanelTexto(
        		"WizardCifrado.contrasenia.contenido.texto1", false), c);
        
		c.gridy = 1;
		c.insets = new Insets(20, 20, 0, 20);
		
        // Etiqueta con el texto "Introduzca una contrasenia"
        JLabel etiquetaContrasenia = new JLabel();
        etiquetaContrasenia.setText(Messages.getString("WizardCifrado.contrasenia")); // NOI18N
        panelCentral.add(etiquetaContrasenia, c);
        
        c.gridy = 2;
        c.insets = new Insets(5, 20, 0, 20);
        
        // Caja de texto con la contrasenia
        campoContrasenia.setToolTipText(Messages.getString("WizardCifrado.contrasenia.description")); // NOI18N
        campoContrasenia.setDocument(new JSEUIManager.JTextFieldASCIIFilter(true));
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			campoContrasenia.setCaret(caret);
		}
        panelCentral.add(campoContrasenia, c);
        
        //Relación entre etiqueta y campo de contraseña
        etiquetaContrasenia.setLabelFor(campoContrasenia);
  		//Asignación de mnemónico
        etiquetaContrasenia.setDisplayedMnemonic(KeyEvent.VK_I);
        
        c.gridy = 3;
        c.insets = new Insets(20, 20, 0, 20);
        
        // Etiqueta con el texto "Introduzca de nuevo..."
        JLabel etiquetaContraseniaRep = new JLabel();
        etiquetaContraseniaRep.setText(Messages.getString("WizardCifrado.recontrasenia")); // NOI18N
        panelCentral.add(etiquetaContraseniaRep, c);
        
        c.gridy = 4;
        c.insets = new Insets(5, 20, 0, 20);
        
        // Caja de texto con la contrasenia
        campoContraseniaRep.setToolTipText(Messages.getString("WizardCifrado.recontrasenia.description")); // NOI18N
        campoContraseniaRep.setDocument(new JSEUIManager.JTextFieldASCIIFilter(true));
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			campoContraseniaRep.setCaret(caret);
		}
        panelCentral.add(campoContraseniaRep, c);
        
        //Relación entre etiqueta y campo de contraseña
        etiquetaContraseniaRep.setLabelFor(campoContraseniaRep);
  		//Asignación de mnemónico
        etiquetaContraseniaRep.setDisplayedMnemonic(KeyEvent.VK_N);
        
        c.fill = GridBagConstraints.BOTH;
        c.gridy = 5;
        c.weighty = 1.0;
        c.insets = new Insets(20, 20, 0, 20);
        
        // Panel que contiene el texto "El olvido o perdida..."
        panelCentral.add(PanelesTexto.generarPanelTexto(
        		"WizardCifrado.contrasenia.contenido.texto5", false), c);
        
        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // La botonera se carga desde el asistente
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoContrasenia, "cifrado.wizard.password");
        HelpUtils.enableHelpKey(campoContraseniaRep, "cifrado.wizard.repeatpassword");
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
			continuar = cifrarFichero();

			if (continuar.equals(true))
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
		}
	}
	
	/**
	 * Cifra un fichero dado
	 * @return	true o false indicando si se ha cifrado correctamente
	 */
	private boolean cifrarFichero() {
		char[] contrasenia = campoContrasenia.getPassword();
		char[] contraseniaRep = campoContraseniaRep.getPassword();
				
		// Comprobamos las dos contrasenias
		if (contrasenia == null || new String(contrasenia).trim().equals("")) {
			JOptionPane.showMessageDialog(this,	Messages.getString("WizardCifrado.contrasenia.error2"),
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		} else if (!Arrays.equals(contrasenia, contraseniaRep)) {
			JOptionPane.showMessageDialog(this,	Messages.getString("WizardCifrado.contrasenia.error"),
					Messages.getString("error"),JOptionPane.ERROR_MESSAGE);
			return false;
		} else {
			// Generamos la clave necesaria para el cifrado
			try {
				this.cipherKey = this.cipherConfig.getCipher()
			    .decodePassphrase(contrasenia, this.cipherConfig.getConfig(), null);
			} catch (Exception ex) {
				logger.severe("Ocurrio un error durante el proceso de generacion de claves: " + ex); //$NON-NLS-1$
				JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.cifrado"), //$NON-NLS-1$
						Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$

		        // Si el error se dio en el proceso de cifrado y es distinto
		        // a una clave incorrecta, entonces abortamos la operacion
		        // cerrando el panel del Wizard
		        this.dispose();
				return false;
			}
			// Leemos el fichero de datos
			byte[] fileContent = null;
			try {
				fileContent = this.getFileContent();
			} catch (NullPointerException ex) {
				logger.warning("No se ha indicado un fichero de datos: " + ex);
				JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.fichero"),  Messages.getString("Cifrado.msg.titulo"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
				this.dispose();
				return false;
			} catch (FileNotFoundException ex) {
				logger.warning("No se encuentra el fichero: " + ex);
				JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.lectura"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
				this.dispose();
				return false;
			} catch (Exception ex) {
				logger.warning("Ocurrio un error al leer el fichero: " + ex);
				JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.lectura"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
				this.dispose();
				return false;
			}

			byte[] result = null;
			try {
				result = cipherConfig.getCipher().cipher(fileContent, cipherConfig.getConfig(), cipherKey);
			} catch (InvalidKeyException ex) {
				logger.severe("No se cumplen con los requisitos de contrase\u00F1a del algoritmo: " + ex);
				JOptionPane.showMessageDialog(this, Messages.getString("WizardCifrado.contrasenia.error.requerimientos"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
				return false;
			} catch (Exception ex) {
				logger.warning("Error al cifrar: " + ex);
				JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.operacion"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
				
		        // Si el error se dio en el proceso de cifrado y es distinto
		        // a una clave incorrecta, entonces abortamos la operacion
		        // cerrando el panel del Wizard
				this.dispose();
				return false;
			}

			if (result == null) 
                JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.noresultado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
            else {
                // Almacenamos el fichero de salida de la operacion
            	final File savedFile = AOUIFactory.getSaveDataToFile(result,
                        new File(new File(rutaFichero).getParentFile(), "cifrado"), null, this);
                if (savedFile == null) {
					return false;
				}
            }
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
}
