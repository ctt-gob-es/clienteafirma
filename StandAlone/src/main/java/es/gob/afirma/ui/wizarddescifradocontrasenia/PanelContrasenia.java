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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.Key;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidKeyException;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.ui.AOUIManager;
import es.gob.afirma.ui.AOUIManager.JTextFieldASCIIFilter;
import es.gob.afirma.ui.utils.CipherConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;

/**
 * Clase que muestra el contenido principal del descifrado de una contrasenia.
 */
public class PanelContrasenia extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelContrasenia.class.getName());
	
	@Override
	public int getMinimumRelation(){
		return 9;
	}
	
	@Override
	public int getInitialHeight() {
		return 440;
	}
	@Override
	public int getInitialWidth() {
		return 630;
	}
	
	/**
	 * Cifrador configurado para un algoritmo dado
	 */
	private CipherConfig cipherConfig;
	
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
	
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("WizardDescifrado.titulo"));
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardDescifrado.contrasenia.explicacion.titulo", "WizardDescifrado.contrasenia.explicacion", null, true);
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
    	
        // Panel que contiene el texto "Introduzca la contrasenia con..."
		panelCentral.add(PanelesTexto.generarPanelTexto(
				"WizardDescifrado.contrasenia.contenido.texto1", false), c);

        c.gridy	= 1;
		
        // Caja de texto donde se guarda la clave
        campoContrasenia.setToolTipText(Messages.getString("WizardDescifrado.contrasenia.contrasenia.description")); // NOI18N
        campoContrasenia.setDocument(new JTextFieldASCIIFilter(true));
        panelCentral.add(campoContrasenia, c);
    	
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(10, 20, 0, 20);
        c.gridy	= 2;
        c.weighty = 1.0;
        
        // Panel que contiene el texto "Introduzca la contrasenia con..."
        panelCentral.add(PanelesTexto.generarPanelTexto(
				"WizardDescifrado.contrasenia.contenido.texto5", false), c);   
        
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

			if (continuar.equals(true))
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
		}
	}

	/**
	 * Descifra un fichero dado
	 * @return	true o false indicando si se ha descifrado correctamente
	 */
	public Boolean descifrarFichero() {
		char[] contrasenia = campoContrasenia.getPassword();
		
		if (contrasenia == null || new String(contrasenia).trim().equals("")){
			JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		}

		byte[] fileContent = null;
		try {
			fileContent = getFileContent();
		}
		catch (NullPointerException ex) {
			logger.warning("No se ha indicado un fichero de datos: " + ex);
			ex.printStackTrace();
			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.fichero"),
					Messages.getString("Descifrado.btndescifrar"),JOptionPane.WARNING_MESSAGE);
			dispose();
			return false;
		} catch (FileNotFoundException ex) {
			logger.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
			ex.printStackTrace();
			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.fichero2"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			dispose();
			return false;
		} catch (Exception ex) {
			logger.warning("Ocurri\u00F3 un error durante la lectura del fichero de datos: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
			ex.printStackTrace();
			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.fichero2"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			dispose();
			return false;
		}

		byte[] result = null;
		try {
			Key tmpKey = cipherConfig.getCipher().decodePassphrase(contrasenia, cipherConfig.getConfig(), null);
			result = cipherConfig.getCipher().decipher(fileContent, cipherConfig.getConfig(), tmpKey);
		} catch (AOInvalidKeyException e) {
			logger.severe("Contrasena no valida: " + e);
			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.error.contrasenia"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		} catch (Exception ex) {
			logger.severe("Error al descifrar: " + ex);
			ex.printStackTrace();
			JOptionPane.showMessageDialog(this,
					Messages.getString("Descifrado.msg.error.operacion"), Messages.getString("error"),
					JOptionPane.ERROR_MESSAGE);

			// Si el error se dio en el proceso de descifrado y es distinto
			// a una contrasena incorrecta, entonces abortamos la operacion
			// cerrando el panel del Wizard
			dispose();
			return false;
		}

		if (result == null) {
			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.noresultado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		}

		// Almacenamos el fichero de salida de la operacion
		String path = AOUIManager.saveDataToFile(this, result, new File(new File(rutaFichero).getParentFile(), "fichero"), null);
		// Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
		if (path == null) {
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
		return AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(rutaFichero), this, true));
	}
}
