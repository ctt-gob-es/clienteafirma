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
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.Key;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.ciphers.AOCipherKeyStoreHelper;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidKeyException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.ui.AOUIManager;
import es.gob.afirma.ui.utils.CipherConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;

/**
 *
 * Clase que muestra el contenido principal del descifrado de una clave.
 */
public class PanelClave extends JDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelClave.class.getName());
	
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
	
    /**
	 * Ruta donde se encuentra el archivo a cifrar
	 */
	private String rutaFichero = "";
    
	public PanelClave(String algoritmo, String rutaFichero) {
		this.cipherConfig = new CipherConfig(algoritmo);
		this.rutaFichero = rutaFichero;
        initComponents();
    }
    
	// Campo donde se guarda la contrasenia
    private JTextField campoClave = new JTextField();
	
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("WizardDescifrado.titulo"));
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardDescifrado.clave.explicacion.titulo", "WizardDescifrado.clave.explicacion", null, true);
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
		c.gridwidth	= 2;
		c.gridx = 0;
    	
        // Panel que contiene el texto "Introduzca la clave con..."
		panelCentral.add(PanelesTexto.generarPanelTexto(
        		"WizardDescifrado.clave.contenido.texto1", false), c);
        
		c.insets = new Insets(20, 20, 0, 0);
		c.gridwidth	= 1;
		c.weightx = 1.0;
		c.gridy	= 1;
		
        // Caja de texto donde se guarda la clave
        campoClave.setToolTipText(Messages.getString("WizardDescifrado.clave.contrasenia.description")); // NOI18N
        panelCentral.add(campoClave, c);
        
        c.insets = new Insets(20, 10, 0, 20);
		c.weightx = 0.0;
		c.gridx = 1;
        
        // Boton para examinar el almacen
        JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setToolTipText(Messages.getString("WizardDescifrado.clave.boton.description")); // NOI18N
        examinar.setText(Messages.getString("WizardDescifrado.clave.boton")); // NOI18N
        examinar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                examinarActionPerformed();
            }
        });
        examinar.getAccessibleContext().setAccessibleDescription(Messages.getString("WizardDescifrado.clave.boton")); // NOI18N
        panelCentral.add(examinar, c);
        
        c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth	= 2;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridy	= 2;
		c.gridx = 0;
        
        // Panel que contiene el texto "En caso de obtener..."
		panelCentral.add(PanelesTexto.generarPanelTexto(
        		"WizardDescifrado.clave.contenido.texto5", false), c);
        
        getContentPane().add(panelCentral, BorderLayout.CENTER);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoClave, "descifrado.wizard.clave");
    }

    /**
     * Accede al almacen de claves para obtener una
     */
    private void examinarActionPerformed() {
    	// Comprobamos que el almacen exista.
    	if(!AOCipherKeyStoreHelper.storeExists()) {
    		JOptionPane.showMessageDialog(this, Messages.getString("WizardDescifrado.msg.error.almacen"), 
    				Messages.getString("WizardDescifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
    		return;
    	}

    	// Mostramos la clave de cifrado recuperada del almacen
    	try {
    		campoClave.setText(getKeyFromCipherKeyStore());
    	} catch (AOCancelledOperationException e) {
    		logger.warning("El usuario ha cancelado la recuperacion de claves de cifrado del almacen.");
    	} catch (AOException e) {
    		JOptionPane.showMessageDialog(this, Messages.getString("WizardDescifrado.msg.error.clave"), 
    				Messages.getString("WizardDescifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
    	} catch (Exception e) {
    		JOptionPane.showMessageDialog(this, Messages.getString("WizardDescifrado.msg.error.clave"), 
    				Messages.getString("WizardDescifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
    	}		
    }

    /**
     * Obtiene una clave de cifrado en base 64 del almac&eacute;n de claves del usuario.
     * Se pedira al usuario que inserte la contrase&ntilde;a del almac&eacute;n de claves
     * y seleccione la clave que desea recuperar del mismo.
     * @return Clave en base 64.
     * @throws AOException Ocurri&oacute; un error durate el proceso de configuraci&oacute;n. 
     */
    private String getKeyFromCipherKeyStore() throws AOException {
    	// Abrimos el Almacen de claves de cifrado preguntandole al usuario la clave si no
    	// la indico
    	AOCipherKeyStoreHelper cKs = null;
    	try {
    		cKs = new AOCipherKeyStoreHelper(
    				AOUIManager.getPassword(Messages.getString("WizardDescifrado.clave.pass"), this)
    		);
    	} catch (AOCancelledOperationException e) {
    		throw e;
    	} catch (Exception e) {
    		throw new AOException("Error al abrir el repositorio de claves del usuario", e); //$NON-NLS-1$
    	}

    	// Si no se establecio el alias de la clave de cifrado, se la pedimos al usuario
    	String alias = null;
    	try {
    		alias = AOUIManager.showCertSelectionDialog(cKs.getAliases(), null, this, true, true, true);
    	} catch (AOCancelledOperationException e) {
    		throw e;
    	} catch (Exception e) {
    		throw new AOException("Error seleccionar la clave de cifrado", e); //$NON-NLS-1$
    	}

    	return AOCryptoUtil.encodeBase64(cKs.getKey(alias).getEncoded(), false);
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
    	// Recuperamos la clave
    	String clave = campoClave.getText();

    	if (clave == null || clave.equals("")) {
    		JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.clave"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	} else {
    		byte[] fileContent = null;
    		try {
    			fileContent = this.getFileContent();
    		} catch (NullPointerException ex) {
    			logger.warning("No se ha indicado un fichero de datos: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
    			ex.printStackTrace();
    			JOptionPane.showMessageDialog(
    					this,
    					Messages.getString("Descifrado.msg.fichero"),
    					Messages.getString("Descifrado.btndescifrar"),
    					JOptionPane.WARNING_MESSAGE
    			);
    			dispose();
    			return false;
    		} catch (FileNotFoundException ex) {
    			logger.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$
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
    			Key tmpKey = cipherConfig.getCipher().decodeKey(clave, cipherConfig.getConfig(), null);
    			result = cipherConfig.getCipher().decipher(fileContent, cipherConfig.getConfig(), tmpKey);
    		} catch (AOInvalidKeyException e) {
    			logger.severe("Clave no valida: " + e);
    			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.error.clave"), 
    					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    			return false;
    		} catch (AOException ex) {
    			logger.severe("Error al descifrar, compruebe que el fichero esta cifrado con el algoritmo seleccionado: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
    			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.error.malcifrado"), 
    					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    			dispose();
    			return false;
    		} catch (Exception ex) {
    			logger.severe("Error al descifrar: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
    			ex.printStackTrace();
    			JOptionPane.showMessageDialog(this, Messages.getString("Descifrado.msg.error.operacion"), 
    					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);

    			// Si el error se dio en el proceso de descifrado y es distinto
    			// a una clave incorrecta, entonces abortamos la operacion
    			// cerrando el panel del Wizard
    			dispose();
    			return false;
    		}

    		// Almacenamos el fichero de salida de la operacion
    		String path = AOUIManager.saveDataToFile(this, result, null, null);
    		if (path == null) {
				return false;
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
    	if (this.rutaFichero == null) 
    		throw new NullPointerException("No se ha indicado un fichero de entrada");
    	return AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(rutaFichero), this, true));
    }
}
