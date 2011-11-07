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
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyException;
import java.util.List;
import java.util.Vector;
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
import es.gob.afirma.keystores.filters.CertificateFilter;
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
import es.gob.afirma.util.AOBase64;

/**
 *
 * Clase que muestra el contenido principal del descifrado de una clave.
 */
public class PanelClave extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelClave.class.getName());
	
	/**
	 * Cifrador configurado para un algoritmo dado
	 */
	private CipherConfig cipherConfig;
	
	/**
	 * Ruta donde se encuentra el archivo a cifrar
	 */
	private String rutaFichero = "";
	
	// Campo donde se guarda la contrasenia
    private JTextField campoClave = new JTextField();

	
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
    
	public PanelClave(String algoritmo, String rutaFichero) {
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
        CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardDescifrado.clave.explicacion.titulo", "WizardDescifrado.clave.explicacion", null, true);
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
		c.gridwidth	= 2;
		c.gridx = 0;
		
		 // Etiqueta que contiene el texto "Introduzca la clave con..."
		InfoLabel insertLabel = new InfoLabel(Messages.getString("WizardDescifrado.clave.contenido.texto1"), false);
		panelCentral.add(insertLabel, c);

		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth	= 1;
		c.weightx = 1.0;
		c.gridy	= 1;
		
		//Etiqueta con el texto Clave de descifrado
    	JLabel keyLabel = new JLabel (Messages.getString("WizardDescifrado.clave"));
    	Utils.setContrastColor(keyLabel);
    	Utils.setFontBold(keyLabel);
    	panelCentral.add(keyLabel, c);
        
    	c.insets = new Insets(0, 20, 0, 20);
		c.gridwidth	= 1;
		c.weightx = 1.0;
		c.gridy	= 2;
		
        // Caja de texto donde se guarda la clave
        this.campoClave.setToolTipText(Messages.getString("WizardDescifrado.clave.contrasenia.description")); // NOI18N
        this.campoClave.getAccessibleContext().setAccessibleName(keyLabel.getText() + " " + this.campoClave.getToolTipText() + "ALT + L.");
	    this.campoClave.getAccessibleContext().setAccessibleDescription(this.campoClave.getToolTipText());
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			this.campoClave.setCaret(caret);
		}
        Utils.remarcar(this.campoClave);
        Utils.setContrastColor(this.campoClave);
        Utils.setFontBold(this.campoClave);
        panelCentral.add(this.campoClave, c);
        
        //Relacion entre etiqueta y campo de texto
        keyLabel.setLabelFor(this.campoClave);
       	//Asignacion de mnemonico
        keyLabel.setDisplayedMnemonic(KeyEvent.VK_L);
        
        c.insets = new Insets(0, 20, 0, 20);
		c.weightx = 0.0;
		c.gridx = 1;
        
		JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton para examinar el almacen
        JButton examinar = new JButton(); 
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setToolTipText(Messages.getString("WizardDescifrado.clave.boton.description")); // NOI18N
        examinar.setText(Messages.getString("WizardDescifrado.clave.boton")); // NOI18N
        examinar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                examinarActionPerformed();
            }
        });
        examinar.getAccessibleContext().setAccessibleName(examinar.getText() + " " + examinar.getToolTipText()); // NOI18N
        examinar.getAccessibleContext().setAccessibleDescription(examinar.getToolTipText()); // NOI18N
        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);
        panelExaminar.add(examinar);
        panelCentral.add(panelExaminar, c);
        
        c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth	= 2;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridy	= 3;
		c.gridx = 0;
        
		 // Etiqueta que contiene el texto "En caso de obtener..."
        InfoLabel endLabel = new InfoLabel(Messages.getString("WizardDescifrado.clave.contenido.texto5"), false);
		panelCentral.add(endLabel, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoClave, "descifrado.wizard.clave");
    }

    /**
     * Accede al almacen de claves para obtener una
     */
    void examinarActionPerformed() {
    	// Comprobamos que el almacen exista.
    	if(!AOCipherKeyStoreHelper.storeExists()) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("WizardDescifrado.msg.error.almacen"), 
    				Messages.getString("WizardDescifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
    		return;
    	}

    	// Mostramos la clave de cifrado recuperada del almacen
    	try {
    		this.campoClave.setText(getKeyFromCipherKeyStore());
    	} catch (AOCancelledOperationException e) {
    		logger.warning("El usuario ha cancelado la recuperacion de claves de cifrado del almacen.");
    	} catch (IOException e) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.contrasenia"), 
    				Messages.getString("WizardDescifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
    	} catch (Exception e) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("WizardDescifrado.msg.error.clave"), 
    				Messages.getString("WizardDescifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
    	}		
    }

    /**
     * Obtiene una clave de cifrado en base 64 del almac&eacute;n de claves del usuario.
     * Se pedira al usuario que inserte la contrase&ntilde;a del almac&eacute;n de claves
     * y seleccione la clave que desea recuperar del mismo.
     * @return Clave en base 64.
     * @throws AOException Ocurri&oacute; un error durate el proceso de configuraci&oacute;n.
     * @throws IOException Cuando no se indique la contrase&ntilde;a correcta del almacen. 
     */
    private String getKeyFromCipherKeyStore() throws AOException, IOException {
    	// Abrimos el Almacen de claves de cifrado preguntandole al usuario la clave si no
    	// la indico
    	AOCipherKeyStoreHelper cKs = null;
    	try {
    		cKs = new AOCipherKeyStoreHelper(
					CustomDialog.showInputPasswordDialog(this, true, null, false, Messages.getString("WizardDescifrado.clave.pass"), Messages.getString("CustomDialog.showInputPasswordDialog.title"), JOptionPane.QUESTION_MESSAGE));
    	} catch (AOCancelledOperationException e) {
    		throw e;
    	} catch (IOException e) {
            throw e;
        } catch (Exception e) {
    		throw new AOException("Error al abrir el repositorio de claves del usuario", e); //$NON-NLS-1$
    	}

    	// Si no se establecio el alias de la clave de cifrado, se la pedimos al usuario
    	String alias = null;
    	try {
    		alias = Utils.showCertSelectionDialog(cKs.getAliases(), null, this, true, true, true,
    				new Vector<CertificateFilter>(0), false);
    	} catch (AOCancelledOperationException e) {
    		throw e;
    	} catch (Exception e) {
    		throw new AOException("Error seleccionar la clave de cifrado", e); //$NON-NLS-1$
    	}

    	return AOBase64.encode(cKs.getKey(alias).getEncoded(), false);
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
				//Si ha ocurrido algun error durante el proceso de cifrado mediante clave
				//el foco vuelve al campo de insercion de clave
				getCampoClave().requestFocusInWindow();
			}
		}
	}

	/**
	 * Descifra un fichero dado
	 * @return	true o false indicando si se ha descifrado correctamente
	 */
    public boolean descifrarFichero() {
    	// Recuperamos la clave
    	String clave = this.campoClave.getText();

    	if (clave == null || clave.equals("")) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.clave"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	}
    	byte[] fileContent = null;
    	try {
    	    fileContent = this.getFileContent();
    	} catch (NullPointerException ex) {
    	    logger.warning("No se ha indicado un fichero de datos: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
    	    ex.printStackTrace();
    	    CustomDialog.showMessageDialog(
    	            this, true,
    	            Messages.getString("Descifrado.msg.fichero"),
    	            Messages.getString("Descifrado.btndescifrar"),
    	            JOptionPane.WARNING_MESSAGE
    	    );
    	    return false;
    	} catch (FileNotFoundException ex) {
    	    logger.warning("Error al leer el fichero: " + ex); //$NON-NLS-1$
    	    ex.printStackTrace();
    	    CustomDialog.showMessageDialog(this, true,  Messages.getString("Descifrado.msg.fichero2"),
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
    	    Key tmpKey = this.cipherConfig.getCipher().decodeKey(clave, this.cipherConfig.getConfig(), null);
    	    result = this.cipherConfig.getCipher().decipher(fileContent, this.cipherConfig.getConfig(), tmpKey);
    	} catch (InvalidKeyException e) {
    	    logger.severe("Clave no valida: " + e); //$NON-NLS-1$
    	    CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.clave"),  //$NON-NLS-1$
    	            Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
    	    return false;
    	} catch (KeyException ex) {
    	    logger.severe("Error al descifrar, compruebe que el fichero esta cifrado con el algoritmo seleccionado: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
    	    CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.malcifrado"),  //$NON-NLS-1$
    	            Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
    	    return false;
    	} catch (Exception ex) {
    	    logger.severe("Error al descifrar: " + ex); //$NON-NLS-1$
    	    ex.printStackTrace();
    	    CustomDialog.showMessageDialog(this, true, Messages.getString("Descifrado.msg.error.operacion"), 
    	            Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    	    return false;
    	}

    	// Almacenamos el fichero de salida de la operacion
    	final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("WizardDescifrado.clave.filechooser.save.title"), result, "fichero", null, this);
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
    	if (this.rutaFichero == null) 
    		throw new NullPointerException("No se ha indicado un fichero de entrada");
    	return AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(this.rutaFichero)));
    }

	/**
	 * Getter para el campo de la clave.
	 * @return Campo de la clave.
	 */
	public JTextField getCampoClave() {
		return this.campoClave;
	}
}
