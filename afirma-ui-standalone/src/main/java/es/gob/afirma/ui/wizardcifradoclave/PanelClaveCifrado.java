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
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.spec.InvalidKeySpecException;
import java.util.List;
import java.util.Vector;
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
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.util.AOBase64;

/**
 * Dialogo con la pagina 2: Clave de cifrado
 */
public class PanelClaveCifrado extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelClaveCifrado.class.getName());
	
	@Override
	public int getMinimumRelation(){
		return 9;
	}
		
	/**
	 * Cifrador configurado para un algoritmo dado
	 */
	private CipherConfig cipherConfig;
    
	/**
	 * Clave de cifrado
	 */
	private Key cipherKey;
	
	/**
	 * Ruta donde se encuentra el archivo a cifrar
	 */
	private String rutaFichero = "";
	
	   // Campo donde se guarda la clave generada
    private JTextField campoClave = new JTextField();
    
    // Check que indica si se debe guardar la clave en el almacen
    private JCheckBox checkGuardar = new JCheckBox();
    
    /**
     * Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas	Listado con todas las paginas del asistente
     */
    public void setVentanas(List<JDialogWizard> ventanas) {
    	this.setBotonera(new Botonera(ventanas, 1));
     	getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
    
    public PanelClaveCifrado(String algoritmo, String rutaFichero) {
    	this.cipherConfig = new CipherConfig(algoritmo);
    	this.rutaFichero = rutaFichero;
        initComponents();
    }

    
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {    	
    	// Titulo de la ventana
    	setTitulo(Messages.getString("WizardCifrado.titulo"));
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("WizardCifrado.explicacion.titulo", "WizardCifrado.explicacion", null, true);
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);
        
    	// Panel central
    	JPanel panelCentral = new JPanel();
    	panelCentral.setMinimumSize(new Dimension(603, 289));
    	panelCentral.setLayout(new GridBagLayout());
        
    	GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(5, 20, 0, 20);
		c.weightx = 1.0;
		c.gridwidth = 3;
		c.gridx = 0;
		c.gridy = 0;

    	
        // Etiqueta que contiene el texto "Introduzca una clave de..."
		InfoLabel insertLabel = new InfoLabel(Messages.getString("WizardCifrado.contenido.texto1"), false);
		panelCentral.add(insertLabel, c);

		c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(0, 20, 0, 20);
		c.gridy = 1;
		c.weighty = 1;
        
		 // Etiqueta que contiene el texto "Adicionalmente, puede almacenar..."
        InfoLabel lostLabel = new InfoLabel(Messages.getString("WizardCifrado.contenido.texto5"), false);
		panelCentral.add(lostLabel, c);
		
		c.gridy = 2;
		c.gridwidth = 1;
		c.weighty = 0;
		c.insets = new Insets(0, 20, 0, 20);
    	
    	//Etiqueta con el texto Clave de cifrado
    	JLabel encodeKeyLabel = new JLabel (Messages.getString("WizardCifrado.claveCifrado"));
    	Utils.setContrastColor(encodeKeyLabel);
    	Utils.setFontBold(encodeKeyLabel);
    	panelCentral.add(encodeKeyLabel, c);
        
    	 c.gridx = 0;
    	 c.gridy = 3;
    	 c.insets = new Insets(0, 20, 0, 0);
    	
        // Caja de texto donde se escribe la clave
        this.campoClave.setToolTipText(Messages.getString("WizardCifrado.campoClave.description")); // NOI18N
        this.campoClave.getAccessibleContext().setAccessibleName(encodeKeyLabel.getText() + " " + "ALT + V.");
        this.campoClave.getAccessibleContext().setAccessibleDescription(encodeKeyLabel.getToolTipText());
        this.campoClave.addKeyListener(new KeyAdapter() {
        	@Override
            public void keyPressed(KeyEvent e) {
        		PanelClaveCifrado.this.checkGuardar.setEnabled(true);
        	}
		});
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			this.campoClave.setCaret(caret);
		}
        Utils.remarcar(this.campoClave);
        Utils.setContrastColor(this.campoClave);
        Utils.setFontBold(this.campoClave);
        panelCentral.add(this.campoClave, c);
        
        //Relación entre etiqueta y campo de texto
        encodeKeyLabel.setLabelFor(this.campoClave);
      	//Asignación de mnemónico
        encodeKeyLabel.setDisplayedMnemonic(KeyEvent.VK_V);
        
        c.weightx = 0.0;
        c.insets = new Insets(0, 10, 0, 0);
        c.gridx = 1;
        
        JPanel panelAutogenerar = new JPanel(new GridLayout(1, 1));
        // Boton autogenerar
        JButton autogenerar = new JButton();
        autogenerar.setMnemonic(KeyEvent.VK_U);
        autogenerar.setToolTipText(Messages.getString("WizardCifrado.autogenerar.description")); // NOI18N
        autogenerar.setText(Messages.getString("WizardCifrado.autogenerar")); // NOI18N
        autogenerar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                autogenerarActionPerformed();
            }
        });
        autogenerar.getAccessibleContext().setAccessibleName(autogenerar.getText() + " " + autogenerar.getToolTipText()); // NOI18N
        autogenerar.getAccessibleContext().setAccessibleDescription(autogenerar.getToolTipText()); // NOI18N
        Utils.remarcar(autogenerar);
        Utils.setContrastColor(autogenerar);
        Utils.setFontBold(autogenerar);
        panelAutogenerar.add(autogenerar);
        panelCentral.add(panelAutogenerar, c);
        
        c.insets = new Insets(0, 10, 0, 20);
        c.weightx = 0.0;
        c.gridx = 2;
        
        JPanel panelAlmacen = new JPanel(new GridLayout(1, 1));
        // Boton cargar clave del almacen
        JButton almacen = new JButton();
        almacen.setMnemonic(KeyEvent.VK_L);
        almacen.setToolTipText(Messages.getString("WizardCifrado.almacen.description")); // NOI18N
        almacen.setText(Messages.getString("WizardCifrado.almacen")); // NOI18N
        almacen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
            	almacenActionPerformed();
            }
        });
        almacen.getAccessibleContext().setAccessibleName(almacen.getText()+ " " + almacen.getToolTipText()); // NOI18N
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
		
		JPanel panelCheckGuardar = new JPanel(new GridLayout(1, 1));
		// Checkbox para guardar en el almacen
        this.checkGuardar.setText(Messages.getString("WizardCifrado.check")); // NOI18N
        this.checkGuardar.setToolTipText(Messages.getString("WizardCifrado.check.description")); // NOI18N
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
        HelpUtils.enableHelpKey(this.campoClave, "cifrado.wizard.autogenerar");  
        HelpUtils.enableHelpKey(autogenerar, "cifrado.wizard.autogenerar");
        HelpUtils.enableHelpKey(almacen, "cifrado.wizard.almacen");
        HelpUtils.enableHelpKey(this.checkGuardar, "cifrado.wizard.salvar");
    }

    /**
     * Obtenemos una clase del almacen/repositorio
     */
	void almacenActionPerformed() {
		// Comprobamos que el almacen exista.
    	if(!AOCipherKeyStoreHelper.storeExists()) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.almacen.noexiste"), 
    				Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE);
    		return;
    	}

    	// Mostramos la clave de cifrado recuperada del almacen
    	try {
    		this.campoClave.setText(getKeyFromCipherKeyStore());
    		this.checkGuardar.setEnabled(false);
    	} catch (AOCancelledOperationException e) {
    		logger.warning("El usuario ha cancelado la recuperacion de claves de cifrado del almacen.");
    	}  catch (IOException e) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.msg.error.contrasenia"), 
    				Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE);
    	}catch (AOException e) {
    		CustomDialog.showMessageDialog(this, true, e.getMessage(), Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE);
    	} catch (Exception e) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.almacen.error.clave"), Messages.getString("WizardCifrado.almacen.claves"), JOptionPane.WARNING_MESSAGE);
    	}
	}
	
	/**
     * Obtiene una clave de cifrado en base 64 del almac&eacute;n de claves del usuario.
     * Se pedira al usuario que inserte la contrase&ntilde;a del almac&eacute;n de claves
     * y seleccione la clave que desea recuperar del mismo. Devuelve {@code null} si
     * ocurre un error durante la transformaci&oacute;n a base 64.
     * @return Clave en base 64 o {@code}
     * @throws AOException Ocurri&oacute; un error durate el proceso de configuraci&oacute;n. 
     */
    private String getKeyFromCipherKeyStore() throws AOException, IOException {
    	// Abrimos el Almacen de claves de cifrado preguntandole al usuario la clave si no
    	// la indico
    	AOCipherKeyStoreHelper cKs = null;
    	try {
    		cKs = new AOCipherKeyStoreHelper(
    				CustomDialog.showInputPasswordDialog(this, true, null, false, Messages.getString("WizardCifrado.almacen.claves.contrasenia"),  KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title"), JOptionPane.QUESTION_MESSAGE));
    	} catch (AOCancelledOperationException e) {
    		throw e;
    	} catch (IOException e) {
	            throw e; //Se lanza el tipo de excepción IO
    	} catch (Exception e) {
    		throw new AOException(Messages.getString("WizardCifrado.almacen.error.abrir"), e); //$NON-NLS-1$
    	}

    	// Si no se establecio el alias de la clave de cifrado, se la pedimos al usuario
    	String alias = null;
    	try {
    		alias = Utils.showCertSelectionDialog(cKs.getAliases(), null, this, true, true, true,
    				new Vector<CertificateFilter>(0), false);
    	} catch (AOCancelledOperationException e) {
    		throw e;
    	} catch (Exception e) {
    		throw new AOException(Messages.getString("WizardCifrado.almacen.error.seleccionar"), e); //$NON-NLS-1$
    	}
    	
    	return AOBase64.encode(cKs.getKey(alias).getEncoded(), false);
    }

	/**
     * Genera la clave
     */
    private void autogenerarActionPerformed() {
        try {
            generateKey(this.cipherConfig.getConfig());
        } catch (Exception ex) {
        	logger.log(Level.SEVERE, null, ex);
        }
        
        this.campoClave.setText(AOBase64.encode(this.cipherKey.getEncoded(), false));
        this.checkGuardar.setEnabled(true);
    }

    /**
	 * Genera una clave v&aacute;lida para un algoritmo de cifrado y la almacena en el repoitorio de @firma si
	 * as&iacute; lo indica el usuario.
	 * @param algorithmConfig Configuraci&oacute;n de cifrado.
	 * @throws NoSuchAlgorithmException Cuando no se reconoce el algoritmo de cifrado
	 * @throws NoSuchProviderException Cuando no se reconoce el proveedor para la generaci&oacute;n de claves
	 * @throws InvalidKeySpecException Cuando la contrase&ntilde;a introducida no cumple los requisitos necesarios
	 * @throws AOCipherAlgorithmException Cuando el algoritmo indicado no soporta el modo de generaci&oacute;n de clave
	 * @throws AOException Cuando se produce un error al auto generar las claves.
	 * @throws NoSuchAlgorithmException Cuando el algoritmo de cifrado no esta soportado.
	 */
	private void generateKey(AOCipherConfig algorithmConfig) throws InvalidKeyException, AOException, NoSuchAlgorithmException {
		this.cipherKey = this.cipherConfig.getCipher().generateKey(algorithmConfig);
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
	 * Cifra un fichero dado
	 * @return	true o false indicando si se ha cifrado correctamente
	 */
	private boolean cifrarFichero() {

	    // Comprobamos si se ha generado alguna clave
	    if (this.campoClave.getText() == null || this.campoClave.getText().equals("")){
	    	CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.clave"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
	        return false;
	    }
	    // Generamos la clave necesaria para el cifrado
	    try {
	        this.cipherKey = this.cipherConfig.getCipher().decodeKey(this.campoClave.getText(), this.cipherConfig.getConfig(), null);
	    } catch (Exception ex) {
	        logger.severe("Ocurrio un error durante el proceso de generacion de claves: " + ex);
	        CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.cifrado"), 
	                Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
	        return false;
	    }

	    // Leemos el fichero de datos
	    byte[] fileContent = null;
	    try {
	        fileContent = getFileContent();
	    } catch (NullPointerException ex) {
	        logger.warning("No se ha indicado un fichero de datos: " + ex);
	        CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.fichero"),  Messages.getString("Cifrado.msg.titulo"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
	        return false;
	    } catch (FileNotFoundException ex) {
	        logger.warning("No se encuentra el fichero: " + ex);
	        CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.lectura"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
	        return false;
	    } catch (Exception ex) {
	        logger.warning("Ocurrio un error al leer el fichero: " + ex);
	        CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.lectura"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
	        return false;
	    }                

	    // Ciframos los datos
	    byte[] result = null;
	    try {
	        result = this.cipherConfig.getCipher().cipher(fileContent, this.cipherConfig.getConfig(), this.cipherKey);
	    } catch (KeyException e) {
	        logger.severe("Clave no valida: " + e);
	        CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.clave"), 
	                Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
	        return false;
	    } catch (Exception ex) {
	        logger.warning("Error al cifrar: " + ex);
	        CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.operacion"), 
	                Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$

	        return false;
	    }

	    // Guardamos los datos
	    if (result == null) {
	    	CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.noresultado"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$//$NON-NLS-2$
	        return false;
	    }
	    // Almacenamos el fichero de salida de la operacion
	    final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("WizardCifrado.clave.filechooser.save.title"), result, "cifrado", null, this); //$NON-NLS-1$
	    if (savedFile == null) {
	        return false;
	    }

	    // Guardamos la clave de cifrado si se solicito 
	    if (this.checkGuardar.isSelected()) {
	        guardarClaveCifrado();
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
		if(this.rutaFichero == null) 
			throw new NullPointerException("No se ha indicado un fichero de entrada");
		return AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(this.rutaFichero)));
	}
	
	/**
     * Almacena la clave de cifrado configurada.
     */
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
						cksh = new AOCipherKeyStoreHelper(
								CustomDialog.showInputPasswordDialog(this, true, null, false, Messages.getString("Cifrado.introducir.pass"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title"), JOptionPane.QUESTION_MESSAGE));
						
					} else {
						PasswordCallback pssCallback = new UIPasswordCallbackAccessibility(Messages.getString("Cifrado.introducir.pass.almacen"), this,
		            			Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title"));
						cksh = new AOCipherKeyStoreHelper(pssCallback.getPassword());
					}
				} catch (IOException e) {
					if (tries < 3) {
						CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.msg.error.contrasenia"),
								Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
					} else {
						// Abortamos al tercer intento incorrecto de introducir la clave
						CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.pass.incorrecto.almacenar"),
								Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
						return;
					}
					gainedAccess = false;
				}
			} while (!gainedAccess);
    		
    		String alias = CustomDialog.showInputDialog(this, true, Messages.getString("Cifrado.introducir.alias"), KeyEvent.VK_I, Messages.getString("Cifrado.introducir.alias.titulo"), JOptionPane.QUESTION_MESSAGE);
    		cksh.storeKey(alias + " (" + this.cipherConfig.getConfig().getAlgorithm().getName() + ")", this.cipherKey);
		} catch(AOCancelledOperationException e) {
			CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.cancelar"),
    				Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.WARNING_MESSAGE);
    	} catch (AOException e) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.clavecifrar"),
    				Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.ERROR_MESSAGE);
    	} catch(Exception e) {
    		CustomDialog.showMessageDialog(this, true,  Messages.getString("Cifrado.msg.error.clavecifrar"),
    				Messages.getString("Cifrado.msg.error.titulo"), JOptionPane.ERROR_MESSAGE );
    	}
	}
	
	/**
	 * Getter para el campo de la clave.
	 * @return Campo de la clave.
	 */
	public JTextField getCampoClave() {
		return this.campoClave;
	}
}
