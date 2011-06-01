/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileFilter;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCertificateKeyException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.DirectorySignatureHelper.MassiveType;
import es.gob.afirma.ui.AOUIManager;
import es.gob.afirma.ui.AOUIManager.ExtFilter;
import es.gob.afirma.ui.utils.DirectorySignatureHelperAdv;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.OpenFileMessageDialog;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

class PanelMultifirmaMasiva extends JDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelMultifirmaMasiva.class.getName());
	
	/**
	 * Configuracion del KeyStore
	 */
	private KeyStoreConfiguration kssc = null;
	
	/**
	 * Indica si debe emitir un beep al finalizar
	 */
	private Boolean beep;
	
	/**
	 * Algoritmo utilizado para firmar
	 */
	private String algoritmo;
	
	/**
	 * Cadena con las extensiones de los ficheros a firmar
	 */
	private String extensiones;
	
	/**
	 * Tipo de firma a desarrollar
	 */
	private Integer tipo;
	
	/**
	 * Tipo de contrafirma (todos o ultimos)
	 */
	private Boolean tipoContrafirma = false;
	
	/**
	 * Indica si debe respetar el formato original
	 */
	private Boolean respetar = true;
	
	/**
	 * Ruta del directorio de entrada
	 */
	private String directorioEntrada;
	
	/**
	 * Indica si se deben incluir los subdirectorio del directorio de entrada
	 */
	private Boolean incluir;
	
	/**
	 * Indica si se esta firmando un documento en CADES implicito o explicito
	 * Si es true - implicito, si es false - explicito
	 */
	private Boolean modoFormato = true;
	
	void setIncluir(Boolean incluir) {
		this.incluir = incluir;
	}
	
	void setDirectorioEntrada(String directorioEntrada) {
		this.directorioEntrada = directorioEntrada;
	}
	
	void setRespetar(Boolean respetar) {
		this.respetar = respetar;
	}
	
	void setTipoContrafirma(Boolean tipoContrafirma) {
		this.tipoContrafirma = tipoContrafirma;
	}
	
	void setTipo(Integer tipo) {
		this.tipo = tipo;
	}
		
	void setExtensiones(String extensiones) {
		this.extensiones = extensiones;
	}
	
	void setAlgoritmo(String algoritmo) {
		this.algoritmo = algoritmo;
	}
	
	void setModoFormato(boolean modoFormato) {
		this.modoFormato = modoFormato;
	}
	
	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	void setVentanas(List<JDialogWizard> ventanas) {
		Botonera botonera = new Botonera(ventanas, 4);
		getContentPane().add(botonera, BorderLayout.PAGE_END);
	}
	
	/**
	 * Carga la pagina con los parametros
	 * @param kssc		Configuracion del KeyStore
	 * @param beep		Indica si debe emitir un "beep" al finalizar
	 */
    PanelMultifirmaMasiva(KeyStoreConfiguration kssc, Boolean beep) {
    	this.kssc = kssc;
        this.beep = beep;
        initComponents();
    }
    
    // Caja de texto donde se guarda el nombre del directorio de firmas
    private JTextField campoDirectorio = new JTextField();
    // Caja de texto donde se guarda el nombre del fichero log
    private JTextField campoFicheroLog = new JTextField();
    // Checkbox con el texto "Sobreescribir ficheros"
    private JCheckBox checkSobreescribir = new JCheckBox();
    // Boton para seleccionar el fichero de log
    private JButton	examinarFichero = new JButton();

    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.multifirma.titulo.ventana"));
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.multifirma.ventana4.titulo", "Wizard.multifirma.ventana4.titulo.descripcion", null, true);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);
		
        // Panel central
    	JPanel panelCentral = new JPanel();
    	panelCentral.setMinimumSize(new Dimension(603, 289));
    	panelCentral.setLayout(new GridBagLayout());
        
    	// Configuramos el layout
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridwidth = 2;
        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
    	
    	// Etiqueta con el texto "Directorio de firmas"
    	JLabel etiquetaFirma = new JLabel();
    	etiquetaFirma.setText(Messages.getString("Wizard.multifirma.ventana4.directorio"));
        panelCentral.add(etiquetaFirma, c);
        
        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth	= 1;
		c.gridy = 1;
		c.gridx = 0;
        
        // Caja de texto donde se guarda el nombre del directorio de firmas
        campoDirectorio.setToolTipText(Messages.getString("Wizard.multifirma.ventana4.directorio.description"));
        campoDirectorio.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana4.directorio"));
        campoDirectorio.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana4.directorio.description"));
        panelCentral.add(campoDirectorio, c);
        
        c.insets = new Insets(0, 10, 0, 20);
        c.weightx = 0.0;
		c.gridx = 1;
        
        // Boton examinar directorio firmas
        JButton	examinarDirectorio = new JButton();
        examinarDirectorio.setMnemonic(KeyEvent.VK_E);
        examinarDirectorio.setText(Messages.getString("PrincipalGUI.Examinar"));
        examinarDirectorio.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description"));
        examinarDirectorio.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	examinarDirectorioActionPerformed();
            }
        });
        panelCentral.add(examinarDirectorio, c);
        
        c.insets = new Insets(5, 20, 0, 20);
        c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 2;
        
        // Checkbox con el texto "Sobreescribir ficheros"
        checkSobreescribir.setText(Messages.getString("Wizard.multifirma.ventana4.check.sobreescribir"));
        checkSobreescribir.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana4.check.sobreescribir"));
        checkSobreescribir.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana4.check.sobreescribir.description"));
        panelCentral.add(checkSobreescribir, c);
        
        c.insets = new Insets(20, 20, 0, 20);
		c.gridy = 3;
        
        // Etiqueta con el texto "Fichero de log"
        JLabel etiquetaFichero = new JLabel();
        etiquetaFichero.setText(Messages.getString("Wizard.multifirma.ventana4.log"));
        panelCentral.add(etiquetaFichero, c);
        
        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth	= 1;
		c.gridy = 4;
        
        // Caja de texto donde se guarda el nombre del fichero log
        campoFicheroLog.setEnabled(false);
        campoFicheroLog.setToolTipText(Messages.getString("Wizard.multifirma.ventana4.log.description"));
        campoFicheroLog.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana4.log"));
        campoFicheroLog.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana4.log.description"));
        panelCentral.add(campoFicheroLog, c);
        
        c.insets = new Insets(0, 10, 0, 20);
        c.weightx = 0.0;
        c.gridx = 1;
        
        // Boton examinar fichero log
        examinarFichero = new JButton();
        examinarFichero.setEnabled(false);
        examinarFichero.setMnemonic(KeyEvent.VK_E);
        examinarFichero.setText(Messages.getString("PrincipalGUI.Examinar"));
        examinarFichero.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description"));
        examinarFichero.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                examinarFicheroLogActionPerformed();
            }
        });
        panelCentral.add(examinarFichero, c);
        
        c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth = 2;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 5;
		
		// Panel introducido para poder mantener la linea superior correcta
		Panel panelVacio = new Panel();
		panelCentral.add(panelVacio, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoDirectorio,"multifirma.masiva.wizard.firma.directoriofirmas");
        HelpUtils.enableHelpKey(checkSobreescribir,"multifirma.masiva.wizard.firma.sobreescribir");
        HelpUtils.enableHelpKey(campoFicheroLog,"multifirma.masiva.wizard.firma.ficheroLog");      
    }

    /**
     * Comprueba que el archivo seleccionado es correcto y guarda su nombre en el campo de texto.
     * Tambien genera el nombre del fichero log y lo guarda en su respectivo campo.
     */
    private void examinarDirectorioActionPerformed() {
    	File selectedFile = new SelectionDialog().showDirOpenDialog(this, Messages.getString("PrincipalGUI.chooser.dir.outtitle"));
    	if (selectedFile != null) {
    		campoDirectorio.setText(selectedFile.getAbsolutePath());
    		campoFicheroLog.setText(new File(selectedFile.getAbsoluteFile().getParent(), "result.txt").getAbsolutePath());
    	}
    	
    	// Activamos el boton de examinar el directorio del log y el campo para introducirlo
    	examinarFichero.setEnabled(true);
    	campoFicheroLog.setEnabled(true);
    }

    /**
     * Comprueba que el archivo log seleccionado es correcto y guarda su nombre en el campo de texto
     */
    private void examinarFicheroLogActionPerformed() {
    	File selectedFile = new SelectionDialog().showFileOpenDialog(this, Messages.getString("PrincipalGUI.chooser.title"));
    	if (selectedFile != null) {
    		campoFicheroLog.setText(selectedFile.getAbsolutePath());
    	}
    }
    
    /**
	 * Botonera con funciones para la pagina panel de multifirma - cofirma
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
			continuar = multifirmarFicheros();
			
			if (continuar == true)
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
		}
	}

	/**
	 * Comprueba si los archivos son correctos
	 * @return
	 */
	Boolean multifirmarFicheros() {
		// Comprobamos rutas de los ficheros
		String directorio = campoDirectorio.getText();
		if (directorio == null || directorio.equals("")){
			JOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.error.directorio.destino"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return false;
		} else {
			//Comprobacion de las extensiones.
			String log = campoFicheroLog.getText();
			if (log == null || log.equals("")){
				JOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.error.fichero.log"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
				return false;
			}
		}
		
		// Comienza la multifirma
		AOKeyStoreManager keyStoreManager = null;
        PrivateKeyEntry privateKeyEntry = null;
        
        boolean resultadoFirma = true;
        try {
        	PasswordCallback pssCallback;

        	AOConstants.AOKeyStore store = kssc.getType();
        	if (store == AOConstants.AOKeyStore.WINDOWS || store == AOConstants.AOKeyStore.WINROOT ||
        			store == AOConstants.AOKeyStore.SINGLE) 
        		pssCallback = new NullPasswordCallback();
        	else 
        		pssCallback = new UIPasswordCallback(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription() + ". \r\nSi no ha establecido ninguna, deje el campo en blanco.", null); 

        	keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(store, kssc.getLib(), kssc.toString(),
        			pssCallback, this);

        	// Seleccionamos un certificado
        	String selectedcert = AOUIManager.showCertSelectionDialog(keyStoreManager.getAliases(), keyStoreManager.getKeyStores(), null, this, true, true, true);

        	// Comprobamos si se ha cancelado la seleccion
        	if (selectedcert == null) 
        		throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario");

        	// Recuperamos la clave del certificado
        	try {
        		privateKeyEntry = keyStoreManager.getKeyEntry(selectedcert, AOCryptoUtil.getCertificatePC(store, this));
        	} catch (AOCertificateKeyException e) {
        		throw e;
        	} catch (AOCancelledOperationException e) {
        		// Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
        		// Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
        		// que las relanza en forma de AOException
        		throw e;
        	}
        	catch (Throwable e) {
        		logger.severe("No se ha podido obtener el certicado con el alias '" + selectedcert + "': " + e);
        		throw new AOException("No se ha podido extraer el certificado seleccionado.");
        	}

        	if (privateKeyEntry == null) {
        		throw new AOCertificateKeyException("No se pudo obtener la informacion del certificado, no se firmara el fichero."); 
        	}
        } catch(AOException e){
        	logger.severe(e.toString());
        	JOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.error.grave"), 
        			Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.ERROR_MESSAGE); 
        	return false;
        } catch(Exception e){
        	logger.severe(e.toString());
        	JOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.error.grave"), 
        			Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.ERROR_MESSAGE); 
        	e.printStackTrace();
        	return false;
        }
        
        try {
            DirectorySignatureHelperAdv dSigner = new DirectorySignatureHelperAdv(
            		GeneralConfig.getSignAlgorithm(), algoritmo, AOConstants.SIGN_MODE_IMPLICIT, this);
         
            // Establecemos el filtro de ficheros por extension
            dSigner.setFileFilter(getExtensionFileFilter(extensiones));

            // Indicamos si deseamos sobrescribir ficheros previos de firma que encontremos
            dSigner.setOverwritePreviuosFileSigns(checkSobreescribir.isSelected());

            Properties config = GeneralConfig.getSignConfig();
            config.setProperty("mode", modoFormato ? AOConstants.SIGN_MODE_IMPLICIT : AOConstants.SIGN_MODE_EXPLICIT);
            config.setProperty("format", algoritmo);
            config.setProperty("ignoreStyleSheets", "true");
            
            // Seleccionamos el tipo de operacion
            MassiveType operation = this.getMassiveOperationType(tipo, tipoContrafirma);

            // Creamos el archivo de log
            dSigner.setLogPath(campoFicheroLog.getText());
            
            // Ejecutamos la operacion masiva
            resultadoFirma = dSigner.massiveSign(operation, directorioEntrada, incluir, campoDirectorio.getText(),
            		true, respetar, privateKeyEntry, keyStoreManager.getCertificate(privateKeyEntry), config);

            // Hacemos el pitido si es necesario
            if (beep) {
            	Toolkit.getDefaultToolkit().beep();
            }
        } catch(AOException e){
        	logger.severe(e.toString());
        	resultadoFirma = false;
        } catch(Exception e){
        	logger.severe(e.toString());
        	logger.log(Level.SEVERE, "Exception", e);
        	resultadoFirma = false;
        }

        if (resultadoFirma) {
        	JOptionPane.showMessageDialog(this, Messages.getString("Wizard.multifirma.ok"), 
        			Messages.getString("Wizard.multifirma.ok.titulo"), JOptionPane.INFORMATION_MESSAGE);  //$NON-NLS-2$
        } else {
      		OpenFileMessageDialog.show(this, Messages.getString("Wizard.multifirma.ko"), 
      				Messages.getString("Wizard.multifirma.ok.titulo"),
      				new File(campoFicheroLog.getText()));
        }
		
		return true;
	}
	
	/**
     * Filtro para extensiones
     * @param extensiones extensiones recogidas del wizard
     * @return  filtro de extensiones
     */
    private FileFilter getExtensionFileFilter(String extensiones) {
		if (extensiones == null || extensiones.trim().equals("")) {
			return null;
		}
		String[] exts = extensiones.split(",");
		return new ExtFilter(exts, getExtensionFileFilterDescription(exts));
	}
    
    /**
     * Descripcion de las extensiones
     * @param exts  extensiones
     * @return  extensiones posibles
     */
    private String getExtensionFileFilterDescription(String[] exts) {
		String extFilter = Messages.getString("fichero.firma.masiva"); 
		if (exts != null && extFilter.length() > 0) {
			extFilter += " ("+ exts[0]; 
			for (int i=1; i<exts.length; i++) 
				extFilter += ", "+exts[i]; 
			
			extFilter += ")"; 
		}
		return extFilter;
	}
    
    /**
     * Obtiene el tipo de multifirma que se está realizando
     * @param tipo  tipo de firma
     * @param hojas si se han de firmar las hojas
     * @return tipo de firma a realizar.
     */
    private MassiveType getMassiveOperationType(int tipo, boolean hojas) {
    	MassiveType operation = null;
    	switch (tipo) {
	    	case 0:
	    		operation = MassiveType.SIGN;
	    		break;
	    	case 1:
	    		operation = MassiveType.COSIGN;
	    		break;
	    	case 2:
	    		operation = hojas ? MassiveType.COUNTERSIGN_LEAFS : MassiveType.COUNTERSIGN_ALL;
	    		break;
    	}
    	return operation;
    }
}
