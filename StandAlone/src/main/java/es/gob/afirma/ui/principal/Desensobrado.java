/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
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
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.signers.aobinarysignhelper.CMSDecipherAuthenticatedEnvelopedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSDecipherEnvelopData;
import es.gob.afirma.signers.aobinarysignhelper.CMSDecipherSignedAndEnvelopedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSHelper;
import es.gob.afirma.ui.AOUIManager;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.Utils;

/**
 * Clase que se encarga de desensobrar el contenido de un fichero.
 */
public class Desensobrado extends JPanel {
	
	private static final long serialVersionUID = 1L;
	
	static Logger logger = Logger.getLogger(Desensobrado.class.getName());	
	
    /** Creates new form desensobrado */
    public Desensobrado() {
        initComponents();
    }

    /**
     * Inicializacion de los componentes
     */
    private void initComponents() {
    	setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridwidth = 2;
		c.gridx = 0;

    	// Etiqueta sobre digital a abrir
    	JLabel etiquetaFichero = new JLabel();
    	etiquetaFichero.setText(Messages.getString("Desensobrado.buscar")); // NOI18N
		add(etiquetaFichero, c);
		
		c.insets = new Insets(0, 13, 0, 0);
		c.gridwidth = 1;
		c.gridy	= 1;

        // Campo con el nombre del archivo a extraer
        final JTextField campoFichero = new JTextField();
        campoFichero.setToolTipText(Messages.getString("Desensobrado.buscar.caja.description")); // NOI18N
        campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Desensobrado.buscar.caja.description.status")));
        campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Desensobrado.buscar.caja.description.status")));
        campoFichero.getAccessibleContext().setAccessibleName(Messages.getString("Desensobrado.buscar.caja")); // NOI18N
        campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Desensobrado.buscar.caja.description")); // NOI18N
		add(campoFichero, c);
		
		c.insets = new Insets(0, 10, 0, 13);
		c.weightx = 0.0;
		c.gridx = 1;
        
        // Boton examinar
        JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); // NOI18N
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N
        examinar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.Examinar.description.status")));
        examinar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.Examinar.description.status")));
        examinar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                examinarActionPerformed(campoFichero);
            }
        });
        examinar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.Examinar")); // NOI18N
        examinar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N
		add(examinar, c);
		
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridwidth = 2;
		c.gridx = 0;
		c.gridy	= 2;
        
        // Etiqueta almacen o repositorio
        JLabel etiquetaAlmacen = new JLabel();
        etiquetaAlmacen.setText(Messages.getString("Desensobrado.almacen")); // NOI18N
        add(etiquetaAlmacen, c);

		c.insets = new Insets(0, 13, 0, 13);
		c.gridy = 3;
		
        // Combo con el almacen o repositorio de certificados
        final JComboBox comboAlmacen = new JComboBox();
        comboAlmacen.setToolTipText(Messages.getString("Desensobrado.almacen.combo.description")); // NOI18N
        comboAlmacen.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Desensobrado.almacen.combo.description.status")));
        comboAlmacen.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Desensobrado.almacen.combo.description.status")));
        comboAlmacen.getAccessibleContext().setAccessibleName(Messages.getString("Desensobrado.almacen.combo")); // NOI18N
        comboAlmacen.getAccessibleContext().setAccessibleDescription(Messages.getString("Desensobrado.almacen.combo.description")); // NOI18N
        cargarComboAlmacen(comboAlmacen);
        add(comboAlmacen, c);
        
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridy = 4;
        
        // Etiqueta con las opciones de apertura
        JLabel etiquetaOpciones = new JLabel();
        etiquetaOpciones.setText(Messages.getString("Desensobrado.opciones")); // NOI18N
        add(etiquetaOpciones, c);
        
		c.insets = new Insets(0, 13, 0, 13);
		c.gridy = 5;

        // Checkbox para iniciar el contenido
        final JCheckBox checkInicar = new JCheckBox();
        checkInicar.setText(Messages.getString("Desensobrado.check")); // NOI18N
        checkInicar.setToolTipText(Messages.getString("Desensobrado.check.check.description")); // NOI18N
        checkInicar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Desensobrado.check.check.description.status")));
        checkInicar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Desensobrado.check.check.description.status")));
        checkInicar.getAccessibleContext().setAccessibleName(Messages.getString("Desensobrado.check.check")); // NOI18N
        checkInicar.getAccessibleContext().setAccessibleDescription(Messages.getString("Desensobrado.check.check.description")); // NOI18N
        add(checkInicar, c);
		
		c.weighty = 1.0;
		c.gridy = 6;
        
		// Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
		JPanel emptyPanel = new JPanel();
		add(emptyPanel, c);
		
		// Panel con los botones
		Panel panelBotones = new Panel(new GridBagLayout());
		
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 15;
		cons.gridx = 0;
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		panelBotones.add(label, cons);
		
        // Boton descifrar
        JButton extraer = new JButton();
        extraer.setMnemonic(KeyEvent.VK_C);
        extraer.setText(Messages.getString("Desensobrado.btnDescifrar")); // NOI18N
        extraer.setToolTipText(Messages.getString("Desensobrado.btnDescifrar.description")); // NOI18N
        extraer.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Desensobrado.btnDescifrar.description.status")));
        extraer.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Desensobrado.btnDescifrar.description.status")));
        extraer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	extraerActionPerformed(comboAlmacen, campoFichero, checkInicar);
            }
        });
        extraer.getAccessibleContext().setAccessibleName(Messages.getString("Desensobrado.btnDescifrar")); // NOI18N
        extraer.getAccessibleContext().setAccessibleDescription(Messages.getString("Desensobrado.btnDescifrar.description")); // NOI18N
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(extraer, BorderLayout.CENTER);
		
		cons.ipadx = 0;
		cons.gridx = 1;
		cons.weightx = 1.0;
        
		panelBotones.add(buttonPanel, cons);
		
        // Boton de ayuda
        JLabel botonAyuda = HelpUtils.fechButton("desensobrado");
		
        cons.ipadx = 15;
		cons.weightx = 0.0;
		cons.gridx = 2;
		
		panelBotones.add(botonAyuda, cons);

		c.gridwidth	= 2;
        c.insets = new Insets(13,13,13,13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridy = 7;
		
		add(panelBotones, c);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoFichero,"desensobrado.sobre");
        HelpUtils.enableHelpKey(examinar,"desensobrado.sobre");
        HelpUtils.enableHelpKey(comboAlmacen,"desensobrado.almacen");
        HelpUtils.enableHelpKey(checkInicar,"desensobrado.iniciar");
    }
    
    /**
     * Carga el combo almacen respecto al sistema operativo en el que se encuentra 
     * la aplicación
     * @param comboAlmacen	Combo donde se cargan los tipos de almacen
     * @return	comboAlmacen cargado
     */
    private void cargarComboAlmacen(JComboBox comboAlmacen) {
    	comboAlmacen.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
	}

	/**
	 * Pulsar boton examinar: Muestra una ventana para seleccinar un archivo.
	 * Modifica el valor de la caja con el nombre del archivo seleccionado
	 * @param campoFichero	Campo en el que se escribe el nombre del fichero seleccionado
	 */
    private void examinarActionPerformed(JTextField campoFichero) {
    	File selectedFile = new SelectionDialog().showFileOpenDialog(this, Messages.getString("Seleccione.fichero.desensobrar"));
    	if (selectedFile != null) {
    		campoFichero.setText(selectedFile.getAbsolutePath());
    	}
    }

    /**
	 * Pulsar boton extraer: Extrae la informacion del sobre
	 * @param comboAlmacen 	Combo con el almacen de claves
	 * @param campoFichero 	Campo con el nombre del fichero a extraer
	 * @param checkIniciar	Checkbox que indica si los datos se deben de iniciar
	 */
    private void extraerActionPerformed(JComboBox comboAlmacen, JTextField campoFichero, 
    		JCheckBox checkIniciar) {
    	// Obtenemos la ruta del sobre
    	String envelopPath = campoFichero.getText();
    	if(envelopPath == null || envelopPath.equals("") || !new File(envelopPath).exists() || !new File(envelopPath).isFile()) 
    		JOptionPane.showMessageDialog(this, Messages.getString("Desensobrado.msg.erro.fichero"), Messages.getString("Desensobrado.btnDescifrar"), JOptionPane.WARNING_MESSAGE);
    	else {
    		byte[] envelopData = null;
    		try{
    			File file = new File(envelopPath);
    			FileInputStream envelopFis = new FileInputStream(file);
    			envelopData = AOUtil.getDataFromInputStream(envelopFis);
    		} catch (Exception e) {
    			logger.severe("No se ha encontrado o no se ha podido leer el fichero: "+envelopPath);
    			JOptionPane.showMessageDialog(this, Messages.getString("Desensobrado.msg.error.fichero2"), "Error", JOptionPane.ERROR_MESSAGE);
    			return;
    		}

    		AOKeyStoreManager keyStoreManager = null;
    		PrivateKeyEntry privateKeyEntry = null;
    		try {
    			keyStoreManager = getKeyStoreManager((KeyStoreConfiguration) comboAlmacen.getSelectedItem());
    			privateKeyEntry = getPrivateKeyEntry(keyStoreManager, comboAlmacen);
    		} catch (AOCancelledOperationException e) {
    			logger.severe("Operacion cancelada por el usuario");
    			return;
    		} catch (AOException e) {
    			logger.severe("Ocurrio un error al abrir el almacen de claves del usuario: "+e);
    			JOptionPane.showMessageDialog(this, Messages.getString("Desensobrado.msg.error.almacen"), "Error", JOptionPane.ERROR_MESSAGE);
    			return;
    		} catch (Exception e) {
    			logger.severe("Ocurrio un error al recuperar el certificado del usuario: "+e);
    			JOptionPane.showMessageDialog(this, Messages.getString("Desensobrado.msg.error.certificado"), "Error", JOptionPane.ERROR_MESSAGE);
    			return;
    		}

    		// Identificamos el tipo de envoltorio y recuperamos los datos
    		byte[] recoveredData = null;
    		try {
    			// EnvelopedData
    			if (CMSHelper.isCMSValid(envelopData, AOConstants.CMS_CONTENTTYPE_ENVELOPEDDATA))
    				recoveredData = new CMSDecipherEnvelopData().dechiperEnvelopData(envelopData, privateKeyEntry);
    			// SignedAndEnvelopedData
    			else if(CMSHelper.isCMSValid(envelopData, AOConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA))
    				recoveredData = new CMSDecipherSignedAndEnvelopedData().dechiperSignedAndEnvelopData(envelopData, privateKeyEntry);
    			// AuthenticatedAndEnvelopedData
    			else if(CMSHelper.isCMSValid(envelopData, AOConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA))
    				recoveredData = new CMSDecipherAuthenticatedEnvelopedData().dechiperAuthenticatedEnvelopedData(envelopData, privateKeyEntry);
    			// Envoltorio no reconocido
    			else {
    				JOptionPane.showMessageDialog(this, Messages.getString("Desensobrado.msg.error.sobre"), "Error", JOptionPane.ERROR_MESSAGE);
    				return;
    			}
    		} catch (AOException e) {
    			logger.severe("Ocurrio un error al abrir el sobre digital: "+e);
    			JOptionPane.showMessageDialog(this, Messages.getString("Desensobrado.msg.error.sobre.abrir"), "Error", JOptionPane.ERROR_MESSAGE);
    			return;
    		} catch (Exception e) {
    			logger.severe("Ocurrio un error al abrir el sobre digital: "+e);
    			JOptionPane.showMessageDialog(this, Messages.getString("Desensobrado.msg.error.sobre.abrir"), "Error", JOptionPane.ERROR_MESSAGE);
    			return;
    		}

    		// Quitamos la extension del nombre de fichero y establecemos su nombre como el por defecto
    		String name = new File(envelopPath).getName();
    		if(name.lastIndexOf('.') != -1) {
    			name = name.substring(0, name.lastIndexOf('.'));
    		}

    		// Salvamos los datos
    		String ruta = AOUIManager.saveDataToFile(this, recoveredData, new File(name), null);
    		if (ruta != null && checkIniciar.isSelected()){
    			Utils.openFile(ruta);
    		}
    	}
    }

    private AOKeyStoreManager getKeyStoreManager(KeyStoreConfiguration ksConfiguration) throws AOException {
    	PasswordCallback pssCallback;
    	AOConstants.AOKeyStore store = ksConfiguration.getType();
    	if (store == AOConstants.AOKeyStore.WINDOWS ||
    			store == AOConstants.AOKeyStore.WINROOT) pssCallback = new NullPasswordCallback();
    	else  pssCallback = new UIPasswordCallback(Messages.getString("Msg.pedir.contraenia") + " " + store.getDescription(), null); //$NON-NLS-1$ //$NON-NLS-2$

    	try {
	    	return AOKeyStoreManagerFactory.getAOKeyStoreManager(
	    			store,
	    			ksConfiguration.getLib(),
	    			ksConfiguration.toString(),
	    			pssCallback,
	    			this
	    	);
    	}
    	catch(final Exception e) {
    		throw new AOException("Error al inicializar el almacen", e);
    	}
    }

    private PrivateKeyEntry getPrivateKeyEntry(AOKeyStoreManager keyStoreManager, JComboBox comboAlmacen) throws AOException {
    	// Seleccionamos un certificado
    	String selectedcert = AOUIManager.showCertSelectionDialog(keyStoreManager.getAliases(), keyStoreManager.getKeyStores(), this, true, true, true);

    	// Comprobamos si se ha cancelado la seleccion
    	if (selectedcert == null) 
    		throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$

    	// Recuperamos la clave del certificado
    	PrivateKeyEntry privateKeyEntry = null;
    	try {
    		privateKeyEntry = keyStoreManager.getKeyEntry(
    				selectedcert,
    				AOCryptoUtil.getCertificatePC(((KeyStoreConfiguration) comboAlmacen.getSelectedItem()).getType(), this)
    		);
    	}
    	catch (AOCertificateKeyException e) {
    		throw e;
    	}
    	catch (AOCancelledOperationException e) {
    		// Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
    		// Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
    		// que las relanza en forma de AOException
    		throw e;
    	}
    	catch (Exception e) {
    		e.printStackTrace();
    		logger.severe("No se ha podido obtener el certicado con el alias '" + selectedcert + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		throw new AOException(e.getMessage());
    	}
    	return privateKeyEntry;
    }
}
