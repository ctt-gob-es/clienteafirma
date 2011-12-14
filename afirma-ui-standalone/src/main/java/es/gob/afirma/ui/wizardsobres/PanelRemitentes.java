/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardsobres;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidKeyException;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.envelopers.cms.AOCMSEnveloper;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.main.common.KeyStoreConfiguration;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.UIPasswordCallbackAccessibility;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.CertificateDestiny;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 *
 * Clase que contiene los elementos necesarios para crear un grupo de Remitenres
 * a partir de una seleccion de certificados de remitentes.
 */
public class PanelRemitentes extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelRemitentes.class.getName());

	public static int SOBRE_AUTENTICADO = 0; 
    public static int SOBRE_FIRMADO = 1;
    public static int SOBRE_SIMPLE = 2;
    
    @Override
	public int getMinimumRelation(){
		return 8;
	}
	
    /**
     * Tipo de ensobrado
     */
	private Integer tipo;
	
	/**
	 * Ruta donde se encuentra el fichero a ensobrar
	 */
	private String rutafichero;
	
	/**
	 * Lista con los certificados a utilizar
	 */
	private List<CertificateDestiny> listaCertificados;
	
	/**
	 * Lista con los remitentes
	 */
	private List<CertificateDestiny> listaCertificadosRe = new ArrayList<CertificateDestiny>();
		
	/**
	 * Configuracion del KeyStore
	 */
	private KeyStoreConfiguration kconf;
	
	/**
	 * Clave del certificado
	 */
	private PrivateKeyEntry privateKeyEntry;
	
	public void setListaCertificados(List<CertificateDestiny> listaCertificados) {
		this.listaCertificados = listaCertificados;
	}
	
	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	public void setVentanas(List<JDialogWizard> ventanas) {
		this.setBotonera(new Botonera(ventanas, 2));
    	getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
	}
	
	public PanelRemitentes(String rutafichero, Integer tipo) {
		this.rutafichero = rutafichero;
		this.tipo = tipo;
		initComponents();
	}

	// Lista de remitentes
	private JList listaRemitentes = new JList();
	 // Etiqueta con el texto "Anadir remitente desde..."
	private JLabel etiquetaAnadir = new JLabel();
	
	/**
	 * Inicializacion de componentes
	 */
	private void initComponents() {
		// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.sobres.titulo"));
		
		// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.sobres.pagina2.titulo", "Wizard.sobres.pagina2.titulo.explicacion1", "Wizard.sobres.pagina2.titulo.explicacion2", null, true);
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
        c.insets = new Insets(10, 20, 0, 20);
        c.gridwidth = 2;
		c.weightx = 1.0;
		c.gridx = 0;
    	
		//Etiqueta con el texto "Puede anadir uno o..."
		InfoLabel labelText = new InfoLabel(Messages.getString("Wizard.sobres.pagina2.contenido.explicacion1"), false);
		panelCentral.add(labelText, c);

        c.gridy = 1;
		c.gridwidth = 1;
        
		JPanel panelEtiquetaAnadir = new JPanel(new GridLayout(1, 1));
        // Etiqueta con el texto "Anadir remitente desde..."
        this.etiquetaAnadir = new JLabel();
        this.etiquetaAnadir.setText(Messages.getString("Wizard.sobres.aniadir.originante"));
        Utils.setContrastColor(this.etiquetaAnadir);
        Utils.setFontBold(this.etiquetaAnadir);
        panelEtiquetaAnadir.add(this.etiquetaAnadir);
		panelCentral.add(panelEtiquetaAnadir, c);
		
		c.insets = new Insets(0, 20, 0, 0);
		c.gridwidth = 1;
		c.gridy = 2;
		c.weightx = 1.0;
		c.weighty = 0.1;
		c.fill = GridBagConstraints.BOTH;
		
		// Combo con los repositorios / almacenes
		final JComboBox comboRepositorios = new JComboBox();
		comboRepositorios.setToolTipText(Messages.getString("wizard.comboRepositorios.description"));
		comboRepositorios.getAccessibleContext().setAccessibleName(this.etiquetaAnadir.getText() + " " + comboRepositorios.getToolTipText() + " ALT + D.");
		comboRepositorios.getAccessibleContext().setAccessibleDescription(comboRepositorios.getToolTipText());
		Utils.remarcar(comboRepositorios);
        Utils.setContrastColor(comboRepositorios);
		Utils.setFontBold(comboRepositorios);
		cargarCombo(comboRepositorios);
		panelCentral.add(comboRepositorios, c);
		
		//Relación entre etiqueta y combo
		this.etiquetaAnadir.setLabelFor(comboRepositorios);
		//Asignación de mnemónico
		this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D);
		
		c.insets = new Insets(0, 10, 0, 20);
		c.gridx = 1;
		c.weightx = 0.0;
		c.weighty = 0.0;
		c.fill = GridBagConstraints.HORIZONTAL;
		
		JPanel panelAnadir = new JPanel(new GridLayout(1, 1));
		// Boton Anadir
		final JButton anadir = new JButton();
		final JButton eliminar = new JButton();
		anadir.setToolTipText(Messages.getString("Wizard.sobres.aniadir.originante.description"));
		anadir.setText(Messages.getString("wizard.aniadir")); 
		anadir.setAutoscrolls(true);
		anadir.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al botón
		anadir.getAccessibleContext().setAccessibleName(anadir.getText() + " " + anadir.getToolTipText());
		anadir.getAccessibleContext().setAccessibleDescription(anadir.getToolTipText());
		anadir.addActionListener(new ActionListener() {
			@Override
            public void actionPerformed(ActionEvent evt) {
				anadirActionPerformed(comboRepositorios, eliminar, anadir);
			}
		});
		Utils.remarcar(anadir);
        Utils.setContrastColor(anadir);
		Utils.setFontBold(anadir);
		panelAnadir.add(anadir);
		panelCentral.add(panelAnadir, c);
		
		c.insets = new Insets(10, 20, 0, 20);
		c.gridx = 0;
		c.gridy = 3;
		c.weightx =0.0;
		
		 // Etiqueta con el texto "Remitentes"
        JLabel senderLabel = new JLabel();
        senderLabel.setText(Messages.getString("wizard.sobres.listaRemitentes"));
        Utils.setContrastColor(senderLabel);
        Utils.setFontBold(senderLabel);
		panelCentral.add(senderLabel, c);
		
		c.insets = new Insets(0, 20, 0, 20);
		c.ipady = 80;
		c.gridwidth = 2;
		c.gridy = 4;
		c.gridx = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.fill = GridBagConstraints.BOTH;
		
		// Panel que contiene la lista de remitentes
		JScrollPane panelLista = new JScrollPane();
		panelCentral.add(panelLista, c);
		
		// Listado de remitentes
		this.listaRemitentes.setToolTipText(Messages.getString("Wizard.sobres.listaRemitentes.description"));
		this.listaRemitentes.setModel(new DefaultListModel());
		this.listaRemitentes.getAccessibleContext().setAccessibleName(senderLabel.getText() + " "+ this.listaRemitentes.getToolTipText());
		this.listaRemitentes.getAccessibleContext().setAccessibleDescription(this.listaRemitentes.getToolTipText());
		Utils.remarcar(this.listaRemitentes);
        Utils.setContrastColor(this.listaRemitentes);
		Utils.setFontBold(this.listaRemitentes);
		panelLista.setViewportView(this.listaRemitentes);
		
		//Relación entre etiqueta y lista
		senderLabel.setLabelFor(this.listaRemitentes);
		//Asignación de mnemónico
		senderLabel.setDisplayedMnemonic(KeyEvent.VK_T);
		
		c.ipady = 0;
		c.gridwidth = 1;
		c.gridy = 5;
		c.weightx = 1.0;
		c.fill = GridBagConstraints.HORIZONTAL;
		
		// Espacio izdo del boton
		Panel panelVacio = new Panel();
		panelCentral.add(panelVacio, c);
		
		c.insets = new Insets(5, 0, 5, 20);
		c.weightx = 0.0;
		c.gridx = 1;
		
		JPanel panelEliminar = new JPanel(new GridLayout(1, 1));
		// Boton eliminar
		eliminar.setEnabled(false);
		eliminar.setToolTipText(Messages.getString("Wizard.sobres.eliminar.remitente.description"));
		eliminar.setText(Messages.getString("wizard.sobres.eliminar.remitente"));
		eliminar.getAccessibleContext().setAccessibleName(eliminar.getText() + " " + eliminar.getToolTipText());
		eliminar.getAccessibleContext().setAccessibleDescription(eliminar.getToolTipText());
		eliminar.addActionListener(new ActionListener() {
			@Override
            public void actionPerformed(ActionEvent evt) {
				eliminarActionPerformed(comboRepositorios, eliminar, anadir);
			}
		});

		Utils.remarcar(eliminar);
        Utils.setContrastColor(eliminar);
		Utils.setFontBold(eliminar);
		panelEliminar.add(eliminar);
		panelCentral.add(panelEliminar, c);
		
		getContentPane().add(panelCentral, BorderLayout.CENTER);

		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(comboRepositorios,"ensobrado.wizard.repositorio.remitente");
		HelpUtils.enableHelpKey(labelText,"ensobrado.wizard. remitentes");
	}

	/**
	 * Carga un combo con los repositorios/almacenes disponibles
	 * @param comboRepositorios	Combo donde se deben cargar los repositorios
	 */
	private void cargarCombo(JComboBox comboRepositorios) {
		comboRepositorios.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
	}

	/**
	 * A&ntilde;ade un nuevo remitente desde el repositorio indicado
	 * @param comboRepositorios	combo con el listado de repositorios / almacenes
	 * @param listModel  		Modelo de la lista de remitentes
	 * @param eliminar			Boton para eliminar un remitente del listado de repositorios
	 * @param anadir			Boton para anadir un remitente al listado de repositorios
	 */
	void anadirActionPerformed(JComboBox comboRepositorios, JButton eliminar, JButton anadir) {
		this.kconf = (KeyStoreConfiguration) comboRepositorios.getSelectedItem();

		final AOKeyStoreManager keyStoreManager;
		try {
			AOKeyStore ao = this.kconf.getType();
			String lib = null;
			if (ao == AOKeyStore.PKCS12 || ao == AOKeyStore.SINGLE) {
				ExtFilter filter;
				if (ao == AOKeyStore.PKCS12) {
					filter = new ExtFilter(
							new String[] { "p12", "pfx" }, //$NON-NLS-1$ //$NON-NLS-2$
							Messages.getString("Filtro.fichero.pkcs12.descripcion")); //$NON-NLS-1$
				} else {
					filter = new ExtFilter(
							new String[] { "cer", "p7b", "p7s" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							Messages.getString("Filtro.fichero.certificado.descripcion")); //$NON-NLS-1$
				}
				File keystorePath = SelectionDialog.showFileOpenDialog(this, Messages.getString("Ensobrado.dialogo.almacen.titulo"), filter); //$NON-NLS-1$
				if (keystorePath == null) {
					throw new AOCancelledOperationException();
				}
				lib = keystorePath.getAbsolutePath();
			}
			keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(ao, lib, null, getPreferredPCB(ao), this);
		} catch (AOCancelledOperationException e) {
			logger.severe("Operacion cancelada por el usuario");
			return;
		}catch (InvalidKeyException e) {
			//Control de la excepcion generada al introducir mal la contrasena para el almacen
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.almacen.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
            return;
        }  catch (AOKeystoreAlternativeException e) {
        	//Control de la excepcion generada al introducir una contraseoa vacia para el almacen
        	CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.almacen.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
             return;
        } catch (Exception e) {
			logger.severe("No se ha podido abrir el almacen de certificados: "+e);
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.certificados.almacen"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return;
		}

		// Obtenemos el certificado
		CertificateDestiny certDest = new CertificateDestiny(keyStoreManager, this);
		
		// Comprobamos que el certificado es correcto
		if (certDest.getAlias() != null && !certDest.equals("")) {	
			boolean copiar = true;
			DefaultListModel listModel = (DefaultListModel) this.listaRemitentes.getModel();
			for (int i=0; i < listModel.getSize(); i++){
				CertificateDestiny c = (CertificateDestiny) listModel.getElementAt(i);
				if (certDest.getAlias().equals(c.getAlias())){
					copiar = false;
				}
			}
			if (copiar) {
				listModel.addElement(certDest.getAlias());
				this.listaCertificadosRe.add(certDest);
				anadir.setEnabled(false);
				anadir.setMnemonic(0); //Se asigna un atajo vacio puesto que se ha deshabilitado el boton
				comboRepositorios.setEnabled(false);
				this.etiquetaAnadir.setDisplayedMnemonic(0); //Se asigna un atajo vacio puesto que se ha deshabilitado el combo asociado
				this.etiquetaAnadir.getAccessibleContext().setAccessibleName(this.etiquetaAnadir.getText() + " " + Messages.getString("wizard.sobres.etiquetaAnadir"));
				this.etiquetaAnadir.setFocusable(true);
				eliminar.setEnabled(true);
				eliminar.setMnemonic(KeyEvent.VK_E); //Se asigna un atajo al boton ya que ha sido habilitado
			} else
				CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.sobres.error.usuario"), 
						Messages.getString("error"), JOptionPane.WARNING_MESSAGE);
		}
		
		// Preguntamos por la contrasena del certificado
		if (!this.listaCertificadosRe.isEmpty())
			try {
				this.privateKeyEntry = getPrivateKeyEntry(keyStoreManager, certDest.getAlias(),this.kconf);
			} 
	       	catch (KeyException e) {
	       		//Control de la excepcion generada al introducir mal la contrasena para el certificado
	            JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.error.certificados.contrasenia"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
	            return;
	        }catch(AOException e){
	    		logger.warning("Error al obtener la clave del certificado: "+e);
	    		CustomDialog.showMessageDialog(this, true, Messages.getString("Ensobrado.msg.error.clave"), 
	    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
	    		
	    		clearWizard(comboRepositorios, eliminar, anadir);
	    		
	    		return;
	    	} catch (AOCancelledOperationException e) {
	    		clearWizard(comboRepositorios, eliminar, anadir);
	    	}
	}

	/**
	 * Resetea todo el wizard de esta pagina dejandolo como al inicio
	 * @param comboRepositorios	Combo con los repositorios
	 * @param eliminar			Boton eliminar
	 * @param anadir			Boton anadir
	 */
	private void clearWizard(JComboBox comboRepositorios, JButton eliminar,
			JButton anadir) {
		this.listaCertificadosRe.remove(this.listaCertificadosRe.get(0));
		((DefaultListModel) this.listaRemitentes.getModel()).remove(0);

		if (this.listaCertificadosRe.isEmpty()) {
			anadir.setEnabled(true);
			anadir.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al botón puesto que se ha habilitado
			comboRepositorios.setEnabled(true); 
			this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); //Se asigna un atajo puesto que se ha habilitado el combo asociado
			this.etiquetaAnadir.setFocusable(false);
			eliminar.setEnabled(false);
			eliminar.setMnemonic(0); //Se asigna un atajo vacio al botón ya que ha sido deshabilitado
		}
	}

	/**
	 * Elimina un remitente de la lista de remitentes
	 * @param comboRepositorios	Combo con el repositorio / almacen
	 * @param eliminar			Boton para eliminar un remitente de la lista
	 * @param anadir			Boton para anadir un remitente a la lista
	 */
	private void eliminarActionPerformed(JComboBox comboRepositorios, JButton eliminar, JButton anadir) {
		for (int i=0; i<this.listaCertificadosRe.size(); i++)
			if (this.listaCertificadosRe.get(i).getAlias().equals(this.listaRemitentes.getSelectedValue())) {
				this.listaCertificadosRe.remove(this.listaCertificadosRe.get(i));
				((DefaultListModel) this.listaRemitentes.getModel()).remove(this.listaRemitentes.getSelectedIndex());
				break;
			}
		
		if (this.listaCertificadosRe.isEmpty()) {
			anadir.setEnabled(true);
			anadir.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al botón puesto que se ha habilitado
			comboRepositorios.setEnabled(true);
			this.etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); //Se asigna un atajo puesto que se ha habilitado el combo asociado
			this.etiquetaAnadir.setFocusable(false);
			eliminar.setEnabled(false);
			eliminar.setMnemonic(0); //Se asigna un atajo vacio al botón ya que ha sido deshabilitado
		}
		
		// Borramos las posibles claves del certificado
		this.privateKeyEntry = null;
	}

	/**
	 * Recupera el PasswordCallback que t&iacute;picamente se requiere para el acceso a un
	 * almac&eacute;n de claves.
	 * @param kStore Almac&eacuten de claves
	 */
	private PasswordCallback getPreferredPCB(AOKeyStore kStore) {
		if (kStore == null)
			throw new NullPointerException("No se ha indicado el KeyStore del que desea " +
				"obtener el PasswordCallBack");

		PasswordCallback pssCallback;
		if (kStore == AOKeyStore.WINDOWS || kStore == AOKeyStore.WINROOT
				|| kStore == AOKeyStore.PKCS11 || kStore == AOKeyStore.SINGLE)
			pssCallback = new NullPasswordCallback();
		else {
			//pssCallback = new UIPasswordCallback(Messages.getString("Wizard.sobres.almacen.pass")+" "+kStore.getDescription(), this);
			pssCallback = new UIPasswordCallbackAccessibility(Messages.getString("Wizard.sobres.almacen.pass")+" "+kStore.getDescription(), this,
        			Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title"));
		}
		
		return pssCallback;
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
			continuar = ensobrar();
			
			if (continuar == true) {
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			}
		}
	}

	/**
	 * Ensobra el fichero dado
	 * @return	True o false si la operacion ha sido satisfactoria
	 */
	public Boolean ensobrar() {
		if (this.tipo.equals(SOBRE_AUTENTICADO) || this.tipo.equals(SOBRE_FIRMADO)) {
            DefaultListModel listModel = (DefaultListModel) this.listaRemitentes.getModel();
            if (listModel.isEmpty()) {
            	CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.error.remitente"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
		
    	try {
    		X509Certificate[] certs = new X509Certificate[this.listaCertificados.size()];
    		for (int i=0; i<this.listaCertificados.size(); i++) {
    			certs[i] = (X509Certificate) this.listaCertificados.get(i).getCertificate();
    		}
    		
    		AOCMSEnveloper enveloper = new AOCMSEnveloper();
    		enveloper.setSignatureAlgorithm(GeneralConfig.getSignAlgorithm());
    		
    		AOCipherConfig cipherConfig = new AOCipherConfig(AOCipherAlgorithm.AES, null, null);
    		
    		// por defecto se realizara un sobre envelopedData
    		byte[] envelopedData = null;
    		byte[] contentData = readFile(this.rutafichero);
    		try {
    			if (this.tipo.equals(SOBRE_AUTENTICADO)) {
    				envelopedData = enveloper.createCMSAuthenticatedEnvelopedData(contentData, this.privateKeyEntry, cipherConfig, certs);
    			} else if (this.tipo.equals(SOBRE_FIRMADO)) {
    				envelopedData = enveloper.createCMSSignedAndEnvelopedData(contentData, this.privateKeyEntry, cipherConfig, certs);
    			} else if (this.tipo.equals(SOBRE_SIMPLE)) {
    				envelopedData = enveloper.createCMSEnvelopedData(contentData, this.privateKeyEntry, cipherConfig, certs);
    			}
    		} catch (Exception e) {
    			System.err.println("ERROR");
    			e.printStackTrace();
    			return false;
    		}

    		// Guardamos el sobre generado
    		final File savedFile = SelectionDialog.saveDataToFile(Messages.getString("wizard.sobres.filechooser.save.title"), envelopedData,
    		        new File(this.rutafichero).getName(), null, this);
    		// Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
			if (savedFile == null) {
				return false;
			}
    		
    	} catch(AOCancelledOperationException e) {
    		logger.warning("La operaci&oacute;n ha sido cancelada por el usuario: "+e);
    		CustomDialog.showMessageDialog(this, true, Messages.getString("Ensobrado.msg.error.generacion"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
		} catch (FileNotFoundException e) {
			logger.warning("No se puede encontrar el fichero seleccionado: "+e);
			CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.error.encontrar.fichero"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
		} catch (IOException e) {
			logger.warning("No ha sido posible leer el fichero indicado: "+e);
			CustomDialog.showMessageDialog(this, true, Messages.getString("Cifrado.msg.error.lectura.generico"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	}  catch(Exception e){
    		logger.warning("Ocurrio un error durante la desenvoltura: "+e);
    		CustomDialog.showMessageDialog(this, true, Messages.getString("WizardCifrado.error.desenvoltura"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	}

    	return true;
	}
	
	/**
	 * Obtiene la clave privada 
	 * @param keyStoreManager	Manager del KeyStore
	 * @param seleccionado		Certificado seleccionado
	 * @param kconf				Configuracion del KeyStore
	 * @return
	 * @throws AOException
	 */
	private PrivateKeyEntry getPrivateKeyEntry(AOKeyStoreManager keyStoreManager, String seleccionado, 
			KeyStoreConfiguration kconf) throws AOException, KeyException {

		// Comprobamos si se ha cancelado la seleccion
		if (seleccionado == null) 
			throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$

		// Recuperamos la clave del certificado
		PrivateKeyEntry privateKeyEntry = null;
		try {
			privateKeyEntry = keyStoreManager.getKeyEntry(seleccionado, Utils.getCertificatePC(kconf.getType(), this));
		} catch (KeyException e) {
			throw new KeyException("Ocurrio un error al extraer la clave del certificado", e);
		} catch (AOCancelledOperationException e) {
			// Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
			// Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
			// que las relanza en forma de AOException
			throw e;
		} catch (Exception e) {
			logger.severe("No se ha podido obtener el certicado con el alias '" + seleccionado + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			throw new AOException(e.getMessage());
		}
		
		return privateKeyEntry;
	}
	
	private byte[] readFile(String filepath) throws FileNotFoundException, IOException {
		byte[] data = null;
		InputStream fileIn = null;
		try {
			fileIn = AOUtil.loadFile(AOUtil.createURI(filepath));
			data = AOUtil.getDataFromInputStream(fileIn);

		} catch (AOException e) {
			throw new IOException("Ocurrio un error al cargar el fichero de datos", e);
		} finally {
			if (fileIn != null) {
				try { fileIn.close(); } catch (Exception e) {}
			}
		}
		
		return data;
	}
}
