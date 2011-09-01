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
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
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
import javax.swing.JTextPane;

import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.cliente.AOCMSEnveloper;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCertificateKeyException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.ui.AOUIManager;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.CertificateDestiny;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;

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
	 * Manager del KeyStore
	 */
	private AOKeyStoreManager keyStoreManager;
	
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
		Botonera botonera = new Botonera(ventanas, 2);
		getContentPane().add(botonera, BorderLayout.PAGE_END);
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
        CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.sobres.pagina2.titulo", "Wizard.sobres.pagina2.titulo.explicacion", null, true);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);
		
        // Panel central
    	JPanel panelCentral = new JPanel();
    	panelCentral.setMinimumSize(new Dimension(603, 289));
    	panelCentral.setLayout(new GridBagLayout());
        
    	// Configuramos el layout
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 2;
		c.weightx = 1.0;
		c.gridx = 0;
    	
    	// Panel con el texto "Puede anadir uno o..."
    	JTextPane panelTexto = PanelesTexto.generarPanelTexto("Wizard.sobres.pagina2.contenido.explicacion1", false);
        panelCentral.add(panelTexto, c);
        
        c.gridy = 1;
		c.gridwidth = 1;
        
        // Etiqueta con el texto "Anadir remitente desde..."
        etiquetaAnadir = new JLabel();
        etiquetaAnadir.setText(Messages.getString("Wizard.sobres.aniadir.originante"));
		panelCentral.add(etiquetaAnadir, c);
		
		c.insets = new Insets(0, 20, 0, 0);
		c.gridwidth = 1;
		c.gridy = 2;
		c.weightx = 1.0;
		c.weighty = 0.1;
		c.fill = GridBagConstraints.BOTH;
		
		// Combo con los repositorios / almacenes
		final JComboBox comboRepositorios = new JComboBox();
		comboRepositorios.setToolTipText(Messages.getString("wizard.comboRepositorios.description"));
		cargarCombo(comboRepositorios);
		panelCentral.add(comboRepositorios, c);
		
		//Relación entre etiqueta y combo
		etiquetaAnadir.setLabelFor(comboRepositorios);
		//Asignación de mnemónico
		etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D);
		
		c.insets = new Insets(0, 10, 0, 20);
		c.gridx = 1;
		c.weightx = 0.0;
		c.weighty = 0.0;
		c.fill = GridBagConstraints.HORIZONTAL;
		
		// Boton Anadir
		final JButton anadir = new JButton();
		final JButton eliminar = new JButton();
		anadir.setToolTipText(Messages.getString("Wizard.sobres.aniadir.originante.description"));
		anadir.setText(Messages.getString("wizard.aniadir")); 
		anadir.setAutoscrolls(true);
		anadir.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al botón
		anadir.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				anadirActionPerformed(comboRepositorios, eliminar, anadir);
			}
		});
		panelCentral.add(anadir, c);
		
		c.insets = new Insets(10, 20, 0, 20);
		c.gridx = 0;
		c.gridy = 3;
		c.weightx =0.0;
		
		 // Etiqueta con el texto "Remitentes"
        JLabel senderLabel = new JLabel();
        senderLabel.setText(Messages.getString("wizard.sobres.listaRemitentes"));
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
		listaRemitentes.setToolTipText(Messages.getString("Wizard.sobres.listaRemitentes.description"));
		listaRemitentes.setModel(new DefaultListModel());
		panelLista.setViewportView(listaRemitentes);
		
		//Relación entre etiqueta y lista
		senderLabel.setLabelFor(listaRemitentes);
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
		
		c.insets = new Insets(10, 0, 10, 20);
		c.weightx = 0.0;
		c.gridx = 1;
		
		// Boton eliminar
		eliminar.setEnabled(false);
		eliminar.setToolTipText(Messages.getString("Wizard.sobres.eliminar.remitente.description"));
		eliminar.setText(Messages.getString("wizard.sobres.eliminar.remitente")); 
		eliminar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				eliminarActionPerformed(comboRepositorios, eliminar, anadir);
			}
		});
		eliminar.getAccessibleContext().setAccessibleDescription(Messages.getString("wizard.sobres.eliminar.remitente")); 
		panelCentral.add(eliminar, c);
		
		getContentPane().add(panelCentral, BorderLayout.CENTER);

		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(comboRepositorios,"ensobrado.wizard.repositorio.remitente");
		HelpUtils.enableHelpKey(panelTexto,"ensobrado.wizard. remitentes");
	}

	/**
	 * Carga un combo con los repositorios/almacenes disponibles
	 * @param comboRepositorios	Combo donde se deben cargar los repositorios
	 */
	private void cargarCombo(JComboBox comboRepositorios) {
		comboRepositorios.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
	}

	/**
	 * Aï¿½ade un nuevo remitente desde el repositorio indicado
	 * @param comboRepositorios	combo con el listado de repositorios / almacenes
	 * @param listModel  		Modelo de la lista de remitentes
	 * @param eliminar			Boton para eliminar un remitente del listado de repositorios
	 * @param anadir			Boton para anadir un remitente al listado de repositorios
	 */
	private void anadirActionPerformed(JComboBox comboRepositorios, JButton eliminar, JButton anadir) {
		kconf = (KeyStoreConfiguration) comboRepositorios.getSelectedItem();

		try {
			AOKeyStore ao= kconf.getType();
			keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(ao, null, null, getPreferredPCB(ao), this);
		} catch (AOCancelledOperationException e) {
			logger.severe("Operacion cancelada por el usuario");
			return;
		} catch (Exception e) {
			logger.severe("No se ha podido abrir el almacen de certificados: "+e);
			JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.error.certificados.almacen"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return;
		}

		// Obtenemos el certificado
		CertificateDestiny certDest = new CertificateDestiny(keyStoreManager, this);
		
		// Comprobamos que el certificado es correcto
		if (certDest.getAlias() != null && !certDest.equals("")) {	
			boolean copiar = true;
			DefaultListModel listModel = (DefaultListModel) listaRemitentes.getModel();
			for (int i=0; i < listModel.getSize(); i++){
				CertificateDestiny c = (CertificateDestiny) listModel.getElementAt(i);
				if (certDest.getAlias().equals(c.getAlias())){
					copiar = false;
				}
			}
			if (copiar) {
				listModel.addElement(certDest.getAlias());
				listaCertificadosRe.add(certDest);
				anadir.setEnabled(false);
				anadir.setMnemonic(0); //Se asigna un atajo vacío puesto que se ha deshabilitado el botón
				comboRepositorios.setEnabled(false);
				etiquetaAnadir.setDisplayedMnemonic(0); //Se asigna un atajo vacío puesto que se ha deshabilitado el combo asociado
				eliminar.setEnabled(true);
				eliminar.setMnemonic(KeyEvent.VK_E); //Se asigna un atajo al botón ya que ha sido habilitado
			} else
				JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.error.usuario"), 
						Messages.getString("error"), JOptionPane.WARNING_MESSAGE);
		}
		
		// Preguntamos por la contraseï¿½a del certificado
		if (!listaCertificadosRe.isEmpty())
			try {
				privateKeyEntry = getPrivateKeyEntry(keyStoreManager, certDest.getAlias(),kconf);
			} catch(AOException e){
	    		logger.warning("Error al obtener la clave del certificado: "+e);
	    		JOptionPane.showMessageDialog(this, Messages.getString("Ensobrado.msg.error.clave"), 
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
		listaCertificadosRe.remove(listaCertificadosRe.get(0));
		((DefaultListModel) listaRemitentes.getModel()).remove(0);

		if (listaCertificadosRe.isEmpty()) {
			anadir.setEnabled(true);
			anadir.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al botón puesto que se ha habilitado
			comboRepositorios.setEnabled(true); 
			etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); //Se asigna un atajo puesto que se ha habilitado el combo asociado
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
		for (int i=0; i<listaCertificadosRe.size(); i++)
			if (listaCertificadosRe.get(i).getAlias().equals(listaRemitentes.getSelectedValue())) {
				listaCertificadosRe.remove(listaCertificadosRe.get(i));
				((DefaultListModel) listaRemitentes.getModel()).remove(listaRemitentes.getSelectedIndex());
				break;
			}
		
		if (listaCertificadosRe.isEmpty()) {
			anadir.setEnabled(true);
			anadir.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al botón puesto que se ha habilitado
			comboRepositorios.setEnabled(true);
			etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); //Se asigna un atajo puesto que se ha habilitado el combo asociado
			eliminar.setEnabled(false);
			eliminar.setMnemonic(0); //Se asigna un atajo vacio al botón ya que ha sido deshabilitado
		}
		
		// Borramos las posibles claves del certificado
		privateKeyEntry = null;
	}

	/**
	 * Recupera el PasswordCallback que t&iacute;picamente se requiere para el acceso a un
	 * almac&eacute;n de claves.
	 * @param kStore Almac&eacuten de claves
	 */
	private PasswordCallback getPreferredPCB(AOConstants.AOKeyStore kStore) {
		if (kStore == null)
			throw new NullPointerException("No se ha indicado el KeyStore del que desea " +
				"obtener el PasswordCallBack");

		PasswordCallback pssCallback;
		if (kStore == AOConstants.AOKeyStore.WINDOWS || kStore == AOConstants.AOKeyStore.WINROOT
				|| kStore == AOConstants.AOKeyStore.PKCS11 || kStore == AOConstants.AOKeyStore.SINGLE)
			pssCallback = new NullPasswordCallback();
		else {
			pssCallback = new UIPasswordCallback(Messages.getString("Wizard.sobres.almacen.pass")+" "+kStore.getDescription(), this);
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
		if (tipo.equals(SOBRE_AUTENTICADO) || tipo.equals(SOBRE_FIRMADO)) {
            DefaultListModel listModel = (DefaultListModel) listaRemitentes.getModel();
            if (listModel.isEmpty()) {
                JOptionPane.showMessageDialog(this, Messages.getString("WizardCifrado.error.remitente"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
		
    	try {
    		X509Certificate[] certs = new X509Certificate[listaCertificados.size()];
    		for (int i=0; i<listaCertificados.size(); i++) {
    			certs[i] = (X509Certificate) listaCertificados.get(i).getCertificate();
    		}
    		
    		AOCMSEnveloper enveloper = new AOCMSEnveloper();
    		enveloper.setSignatureAlgorithm(GeneralConfig.getSignAlgorithm());
    		
    		AOCipherConfig cipherConfig = new AOCipherConfig(AOCipherAlgorithm.AES, null, null);
    		
    		// por defecto se realizara un sobre envelopedData
    		byte[] envelopedData = null;
    		byte[] contentData = readFile(rutafichero);
    		try {
    			if (tipo.equals(SOBRE_AUTENTICADO))
    				envelopedData = enveloper.createCMSAuthenticatedEnvelopedData(contentData, privateKeyEntry, cipherConfig, certs);
    			else if (tipo.equals(SOBRE_FIRMADO))
    				envelopedData = enveloper.createCMSSignedAndEnvelopedData(contentData, privateKeyEntry, cipherConfig, certs);
    			else if (tipo.equals(SOBRE_SIMPLE))
    				envelopedData = enveloper.createCMSEnvelopedData(contentData, privateKeyEntry, cipherConfig, certs);
    		} catch (Exception e) {
    			System.err.println("ERROR");
    			e.printStackTrace();
    			return false;
    		}

    		// Guardamos el sobre generado
    		String path = AOUIManager.saveDataToFile(this, envelopedData, new File(rutafichero), null);
    		// Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
			if (path == null) {
				return false;
			}
    		
    	} catch(AOCancelledOperationException e) {
    		logger.warning("La operaci&oacute;n ha sido cancelada por el usuario: "+e);
    		JOptionPane.showMessageDialog(this, Messages.getString("Ensobrado.msg.error.generacion"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
		} catch (FileNotFoundException e) {
			logger.warning("No se puede encontrar el fichero seleccionado: "+e);
    		JOptionPane.showMessageDialog(this, Messages.getString("WizardCifrado.error.encontrar.fichero"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
		} catch (IOException e) {
			logger.warning("No ha sido posible leer el fichero indicado: "+e);
    		JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.lectura.generico"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	}  catch(Exception e){
    		logger.warning("Ocurrio un error durante la desenvoltura: "+e);
    		JOptionPane.showMessageDialog(this, Messages.getString("WizardCifrado.error.desenvoltura"), 
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
			KeyStoreConfiguration kconf) throws AOException {

		// Comprobamos si se ha cancelado la seleccion
		if (seleccionado == null) 
			throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$

		// Recuperamos la clave del certificado
		PrivateKeyEntry privateKeyEntry = null;
		try {
			privateKeyEntry = keyStoreManager.getKeyEntry(seleccionado, AOCryptoUtil.getCertificatePC(kconf.getType(), this));
		} catch (AOCertificateKeyException e) {
			throw e;
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
			fileIn = AOUtil.loadFile(AOUtil.createURI(filepath), this, true);
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
