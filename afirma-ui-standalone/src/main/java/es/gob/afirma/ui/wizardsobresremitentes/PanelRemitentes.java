/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardsobresremitentes;

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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
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
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.envelopers.cms.AOCMSEnveloper;
import es.gob.afirma.envelopers.cms.AOCMSMultiEnveloper;
import es.gob.afirma.envelopers.cms.CMSAuthenticatedEnvelopedData;
import es.gob.afirma.envelopers.cms.CMSEnvelopedData;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.common.AOKeyStore;
import es.gob.afirma.keystores.common.AOKeyStoreManager;
import es.gob.afirma.keystores.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.common.KeyStoreConfiguration;
import es.gob.afirma.keystores.common.KeyStoreUtilities;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SignFileUtils;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.CertificateDestiny;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 *
 * Clase que contiene los elementos necesarios para crear un grupo de Remitentes
 * a partir de una seleccion de certificados de remitentes.
 */
public class PanelRemitentes extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelRemitentes.class.getName());
	
	@Override
	public int getMinimumRelation(){
		return 9;
	}
	
	/**
	 * Ruta donde se encuentra el fichero a ensobrar
	 */
	private String rutafichero;
	
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
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	public void setVentanas(List<JDialogWizard> ventanas) {
		Botonera botonera = new Botonera(ventanas, 1);
		getContentPane().add(botonera, BorderLayout.PAGE_END);
	}
	
	public PanelRemitentes(String rutafichero) {
		this.rutafichero = rutafichero;
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
        CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.sobres.remitentes.titulo", "Wizard.sobres.remitentes.titulo.explicacion", null, true);
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
        c.gridwidth = 2;
		c.weightx = 1.0;
		c.gridx = 0;
		
		//Etiqueta con el texto "Puede anadir uno o..."
		InfoLabel labelText = new InfoLabel(Messages.getString("Wizard.sobres.remitentes.contenido.explicacion1"), false);
		panelCentral.add(labelText, c);

		c.gridy = 1;
		c.gridwidth = 1;
		
        // Etiqueta con el texto "Anadir remitente desde..."
        etiquetaAnadir.setText(Messages.getString("Wizard.sobres.aniadir.originante"));
        Utils.setContrastColor(etiquetaAnadir);
        Utils.setFontBold(etiquetaAnadir);
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
		comboRepositorios.getAccessibleContext().setAccessibleName(etiquetaAnadir.getText() + " " + comboRepositorios.getToolTipText() + " ALT + D.");
		comboRepositorios.getAccessibleContext().setAccessibleDescription(comboRepositorios.getToolTipText());
		comboRepositorios.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				listaCertificadosRe.clear();
				((DefaultListModel) listaRemitentes.getModel()).removeAllElements();
			}
		});
		cargarCombo(comboRepositorios);
		Utils.remarcar(comboRepositorios);
        Utils.setContrastColor(comboRepositorios);
		Utils.setFontBold(comboRepositorios);
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
		anadir.getAccessibleContext().setAccessibleName(anadir.getText() + " " + anadir.getToolTipText());
		anadir.getAccessibleContext().setAccessibleDescription(anadir.getToolTipText());
		anadir.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				anadirActionPerformed(comboRepositorios, eliminar, anadir);
			}
		});
		Utils.remarcar(anadir);
        Utils.setContrastColor(anadir);
		Utils.setFontBold(anadir);
		panelCentral.add(anadir, c);
		
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
		
		c.insets = new Insets(10, 20, 0, 20);
		c.ipady = 80;
		c.gridwidth = 2;
		c.gridy = 4;
		c.gridx = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;

		// Panel que contiene la lista de remitentes
		JScrollPane panelLista = new JScrollPane();
		panelCentral.add(panelLista, c);
		
		// Listado de remitentes
		listaRemitentes.setToolTipText(Messages.getString("Wizard.sobres.listaRemitentes.description"));
		listaRemitentes.setModel(new DefaultListModel());
		listaRemitentes.getAccessibleContext().setAccessibleName(senderLabel.getText() + " "+ listaRemitentes.getToolTipText());
		listaRemitentes.getAccessibleContext().setAccessibleDescription(listaRemitentes.getToolTipText());
		Utils.remarcar(listaRemitentes);
        Utils.setContrastColor(listaRemitentes);
		Utils.setFontBold(listaRemitentes);
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
		eliminar.getAccessibleContext().setAccessibleName(eliminar.getText() + " " + eliminar.getToolTipText());
		eliminar.getAccessibleContext().setAccessibleDescription(eliminar.getToolTipText());
		eliminar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				eliminarActionPerformed(comboRepositorios, eliminar, anadir);
			}
		});
		eliminar.getAccessibleContext().setAccessibleDescription(Messages.getString("wizard.sobres.eliminar.remitente"));
		Utils.remarcar(eliminar);
        Utils.setContrastColor(eliminar);
		Utils.setFontBold(eliminar);
		panelCentral.add(eliminar, c);
		
		getContentPane().add(panelCentral, BorderLayout.CENTER);

		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(comboRepositorios,"ensobrado.wizard.repositorio.remitente");
		HelpUtils.enableHelpKey(labelText,"ensobrado.wizard.remitentes");
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
		if (certDest.getAlias() != null && !certDest.equals("")) { //$NON-NLS-1$
			DefaultListModel listModel = (DefaultListModel) listaRemitentes.getModel();
			
			boolean copiar = true;
			for (int i = 0; i < listModel.getSize(); i++)
				if (listModel.getElementAt(i).equals(certDest.getAlias())) {
					copiar = false;
					break;
				}
				
			if (copiar) {
				listModel.addElement(certDest.getAlias());
				listaCertificadosRe.add(certDest);
				anadir.setEnabled(false);
				comboRepositorios.setEnabled(false);
				etiquetaAnadir.setDisplayedMnemonic(0); //Se asigna un atajo vacío puesto que se ha deshabilitado el combo asociado
				etiquetaAnadir.getAccessibleContext().setAccessibleName(etiquetaAnadir.getText() + " " + Messages.getString("wizard.sobres.etiquetaAnadir"));
				etiquetaAnadir.setFocusable(true);
				eliminar.setEnabled(true);
				eliminar.setMnemonic(KeyEvent.VK_E); //Se asigna un atajo al botón ya que ha sido habilitado
			} else
				JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.error.usuario"), 
						Messages.getString("error"), JOptionPane.WARNING_MESSAGE);
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
				
				eliminar.setEnabled(false);
				eliminar.setMnemonic(0); //Se asigna un atajo vacio al botón ya que ha sido deshabilitado
				anadir.setEnabled(true);
				anadir.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al botón puesto que se ha habilitado
				comboRepositorios.setEnabled(true);
				etiquetaAnadir.setDisplayedMnemonic(KeyEvent.VK_D); //Se asigna un atajo puesto que se ha habilitado el combo asociado
				etiquetaAnadir.setFocusable(false);
				break;
			}
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
			continuar = anadirRemitentes();
			
			if (continuar == true) {
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			}
		}
	}

	/**
	 * Ensobra el fichero dado
	 * @return	True o false si la operacion ha sido satisfactoria
	 */
	public Boolean anadirRemitentes() {
    	try {
    		FileInputStream dataFis = new FileInputStream(rutafichero);
    		byte[] envelopedData = AOUtil.getDataFromInputStream(dataFis);
    		
    		// Anadimos solo el ultimo
    		PrivateKeyEntry privateKey = getPrivateKeyEntry(keyStoreManager, 
    				listaCertificadosRe.get(listaCertificadosRe.size()-1).getAlias(),kconf);
    		String contentType = comprobarTipo(envelopedData, privateKey);
    		if (contentType == null)
    			return false;
    		
    		envelopedData = doCoEnvelopOperation(envelopedData, contentType, privateKey);

    		// Guardamos el sobre generado
    		File savedFile = AOUIFactory.getSaveDataToFile(envelopedData,
    		        new File(this.rutafichero + ".csig"), //$NON-NLS-1$
    		        SignFileUtils.getOutFileFilter(AOSignConstants.SIGN_FORMAT_CMS),
    		        this);
    		// Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
			if (savedFile == null) {
				return false;
			}
    		
    		try { 
    			dataFis.close(); 
    		} catch (Exception e) { 
    			logger.warning("No se pudo cerrar el fichero de datos"); 
    		} 
    	} catch(AOCancelledOperationException e) {
    		logger.warning("La operaci&oacute;n ha sido cancelada por el usuario: "+e);
    		JOptionPane.showMessageDialog(this, Messages.getString("Ensobrado.msg.error.generacion"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	} catch(FileNotFoundException e){
    		logger.warning("No ha sido posible leer el fichero indicado: "+e);
    		JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.lectura.generico"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	} catch(Exception e){
    		logger.warning("Ocurrio un error durante el proceso de aï¿½adir un nuevo remitente: "+e);
    		JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.almacen.anadir.remitentes"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return false;
    	}
    	
    	return true;
	}
	
	/**
	 * Anade un nuevo remitente al sobre
	 * @param data			Sobre electr&oacute;nico.
	 * @param contentType	Tipo del contenido
	 * @param privateKey	Clave privada
	 * @return
	 */
	private byte[] doCoEnvelopOperation(byte[] data, String contentType, PrivateKeyEntry privateKey) {
		byte[] envelop;
		X509Certificate[] originatorCertChain = (X509Certificate[]) privateKey.getCertificateChain();
		
		if (contentType.equals(AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
			CMSEnvelopedData enveloper = new CMSEnvelopedData();
			envelop = enveloper.addOriginatorInfo(data, originatorCertChain);
		} else if (contentType.equals(AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
		    AOCMSMultiEnveloper coEnveloper = new AOCMSMultiEnveloper();
			try {
				envelop = coEnveloper.cosign(
						data,
						AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
						privateKey,
						null);
			} catch (AOException e) {
				logger.warning("Ocurrio un error durante el proceso de aï¿½adir un nuevo remitente: "+e); //$NON-NLS-1$
	    		JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.almacen.anadir.remitentes"),  //$NON-NLS-1$
	    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
	    		return null;
			}
		} else if (contentType.equals(AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
			CMSAuthenticatedEnvelopedData enveloper = new CMSAuthenticatedEnvelopedData();
			envelop = enveloper.addOriginatorInfo(data, originatorCertChain);
		} else {
    		JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.almacen.certificado.soportado"), 
    				Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
    		return null;
		}
		
		return envelop;
	}

	/**
	 * Comprueba el tipo de todos los certificados
	 * @param data		Sobre electr&oacute;nico.
	 * @param privateKey	Certificado
	 * @return Tipo de sobre
	 */
	private String comprobarTipo(byte[] data, PrivateKeyEntry privateKey) {
		String tipo = null;
		AOCMSEnveloper enveloper = new AOCMSEnveloper();
		if (enveloper.isCMSValid(data, AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
			tipo = AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA;
		} else if (enveloper.isCMSValid(data, AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
			tipo = AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA;
		} else if (enveloper.isCMSValid(data, AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
			tipo = AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA;
		} 
		else {
			JOptionPane.showMessageDialog(this, Messages.getString("Wizard.sobres.almacen.sobre.soportado"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
			return null;
		}
		
		return tipo;
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
			privateKeyEntry = keyStoreManager.getKeyEntry(seleccionado, KeyStoreUtilities.getCertificatePC(kconf.getType(), this));
		} catch (KeyException e) {
			throw new AOException("Error al recuperar la clave privada del certificado", e); //$NON-NLS-1$
		} catch (AOCancelledOperationException e) {
			// Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
			// Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
			// que las relanza en forma de AOException
			throw e;
		} catch (Exception e) {
			logger.severe("No se ha podido obtener el certificado con el alias '" + seleccionado + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw new AOException(e.getMessage());
		}
		
		return privateKeyEntry;
	}
}
