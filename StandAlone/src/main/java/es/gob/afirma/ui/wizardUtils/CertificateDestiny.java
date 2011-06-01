/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardUtils;

import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.ui.AOUIManager;
import es.gob.afirma.ui.utils.Messages;

/**
 * Certificado destinatario de un sobre digital. 
 */
public class CertificateDestiny {

	static Logger logger = Logger.getLogger(CertificateDestiny.class.getName());
	
	/**
	 * Alias del certificado
	 */
	private String alias = null;
	
	/**
	 * Certificado
	 */
	private Certificate cert = null;

	public Certificate getCertificate() {
		return cert;
	}
	
	public String getAlias() {
		return alias;
	}
	
	public CertificateDestiny(String alias, Certificate cert) {
		this.alias = alias;
		this.cert = cert;
	}
	
	public CertificateDestiny(AOKeyStoreManager keyStoreManager, JDialogWizard dialogo) {
		try {
			// Seleccionamos un certificado
			String selectedcert = AOUIManager.showCertSelectionDialog(keyStoreManager.getAliases(), keyStoreManager.getKeyStores(),
					null, dialogo, false, true, true);

			// Comprobamos si se ha cancelado la seleccion
			if (selectedcert == null) 
				throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$

			Certificate cert = null;
			for (KeyStore tmpKs : keyStoreManager.getKeyStores()) {
				try {
					cert = tmpKs.getCertificate(selectedcert);
					if (cert != null) 
						break;
					else
						throw new AOException("El alias '"+selectedcert+"' no se corresponde con ning\u00FAn certificado accesible"); //$NON-NLS-1$
				} catch (KeyStoreException e) {
					throw new AOException("El almac\u00E9n seleccionado no estaba inicializado: "+e); //$NON-NLS-1$
				}
			}

			if (!(cert instanceof X509Certificate)) {
				throw new AOException("El certificado recuperado no es un certificado v\u00E1lido para esta operaci\u00F3n"); //$NON-NLS-1$
			}
			
			this.alias = selectedcert;
			this.cert = cert;
		} catch (AOCancelledOperationException e) {
			logger.severe("Operacion cancelada por el usuario");
		} catch (AOException e) {
			logger.severe(e.getMessage()+": "+e);
			JOptionPane.showMessageDialog(dialogo, e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
		} catch (Exception e) {
			logger.severe("No se ha podido recuperar el certificado seleccionado: "+e);
			JOptionPane.showMessageDialog(dialogo, Messages.getString("Certificado.no.soportado"), 
					Messages.getString("error"), JOptionPane.ERROR_MESSAGE);
		}
	}	
}