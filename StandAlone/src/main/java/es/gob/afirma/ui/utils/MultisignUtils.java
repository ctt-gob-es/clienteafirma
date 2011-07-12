/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

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
import es.gob.afirma.ui.AOUIManager;

/**
 * Utilidades para las multifirmas
 */
public class MultisignUtils {

	static Logger logger = Logger.getLogger(MultisignUtils.class.toString());
	
    /**
     * Obtiene el manager del almacen o repositorio de certificados
     * @param kssc		Configuracion del almacen o repositorio de certificados
     * @param padre		Componente padre
     * @return			<code>AOKeyStoreManager</code> solicitado
     * @throws AOException	Si ocurre cualquier error al intentar obtener el gestor del almac&eacute;n o repositorio de certificados
     */
    public AOKeyStoreManager getAOKeyStoreManager(KeyStoreConfiguration kssc, Component padre) throws AOException {
        PasswordCallback pssCallback;

        AOConstants.AOKeyStore store = kssc.getType();
        if (store == AOConstants.AOKeyStore.WINDOWS || 
        		store == AOConstants.AOKeyStore.WINROOT || 
        		store == AOConstants.AOKeyStore.SINGLE) pssCallback = new NullPasswordCallback();
        else 
        	pssCallback = new UIPasswordCallback(
    			Messages.getString("Msg.pedir.contraenia") + 
				" " + 
				store.getDescription() + 
				". \r\nSi no ha establecido ninguna, deje el campo en blanco.", null);

        try {
	        return AOKeyStoreManagerFactory.getAOKeyStoreManager(
	            store,
	            kssc.getLib(),
	            kssc.toString(),
	            pssCallback,
	            padre
	        );
        }
        catch(final Exception e) {
        	throw new AOException("Error inicializando el almacen", e);
        }
        
    }

    /**
     * Solicita que se seleccione un certificado del almac&eacute;n indicado y devuelve su clave privada.
     * @param kssc				Configuraci&oacute;n del almac&eacute;n o repositorio de certificados
     * @param keyStoreManager	Manager del almac&eacute;n o repositorio de certificados
     * @param padre				Componente padre
     * @return                  
     * @throws AOException	se ha producido un error al intentar obtener la clave privada
     */
    public PrivateKeyEntry getPrivateKeyEntry(KeyStoreConfiguration kssc, AOKeyStoreManager keyStoreManager, Component padre) throws AOException {
        // Recuperamos la clave del certificado
        PrivateKeyEntry privateKeyEntry = null;
        
        // Seleccionamos un certificado
        String selectedcert = AOUIManager.showCertSelectionDialog(keyStoreManager.getAliases(), keyStoreManager.getKeyStores(), padre, true, true, true);

        // Comprobamos si se ha cancelado la seleccion
        if (selectedcert == null) 
        	throw new AOCancelledOperationException("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$

        AOConstants.AOKeyStore store = kssc.getType();
        try {
        	privateKeyEntry = keyStoreManager.getKeyEntry(
                        selectedcert,
                        AOCryptoUtil.getCertificatePC(store, padre)
                        );
            } catch (AOCertificateKeyException e) {
                throw e;
            } catch (AOCancelledOperationException e) {
                // Si se ha cancelado la operacion lo informamos en el nivel superior para que se trate.
                // Este relanzamiento se realiza para evitar la siguiente captura generica de excepciones
                // que las relanza en forma de AOException
                throw e;
            } catch (Exception e) {
                logger.severe("No se ha podido obtener el certicado con el alias '" + selectedcert + "': " + e);
                throw new AOException(e.getMessage());
            }
            
        return privateKeyEntry;
    }
}
