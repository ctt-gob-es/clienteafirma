/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
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
        catch(final Throwable e) {
        	throw new AOException("Error inicializando el almacen", e);
        }
        
    }

    /**
     * Obtiene la clave privada 
     * @param kssc				Configuracion del almacen o repositorio de certificados
     * @param keyStoreManager	Manager del almacen o repositorio de certificados
     * @param padre				Componente padre
     * @return
     * @throws AOException	se ha producido un error al intentar obtener la clave privada
     */
    public PrivateKeyEntry getPrivateKeyEntry(KeyStoreConfiguration kssc, AOKeyStoreManager keyStoreManager, Component padre) throws AOException {
        // Recuperamos la clave del certificado
        PrivateKeyEntry privateKeyEntry = null;
        
        // Seleccionamos un certificado
        String selectedcert = AOUIManager.showCertSelectionDialog(keyStoreManager.getAliases(), keyStoreManager.getKeyStores(), null, padre, true, true, true);

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
            } catch (Throwable e) {
                logger.severe("No se ha podido obtener el certicado con el alias '" + selectedcert + "': " + e);
                throw new AOException(e.getMessage());
            }
            
        return privateKeyEntry;
    }
}
