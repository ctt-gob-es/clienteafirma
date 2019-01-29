/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.awt.Component;
import java.io.IOException;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.SmartCardLockedException;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Gestor simple de <code>KeyStores</code>. Obtiene o un <code>KeyStore</code> de DNIe
 * v&iacute;a controlador 100% Java o el <code>KeyStore</code> por defecto del sistema operativo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SimpleKeyStoreManager {

    private SimpleKeyStoreManager() { /* No permitimos la instanciacion */ }

    /** Obtiene un <code>KeyStore</code>.
     * @param dnie <code>true</code> si desea obtenerse un <code>KeyStore</code> para DNIe, <code>false</code> si desea obtenerse
     *        el <code>KeyStore</code> por defecto del sistema operativo.
     * @param parent Componente padre para la modalidad.
     * @return <code>KeyStore</code> apropiado.
     * @throws AOKeyStoreManagerException Si ocurre cualquier problema durante la obtenci&oacute;n del <code>KeyStore</code>. */
    static AOKeyStoreManager getKeyStore(final boolean dnie, final Component parent) throws AOKeyStoreManagerException {

    	// -- Se ha habilitado el uso de DNIe --

        if (dnie) {
            try {
            	return getKeyStoreManager(AOKeyStore.DNIEJAVA, parent);
            }
            catch(final SmartCardLockedException e) {
            	AOUIFactory.showErrorMessage(
    				parent,
    				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.9"), //$NON-NLS-1$
    				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.10"), //$NON-NLS-1$
    				JOptionPane.WARNING_MESSAGE
				);
            }
            catch (final Exception e) {
            	String exClassName = e.getClass().getName();
            	Throwable t = e.getCause();
            	if (t != null) {
            		t = t.getCause();
            		if (t != null) {
            			t = t.getCause();
            			if (t != null) {
            				exClassName = t.getClass().getName();
            			}
            		}
            	}
            	switch(exClassName) {
            		case "es.gob.jmulticard.apdu.connection.CardNotPresentException": //$NON-NLS-1$
                		if (JOptionPane.YES_OPTION == AOUIFactory.showConfirmDialog(
            				parent,
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.1"),  //$NON-NLS-1$
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.2"),  //$NON-NLS-1$
            				JOptionPane.YES_NO_OPTION,
            				JOptionPane.WARNING_MESSAGE
        				)) {
                			return getKeyStore(true, parent);
                		}
                		break;
            		case "es.gob.jmulticard.card.InvalidCardException": //$NON-NLS-1$
            			if (JOptionPane.YES_OPTION == AOUIFactory.showConfirmDialog(
                				parent,
                				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.3"),  //$NON-NLS-1$
                				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.4"),  //$NON-NLS-1$
                				JOptionPane.YES_NO_OPTION,
                				JOptionPane.WARNING_MESSAGE
            				)) {
                    			return getKeyStore(true, parent);
                    		}
                    		break;

            		case "es.gob.jmulticard.card.dnie.BurnedDnieCardException": //$NON-NLS-1$
	            		AOUIFactory.showErrorMessage(
            				parent,
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.5"), //$NON-NLS-1$
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.6"), //$NON-NLS-1$
            				JOptionPane.WARNING_MESSAGE
        				);
	            		break;
            		default:
                		Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
            				"No se ha podido inicializar el controlador 100% Java del DNIe: " + e //$NON-NLS-1$
        				);
                		AOUIFactory.showErrorMessage(
            				parent,
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.7"), //$NON-NLS-1$
            				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.8"), //$NON-NLS-1$
            				JOptionPane.ERROR_MESSAGE
        				);
            	}
            }
        }

        // El por defecto

        // -- Comportamiento por defecto --

        // Configuramos el uso de JMulticard segun lo establecido en el dialogo
        // de preferencias
        final boolean enableJMulticard = PreferencesManager.getBoolean(
        		PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD);
        final boolean cachePassword = PreferencesManager.getBoolean(
        		PreferencesManager.PREFERENCE_GENERAL_JMULTICARD_CACHE_PASSWORD);

        JMulticardUtilities.configureJMulticard(enableJMulticard, cachePassword);

        // Cargamos el almacen por defecto

        // El por defecto
        final AOKeyStore aoks = getDefaultKeyStoreType();
        try {
			return getKeyStoreManager(
				aoks,
				parent
			);
		}
        catch (final Exception e) {
            throw new AOKeyStoreManagerException(
        		"No se ha podido incializar el almacen '" +  aoks  + "': " + e, e //$NON-NLS-1$ //$NON-NLS-2$
    		);
		}
    }

    private static AOKeyStoreManager getKeyStoreManager(final AOKeyStore aoks, final Component parent) throws IOException, AOKeystoreAlternativeException {
    	return AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		aoks,
    		null,
    		null,
    		aoks.getStorePasswordCallback(parent),
    		parent
		);
    }

    /** Indica si est&aacute; disponible el almac&eacute;n de claves de Mozilla Firefox.
     * @return <code>true</code> si est&aacute; disponible el almac&eacute;n de claves de Mozilla Firefox,
     *         <code>false</code> en caso contrario. */
    public static boolean isFirefoxAvailable() {
		final String mozProfileDir;
		final String nssLibDir;
		try {
			mozProfileDir = MozillaKeyStoreUtilities.getMozillaUserProfileDirectory();
			nssLibDir = MozillaKeyStoreUtilities.getSystemNSSLibDir();
		}
		catch(final Exception e) {
			return false;
		}
		if (mozProfileDir != null && nssLibDir !=null) {
			return true;
		}
		return false;
    }

    private static AOKeyStore getDefaultKeyStoreTypeByOs(final OS os) {
    	if (Platform.OS.WINDOWS.equals(os)) {
    		return AOKeyStore.WINDOWS;
    	}
    	if (Platform.OS.MACOSX.equals(os)) {
    		return AOKeyStore.APPLE;
    	}
    	return AOKeyStore.SHARED_NSS;
    }

    /** Obtiene el almac&eacute;n de claves por defecto de la aplicaci&oacute;n.
     * @return Almac&eacute;n de claves por defecto de la aplicaci&oacute;n. */
    public static AOKeyStore getDefaultKeyStoreType() {
    	final String savedStoreName = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
    	final OS os = Platform.getOS();
    	if (savedStoreName != null) {
    		final AOKeyStore ks = AOKeyStore.getKeyStore(savedStoreName);
    		if (ks != null) {
    			// Si desinstalan Firefox que no se quede una seleccion mala
    			if (AOKeyStore.MOZ_UNI.equals(ks)) {
    				if (isFirefoxAvailable()) {
    					return ks;
    				}
					return getDefaultKeyStoreTypeByOs(os);
    			}
    			// No deberia pasar
    			if (AOKeyStore.WINDOWS.equals(ks)) {
    				if (OS.WINDOWS.equals(os)) {
    					return ks;
    				}
    				return getDefaultKeyStoreTypeByOs(os);
    			}
    			// No deberia pasar
    			if (AOKeyStore.APPLE.equals(ks)) {
    				if (OS.MACOSX.equals(os)) {
    					return ks;
    				}
    				return getDefaultKeyStoreTypeByOs(os);
    			}
    			return ks;
    		}
    	}
    	return getDefaultKeyStoreTypeByOs(os);
    }

}
