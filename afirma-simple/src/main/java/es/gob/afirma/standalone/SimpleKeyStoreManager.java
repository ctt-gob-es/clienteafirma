/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone;

import java.awt.Component;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Gestor simple de <code>KeyStores</code>. Obtiene o un <code>KeyStore</code> de DNIe
 * v&iacute;a controlador 100% Java o el <code>KeyStore</code> por defecto del sistema operativo
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class SimpleKeyStoreManager {

    private SimpleKeyStoreManager() { /* No permitimos la instanciacion */ }

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Obtiene un <code>KeyStore</code>.
     * @param dnie <code>true</code> si desea obtenerse un <code>KeyStore</code> para DNIe v&iacute;a PKCS#11, <code>false</code> si desea obtenerse
     *        el <code>KeyStore</code> por defecto del sistema operativo
     * @param parent Componente padre para la modalidad
     * @return <code>KeyStore</code> apropiado
     * @throws AOKeyStoreManagerException Si ocurre cualquier problema durante la obtenci&oacute;n del <code>KeyStore</code> */
    static AOKeyStoreManager getKeyStore(final boolean dnie, final Component parent) throws AOKeyStoreManagerException {

        if (dnie) {
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.DNIEJAVA, null, null, AOKeyStore.DNIEJAVA.getStorePasswordCallback(parent), parent);
            }
            catch (final Exception e) {
            	if ("es.gob.jmulticard.apdu.connection.CardNotPresentException".equals(e.getClass().getName())) { //$NON-NLS-1$
            		if (0 == AOUIFactory.showConfirmDialog(
        				parent,
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.1"),  //$NON-NLS-1$
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.2"),  //$NON-NLS-1$
        				JOptionPane.YES_NO_OPTION,
        				JOptionPane.WARNING_MESSAGE
    				)) {
            			return getKeyStore(true, parent);
            		}
            	}
            	else if ("es.gob.jmulticard.card.InvalidCardException".equals(e.getClass().getName())) { //$NON-NLS-1$
            		AOUIFactory.showErrorMessage(
        				parent,
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.3"), //$NON-NLS-1$
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.4"), //$NON-NLS-1$
        				JOptionPane.WARNING_MESSAGE
    				);
        			return getKeyStore(true, parent);
            	}
            	else if ("es.gob.jmulticard.card.dnie.BurnedDnieCardException".equals(e.getClass().getName())) { //$NON-NLS-1$
            		AOUIFactory.showErrorMessage(
        				parent,
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.5"), //$NON-NLS-1$
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.6"), //$NON-NLS-1$
        				JOptionPane.WARNING_MESSAGE
    				);
            	}
            	else {
            		Logger.getLogger("es.gob.afirma").severe("No se ha podido inicializar el controlador 100% Java del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            		AOUIFactory.showMessageDialog(
        				parent,
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.7"), //$NON-NLS-1$
        				SimpleAfirmaMessages.getString("SimpleKeyStoreManager.8"), //$NON-NLS-1$
        				JOptionPane.ERROR_MESSAGE
    				);
            	}
            }
        }

        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(
            		AOKeyStore.WINDOWS,
            		null,
            		null,
            		AOKeyStore.WINDOWS.getStorePasswordCallback(parent),
            		parent
        		);
            }
            catch (final Exception e) {
            	LOGGER.severe("Error inicializando MSCAPI: " + e); //$NON-NLS-1$
                throw new AOKeyStoreManagerException("No se ha podido inicializar SunMSCAPI", e); //$NON-NLS-1$
            }
        }

        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, null, NullPasswordCallback.getInstance(), parent);
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido incializar el Llavero de Mac OS X", e); //$NON-NLS-1$
            }
        }

        // Linux y Solaris
    	try {
            return AOKeyStoreManagerFactory.getAOKeyStoreManager(
        		AOKeyStore.MOZ_UNI,
        		null,
        		null,
        		new UIPasswordCallback(SimpleAfirmaMessages.getString("SimpleKeyStoreManager.0"), parent),  //$NON-NLS-1$
        		parent
    		);
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido incializar el almacen de Mozilla", e); //$NON-NLS-1$
        }

    }

}
