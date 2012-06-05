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

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerException;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;

/** Gestor simple de <code>KeyStores</code>. Obtiene o un <code>KeyStore</code> de DNIe
 * v&iacute;a PKCS#11 o el <code>KeyStore</code> por defecto del sistema operativo
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class SimpleKeyStoreManager {

    private SimpleKeyStoreManager() { /* No permitimos la instanciacion */ }

    /** Obtiene un <code>KeyStore</code>.
     * @param dnie <code>true</code> si desea obtenerse un <code>KeyStore</code> para DNIe v&iacute;a PKCS#11, <code>false</code> si desea obtenerse
     *        el <code>KeyStore</code> por defecto del sistema operativo
     * @param parent Componente padre para la modalidad
     * @return <code>KeyStore</code> apropiado
     * @throws AOKeyStoreManagerException Si ocurre cualquier problema durante la obtenci&oacute;n del <code>KeyStore</code> */
    static AOKeyStoreManager getKeyStore(final boolean dnie, final Component parent) throws AOKeyStoreManagerException {

        if (dnie) {
            System.setProperty("es.gob.jmulticard.fastmode", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.DNIEJAVA, null, null, null, parent);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No se ha podido inicializar el controlador 100% Java del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }

        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.WINDOWS, null, null, new NullPasswordCallback(), parent);
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido inicializar SunMSCAPI", e); //$NON-NLS-1$
            }
        }

        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, null, new NullPasswordCallback(), parent);
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
        		new UIPasswordCallback(Messages.getString("SimpleKeyStoreManager.0"), parent),  //$NON-NLS-1$
        		parent
    		);
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido incializar el almacen de Mozilla", e); //$NON-NLS-1$
        }

    }

}
