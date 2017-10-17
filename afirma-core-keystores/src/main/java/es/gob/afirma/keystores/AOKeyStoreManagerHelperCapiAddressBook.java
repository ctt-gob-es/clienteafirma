/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.util.logging.Logger;

import es.gob.afirma.core.InvalidOSException;
import es.gob.afirma.core.MissingLibraryException;
import es.gob.afirma.core.misc.Platform;

final class AOKeyStoreManagerHelperCapiAddressBook {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private AOKeyStoreManagerHelperCapiAddressBook() {
    	// No permitimos la instanciacion
    }

    static KeyStore initCAPIAddressBook(final AOKeyStore ksType) throws AOKeyStoreManagerException {

        if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
            throw new InvalidOSException("Microsoft Windows"); //$NON-NLS-1$
        }

        // Nos aseguramos de que SunMSCAPI este cargado, para que la DLL
        // sunmscapi.dll tambien lo este
        if (Security.getProvider("SunMSCAPI") == null) { //$NON-NLS-1$
            try {
                Security.addProvider((Provider) Class.forName("sun.security.mscapi.SunMSCAPI").getConstructor().newInstance()); //$NON-NLS-1$
            }
            catch (final Exception e) {
            	LOGGER.severe("No se ha podido instanciar 'sun.security.mscapi.SunMSCAPI': " + e); //$NON-NLS-1$
                throw new MissingSunMSCAPIException(e);
            }
        }

        Provider p = Security.getProvider("MSCAPIAddressBook"); //$NON-NLS-1$
        if (p == null) {
            try {
                p = (Provider) Class.forName("es.gob.afirma.keystores.capiaddressbook.MSCAPIAddressBook").getConstructor().newInstance(); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new MissingLibraryException("No se ha podido instanciar el proveedor MSCAPIAddressBook", e); //$NON-NLS-1$
            }
            Security.addProvider(p);
        }

        final KeyStore ks;
        try {
            ks = KeyStore.getInstance(ksType.getProviderName(), p);
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen " + ksType.getProviderName() + ": " + e, e);  //$NON-NLS-1$ //$NON-NLS-2$
        }

        try {
            ks.load(null, null);
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido abrir el almacen " + ksType.getProviderName() + ": " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        return ks;
    }

}
