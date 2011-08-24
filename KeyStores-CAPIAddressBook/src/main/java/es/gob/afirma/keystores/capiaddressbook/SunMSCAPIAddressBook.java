/*
 * @(#)SunMSCAPIAddressBook.java	1.6 09/11/08
 *
 * Copyright 2006 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package es.gob.afirma.keystores.capiaddressbook;

import java.security.AccessController;
import java.security.Provider;
import java.util.HashMap;
import java.util.Map;

import sun.security.action.PutAllAction;

/** Proveedor JCA para el acceso a los almacenes de claves de CAPI <i>CA</i> y
 * <i>ADDRESSBOOK</i>. Basado en SunMSCAPI.java
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@SuppressWarnings({ "unchecked", "rawtypes", "restriction" })
final class SunMSCAPIAddressBook extends Provider {

    private static final long serialVersionUID = 2561943190524802403L;

    private static final String INFO = "Provider for CAPI ADDRESSBOOK and CA KeyStores, based on SunMSCAPI"; //$NON-NLS-1$

    /** Construye el proveedor JCA para el acceso a los almacenes de claves
     * <i>CA</i> y <i>ADDRESSBOOK</i>. */
    SunMSCAPIAddressBook() {
        super("SunMSCAPIAddressBook", 0.1d, INFO); //$NON-NLS-1$

        // if there is no security manager installed, put directly into
        // the provider. Otherwise, create a temporary map and use a
        // doPrivileged() call at the end to transfer the contents
        final Map map = (System.getSecurityManager() == null) ? (Map) this : new HashMap<String, String>();

        map.put("KeyStore.Windows-ADDRESSBOOK", "sun.security.mscapi.KeyStoreAddressBook$ADDRESSBOOK"); //$NON-NLS-1$ //$NON-NLS-2$
        map.put("KeyStore.Windows-CA", "sun.security.mscapi.KeyStoreAddressBook$CA"); //$NON-NLS-1$ //$NON-NLS-2$

        if (map != this) {
            AccessController.doPrivileged(new PutAllAction(this, map));
        }
    }

}
