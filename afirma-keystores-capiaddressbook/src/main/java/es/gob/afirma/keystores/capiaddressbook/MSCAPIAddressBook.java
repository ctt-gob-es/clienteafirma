/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.capiaddressbook;

import java.security.Provider;

/** Proveedor JCA para el acceso a los almacenes de claves de CAPI <i>CA</i> y
 * <i>ADDRESSBOOK</i>. Basado en SunMSCAPI.java
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class MSCAPIAddressBook extends Provider {

    private static final long serialVersionUID = 2561943190524802403L;

    private static final double VERSION = 0.1d;

    private static final String INFO = "Proveedor para la libreta de direcciones (Other People) de CAPI"; //$NON-NLS-1$

    /** Construye el proveedor JCA para el acceso a los almacenes de claves
     * <i>CA</i> y <i>ADDRESSBOOK</i>. */
    public MSCAPIAddressBook() {
        super("SunMSCAPIAddressBook", VERSION, INFO); //$NON-NLS-1$
        put("KeyStore.Windows-ADDRESSBOOK", "sun.security.mscapi.KeyStoreAddressBook$ADDRESSBOOK"); //$NON-NLS-1$ //$NON-NLS-2$
        put("KeyStore.Windows-CA", "sun.security.mscapi.KeyStoreAddressBook$CA"); //$NON-NLS-1$ //$NON-NLS-2$
    }

}
