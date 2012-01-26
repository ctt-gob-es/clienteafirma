/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;

/** Clase de enlace con la ayuda nativa de Mac OS X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class MacHelpHooker {

	private MacHelpHooker() {
		// No permitimos la instanciacion
	}

    private static boolean loaded = false;

    static {
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            try {
                System.loadLibrary("JavaHelpHook"); //$NON-NLS-1$
                loaded = true;
            }
            catch(final UnsatisfiedLinkError e) {
                Logger.getLogger("es.gob.afirma").warning("No se encuentra la biblioteca nativa de apertura de Apple Help: " + e);  //$NON-NLS-1$//$NON-NLS-2$
            }
            catch(final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No ha sido posible cargar la biblioteca nativa de apertura de Apple Help: " + e);  //$NON-NLS-1$//$NON-NLS-2$
            }
        }
    }

    /** Muestra la ayuda de la aplicaci&oacute;n en Mac OS X con formato Apple Help (que debe estar declara en el
     * <code>Info.plist</code> del empaquetado <code>.app</code>). */
    public static native void showHelp();

    /** Indica si es posible mostrar la ayuda nativa Apple Help.
     * @return <code>true</code> si se detecta Mac OS X y la biblioteca nativa de enlace (<code>JavaHelpHook.jnilib</code>)
     *         est&aacute; disponible, <code>false</code> en caso contrario
     */
    public static boolean isMacHelpAvailable() {
        return loaded;
    }

}
