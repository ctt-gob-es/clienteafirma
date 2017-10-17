/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.util.tree;

import java.util.logging.Logger;

/** Utilidades para la gesti&oacute;n de &aacute;rboles de objetos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOTreeUtil {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private AOTreeUtil() {
		// No instanciable
	}

    /** Genera una cadena representativa del &aacute;rbol que recibe.
     * @param tree &Aacute;rbol que se desea representar.
     * @param linePrefx Prefijo de cada l&iacute;nea de firma (por defecto, cadena
     *                  vac&iacute;a).
     * @param identationString Cadena para la identaci&oacute;n de los nodos de firma (por
     *                         defecto, tabulador).
     * @return Cadena de texto. */
    public static String showTreeAsString(final AOTreeModel tree, final String linePrefx, final String identationString) {

        if (tree == null || tree.getRoot() == null) {
            LOGGER.severe("Se ha proporcionado un arbol de firmas vacio"); //$NON-NLS-1$
            return null;
        }

        if (!(tree.getRoot() instanceof AOTreeNode)) {
            LOGGER.severe("La raiz del arbol de firmas no es de tipo DafaultMutableTreeNode"); //$NON-NLS-1$
            return null;
        }

        final StringBuilder buffer = new StringBuilder();

        // Transformamos en cadenas de texto cada rama que surja del nodo raiz
        // del arbol
        final AOTreeNode root = (AOTreeNode) tree.getRoot();
        for (int i = 0; i < root.getChildCount(); i++) {
            archiveTreeNode(root.getChildAt(i), 0, linePrefx != null ? linePrefx : "", identationString != null ? identationString : "\t", buffer);  //$NON-NLS-1$//$NON-NLS-2$
        }

        return buffer.toString();
    }

    /** Transforma en cadena una rama completa de un &aacute;rbol. Para una
     * correcta indexaci&oacute; es necesario indicar la profundidad en la que
     * esta el nodo del que pende la rama. En el caso de las ramas que penden
     * del nodo ra&iacute;z del &aacute;rbol la profundidad es cero (0).
     * @param node Nodo del que cuelga la rama.
     * @param depth Profundidad del nodo del que pende la rama.
     * @param linePrefx Prefijo de cada l&iacute;nea de firma (por defecto, cadena
     *                  vac&iacute;a).
     * @param identationString Cadena para la identaci&oacute;n de los nodos de firma (por
     *                         defecto, tabulador).
     * @param buffer Buffer en donde se genera la cadena de texto. */
    private static void archiveTreeNode(final AOTreeNode node,
                                              final int depth,
                                              final String linePrefx,
                                              final String identationString,
                                              final StringBuilder buffer) {
        buffer.append('\n').append(linePrefx);
        for (int i = 0; i < depth; i++) {
            buffer.append(identationString);
        }
        buffer.append(node.getUserObject());
        for (int i = 0; i < node.getChildCount(); i++) {
            archiveTreeNode(node.getChildAt(i), depth + 1, linePrefx, identationString, buffer);
        }
    }


}
