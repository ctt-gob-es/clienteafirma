/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.visor.ui;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import es.gob.afirma.ui.utils.Messages;

final class LinksTreeCellRenderer extends DefaultTreeCellRenderer {

    /** Version ID */
    private static final long serialVersionUID = 335850028229506214L;

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean sel, final boolean expanded, final boolean leaf, final int row, final boolean focus) {
        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, focus);
    if ((value instanceof DefaultMutableTreeNode) && (!(((DefaultMutableTreeNode) value).getUserObject() instanceof String))) {
        	//Comprobacion de que el texto no sea nulo o vacio.
        	final String text = getText();
        	//Color de fondo cuando se ha seleccionado el elemento
        	final Color selectionColor =getBackgroundSelectionColor();
        	//Color de fondo cuando un elemento no esta seleccionado
        	final Color nonSelectionColor =getBackgroundNonSelectionColor();

        	if (text!=null && !text.equalsIgnoreCase("")) { //$NON-NLS-1$
                if (focus) {

                    //setText("<html><font color=\"white\"><u>" + text + "</u></font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                	setText("<html><span style=\"text-decoration: underline; color:white;\">" + text + //$NON-NLS-1$
                			"</span><span style=\"color:rgb(" + selectionColor.getRed() + "," + selectionColor.getGreen() + //$NON-NLS-1$ //$NON-NLS-2$
                			","+selectionColor.getBlue()+");\">" + " " + Messages.getString("Link.access") + "</span></html>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

                }
                else {
                    //setText("<html><font color=\"blue\"><u>" + text + "</u></font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                	setText("<html><span style=\"text-decoration: underline; color:blue;\">" + text + //$NON-NLS-1$
                			"</span><span style=\"color:rgb(" + nonSelectionColor.getRed() + "," + nonSelectionColor.getGreen() + //$NON-NLS-1$ //$NON-NLS-2$
                			","+nonSelectionColor.getBlue()+");\">" + " " + Messages.getString("Link.access") + "</span></html>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
                }

        	}
        }
        return this;
    }
}
