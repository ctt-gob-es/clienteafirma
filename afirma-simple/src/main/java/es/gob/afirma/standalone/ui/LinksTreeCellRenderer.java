/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

final class LinksTreeCellRenderer extends DefaultTreeCellRenderer {

    /** Version ID */
    private static final long serialVersionUID = 335850028229506214L;

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean sel, final boolean expanded, final boolean leaf, final int row, final boolean focus) {
        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, focus);
        if (value instanceof DefaultMutableTreeNode && (!(((DefaultMutableTreeNode) value).getUserObject() instanceof String))) {
            if (focus) {
                setText("<html><font color=\"white\"><u>" + getText() + "</u></font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
            }
            else {
                setText("<html><font color=\"blue\"><u>" + getText() + "</u></font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return this;
    }
}
