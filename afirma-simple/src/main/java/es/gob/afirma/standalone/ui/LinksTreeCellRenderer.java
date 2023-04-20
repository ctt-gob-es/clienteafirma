/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.font.TextAttribute;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class LinksTreeCellRenderer extends DefaultTreeCellRenderer {

    /** Version ID */
    private static final long serialVersionUID = 335850028229506214L;

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean sel, final boolean expanded, final boolean leaf, final int row, final boolean focus) {

    	final Component ret = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, focus);

        if (((DefaultMutableTreeNode) value).getUserObject() instanceof ShowFileLinkAction
        		|| ((DefaultMutableTreeNode) value).getUserObject() instanceof AOSimpleSignInfo) {

        	final JLabel linkLbl = new JLabel();
        	linkLbl.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") + " " + getText());  //$NON-NLS-1$//$NON-NLS-2$

        	if (focus) {
        		linkLbl.setText(getText());
        		linkLbl.setOpaque(true);
        		if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        			linkLbl.setForeground(Color.WHITE);
            		linkLbl.setBackground(Color.decode("#39698a")); //$NON-NLS-1$
        		} else {
        			linkLbl.setForeground(Color.YELLOW);
            		linkLbl.setBackground(Color.BLUE);
        		}
        	} else {
        		linkLbl.setText(getText());
        		if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        			linkLbl.setForeground(Color.BLUE);
        		} else {
        			linkLbl.setForeground(Color.YELLOW);
        		}
        		linkLbl.setOpaque(false);
        	}

        	final Font font = linkLbl.getFont();
        	final Map attributes = font.getAttributes();
        	attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
        	linkLbl.setFont(font.deriveFont(attributes));

        	return linkLbl;
        }

        return ret;
    }
}
