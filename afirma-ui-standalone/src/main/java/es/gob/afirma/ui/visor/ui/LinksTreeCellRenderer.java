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
        if (value instanceof DefaultMutableTreeNode) {
            if (!(((DefaultMutableTreeNode) value).getUserObject() instanceof String)) {
            	//Comprobacion de que el texto no sea nulo o vacio.
            	String text = getText();
            	//Color de fondo cuando se ha seleccionado el elemento
            	Color selectionColor =getBackgroundSelectionColor();
            	//Color de fondo cuando un elemento no está seleccionado
            	Color nonSelectionColor =getBackgroundNonSelectionColor();
            	
            	if (text!=null && !text.equalsIgnoreCase("")) {
	                if (focus) {
	                	
	                    //setText("<html><font color=\"white\"><u>" + text + "</u></font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
	                	setText("<html><span style=\"text-decoration: underline; color:white;\">" + text + 
	                			"</span><span style=\"color:rgb("+selectionColor.getRed()+","+selectionColor.getGreen() +
	                			","+selectionColor.getBlue()+");\">" + " " + Messages.getString("Link.access") + "</span></html>"); //$NON-NLS-1$ //$NON-NLS-2$

	                }
	                else {
	                    //setText("<html><font color=\"blue\"><u>" + text + "</u></font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
	                	setText("<html><span style=\"text-decoration: underline; color:blue;\">" + text + 
	                			"</span><span style=\"color:rgb("+nonSelectionColor.getRed()+","+nonSelectionColor.getGreen() +
	                			","+nonSelectionColor.getBlue()+");\">" + " " + Messages.getString("Link.access") + "</span></html>"); //$NON-NLS-1$ //$NON-NLS-2$
	                }

            	}
            }
        }
        return this;
    }
}
