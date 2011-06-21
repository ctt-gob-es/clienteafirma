package es.gob.afirma.standalone.ui;

import java.awt.Component;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

public class LinksTreeCellRenderer extends DefaultTreeCellRenderer {
    
    /** Version ID */
    private static final long serialVersionUID = 335850028229506214L;

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
        if (value instanceof DefaultMutableTreeNode) {
            if (!(((DefaultMutableTreeNode) value).getUserObject() instanceof String)) {
                setText("<html><a href='#'>" + getText() + "</a>");
            }
        }
        return this;
    }
}
