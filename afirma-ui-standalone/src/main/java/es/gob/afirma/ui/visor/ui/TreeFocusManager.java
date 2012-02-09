package es.gob.afirma.ui.visor.ui;

import java.awt.Cursor;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import es.gob.afirma.core.signers.AOSimpleSignInfo;

class TreeFocusManager extends KeyAdapter implements FocusListener, MouseMotionListener, MouseListener {
    
    private final JTree tree;
    private final TreeFocusManagerAction focusAction;
    
    private TreePath selectedPath;
    
    // Como los cursores los usamos dentro de un MouseMotionListener los precreamos para
    // evitar que se creen objetos solo por mover el raton
    private static final Cursor DEFAULT_CURSOR = new Cursor(Cursor.DEFAULT_CURSOR);
    private static final Cursor HAND_CURSOR = new Cursor(Cursor.HAND_CURSOR);
    
    TreeFocusManager(final JTree t, final TreeFocusManagerAction focusManagerAction) {
        this.tree = t;
        this.focusAction = focusManagerAction;
        this.selectedPath = this.tree.getPathForRow(0);
    }

    @Override
    public void focusGained(final FocusEvent fe) {
        if (this.tree.getRowCount() > 0) {
            this.tree.setSelectionPath(this.selectedPath);
            this.tree.scrollPathToVisible(this.selectedPath);
        }
    }

    @Override
    public void focusLost(final FocusEvent fe) {
        this.selectedPath = this.tree.getSelectionPath();
        this.tree.setSelectionPath(null);
    }

    @Override
    public void mouseDragged(final MouseEvent e) { /* No imlementado */}

    @Override
    public void mouseMoved(final MouseEvent e) {
        final TreePath path = this.tree.getPathForLocation((int) e.getPoint().getX(), (int) e.getPoint().getY());
        if (path != null) {
            if (path.getLastPathComponent() instanceof DefaultMutableTreeNode) {
                final Object o = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
                if (o instanceof ShowFileLinkAction || o instanceof AOSimpleSignInfo) {
                    this.tree.setCursor(this.HAND_CURSOR);
                }
                else {
                    this.tree.setCursor(this.DEFAULT_CURSOR);
                }
            }
            else {
                this.tree.setCursor(this.DEFAULT_CURSOR);
            }
        }
        else {
            this.tree.setCursor(this.DEFAULT_CURSOR);
        }
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        final DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.tree.getLastSelectedPathComponent();
        if (node == null) {
            return;
        }
        if (this.focusAction != null) {
            this.focusAction.openTreeNode(node.getUserObject());
        }
    }

    @Override public void mouseEntered(final MouseEvent e) { /* No implementado */}
    @Override public void mouseExited(final MouseEvent e) { /* No implementado */ }
    @Override public void mousePressed(final MouseEvent e) { /* No implementado */ }
    @Override public void mouseReleased(final MouseEvent e) { /* No implementado */ }
    
    @Override
    public void keyPressed(final KeyEvent e) {
        if (KeyEvent.VK_SPACE == e.getKeyCode() || KeyEvent.VK_ENTER == e.getKeyCode()) {
            if (this.focusAction != null) {
                final DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.tree.getLastSelectedPathComponent();
                if (node == null) {
                    return;
                }
                this.focusAction.openTreeNode(node.getUserObject());
            }
        }
    }

}
