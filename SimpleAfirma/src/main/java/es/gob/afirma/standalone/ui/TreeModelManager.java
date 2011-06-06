package es.gob.afirma.standalone.ui;

import javax.swing.tree.DefaultMutableTreeNode;

import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;

/**
 * Operaciones para la gesti&oacute;n de objetos TreeModel.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class TreeModelManager {

    private TreeModel tree;
    
    public TreeModelManager(TreeModel tree) {
        this.tree = tree;
    }
    
    /**
     * Recupera un &aacute;rbol Swing a partir de un TreeModel con los certificados
     * del Cliente @firma.
     * @return &Aacute;rbol Swing.
     */
    public DefaultMutableTreeNode getSwingTree() {
        
        DefaultMutableTreeNode swingTreeRoot = new DefaultMutableTreeNode();
        copyBranch((TreeNode) this.tree.getRoot(), swingTreeRoot);
        
        return swingTreeRoot;
    }
    
    /**
     * Copia el contenido de un nodo y replica la rama que cuelga del mismo.
     * @param treeNode Nodo que deseamos replicar.
     * @param swingTreeNode Nodo al que se desea copiar
     * @return Nodo replicado.
     */
    private DefaultMutableTreeNode copyBranch(TreeNode treeNode, DefaultMutableTreeNode swingTreeNode) {
        swingTreeNode.setUserObject(treeNode.getUserObject());
        DefaultMutableTreeNode newChild;
        for (int i = 0; i < treeNode.getChildCount(); i++) {
        newChild = new DefaultMutableTreeNode();
        copyBranch(treeNode.getChildAt(i), newChild);
        swingTreeNode.add(newChild);
        }
        return swingTreeNode;
    } 
}
