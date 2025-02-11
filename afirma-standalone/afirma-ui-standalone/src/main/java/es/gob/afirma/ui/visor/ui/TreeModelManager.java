/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.ui.visor.ui;

import javax.swing.tree.DefaultMutableTreeNode;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;

/** Operaciones para la gesti&oacute;n de objetos TreeModel.
 * @author Carlos Gamuci Mill&aacute;n */
final class TreeModelManager {

    private final AOTreeModel tree;

    /** Construye una clase de gesti&oacute;n de &aacute;rboles <code>Swing</code>
     * @param tree &Aacute;rbol <code>Swing</code> a gestionar */
    public TreeModelManager(final AOTreeModel tree) {
        this.tree = tree;
    }

    /** Recupera un &aacute;rbol Swing a partir de un TreeModel con los certificados
     * del Cliente @firma.
     * @return &Aacute;rbol Swing. */
    public DefaultMutableTreeNode getSwingTree() {
        final DefaultMutableTreeNode swingTreeRoot = new DefaultMutableTreeNode();
        copyBranch((AOTreeNode) this.tree.getRoot(), swingTreeRoot);
        return swingTreeRoot;
    }

    /** Copia el contenido de un nodo y replica la rama que cuelga del mismo.
     * @param treeNode Nodo que deseamos replicar.
     * @param swingTreeNode Nodo al que se desea copiar
     * @return Nodo replicado. */
    private DefaultMutableTreeNode copyBranch(final AOTreeNode treeNode, final DefaultMutableTreeNode swingTreeNode) {
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
