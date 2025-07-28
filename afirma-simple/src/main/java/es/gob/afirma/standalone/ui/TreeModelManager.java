/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import javax.swing.tree.DefaultMutableTreeNode;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;

/** Operaciones para la gesti&oacute;n de objetos TreeModel.
 * @author Carlos Gamuci Mill&aacute;n */
public final class TreeModelManager {

    private final AOTreeModel tree;

    /** Construye una clase de gesti&oacute;n de &aacute;rboles <code>Swing</code>
     * @param tree &Aacute;rbol <code>Swing</code> a gestionar */
    public TreeModelManager(final AOTreeModel tree) {
        this.tree = tree;
    }

    /** Recupera un &aacute;rbol Swing a partir de un TreeModel con los certificados
     * de Autofirma.
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
