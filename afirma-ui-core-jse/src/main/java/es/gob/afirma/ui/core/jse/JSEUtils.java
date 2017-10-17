/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse;

import java.util.ArrayList;
import java.util.List;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;

/**
 * Utilidades gr&aacute;ficas propias de JSE.
 */
public final class JSEUtils {

    private JSEUtils() {
        // No permitimos la instanciacion
    }

    /** Transforma un TreeModel de Swing a uno propio
     * @param treeModel
     *        &Aacute;rbol gen&eacute;rico de AFirma
     * @return &Aacute;rbol Swing creado a partir de un &aacute;rbol
     *         gen&eacute;rico de AFirma */
    public static DefaultTreeModel convertToSwingModel(final AOTreeModel treeModel) {
        // Primero hay que obtener el objeto raiz
        final AOTreeNode root = (AOTreeNode) treeModel.getRoot();
        final Object rootObject = root.getUserObject();

        // Iniciamos el DefaultTreeModel
        final DefaultMutableTreeNode rootSwing = new DefaultMutableTreeNode(rootObject);
        final DefaultTreeModel swingDefaultTreeModel = new DefaultTreeModel(rootSwing);

        // Listado con los padres del modelo original
        final List<AOTreeNode> parents = new ArrayList<>();
        parents.add(root);

        // Listado con los padres que introduciremos en el modelo swing
        final List<DefaultMutableTreeNode> parentsSwing = new ArrayList<>();
        parentsSwing.add(rootSwing);

        int nParent = 0;
        AOTreeNode node;
        AOTreeNode childNode;
        DefaultMutableTreeNode swingNode;
        while (nParent < parents.size()) {

            node = parents.get(nParent);

            final int nNodes = node.getChildCount();
            for (int i = 0; i < nNodes; i++) {
                // Tomamos el hijo de la lista de padres del arbol original
                childNode = node.getChildAt(i);
                parents.add(childNode);

                // Agregamos un hijo equivalente a la lista de padres del arbol
                // Swing
                swingNode = new DefaultMutableTreeNode(childNode.getUserObject());
                parentsSwing.add(swingNode);

                // Agregamos el nuevo hijo a su padre del arbol
                parentsSwing.get(nParent).add(swingNode);
            }
            nParent++;
        }
        return swingDefaultTreeModel;
    }
}
