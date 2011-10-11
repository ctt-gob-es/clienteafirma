/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.core.ui.jse;

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
        final DefaultTreeModel swingDefaultTreeModel = new DefaultTreeModel(rootSwing, treeModel.asksAllowsChildren());

        // Listado con los padres del modelo original
        final List<AOTreeNode> parents = new ArrayList<AOTreeNode>();
        parents.add(root);

        // Listado con los padres que introduciremos en el modelo swing
        final List<DefaultMutableTreeNode> parentsSwing = new ArrayList<DefaultMutableTreeNode>();
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
