/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeSelectionModel;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.ui.core.jse.JSEUtils;


/**
 * Funciones para el uso de di&aacute;logos modales.
 * @author Carlos Gamuci
 */
final class UIDialogs {

	private UIDialogs() {
		// No permitimos la instanciacion
	}

    /** Muestra el di&aacute;logo de selecci&oacute;n de firmantes.
     * @param treeModel
     *        &Aacute;rbol de firmantes asociado a un fichero de firma.
     * @param parentComponent
     *        Componente padre sobre el que se mostrar&aacute;n los
     *        di&aacute;logos.
     * @return Listado de firmantes seleccionados.
     * @throws AOException
     *         Cuando el &aacute;rbol de firmantes contiene errores.
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando el usuario cancel&oacute; la operaci&oacute;n. */
    static String[] showSignersSelectionPane(final AOTreeModel treeModel, final Component parentComponent) throws AOException {
        final TreeModel tree = JSEUtils.convertToSwingModel(treeModel);

        final Set<String> signersSet = new HashSet<String>();

        if (tree == null || !(tree.getRoot() instanceof DefaultMutableTreeNode)) {
            throw new AOException("El arbol introducido no es valido"); //$NON-NLS-1$
        }

        // Recorremos todos los nodos menos el root que no contiene informacion
        // de firmante
        final DefaultMutableTreeNode root = (DefaultMutableTreeNode) tree.getRoot();
        try {
            for (int i = 0; i < root.getChildCount(); i++) {
                getSigners((DefaultMutableTreeNode) root.getChildAt(i), signersSet);
            }
        }
        catch (final Exception e) {
            throw new AOException("El arbol introducido contiene elementos no validos", e); //$NON-NLS-1$
        }

        // Recogemos los firmantes de los nodos
        String[] signers = new String[signersSet.size()];
        signersSet.toArray(signers);

        // Mostramos la lista de firmantes de la firma introducida
        final JList jList = new JList(signers);
        final JScrollPane spArbolNodos = new JScrollPane(jList);
        spArbolNodos.setPreferredSize(new Dimension(280, 200));
        final JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        panel.add(new JLabel(AppletMessages.getString("UIDialogs.1")), c); //$NON-NLS-1$
        c.weighty = 1.0;
        c.gridy = 1;
        panel.add(spArbolNodos, c);

        do {
            final int action = AOUIFactory.showConfirmDialog(
        		parentComponent,
        		panel,
        		AppletMessages.getString("UIDialogs.2"), //$NON-NLS-1$
        		AOUIFactory.OK_CANCEL_OPTION,
        		AOUIFactory.QUESTION_MESSAGE
            );
            if (action != JOptionPane.OK_OPTION) {
                throw new AOCancelledOperationException("La operacion de firma ha sido cancelada por el usuario"); //$NON-NLS-1$
            }

            if (jList.getSelectedIndices() == null || jList.getSelectedIndices().length < 1) {
            	AOUIFactory.showMessageDialog(
        			parentComponent,
            		AppletMessages.getString("UIDialogs.3"), //$NON-NLS-1$
            		AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
            		JOptionPane.WARNING_MESSAGE
        		);
            }

        } while (jList.getSelectedIndices() == null || jList.getSelectedIndices().length < 1);

        // Devolvemos los firmantes seleccionados
        final Object[] selectedValues = jList.getSelectedValues();
        signers = new String[selectedValues.length];
        for (int i = 0; i < selectedValues.length; i++) {
            signers[i] = (String) selectedValues[i];
        }
        return signers;
    }

	/**
	 * Introduce los alias de todos los certificados de firma del nodo indicado y todos sus subnodos.
	 * @param node Nodo de firma.
	 * @param signersSet Conjunto con los alias de los certificados de firma.
	 */
	private static void getSigners(final DefaultMutableTreeNode node, final Set<String> signersSet) {
		signersSet.add((String)node.getUserObject());
		for(int i=0; i<node.getChildCount(); i++) {
			getSigners((DefaultMutableTreeNode)node.getChildAt(i), signersSet);
		}
	}

    /** Muestra el di&aacute;logo de selecci&oacute;n de nodos de firma.
     * @param treeModel
     *        &Aacute;rbol de firmantes asociado a un fichero de firma.
     * @param parentComponent
     *        Componente padre sobre el que se mostrar&aacute;n los
     *        di&aacute;logos.
     * @return Listado con los &iacute;ndices de los nodos seleccionados.
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando el usuario cancel&oacute; la operaci&oacute;n. */
    static int[] showNodeSignSelectionPane(final AOTreeModel treeModel, final Component parentComponent) {
        final TreeModel tree = JSEUtils.convertToSwingModel(treeModel);

        final DefaultTreeCellRenderer treeRenderer = new DefaultTreeCellRenderer();
        treeRenderer.setLeafIcon(null);
        treeRenderer.setClosedIcon(null);
        treeRenderer.setOpenIcon(null);

        final JTree arbolNodos = new JTree(tree);
        arbolNodos.setRootVisible(false);
        arbolNodos.setCellRenderer(treeRenderer);
        arbolNodos.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        for (int i = 0; i < arbolNodos.getRowCount(); i++) {
            arbolNodos.expandRow(i);
        }
        arbolNodos.addTreeExpansionListener(new TreeExpansionListener() {
            @Override
			public void treeCollapsed(final TreeExpansionEvent event) {
                ((JTree) event.getSource()).expandPath(event.getPath());
            }
            @Override
			public void treeExpanded(final TreeExpansionEvent event) { /* No se implementa */ }
        });

        // Seleccionamos el primer elemento para que siempre haya
        arbolNodos.setSelectionRow(1);

        final JScrollPane spArbolNodos = new JScrollPane(arbolNodos);
        spArbolNodos.setPreferredSize(new Dimension(280, 200));

        final JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        panel.add(new JLabel(AppletMessages.getString("UIDialogs.4")), c); //$NON-NLS-1$
        c.weighty = 1.0;
        c.gridy = 1;
        panel.add(spArbolNodos, c);

        do {
            final int action = AOUIFactory.showConfirmDialog(
        		parentComponent,
        		panel,
        		AppletMessages.getString("UIDialogs.5"), //$NON-NLS-1$
        		AOUIFactory.OK_CANCEL_OPTION,
        		AOUIFactory.PLAIN_MESSAGE
            );
            if (action != AOUIFactory.OK_OPTION) {
                throw new AOCancelledOperationException("La operacion de firma ha sido cancelada por el usuario"); //$NON-NLS-1$
            }

            if (arbolNodos.getSelectionRows() == null || arbolNodos.getSelectionRows().length < 1) {
            	AOUIFactory.showMessageDialog(
        			parentComponent,
            		AppletMessages.getString("UIDialogs.6"), //$NON-NLS-1$
            		AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
            		JOptionPane.WARNING_MESSAGE
        		);
            }

        } while (arbolNodos.getSelectionRows() == null || arbolNodos.getSelectionRows().length < 1);

        return arbolNodos.getSelectionRows();
    }

}
