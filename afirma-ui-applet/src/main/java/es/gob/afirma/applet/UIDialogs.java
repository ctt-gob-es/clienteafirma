/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import javax.swing.JFileChooser;
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
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.ui.core.jse.JSEUtils;


/**
 * Funciones para el uso de di&aacute;logos modales.
 * @author Carlos Gamuci
 */
public final class UIDialogs {

	private UIDialogs() {
		// No permitimos la instanciacion
	}

    /** Pregunta al usuario por un nombre de fichero para salvar datos en disco.
     * @param extensions
     *        Extensiones predeterminadas para el fichero
     * @param description
     *        Descripci&oacute;n del tipo de fichero correspondiente con las
     *        extensiones
     * @param parentComponent
     *        Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    public final static String getSaveFileName(final String[] extensions, final String description, final Component parentComponent) {

        final JFileChooser jfc = new JFileChooser();
        jfc.setLocale(Locale.getDefault());
        if (extensions != null && extensions.length > 0) {
            jfc.setFileFilter(new ExtFilter(extensions, description));
            jfc.setSelectedFile(new File("*." + extensions[0])); //$NON-NLS-1$
        }

        boolean selectedFilename = false;
        String finalFilename = null;
        do {
            final int ret = jfc.showSaveDialog(parentComponent);
            if (ret == JFileChooser.APPROVE_OPTION) {
                final File tempFile = jfc.getSelectedFile();
                if (tempFile.exists()) {
                    if (tempFile.isDirectory() && !tempFile.canWrite()) {
                        JOptionPane.showMessageDialog(parentComponent,
                        		AppletMessages.getString("AOUIManager.74") + jfc.getSelectedFile().getAbsolutePath() + ".", //$NON-NLS-1$ //$NON-NLS-2$
                        		AppletMessages.getString("AOUIManager.81"), //$NON-NLS-1$
                                                      JOptionPane.WARNING_MESSAGE);
                        continue;
                    }
                    final int resp =
                            JOptionPane.showConfirmDialog(parentComponent,
                            		AppletMessages.getString("AOUIManager.77") + "\r\n" + jfc.getSelectedFile().getAbsolutePath(), //$NON-NLS-1$ //$NON-NLS-2$
                            		AppletMessages.getString("AOUIManager.81"), //$NON-NLS-1$
                                                          JOptionPane.YES_NO_CANCEL_OPTION,
                                                          JOptionPane.QUESTION_MESSAGE);
                    if (resp == JOptionPane.YES_OPTION) { // Sobreescribir
                                                          // fichero
                        finalFilename = jfc.getSelectedFile().getAbsolutePath();
                        selectedFilename = true;
                    }
                    else if (resp == JOptionPane.NO_OPTION) { // Seleccionar
                                                              // fichero
                        continue;
                    }
                    else { // Cancelar operacion de guardado
                        finalFilename = null;
                        selectedFilename = true;
                    }
                }
                else {
                    finalFilename = jfc.getSelectedFile().getAbsolutePath();
                    selectedFilename = true;
                }
            }
            else { // Cancelar operacion de seleccion de nombre
                finalFilename = null;
                selectedFilename = true;
            }
        } while (!selectedFilename);

        return finalFilename;
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
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancel&oacute; la operaci&oacute;n. */
    public static final String[] showSignersSelectionPane(final AOTreeModel treeModel, final Component parentComponent) throws AOException,
                                                                                                                                             AOCancelledOperationException {
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
        panel.add(new JLabel(AppletMessages.getString("AOUIManager.65")), c); //$NON-NLS-1$
        c.weighty = 1.0;
        c.gridy = 1;
        panel.add(spArbolNodos, c);

        do {
            final int action = JOptionPane.showConfirmDialog(parentComponent, panel, AppletMessages.getString("AOUIManager.66"), //$NON-NLS-1$
                                                             JOptionPane.OK_CANCEL_OPTION);
            if (action != JOptionPane.OK_OPTION) {
                throw new AOCancelledOperationException("La operacion de firma ha sido cancelada por el usuario"); //$NON-NLS-1$
            }

            if (jList.getSelectedIndices() == null || jList.getSelectedIndices().length < 1) {
                JOptionPane.showMessageDialog(parentComponent,
                		AppletMessages.getString("AOUIManager.20"), AppletMessages.getString("AOUIManager.21"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
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
	private static final void getSigners(final DefaultMutableTreeNode node, final Set<String> signersSet) {
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
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancel&oacute; la operaci&oacute;n. */
    public static final int[] showNodeSignSelectionPane(final AOTreeModel treeModel, final Component parentComponent) throws AOCancelledOperationException {
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
            public void treeCollapsed(final TreeExpansionEvent event) {
                ((JTree) event.getSource()).expandPath(event.getPath());
            }
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
        panel.add(new JLabel(AppletMessages.getString("AOUIManager.60")), c); //$NON-NLS-1$
        c.weighty = 1.0;
        c.gridy = 1;
        panel.add(spArbolNodos, c);

        do {
            final int action = JOptionPane.showConfirmDialog(parentComponent, panel, AppletMessages.getString("AOUIManager.61"), //$NON-NLS-1$
                                                             JOptionPane.OK_CANCEL_OPTION,
                                                             JOptionPane.PLAIN_MESSAGE);
            if (action != JOptionPane.OK_OPTION) {
                throw new AOCancelledOperationException("La operacion de firma ha sido cancelada por el usuario"); //$NON-NLS-1$
            }

            if (arbolNodos.getSelectionRows() == null || arbolNodos.getSelectionRows().length < 1) {
                JOptionPane.showMessageDialog(parentComponent,
                		AppletMessages.getString("AOUIManager.18"), AppletMessages.getString("AOUIManager.19"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            }

        } while (arbolNodos.getSelectionRows() == null || arbolNodos.getSelectionRows().length < 1);

        return arbolNodos.getSelectionRows();
    }

    /** Permite al usuario seleccionar un directorio y devuelve su ruta absoluta.
     * Si no se selecciona ninguno se devuelve <code>null</code>.
     * @param parentComponent
     *        Componente padre (para la modalidad)
     * @param title
     *        T&iacute;tulo del di&aacute;logo de selecci&oacute;n
     * @return Ruta absoluta del directorio seleccionado. */
    public final static String selectDirectory(final Component parentComponent, final String title) {
        final JFileChooser jfc = new JFileChooser();
        jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        jfc.setDialogTitle(title);
        final int ret = jfc.showOpenDialog(parentComponent);
        if (ret == JFileChooser.APPROVE_OPTION) {
            return jfc.getSelectedFile().getAbsolutePath();
        }
        return null;
    }
}
