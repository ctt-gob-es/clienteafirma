/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.util.tree;

/** Adaptaci&oacute;n de las clases AOTreeModel de Swing para su uso sin interfaz gr6aacute;fico. */
public final class AOTreeModel {

    /** Root of the tree. */
    private final AOTreeNode root;

    /** Crea un &aacute;rbol en el que cualquier nodo puede tener hijos.
     * @param root La ra&iacute;z del &aacute;rbol */
    public AOTreeModel(final AOTreeNode root) {
        this.root = root;
    }

    /** Returns the root of the tree. Returns null only if the tree has no nodes.
     * @return the root of the tree */
    public Object getRoot() {
        return this.root;
    }

    /** Returns the child of <I>parent</I> at index <I>index</I> in the parent's
     * child array. <I>parent</I> must be a node previously obtained from this
     * data source. This should not return null if <i>index</i> is a valid index
     * for <i>parent</i> (that is <i>index</i> &gt;= 0 &amp;&amp; <i>index</i> &lt;
     * getChildCount(<i>parent</i>)).
     * @param index
     *        the position index of the child
     * @param parent
     *        a node in the tree, obtained from this data source
     * @return the child of <I>parent</I> at index <I>index</I> */
    public static Object getChild(final Object parent, final int index) {
        return ((AOTreeNode) parent).getChildAt(index);
    }

    /** Returns the number of children of <I>parent</I>. Returns 0 if the node is
     * a leaf or if it has no children. <I>parent</I> must be a node previously
     * obtained from this data source.
     * @param parent
     *        a node in the tree, obtained from this data source
     * @return the number of children of the node <I>parent</I> */
    public static int getChildCount(final Object parent) {
        return ((AOTreeNode) parent).getChildCount();
    }

}
