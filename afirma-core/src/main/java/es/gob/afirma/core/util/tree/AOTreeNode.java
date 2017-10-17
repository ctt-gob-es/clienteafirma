/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.util.tree;

import java.util.ArrayList;
import java.util.List;

/** Adaptaci&oacute;n de las clases TreeNode de Swing para su uso sin interfaz gr6aacute;fico. */
public final class AOTreeNode {

    /** this node's parent, or null if this node has no parent */
    private AOTreeNode parent;

    /** array of children, may be null if this node has no children */
    private List<AOTreeNode> children;

    /** optional user object */
    private transient Object userObject;

    /** Creates a tree node with no parent, no children, but which allows
     * children, and initializes it with the specified user object.
     * @param userObject
     *        an Object provided by the user that constitutes the node's
     *        data */
    public AOTreeNode(final Object userObject) {
        super();
        this.parent = null;
        this.userObject = userObject;
    }

    //
    // Primitives
    //

    /** Removes <code>newChild</code> from its present parent (if it has a
     * parent), sets the child's parent to this node, and then adds the child to
     * this node's child array at index <code>childIndex</code>. <code>newChild</code> must not be null and must not be an ancestor of
     * this node.
     * @param newChild
     *        the TreeNode to insert under this node
     * @param childIndex
     *        the index in this node's child array where this node is to be
     *        inserted
     * @exception ArrayIndexOutOfBoundsException
     *            if <code>childIndex</code> is out of bounds
     * @exception IllegalArgumentException
     *            if <code>newChild</code> is null or is an ancestor of this
     *            node
     * @exception IllegalStateException
     *            if this node does not allow children */
    private void insert(final AOTreeNode newChild, final int childIndex) {

        if (newChild == null) {
            throw new IllegalArgumentException("EL nuevo hijo es nulo"); //$NON-NLS-1$
        }
        else if (isNodeAncestor(newChild)) {
            throw new IllegalArgumentException("El nuevo hijo es ya un ancestro"); //$NON-NLS-1$
        }

        final AOTreeNode oldParent = newChild.getParent();

        if (oldParent != null) {
            oldParent.remove(newChild);
        }
        newChild.setParent(this);
        if (this.children == null) {
            this.children = new ArrayList<>();
        }
        this.children.add(childIndex, newChild);
    }

    /** Removes the child at the specified index from this node's children and
     * sets that node's parent to null. The child node to remove must be a <code>TreeNode</code>.
     * @param childIndex
     *        the index in this node's child array of the child to remove
     * @exception ArrayIndexOutOfBoundsException
     *            if <code>childIndex</code> is out of bounds */
    private void remove(final int childIndex) {
        final AOTreeNode child = getChildAt(childIndex);
        this.children.remove(childIndex);
        child.setParent(null);
    }

    /** Sets this node's parent to <code>newParent</code> but does not change the
     * parent's child array. This method is called from <code>insert()</code> and <code>remove()</code> to reassign a child's parent, it should not be
     * messaged from anywhere else.
     * @param newParent
     *        this node's new parent */
    public void setParent(final AOTreeNode newParent) {
        this.parent = newParent;
    }

    /** Returns this node's parent or null if this node has no parent.
     * @return this node's parent TreeNode, or null if this node has no parent */
    public AOTreeNode getParent() {
        return this.parent;
    }

    /** Returns the child at the specified index in this node's child array.
     * @param index
     *        an index into this node's child array
     * @exception ArrayIndexOutOfBoundsException
     *            if <code>index</code> is out of bounds
     * @return the TreeNode in this node's child array at the specified index */
    public AOTreeNode getChildAt(final int index) {
        if (this.children == null) {
            throw new ArrayIndexOutOfBoundsException("El nodo no tiene hijos"); //$NON-NLS-1$
        }
        return this.children.get(index);
    }

    /** Returns the number of children of this node.
     * @return an int giving the number of children of this node */
    public int getChildCount() {
        if (this.children == null) {
            return 0;
        }
        return this.children.size();
    }

    /** Returns the index of the specified child in this node's child array. If
     * the specified node is not a child of this node, returns <code>-1</code>.
     * This method performs a linear search and is O(n) where n is the number of
     * children.
     * @param aChild
     *        the TreeNode to search for among this node's children
     * @exception IllegalArgumentException
     *            if <code>aChild</code> is null
     * @return an int giving the index of the node in this node's child array,
     *         or <code>-1</code> if the specified node is a not a child of this
     *         node */
    private int getIndex(final AOTreeNode aChild) {
        if (aChild == null) {
            throw new IllegalArgumentException("Argumento nulo"); //$NON-NLS-1$
        }

        if (!isNodeChild(aChild)) {
            return -1;
        }
        return this.children.indexOf(aChild); // linear search
    }

    /** Returns this node's user object.
     * @return the Object stored at this node by the user */
    public Object getUserObject() {
        return this.userObject;
    }

    //
    // Derived methods
    //

    /** Removes <code>aChild</code> from this node's child array, giving it a
     * null parent.
     * @param aChild
     *        a child of this node to remove
     * @exception IllegalArgumentException
     *            if <code>aChild</code> is null or is not a child of this
     *            node */
    private void remove(final AOTreeNode aChild) {
        if (aChild == null) {
            throw new IllegalArgumentException("Argumento nulo"); //$NON-NLS-1$
        }

        if (!isNodeChild(aChild)) {
            throw new IllegalArgumentException("El argumento no es un hijo"); //$NON-NLS-1$
        }
        remove(getIndex(aChild)); // linear search
    }

    /** Removes <code>newChild</code> from its parent and makes it a child of
     * this node by adding it to the end of this node's child array.
     * @see #insert
     * @param newChild
     *        node to add as a child of this node
     * @exception IllegalArgumentException
     *            if <code>newChild</code> is null
     * @exception IllegalStateException
     *            if this node does not allow children */
    public void add(final AOTreeNode newChild) {
        if (newChild != null && newChild.getParent() == this) {
            insert(newChild, getChildCount() - 1);
        }
        else {
            insert(newChild, getChildCount());
        }
    }

    //
    // TREE Queries
    //

    /** Returns true if <code>anotherNode</code> is an ancestor of this node --
     * if it is this node, this node's parent, or an ancestor of this node's
     * parent. (Note that a node is considered an ancestor of itself.) If <code>anotherNode</code> is null, this method returns false. This
     * operation is at worst O(h) where h is the distance from the root to this
     * node.
     * @param anotherNode
     *        node to test as an ancestor of this node
     * @return true if this node is a descendant of <code>anotherNode</code> */
    private boolean isNodeAncestor(final AOTreeNode anotherNode) {
        if (anotherNode == null) {
            return false;
        }

        AOTreeNode ancestor = this;

        do {
            if (ancestor.equals(anotherNode)) {
                return true;
            }
            ancestor = ancestor.getParent();
        } while (ancestor != null);

        return false;
    }

    //
    // Child Queries
    //

    private boolean isNodeChild(final AOTreeNode aNode) {
        boolean retval;

        if (aNode == null) {
            retval = false;
        }
        else {
            if (getChildCount() == 0) {
                retval = false;
            }
            else {
                retval = aNode.getParent() == this;
            }
        }

        return retval;
    }

    //
    // Leaf Queries
    //

    /** Returns true if this node has no children. To distinguish between nodes
     * that have no children and nodes that <i>cannot</i> have children (e.g. to
     * distinguish files from empty directories), use this method in conjunction
     * with <code>getAllowsChildren</code>
     * @return true if this node has no children */
    public boolean isLeaf() {
        return getChildCount() == 0;
    }

    //
    // Overrides
    //

    /** Returns the result of sending <code>toString()</code> to this node's user
     * object, or null if this node has no user object.
     * @see #getUserObject */
    @Override
    public String toString() {
        if (this.userObject == null) {
            return "null"; //$NON-NLS-1$
        }
        return this.userObject.toString();
    }
}
