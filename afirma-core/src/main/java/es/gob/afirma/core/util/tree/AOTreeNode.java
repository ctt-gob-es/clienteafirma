/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.core.util.tree;

import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Stack;
import java.util.Vector;

/** Adaptaci&oacute;n de las clases TreeNode de Swing para su uso sin interfaz gr6aacute;fico. */
public final class AOTreeNode {

    private static final long serialVersionUID = 1L;

    /** An enumeration that is always empty. This is used when an enumeration of
     * a leaf node's children is requested. */
    private static final Enumeration<AOTreeNode> EMPTY_ENUMERATION = new Enumeration<AOTreeNode>() {
        public boolean hasMoreElements() {
            return false;
        }

        public AOTreeNode nextElement() {
            throw new NoSuchElementException("No hay mas elementos"); //$NON-NLS-1$
        }
    };

    /** this node's parent, or null if this node has no parent */
    private AOTreeNode parent;

    /** array of children, may be null if this node has no children */
    private Vector<AOTreeNode> children;

    /** optional user object */
    private transient Object userObject;

    /** true if the node is able to have children */
    private final boolean allowsChildren;

    /** Creates a tree node with no parent, no children, but which allows
     * children, and initializes it with the specified user object.
     * @param userObject
     *        an Object provided by the user that constitutes the node's
     *        data */
    public AOTreeNode(final Object userObject) {
        this(userObject, true);
    }

    /** Creates a tree node with no parent, no children, initialized with the
     * specified user object, and that allows children only if specified.
     * @param userObject
     *        an Object provided by the user that constitutes the node's
     *        data
     * @param allowsChildren
     *        if true, the node is allowed to have child nodes -- otherwise,
     *        it is always a leaf node */
    private AOTreeNode(final Object userObject, final boolean allowsChildren) {
        super();
        this.parent = null;
        this.allowsChildren = allowsChildren;
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
        if (!this.allowsChildren) {
            throw new IllegalStateException("El nodo no permite hijos"); //$NON-NLS-1$
        }
        else if (newChild == null) {
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
            this.children = new Vector<AOTreeNode>();
        }
        this.children.insertElementAt(newChild, childIndex);
    }

    /** Removes the child at the specified index from this node's children and
     * sets that node's parent to null. The child node to remove must be a <code>TreeNode</code>.
     * @param childIndex
     *        the index in this node's child array of the child to remove
     * @exception ArrayIndexOutOfBoundsException
     *            if <code>childIndex</code> is out of bounds */
    private void remove(final int childIndex) {
        final AOTreeNode child = getChildAt(childIndex);
        this.children.removeElementAt(childIndex);
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
        return this.children.elementAt(index);
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

    /** Creates and returns a forward-order enumeration of this node's children.
     * Modifying this node's child array invalidates any child enumerations
     * created before the modification.
     * @return an Enumeration of this node's children */
    Enumeration<AOTreeNode> children() {
        if (this.children == null) {
            return EMPTY_ENUMERATION;
        }
        return this.children.elements();
    }

    /** Returns true if this node is allowed to have children.
     * @return true if this node allows children, else false */
    public boolean getAllowsChildren() {
        return this.allowsChildren;
    }

    /** Sets the user object for this node to <code>userObject</code>.
     * @param userObject
     *        the Object that constitutes this node's user-specified data
     * @see #getUserObject
     * @see #toString */
    public void setUserObject(final Object userObject) {
        this.userObject = userObject;
    }

    /** Returns this node's user object.
     * @return the Object stored at this node by the user
     * @see #setUserObject
     * @see #toString */
    public Object getUserObject() {
        return this.userObject;
    }

    //
    // Derived methods
    //

    /** Removes the subtree rooted at this node from the tree, giving this node a
     * null parent. Does nothing if this node is the root of its tree. */
    public void removeFromParent() {
        final AOTreeNode part = getParent();
        if (part != null) {
            part.remove(this);
        }
    }

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
        } while ((ancestor = ancestor.getParent()) != null);

        return false;
    }

    /** Creates and returns an enumeration that traverses the subtree rooted at
     * this node in preorder. The first node returned by the enumeration's <code>nextElement()</code> method is this node.
     * <P>
     * Modifying the tree by inserting, removing, or moving a node invalidates any enumerations created before the modification.
     * @return an enumeration for traversing the tree in preorder */
    public Enumeration<AOTreeNode> preorderEnumeration() {
        return new PreorderEnumeration(this);
    }

    //
    // Child Queries
    //

    /** Returns true if <code>aNode</code> is a child of this node. If <code>aNode</code> is null, this method returns false.
     * @return true if <code>aNode</code> is a child of this node; false if <code>aNode</code> is null */
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
                retval = (aNode.getParent() == this);
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
     * @see #getAllowsChildren
     * @return true if this node has no children */
    public boolean isLeaf() {
        return (getChildCount() == 0);
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

    private static final class PreorderEnumeration implements Enumeration<AOTreeNode> {
        private final Stack<Enumeration<AOTreeNode>> stack;

        PreorderEnumeration(final AOTreeNode rootNode) {
            super();
            final Vector<AOTreeNode> v = new Vector<AOTreeNode>(1);
            v.addElement(rootNode); // PENDING: don't really need a vector
            this.stack = new Stack<Enumeration<AOTreeNode>>();
            this.stack.push(v.elements());
        }

        public boolean hasMoreElements() {
            return (!this.stack.empty() && (this.stack.peek()).hasMoreElements());
        }

        public AOTreeNode nextElement() {
            final Enumeration<AOTreeNode> enumer = this.stack.peek();
            final AOTreeNode node = enumer.nextElement();
            final Enumeration<AOTreeNode> child = node.children();

            if (!enumer.hasMoreElements()) {
                this.stack.pop();
            }
            if (child.hasMoreElements()) {
                this.stack.push(child);
            }
            return node;
        }

    } // End of class PreorderEnumeration

}
