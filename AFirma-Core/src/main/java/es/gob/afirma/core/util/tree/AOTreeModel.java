package es.gob.afirma.core.util.tree;

/** Adaptaci&oacute;n de las clases AOTreeModel de Swing para su uso sin interfaz gr6aacute;fico. */
public final class AOTreeModel {

    private static final long serialVersionUID = 1L;

    /** Root of the tree. */
    private final TreeNode root;

    /** Children count. Always 1 starting with the root */
    private int count = 1;

    /** Obtiene el n&uacute;mero de elementos del &aacute;rbol.
     * @return N&uacute;mero de elementos del &aacute;rbol */
    public Integer getCount() {
        return new Integer(count);
    }

    /** Determines how the <code>isLeaf</code> method figures out if a node is a
     * leaf node. If true, a node is a leaf node if it does not allow children.
     * (If it allows children, it is not a leaf node, even if no children are
     * present.) That lets you distinguish between <i>folder</i> nodes and
     * <i>file</i> nodes in a file system, for example.
     * <p>
     * If this value is false, then any node which has no children is a leaf node, and any node may acquire children.
     * @see TreeNode#getAllowsChildren
     * @see AOTreeModel#isLeaf
     * @see #setAsksAllowsChildren */
    private final boolean asksAllowsChildren;

    /** Creates a tree in which any node can have children.
     * @param root
     *        a TreeNode object that is the root of the tree
     * @see #AOTreeModel(TreeNode, boolean) */
    public AOTreeModel(final TreeNode root) {
        this(root, false);
    }

    /** Creates a tree specifying whether any node can have children, or whether
     * only certain nodes can have children.
     * @param root
     *        a TreeNode object that is the root of the tree
     * @param asksAllowsChildren
     *        a boolean, false if any node can have children, true if each
     *        node is asked to see if it can have children
     * @see #asksAllowsChildren */
    private AOTreeModel(final TreeNode root, final boolean asksAllowsChildren) {
        super();
        this.root = root;
        this.asksAllowsChildren = asksAllowsChildren;
    }

    /** Construye un nuevo &aacute;rbol.
     * @param treeRoot Ra&iacute;z del &aacute;rbol
     * @param count N&uacute;mero de elementos iniciales del nuevo &aacute;rbol */
    public AOTreeModel(final TreeNode treeRoot, final int count) {
        this(treeRoot, false);
        this.count = count;
    }

    /** Tells how leaf nodes are determined.
     * @return true if only nodes which do not allow children are leaf nodes,
     *         false if nodes which have no children (even if allowed) are leaf
     *         nodes
     * @see #asksAllowsChildren */
    public boolean asksAllowsChildren() {
        return asksAllowsChildren;
    }

    /** Returns the root of the tree. Returns null only if the tree has no nodes.
     * @return the root of the tree */
    public Object getRoot() {
        return root;
    }

    /** Returns the child of <I>parent</I> at index <I>index</I> in the parent's
     * child array. <I>parent</I> must be a node previously obtained from this
     * data source. This should not return null if <i>index</i> is a valid index
     * for <i>parent</i> (that is <i>index</i> >= 0 && <i>index</i> <
     * getChildCount(<i>parent</i>)).
     * @param index
     *        the position index of the child
     * @param parent
     *        a node in the tree, obtained from this data source
     * @return the child of <I>parent</I> at index <I>index</I> */
    public Object getChild(final Object parent, final int index) {
        return ((TreeNode) parent).getChildAt(index);
    }

    /** Returns the number of children of <I>parent</I>. Returns 0 if the node is
     * a leaf or if it has no children. <I>parent</I> must be a node previously
     * obtained from this data source.
     * @param parent
     *        a node in the tree, obtained from this data source
     * @return the number of children of the node <I>parent</I> */
    public int getChildCount(final Object parent) {
        return ((TreeNode) parent).getChildCount();
    }

}
