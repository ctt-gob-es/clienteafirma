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

final class AOTreePath {

    private static final long serialVersionUID = 1L;

    /** Path representing the parent, null if lastPathComponent represents the
     * root. */
    private AOTreePath parentPath;

    /** Last path component. */
    transient private Object lastPathComponent;

    /** Constructs a path from an array of Objects, uniquely identifying the path
     * from the root of the tree to a specific node, as returned by the tree's
     * data model.
     * <p>
     * The model is free to return an array of any Objects it needs to represent the path. The DefaultTreeModel returns an array of TreeNode objects.
     * The first TreeNode in the path is the root of the tree, the last TreeNode is the node identified by the path.
     * @param path
     *        an array of Objects representing the path to a node */
    AOTreePath(final Object[] path) {
        if (path == null || path.length == 0) {
            throw new IllegalArgumentException("La ruta en el TreePath no puede ser ni nula ni vacia"); //$NON-NLS-1$
        }
        this.lastPathComponent = path.clone()[path.length - 1];
        if (path.length > 1) {
            this.parentPath = new AOTreePath(path, path.length - 1);
        }
    }

    /** Constructs a new TreePath with the identified path components of length <code>length</code>. */
    private AOTreePath(final Object[] path, final int length) {
        this.lastPathComponent = path.clone()[length - 1];
        if (length > 1) {
            this.parentPath = new AOTreePath(path, length - 1);
        }
    }

    /** Returns the number of elements in the path.
     * @return an int giving a count of items the path */
    private int getPathCount() {
        int result = 0;
        for (AOTreePath path = this; path != null; path = path.parentPath) {
            result++;
        }
        return result;
    }

    /** Returns the path component at the specified index.
     * @param element
     *        an int specifying an element in the path, where 0 is the first
     *        element in the path
     * @return the Object at that index location
     * @throws IllegalArgumentException
     *         if the index is beyond the length of the path
     * @see #TreePath(Object[]) */
    private Object getPathComponent(final int element) {
        final int pathLength = getPathCount();

        if (element < 0 || element >= pathLength) {
            throw new IllegalArgumentException("El indice " + element + " esta fuera del rango especificado"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        AOTreePath path = this;

        for (int i = pathLength - 1; i != element; i--) {
            path = path.parentPath;
        }
        return path.lastPathComponent;
    }

//    /** Tests two TreePaths for equality by checking each element of the paths
//     * for equality. Two paths are considered equal if they are of the same
//     * length, and contain the same elements (<code>.equals</code>).
//     * @param o
//     *        the Object to compare */
//    @Override
//    public boolean equals(final Object o) {
//        if (o == this) {
//            return true;
//        }
//        if (o instanceof TreePath) {
//            TreePath oTreePath = (TreePath) o;
//
//            if (getPathCount() != oTreePath.getPathCount()) {
//                return false;
//            }
//            for (TreePath path = this; path != null; path = path.parentPath) {
//                if (!(path.lastPathComponent.equals(oTreePath.lastPathComponent))) {
//                    return false;
//                }
//                oTreePath = oTreePath.parentPath;
//            }
//            return true;
//        }
//        return false;
//    }

    /** Returns a string that displays and identifies this object's properties.
     * @return a String representation of this object */
    @Override
    public String toString() {
        final StringBuffer tempSpot = new StringBuffer("["); //$NON-NLS-1$

        for (int counter = 0, maxCounter = getPathCount(); counter < maxCounter; counter++) {
            if (counter > 0) {
                tempSpot.append(", "); //$NON-NLS-1$
            }
            tempSpot.append(getPathComponent(counter));
        }
        tempSpot.append("]"); //$NON-NLS-1$
        return tempSpot.toString();
    }

}
