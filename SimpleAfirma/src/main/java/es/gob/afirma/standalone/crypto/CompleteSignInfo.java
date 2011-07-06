/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.crypto;

import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.signers.beans.AOSignInfo;

public final class CompleteSignInfo {

    private byte[] signData;

    private byte[] data;

    private AOSignInfo signInfo;

    private TreeModel signsTree;

    public byte[] getSignData() {
        return this.signData;
    }

    public void setSignData(final byte[] signData) {
        this.signData = signData;
    }

    public AOSignInfo getSignInfo() {
        return this.signInfo;
    }

    public void setSignInfo(final AOSignInfo signInfo) {
        this.signInfo = signInfo;
    }

    public TreeModel getSignsTree() {
        return this.signsTree;
    }

    public void setSignsTree(final TreeModel signsTree) {
        this.signsTree = signsTree;
    }

    public byte[] getData() {
        return this.data;
    }

    public void setData(final byte[] data) {
        this.data = data;
    }
}
