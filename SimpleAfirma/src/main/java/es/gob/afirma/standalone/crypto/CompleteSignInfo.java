package es.gob.afirma.standalone.crypto;

import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.signers.beans.AOSignInfo;

public class CompleteSignInfo {

    private byte[] signData;

    private AOSignInfo signInfo;

    private TreeModel signsTree;

    public byte[] getSignData() {
        return signData;
    }

    public void setSignData(byte[] signData) {
        this.signData = signData;
    }

    public AOSignInfo getSignInfo() {
        return signInfo;
    }

    public void setSignInfo(AOSignInfo signInfo) {
        this.signInfo = signInfo;
    }

    public TreeModel getSignsTree() {
        return signsTree;
    }

    public void setSignsTree(TreeModel signsTree) {
        this.signsTree = signsTree;
    }
}
