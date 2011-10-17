/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.ui.visor.crypto;

import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;

/**
 * Informaci&oacute;n extra&iacute;da de una firma.
 * @author Carlos Gamuci
 */
public final class CompleteSignInfo {

    private byte[] signData;

    private byte[] data;

    private AOSignInfo signInfo;

    private AOTreeModel signsTree;

    /**
     * Recupera la firma de la que se extraen los datos.
     * @return Firma.
     * @see #setSignData(byte[])
     */
    public byte[] getSignData() {
        return this.signData;
    }

    /**
     * Establece la firma de la que se extraen los datos.
     * @param signData Firma.
     */
    public void setSignData(final byte[] signData) {
        this.signData = signData;
    }

    /**
     * Recupera la informaci&oacute;n general de la firma.
     * @return Informaci&oacute;n general de la firma.
     */
    public AOSignInfo getSignInfo() {
        return this.signInfo;
    }

    /**
     * Establece la informaci&oacute;n general de la firma.
     * @param signInfo Informaci&oacute;n general de la firma.
     */
    public void setSignInfo(final AOSignInfo signInfo) {
        this.signInfo = signInfo;
    }

    /**
     * Recupera el &aacute;rbol de firma con los datos de cada firma individucal.
     * @return &Aacute;rbol de firma.
     */
    public AOTreeModel getSignsTree() {
        return this.signsTree;
    }

    /**
     * Establece el &aacute;rbol de firma con los datos de cada firma individucal.
     * @param signsTree &Aacute;rbol de firma.
     */
    public void setSignsTree(final AOTreeModel signsTree) {
        this.signsTree = signsTree;
    }

    /**
     * Recupera los datos que se firmaon.
     * @return Datos que se firmaron.
     */
    public byte[] getData() {
        return this.data;
    }

    /**
     * Establece los datos que se firmaon.
     * @param data Datos que se firmaron.
     */
    public void setData(final byte[] data) {
        this.data = data;
    }
}
