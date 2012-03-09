/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.crypto;

import java.io.IOException;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
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
        return this.signData == null ? null : this.signData.clone();
    }

    /**
     * Establece la firma de la que se extraen los datos.
     * @param signData Firma.
     */
    public void setSignData(final byte[] signData) {
        this.signData = signData.clone();
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
     * Recupera los datos que se firmaron.
     * @return Datos que se firmaron.
     */
    public byte[] getData() {
        return this.data == null ? null : this.data.clone();
    }

    /**
     * Establece los datos que se firmaron.
     * @param data Datos que se firmaron.
     */
    public void setData(final byte[] data) {
    	if (AOUtil.isBase64(data)) {
    		try {
    			final byte[] tmpData = Base64.decode(data, 0, data.length, 0);
    			final String ext = new MimeHelper(tmpData).getExtension();
				if (ext != null && !"".equals(ext)) { //$NON-NLS-1$
					this.data = tmpData;
					return;
				}
			}
    		catch (final IOException e) { /* Se ignora */ }
    	}
        this.data = data.clone();
    }
}
