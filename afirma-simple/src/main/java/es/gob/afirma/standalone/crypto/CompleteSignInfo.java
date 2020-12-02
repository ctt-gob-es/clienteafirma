/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.crypto;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.List;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.AOTimestampInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;

/** Informaci&oacute;n extra&iacute;da de una firma.
 * @author Carlos Gamuci. */
public final class CompleteSignInfo {

    private byte[] signData = null;
    private byte[] data = null;
    private AOSignInfo signInfo = null;
    private AOTreeModel signsTree = null;
    private List<AOTimestampInfo> timestampsInfo = null;
    private AOTreeModel timestampsTree = null;

    /** Obtiene el &aacute;rbol de sellos de tiempo de la firma.
     * @return &Aacute;rbol de sellos de tiempo de la firma. */
    public AOTreeModel getTimestampsTree() {
    	if (this.timestampsTree == null) {
	        final AOTreeNode root = new AOTreeNode("Datos"); //$NON-NLS-1$
	        if (this.timestampsInfo != null && !this.timestampsInfo.isEmpty()) {
		        for (final AOTimestampInfo tsi : this.timestampsInfo) {
		        	root.add(
		    			new AOTreeNode(
			    			new AOSimpleSignInfo(
								new X509Certificate[] { tsi.getIssuer() },
								tsi.getDate()
							)
		    			)
					);
		        }
	        }
	        this.timestampsTree = new AOTreeModel(root);
    	}
		return this.timestampsTree;
    }

    /** Obtiene la lista de sellos de tiempo de la firma.
     * @return Lista de sellos de tiempo de la firma. */
    public List<AOTimestampInfo> getTimestampsInfo() {
    	return this.timestampsInfo;
    }

    /** Establece la lista de sellos de tiempo de la firma.
     * @param tssInfo Lista de sellos de tiempo de la firma. */
    public void setTimestampsInfo(final List<AOTimestampInfo> tssInfo) {
    	this.timestampsInfo = tssInfo;
    }

    /** Recupera la firma de la que se extraen los datos.
     * @return Firma.
     * @see #setSignData(byte[]) */
    public byte[] getSignData() {
        return this.signData == null ? null : this.signData.clone();
    }

    /** Establece la firma de la que se extraen los datos.
     * @param signData Firma. */
    public void setSignData(final byte[] signData) {
        this.signData = signData.clone();
    }

    /** Recupera la informaci&oacute;n general de la firma.
     * @return Informaci&oacute;n general de la firma. */
    public AOSignInfo getSignInfo() {
        return this.signInfo;
    }

    /** Establece la informaci&oacute;n general de la firma.
     * @param signInfo Informaci&oacute;n general de la firma. */
    public void setSignInfo(final AOSignInfo signInfo) {
        this.signInfo = signInfo;
    }

    /** Recupera el &aacute;rbol de firma con los datos de cada firma individucal.
     * @return &Aacute;rbol de firma. */
    public AOTreeModel getSignsTree() {
        return this.signsTree;
    }

    /** Establece el &aacute;rbol de firma con los datos de cada firma individual.
     * @param signsTree &Aacute;rbol de firma. */
    public void setSignsTree(final AOTreeModel signsTree) {
        this.signsTree = signsTree;
    }

    /** Recupera los datos que se firmaron.
     * @return Datos que se firmaron. */
    public byte[] getData() {
        return this.data == null ? null : this.data.clone();
    }

    /** Establece los datos que se firmaron.
     * @param data Datos que se firmaron. */
    public void setData(final byte[] data) {
    	if (data != null) {
    		if (Base64.isBase64(data)) {
    			try {
    				final byte[] tmpData = Base64.decode(data, 0, data.length, false);
    				final String ext = new MimeHelper(tmpData).getExtension();
    				if (ext != null && !ext.isEmpty()) {
    					this.data = tmpData;
    					return;
    				}
    			}
    			catch (final IOException e) { /* Se ignora */ }
    		}
    		this.data = data.clone();
    	}
    	else {
    		this.data = null;
    	}
    }
}
