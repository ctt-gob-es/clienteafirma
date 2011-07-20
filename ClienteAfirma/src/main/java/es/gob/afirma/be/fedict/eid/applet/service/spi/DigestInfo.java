/*
 * eID Applet Project.
 * Copyright (C) 2008-2009 FedICT.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version
 * 3.0 as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, see
 * http://www.gnu.org/licenses/.
 */

package es.gob.afirma.be.fedict.eid.applet.service.spi;

import java.io.Serializable;

/** Digest Information data transfer class.
 * @author Frank Cornelis */
public class DigestInfo implements Serializable {

    private static final long serialVersionUID = 1L;

    /** Main constructor.
     * @param digestValue
     * @param digestAlgo
     * @param description */
    public DigestInfo(final byte[] digestValue, final String digestAlgo, final String description) {
        this.digestValue = digestValue.clone();
        this.digestAlgo = digestAlgo;
        this.description = description;
    }

    private final byte[] digestValue;
    
    /**
     * Obtiene la huella digital.
     * @return Huella digital
     */
    public byte[] getDigestValue() {
        return this.digestValue.clone();
    }

    private final String description;
    
    /**
     * Obtiene la descripci&oacute;n.
     * @return Descripci&oacute;n
     */
    public String getDescription() {
        return this.description;
    }

    private final String digestAlgo;
    
    /**
     * Obtiene el algoritmo de huella digital.
     * @return Algoritmo de huella digital
     */
    public String getDigestAlgo() {
        return digestAlgo;
    }
}
