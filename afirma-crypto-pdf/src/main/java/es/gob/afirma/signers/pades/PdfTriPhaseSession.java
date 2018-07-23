/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;

import com.aowagie.text.pdf.PdfSignatureAppearance;

/** Datos PDF relevantes en cuanto a las firmas electr&oacute;nicas, consistentes en los datos
 * a ser firmados con CAdES o PKCS#7 y los metadatos necesarios para su correcta inserci&oacute;n en el PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfTriPhaseSession {

    private final PdfSignatureAppearance sap;
    private final ByteArrayOutputStream baos;
    private final String fileID;

    @Override
	public String toString() {
    	return "Sesion trifasica del PDF con identificador: " + this.fileID; //$NON-NLS-1$
    }

    PdfTriPhaseSession(final PdfSignatureAppearance s, final ByteArrayOutputStream b, final String fid) {
        this.sap = s;
        this.baos = b;
        this.fileID = fid;
    }

    /** Obtiene el flujo de datos del propio PDF firmado.
     * @return Flujo de datos del propio PDF firmado */
    public ByteArrayOutputStream getBAOS() {
        return this.baos;
    }

    /** Devuelve el <code>PdfSignatureAppearance</code> de la firma PDF en curso.
     * @return <code>PdfSignatureAppearance</code> de la firma PDF en curso */
    public PdfSignatureAppearance getSAP() {
        return this.sap;
    }

    /** Obtiene el identificador PDF &uacute;nico del documento.
     * @return Identificador PDF &uacute;nico del documento */
    public String getFileID() {
        return this.fileID;
    }

}
