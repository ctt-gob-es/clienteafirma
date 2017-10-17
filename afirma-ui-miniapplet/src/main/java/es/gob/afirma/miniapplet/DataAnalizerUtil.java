/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;

/** Utilidad para el an&aacute;lisis de ficheros de datos.
 * @author Carlos Gamuci. */
final class DataAnalizerUtil {

    private DataAnalizerUtil() {
        // No permitimos la instanciacion
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero XML.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son XML. */
    public static boolean isXML(final byte[] data) {
    	return AOFileUtils.isXML(data);
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero PDF.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son un PDF. */
    public static boolean isPDF(final byte[] data) {
        try {
        	final AOSigner signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_PADES);
        	if (signer == null) {
        		return false;
        	}
            return signer.isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Indica si los datos son una factura electr&oacute;nica.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    public static boolean isFacturae(final byte[] data) {
        try {
        	final AOSigner signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_FACTURAE);
        	if (signer == null) {
        		return false;
        	}
            return signer.isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un documento ODF.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son ODF. */
    public static boolean isODF(final byte[] data) {
        try {
        	final AOSigner signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_ODF);
        	if (signer == null) {
        		return false;
        	}
            return signer.isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un documento OOXML.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son OOXML. */
    public static boolean isOOXML(final byte[] data) {
        try {
        	final AOSigner signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_OOXML);
        	if (signer == null) {
        		return false;
        	}
            return signer.isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }
}
