/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone;

import java.io.ByteArrayInputStream;
import java.util.HashSet;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import es.gob.afirma.signers.pades.AOPDFSigner;

/**
 * M&eacute;todos de utilidad para el an&aacute;lisis de ficheros de datos.
 * @author Carlos Gamuci
 */
public final class DataAnalizerUtil {
    
    private DataAnalizerUtil() {
        // No permitimos la instanciacion
    }

    /**
     * Comprueba si los datos introducidos se corresponden a un fichero XML.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son XML.
     */
    public static boolean isXML(final byte[] data) {
        try {
            DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(data));
        }
        catch(final Exception e) {
            return false;
        }
        return true;
    }

    /**
     * Comprueba si los datos introducidos se corresponden a un fichero PDF.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son PDF.
     */
    public static boolean isPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }
    
    /** Indica si los datos son una factura electr&oacute;nica.
     * @param file Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    public static boolean isFacturae(final byte[] file) {
        if (file == null || file.length == 0) {
            return false;
        }
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        
        try {
            final Document doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(file));
            final Element rootNode = doc.getDocumentElement();
            final String rootNodePrefix = rootNode.getPrefix();
            
            if (!(((rootNodePrefix != null) ? (rootNodePrefix + ":") : "") + "Facturae").equals(rootNode.getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                return false;
            }
            
            final Set<String> childs = new HashSet<String>(3);
            childs.add("FileHeader"); //$NON-NLS-1$
            childs.add("Parties"); //$NON-NLS-1$
            childs.add("Invoices"); //$NON-NLS-1$
            
            final NodeList nl = rootNode.getChildNodes();
            for (int i=0;i<nl.getLength();i++) {
                final String nodeName = nl.item(i).getNodeName();
                if (childs.contains(nodeName)) {
                    childs.remove(nodeName);
                }
            }
            if (childs.size() > 0) {
                return false;
            }
            
        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }
}
