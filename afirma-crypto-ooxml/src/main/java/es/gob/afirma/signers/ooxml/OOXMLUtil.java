/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.AOUtil;

/** Clase con m&eacute;todos de utilidad para las firmas OOXML. */
final class OOXMLUtil {

    private OOXMLUtil() {
        // No permitimos la instanciacion
    }

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Tipo de relaci&oacute;n correspondiente a una firma OOXML. */
    private static final String OOXML_SIGNATURE_RELATIONSHIP_TYPE =
            "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature"; //$NON-NLS-1$

    /** Tipo de relaci&oacute;n correspondiente a la relaci&oacute;n de firmas OOXML. */
    private static final String OOXML_SIGNATURE_ORIGIN_RELATIONSHIP_TYPE =
            "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin"; //$NON-NLS-1$

    /** Cuenta el n&uacute;mero de firmas del documento OOXML.
     * @param ooxmlFile Documento OOXML.
     * @return N&uacute;mero de firma del documento OOXML.
     * @throws ParserConfigurationException Cuando hay problemas con el analizador SAX.
     * @throws IOException Cuando hay incosistencias de formato OOXML en los XML internos del fichero.
     * @throws SAXException Cuando alguno de los XML internos del fichero no est&aacute; bien formado. */
    static int countOOXMLSignatures(final byte[] ooxmlFile) throws IOException, SAXException, ParserConfigurationException {
        final Relationship[] rels = getOOXMLSignaturesRelationships(ooxmlFile);
        return rels == null ? 0 : rels.length;
    }

    /** Cuenta el n&uacute;mero de firmas del documento OOXML. Si se produce
     * alg&uacute;n error durante el an&aacute;lisis del fichero, se
     * devolver&aacute; 0.
     * @param ooxmlData Documento OOXML.
     * @return N&uacute;mero de firma del documento OOXML.
     * @throws ParserConfigurationException Cuando hay problemas con el analizador SAX.
     * @throws IOException Cuando hay incosistencias de formato OOXML en los XML internos del fichero.
     * @throws SAXException Cuando alguno de los XML internos del fichero no est&aacute; bien formado. */
    private static Relationship[] getOOXMLSignaturesRelationships(final byte[] ooxmlData) throws IOException, SAXException, ParserConfigurationException {

        final List<Relationship> relations = new ArrayList<>();

        // Cargamos la entrada con las relaciones del documento
        try {
       		final List<ZipEntry> entries = getEntryList(ooxmlData);
        	final byte[] signatureRels = getSignaturesRels(ooxmlData, entries);

	        // Si no existe el fichero de relaciones, el documento no contiene firmas
	        if (signatureRels == null) {
	            throw new IOException("No se ha encontrado el listado de relaciones"); //$NON-NLS-1$
	        }

	        // Analizamos el fichero de relaciones
	        final RelationshipsParser parser = new RelationshipsParser(new ByteArrayInputStream(signatureRels));

	        // Contamos las relaciones de firma
	        for (final Relationship rel : parser.getRelationships()) {
	            if (OOXML_SIGNATURE_RELATIONSHIP_TYPE.equals(rel.getType())) {
	                relations.add(rel);
	            }
	        }
    	}
        catch (final Exception e) {
            LOGGER.severe("No se pudieron leer las firmas del documento OOXML: " + e); //$NON-NLS-1$
        }

        return relations.toArray(new Relationship[0]);
    }

    /** Recupera las firmas XMLdSig empotradas en el documento OOXML.
     * @param ooxmlData Documento OOXML.
     * @return Firmas empotradas en el documento.
     * @throws ParserConfigurationException Cuando hay problemas con el analizador SAX.
     * @throws IOException Cuando hay incosistencias de formato OOXML en los XML internos del fichero.
     * @throws SAXException Cuando alguno de los XML internos del fichero no est&aacute; bien formado. */
    static byte[][] getOOXMLSignatures(final byte[] ooxmlData) throws IOException, SAXException, ParserConfigurationException {

    	final List<byte[]> relations = new ArrayList<>();

    	boolean error = false;
    	try {
    		// Comprobamos si existe la relacion de firmas del documento
    		final List<ZipEntry> entries = getEntryList(ooxmlData);
    		final byte[] signatureRels = getSignaturesRels(ooxmlData, entries);
    		if (signatureRels == null) {
    			throw new IOException("No se han contrado relaciones en el documento"); //$NON-NLS-1$
    		}

    		// Analizamos el fichero de relaciones
    		final RelationshipsParser parser = new RelationshipsParser(new ByteArrayInputStream(signatureRels));

    		// Contamos las relaciones de firma
    		for (final Relationship rel : parser.getRelationships()) {
    			if (OOXML_SIGNATURE_RELATIONSHIP_TYPE.equals(rel.getType())) {

    				// Comprobamos que exista el firma referenciada
    				final String target = rel.getTarget();

    				byte[] signEntryData;
    				if (hasEntry(entries, "_xmlsignatures/" + target)) { //$NON-NLS-1$
    					signEntryData = readEntry(ooxmlData, "_xmlsignatures/" + target); //$NON-NLS-1$
    				}
    				else if (hasEntry(entries, "_xmlsignatures\\" + target)) { //$NON-NLS-1$
    					signEntryData = readEntry(ooxmlData, "_xmlsignatures\\" + target); //$NON-NLS-1$
    				}
    				else {
    					LOGGER.severe("El documento OOXML no contiene las firmas declaradas"); //$NON-NLS-1$
    					throw new IOException("El documento OOXML no contiene las firmas declaradas"); //$NON-NLS-1$
    				}

    				// Guardamos la firma
    				relations.add(signEntryData);
    			}
    		}
    	}
        catch (final Exception e) {
            LOGGER.severe("No se pudieron leer las firmas del documento OOXML: " + e); //$NON-NLS-1$
            error = true;
        }

    	if (error) {
    		return new byte[0][];
    	}

        return relations.toArray(new byte[0][]);
    }

    /** Recupera la entrada con la relaci&oacute;n de firmas del documento.
     * @param ooxmlData Datos OOXML.
     * @param entries Listado de entradas del documento (entradas del ZIP).
     * @return Entrada con la relaci&oacute;n de firmas.
     * @throws IOException Cuando ocurre un error durante la lectura. */
    private static byte[] getSignaturesRels(final byte[] ooxmlData, final List<ZipEntry> entries) throws IOException {

   		byte[] relsData;
   		if (hasEntry(entries, "_rels/.rels")) { //$NON-NLS-1$
   			relsData = readEntry(ooxmlData, "_rels/.rels"); //$NON-NLS-1$
   		}
   		else if (hasEntry(entries, "_rels\\.rels")) { //$NON-NLS-1$
   			relsData = readEntry(ooxmlData, "_rels\\.rels"); //$NON-NLS-1$
   		}
   		else {
   			return null;
   		}

        // Analizamos el fichero de relaciones
        final RelationshipsParser parser;
        try (InputStream relsIs = new ByteArrayInputStream(relsData)) {
            parser = new RelationshipsParser(relsIs);
        }
        catch (final Exception e) {
            LOGGER.severe("Error en la lectura del OOXML: " + e); //$NON-NLS-1$
            return null;
        }

        byte[] signsEntry = null;
        for (final Relationship rel : parser.getRelationships()) {
            if (OOXML_SIGNATURE_ORIGIN_RELATIONSHIP_TYPE.equals(rel.getType())) {
                final String middleTarget = rel.getTarget().substring(0, "_xmlsignatures".length() + 1); //$NON-NLS-1$
                final String target = rel.getTarget().substring("_xmlsignatures".length() + 1); //$NON-NLS-1$
                if (hasEntry(entries, middleTarget + "_rels/" + target + ".rels")) { //$NON-NLS-1$ //$NON-NLS-2$
                	signsEntry = readEntry(ooxmlData, middleTarget + "_rels/" + target + ".rels"); //$NON-NLS-1$ //$NON-NLS-2$
                }
                else if (hasEntry(entries, middleTarget + "_rels\\" + target + ".rels")) { //$NON-NLS-1$ //$NON-NLS-2$
                	signsEntry = readEntry(ooxmlData, middleTarget + "_rels\\" + target + ".rels"); //$NON-NLS-1$ //$NON-NLS-2$
                }
                break;
            }
        }

        return signsEntry;
    }


	/**
	 * Recupera el listado de entradas del ZIP en memoria.
	 * @param zipData Archivo comprimido.
	 * @return Listado de entradas del ZIP.
	 * @throws IOException Cuando ocurre un error durante la lectura del fichero.
	 */
	static List<ZipEntry> getEntryList(final byte[] zipData) throws IOException {
		ZipEntry entry = null;
		final List<ZipEntry> entryList = new ArrayList<>();
		try (ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(zipData))) {
			while ((entry = zis.getNextEntry()) != null) {
				entryList.add(entry);
			}
		}
		return entryList;
	}

	/**
	 * Comprueba si entre un listado de entradas de un ZIP se encuentra una en concreto.
	 * @param entryList Listado de entradas.
	 * @param entryName Nombre de la entrada buscada.
	 * @return {@code true} si se encuentra la entrada, {@code false} en caso contrario.
	 */
	static boolean hasEntry(final List<ZipEntry> entryList, final String entryName) {
		int i = 0;
		boolean found = false;
		while (!found && i < entryList.size()) {
			if (entryName.equals(entryList.get(i).getName())) {
				found = true;
			}
			i++;
		}
		return found;
	}

	/**
	 * Comprueba si entre un listado de entradas de un ZIP se encuentra una en concreto.
	 * @param ooxmlData Documento OOXML.
	 * @param entryName Nombre de la entrada buscada.
	 * @return {@code true} si se encuentra la entrada, {@code false} en caso contrario.
	 * @throws IOException Cuando ocurre un error en la lectura.
	 */
	static byte[] readEntry(final byte[] ooxmlData, final String entryName) throws IOException {
		ZipEntry entry;
		byte[] data = null;
		boolean found = false;
		try (ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(ooxmlData))) {
			while (!found && (entry = zis.getNextEntry()) != null) {
				if (entryName.equals(entry.getName())) {
					data = AOUtil.getDataFromInputStream(zis);
					found = true;
				}
			}
		}
		return data;
	}
}
