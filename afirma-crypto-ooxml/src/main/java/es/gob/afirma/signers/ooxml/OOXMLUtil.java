/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.AOFileUtils;
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
     * @param ooxmlFile Documento OOXML.
     * @return N&uacute;mero de firma del documento OOXML.
     * @throws ParserConfigurationException Cuando hay problemas con el analizador SAX.
     * @throws IOException Cuando hay incosistencias de formato OOXML en los XML internos del fichero.
     * @throws SAXException Cuando alguno de los XML internos del fichero no est&aacute; bien formado. */
    private static Relationship[] getOOXMLSignaturesRelationships(final byte[] ooxmlFile) throws IOException, SAXException, ParserConfigurationException {

        final List<Relationship> relations = new ArrayList<>();
    	try (
    			final ZipFile zipFile = AOFileUtils.createTempZipFile(ooxmlFile);
		) {

	        // Comprobamos si existe la relacion de firmas del documento
	        final ZipEntry relsEntry = getSignaturesRelsEntry(zipFile);

	        // Si no existe el fichero, el documento no contiene firmas
	        if (relsEntry == null) {
	            return new Relationship[0];
	        }

	        // Analizamos el fichero de relaciones
	        final RelationshipsParser parser = new RelationshipsParser(zipFile.getInputStream(relsEntry));

	        // Contamos las relaciones de firma
	        for (final Relationship rel : parser.getRelationships()) {
	            if (OOXML_SIGNATURE_RELATIONSHIP_TYPE.equals(rel.getType())) {
	                relations.add(rel);
	            }
	        }
    	}

        return relations.toArray(new Relationship[0]);
    }

    /** Recupera las firmas XMLdSig empotradas en el documento OOXML.
     * @param ooxmlFile Documento OOXML.
     * @return Firmas empotradas en el documento.
     * @throws ParserConfigurationException Cuando hay problemas con el analizador SAX.
     * @throws IOException Cuando hay incosistencias de formato OOXML en los XML internos del fichero.
     * @throws SAXException Cuando alguno de los XML internos del fichero no est&aacute; bien formado. */
    static byte[][] getOOXMLSignatures(final byte[] ooxmlFile) throws IOException, SAXException, ParserConfigurationException {
    	final List<byte[]> relations = new ArrayList<>();
        try (
    		final ZipFile zipFile = AOFileUtils.createTempZipFile(ooxmlFile);
        ) {

	        // Comprobamos si existe la relacion de firmas del documento
	        final ZipEntry relsEntry = getSignaturesRelsEntry(zipFile);

	        // Si no existe el fichero, el documento no contiene firmas
	        if (relsEntry == null) {
	            return new byte[0][];
	        }

	        // Analizamos el fichero de relaciones
	        final RelationshipsParser parser = new RelationshipsParser(zipFile.getInputStream(relsEntry));

	        // Contamos las relaciones de firma
	        for (final Relationship rel : parser.getRelationships()) {
	            if (OOXML_SIGNATURE_RELATIONSHIP_TYPE.equals(rel.getType())) {

	                // Comprobamos que exista el firma referenciada
	                final String target = rel.getTarget();
	                ZipEntry signEntry = zipFile.getEntry("_xmlsignatures/" + target); //$NON-NLS-1$
	                if (signEntry == null) {
	                    signEntry = zipFile.getEntry("_xmlsignatures\\" + target); //$NON-NLS-1$
	                }
	                if (signEntry == null) {
	                    LOGGER.severe("El documento OOXML no contiene las firmas declaradas"); //$NON-NLS-1$
	                    zipFile.close();
	                    return new byte[0][];
	                }

	                // Guardamos la firma
	                try {
	                    relations.add(AOUtil.getDataFromInputStream(zipFile.getInputStream(signEntry)));
	                }
	                catch (final Exception e) {
	                    LOGGER.severe("No se pudo leer una de las firmas del documento OOXML: " + e); //$NON-NLS-1$
	                    zipFile.close();
	                    return new byte[0][];
	                }
	            }
	        }
        }

        return relations.toArray(new byte[0][]);
    }

    /** Recupera la entrada con la relaci&oacute;n de firmas del documento.
     * @param ooxmlZipFile Fichero OOXML.
     * @return Entrada con la relaci&oacute;n de firmas. */
    private static ZipEntry getSignaturesRelsEntry(final ZipFile ooxmlZipFile) {
        ZipEntry relsEntry = ooxmlZipFile.getEntry("_rels/.rels"); //$NON-NLS-1$
        if (relsEntry == null) {
            relsEntry = ooxmlZipFile.getEntry("_rels\\.rels"); //$NON-NLS-1$
        }

        // Analizamos el fichero de relaciones
        final RelationshipsParser parser;
        try {
            parser = new RelationshipsParser(ooxmlZipFile.getInputStream(relsEntry));
        }
        catch (final Exception e) {
            LOGGER.severe("Error en la lectura del OOXML: " + e); //$NON-NLS-1$
            return null;
        }

        ZipEntry signsEntry = null;
        for (final Relationship rel : parser.getRelationships()) {
            if (OOXML_SIGNATURE_ORIGIN_RELATIONSHIP_TYPE.equals(rel.getType())) {
                final String middleTarget = rel.getTarget().substring(0, "_xmlsignatures".length() + 1); //$NON-NLS-1$
                final String target = rel.getTarget().substring("_xmlsignatures".length() + 1); //$NON-NLS-1$
                signsEntry = ooxmlZipFile.getEntry(middleTarget + "_rels/" + target + ".rels"); //$NON-NLS-1$ //$NON-NLS-2$
                if (signsEntry == null) {
                    signsEntry = ooxmlZipFile.getEntry(middleTarget + "_rels\\" + target + ".rels"); //$NON-NLS-1$ //$NON-NLS-2$
                }
                break;
            }
        }

        return signsEntry;
    }
}
