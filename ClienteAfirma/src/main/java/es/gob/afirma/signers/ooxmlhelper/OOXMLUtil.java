/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.ooxmlhelper;

import java.io.IOException;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import es.gob.afirma.misc.AOFileUtils;
import es.gob.afirma.misc.AOUtil;

/** Clase con m&eacute;todos de utilidad para las firmas OOXML. */
public final class OOXMLUtil {

    /** Tipo de relaci&oacute;n correspondiente a una firma OOXML. */
    private final static String OOXML_SIGNATURE_RELATIONSHIP_TYPE =
            "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature";

    /** Tipo de relaci&oacute;n correspondiente a la relaci&oacute;n de firmas OOXML. */
    private final static String OOXML_SIGNATURE_ORIGIN_RELATIONSHIP_TYPE =
            "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin";

    /** Cuenta el n&uacute;mero de firmas del documento OOXML. Si se produce
     * alg&uacute;n error durante el an&aacute;lisis del fichero, se
     * devolver&aacute; 0.
     * @param ooxmlFile
     *        Documento OOXML.
     * @return N&uacute;mero de firma del documento OOXML. */
    public static int countOOXMLSignatures(final byte[] ooxmlFile) {
        final RelationShip[] rels = getOOXMLSignaturesRelationships(ooxmlFile);
        return (rels == null ? 0 : rels.length);
    }

    /** Cuenta el n&uacute;mero de firmas del documento OOXML. Si se produce
     * alg&uacute;n error durante el an&aacute;lisis del fichero, se
     * devolver&aacute; 0.
     * @param ooxmlFile
     *        Documento OOXML.
     * @return N&uacute;mero de firma del documento OOXML. */
    public static RelationShip[] getOOXMLSignaturesRelationships(final byte[] ooxmlFile) {

        final ZipFile zipFile;
        try {
            zipFile = AOFileUtils.createTempZipFile(ooxmlFile);
        }
        catch (final ZipException e) {
            Logger.getLogger("es.gob.afirma").severe("El documento indicado no es un documento OOXML: " + e);
            return new RelationShip[0];
        }
        catch (final IOException e) {
            Logger.getLogger("es.gob.afirma").severe("Error al abrir el documento OOXML: " + e);
            return new RelationShip[0];
        }

        // Comprobamos si existe la relacion de firmas del documento
        // ZipEntry relsEntry = zipFile.getEntry("_xmlsignatures/_rels/origin.sigs.rels");
        // if (relsEntry == null) relsEntry = zipFile.getEntry("_xmlsignatures\\_rels\\origin.sigs.rels");
        final ZipEntry relsEntry = getSignaturesRelsEntry(zipFile);

        // Si no existe el fichero, el documento no contiene firmas
        if (relsEntry == null) {
            return new RelationShip[0];
        }

        // Analizamos el fichero de relaciones
        final RelationshipsParser parser;
        try {
            parser = new RelationshipsParser(zipFile.getInputStream(relsEntry));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error en la lectura del OOXML: " + e);
            return new RelationShip[0];
        }

        // ya podemos cerrar el documento
        try {
            zipFile.close();
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cerrar el documento OOXML: " + e);
        }

        // Contamos las relaciones de firma
        final Vector<RelationShip> relations = new Vector<RelationShip>();
        for (final RelationShip rel : parser.getRelationships()) {
            if (OOXML_SIGNATURE_RELATIONSHIP_TYPE.equals(rel.getType())) {
                relations.add(rel);
            }
        }

        return relations.toArray(new RelationShip[0]);
    }

    /** Recupera las firmas XMLdSig empotradas en el documento OOXML.
     * @param ooxmlFile
     *        Documento OOXML.
     * @return Firmas empotradas en el documento. */
    public static byte[][] getOOXMLSignatures(final byte[] ooxmlFile) {

        final ZipFile zipFile;
        try {
            zipFile = AOFileUtils.createTempZipFile(ooxmlFile);
        }
        catch (final ZipException e) {
            Logger.getLogger("es.gob.afirma").severe("El documento indicado no es un documento OOXML: " + e);
            return new byte[0][];
        }
        catch (final IOException e) {
            Logger.getLogger("es.gob.afirma").severe("Error al abrir el documento OOXML: " + e);
            return new byte[0][];
        }

        // Comprobamos si existe la relacion de firmas del documento
        // ZipEntry relsEntry = zipFile.getEntry("_xmlsignatures/_rels/origin.sigs.rels");
        // if (relsEntry == null) relsEntry = zipFile.getEntry("_xmlsignatures\\_rels\\origin.sigs.rels");
        final ZipEntry relsEntry = getSignaturesRelsEntry(zipFile);

        // Si no existe el fichero, el documento no contiene firmas
        if (relsEntry == null) {
            return new byte[0][];
        }

        // Analizamos el fichero de relaciones
        final RelationshipsParser parser;
        try {
            parser = new RelationshipsParser(zipFile.getInputStream(relsEntry));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error en la lectura del OOXML: " + e);
            return new byte[0][];
        }

        // Contamos las relaciones de firma
        final Vector<byte[]> relations = new Vector<byte[]>();
        for (final RelationShip rel : parser.getRelationships()) {
            if (OOXML_SIGNATURE_RELATIONSHIP_TYPE.equals(rel.getType())) {

                // Comprobamos que exista el firma referenciada
                final String target = rel.getTarget();
                ZipEntry signEntry = zipFile.getEntry("_xmlsignatures/" + target);
                if (signEntry == null) {
                    signEntry = zipFile.getEntry("_xmlsignatures\\" + target);
                }
                if (signEntry == null) {
                    Logger.getLogger("es.gob.afirma").severe("El documento OOXML no contiene las firmas declaradas");
                    return new byte[0][];
                }

                // Guardamos la firma
                try {
                    relations.add(AOUtil.getDataFromInputStream(zipFile.getInputStream(signEntry)));
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").severe("No se pudo leer una de las firmas del documento OOXML: " + e);
                    return new byte[0][];
                }
            }
        }

        // Ya podemos cerrar el documento
        try {
            zipFile.close();
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cerrar el documento OOXML: " + e);
        }

        return relations.toArray(new byte[0][]);
    }

    /** Recupera la entrada con la relaci&oacute;n de firmas del documento.
     * @param ooxmlZipFile Fichero OOXML.
     * @return Entrada con la relaci&oacute;n de firmas. */
    private static ZipEntry getSignaturesRelsEntry(final ZipFile ooxmlZipFile) {
        ZipEntry relsEntry = ooxmlZipFile.getEntry("_rels/.rels");
        if (relsEntry == null) {
            relsEntry = ooxmlZipFile.getEntry("_rels\\.rels");
        }

        // Analizamos el fichero de relaciones
        final RelationshipsParser parser;
        try {
            parser = new RelationshipsParser(ooxmlZipFile.getInputStream(relsEntry));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error en la lectura del OOXML: " + e);
            return null;
        }

        ZipEntry signsEntry = null;
        for (final RelationShip rel : parser.getRelationships()) {
            //String c = OOXML_SIGNATURE_ORIGIN_RELATIONSHIP_TYPE;
            //String r = rel.getType();

            if (OOXML_SIGNATURE_ORIGIN_RELATIONSHIP_TYPE.equals(rel.getType())) {
                final String middleTarget = rel.getTarget().substring(0, "_xmlsignatures".length() + 1);
                final String target = rel.getTarget().substring("_xmlsignatures".length() + 1);
                signsEntry = ooxmlZipFile.getEntry(middleTarget + "_rels/" + target + ".rels");
                if (signsEntry == null) {
                    signsEntry = ooxmlZipFile.getEntry(middleTarget + "_rels\\" + target + ".rels");
                }
                break;
            }
        }

        return signsEntry;
    }
}
