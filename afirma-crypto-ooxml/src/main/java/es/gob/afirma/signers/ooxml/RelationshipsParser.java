/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.ooxml;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;

/** Parser de ficheros XML Relationships. Este tipo de fichero se encuentra
 * comunmente dentro de los ficheros OOXML de Microsoft Office con el nombre
 * ".rels". */
final class RelationshipsParser {

    /** Esquema del XML Relationships. */
    private static final String RELATIONSHIPS_SCHEMA = "http://schemas.openxmlformats.org/package/2006/relationships"; //$NON-NLS-1$

    /** Listado de relaciones obtenido. */
    private Relationship[] relations = null;

    /** Construye un parser de objetos XML RelationsShips.
     * @param xmlRelationships
     *        XML con el objeto RelationShips.
     * @throws AOException
     *         Cuando ocurre un error durante la lectura del XML o si no es
     *         un XML v&aacute;lido. */
    RelationshipsParser(final InputStream xmlRelationships) throws AOException {
        this.relations = getRelationships(xmlRelationships);
    }

    /** Recupera el listado de relaciones extraido del XML.
     * @return Listado de relaciones. */
    Relationship[] getRelationships() {
        return this.relations;
    }

    /** Recupera las relaciones definidas en el XML.
     * @param xmlRelationships
     *        Entrada del XML con las relaciones.
     * @return Listado de relaciones.
     * @throws AOException
     *         Cuando la entrada no se corresponde con un objeto
     *         RelationShips v&aacute;lido. */
    private static Relationship[] getRelationships(final InputStream xmlRelationships) throws AOException {

        final Document doc;
        try {
            doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(xmlRelationships);
        }
        catch (final Exception e) {
            throw new AOException("El flujo de datos proporcionado no era un XML valido", e); //$NON-NLS-1$
        }

        // Obtenemos la raiz
        final Element root = doc.getDocumentElement();

        // Si no se ajusta a la estructura de las Relationships, devolvemos null
        if (!root.getNodeName().equals("Relationships") || root.getAttributeNode("xmlns") == null //$NON-NLS-1$ //$NON-NLS-2$
            || !root.getAttribute("xmlns").equals(RELATIONSHIPS_SCHEMA)) { //$NON-NLS-1$
            throw new AOException("El nodo principal no es una etiqueta Relationships"); //$NON-NLS-1$
        }

        final NodeList relationsList = root.getChildNodes();
        final List<Relationship> relationsVector = new ArrayList<Relationship>();
        for (int i = 0; i < relationsList.getLength(); i++) {
            relationsVector.add(RelationshipsParser.getRelationship(relationsList.item(i)));
        }

        return relationsVector.toArray(new Relationship[0]);
    }

    /** Recupera una relaci&oacute;n individual de un nodo Relationship.
     * Un nodo <code>Relationship</code> tiene estas propiedades:
     * <ul>
     * <li><b>Nombre:</b> Relationship</li>
     * <li><b>Atributos:</b> id, type y target</li>
     * </ul>
     * @param node
     *        Nodo xml Relationship.
     * @return Objeto de relaci&oacute;n.
     * @throws AOException
     *         Cuando el nodo no encaja con el patr&oacute;n Relationship. */
    private static Relationship getRelationship(final Node node) throws AOException {

        // Comprobamos que sea un nodo de relacion
        if (!node.getNodeName().equals("Relationship")) { //$NON-NLS-1$
            throw new AOException("Se ha encontrado un nodo que es de relacion: " + node.getNodeName()); //$NON-NLS-1$
        }

        // Comprobamos que tenga todos sus atributos
        final NamedNodeMap attributes = node.getAttributes();

        if (attributes.getNamedItem("Id") == null || attributes.getNamedItem("Type") == null || attributes.getNamedItem("Target") == null) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            throw new AOException("Se ha encontrado un nodo de relacion que no disponia de todos sus atributos"); //$NON-NLS-1$
        }

        // Creamos la relacion
        return new Relationship(attributes.getNamedItem("Id").getNodeValue(), //$NON-NLS-1$
                                attributes.getNamedItem("Type").getNodeValue(), //$NON-NLS-1$
                                attributes.getNamedItem("Target").getNodeValue()); //$NON-NLS-1$
    }

}
