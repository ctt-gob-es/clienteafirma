/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers.ooxmlhelper;

import java.io.InputStream;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.exceptions.AOException;

/**
 * Parser de ficheros XML Relationships. Este tipo de fichero se encuentra comunmente dentro
 * de los ficheros OOXML de Microsoft Office con el nombre ".rels".
 */
public class RelationshipsParser {

    /** Esquema del XML Relationships. */
    private static final String RELATIONSHIPS_SCHEMA = 
        "http://schemas.openxmlformats.org/package/2006/relationships";

    /** Listado de relaciones obtenido. */
    private RelationShip[] relations = null; 
    
    /**
     * Construye un parser de objetos XML RelationsShips.
     * @param xmlRelationships XML con el objeto RelationShips.
     * @throws AOException Cuando ocurre un error durante la lectura del XML o si no
     * es un XML v&aacute;lido.
     */
    public RelationshipsParser(InputStream xmlRelationships) throws AOException {
        this.relations = getRelationships(xmlRelationships);
    }
    
    /**
     * Recupera el identificador de la primera relaci&oacute;n que se encuentra del tipo indicado.
     * Si no se encuentra ninguna, se devuelve <code>null</code>.
     * @param type Tipo de relaci&oacute;n.
     * @return Identificador de relaci&oacute;n.
     */
    public String getRelationshipId(String type) {
        for(RelationShip relation : relations) {
            if(relation.getType().equalsIgnoreCase(type))
                return relation.getId();
        }
        return null;
    }

    /**
     * Recupera la relaci&oacute;n con el identificador indicado. Si no se encuentra
     * ninguna, se devuelve <code>null</code>.
     * @param id Identificador de la relaci&oacute;n.
     * @return Relaci&oacute;n.
     */
    public RelationShip getRelation(String id) {
        for(RelationShip relation : relations) {
            if(relation.getId().equalsIgnoreCase(id))
                return relation;
        }
        return null;
    }
    
    /**
     * Recupera el listado de relaciones extraido del XML.
     * @return Listado de relaciones.
     */
    public RelationShip[] getRelationships() {
        return this.relations;
    }
    
    
    /**
     * Recupera las relaciones definidas en el XML.
     * @param xmlRelationships Entrada del XML con las relaciones.
     * @return Listado de relaciones.
     * @throws Exception Cuando la entrada no se corresponde con un objeto RelationShips v&aacute;lido.
     */
    private RelationShip[] getRelationships(InputStream xmlRelationships) throws AOException {

        Document doc = null;
        try {
            doc = DocumentBuilderFactory.newInstance()
            .newDocumentBuilder().parse(xmlRelationships);
        } catch (Throwable e) {
            throw new AOException("El flujo de datos proporcionado no era un XML valido");
        }

        // Obtenemos la raiz
        Element root = doc.getDocumentElement();
       
        // Si no se ajusta a la estructura de las Relationships, devolvemos null
        if(!root.getNodeName().equals("Relationships") ||
                root.getAttributeNode("xmlns") == null ||
                !root.getAttribute("xmlns").equals(RELATIONSHIPS_SCHEMA))
            throw new AOException("El nodo principal no es una etiqueta Relationships");

        NodeList relationsList = root.getChildNodes();
        Vector<RelationShip> relationsVector = new Vector<RelationShip>();
        for(int i=0; i<relationsList.getLength(); i++) {
            relationsVector.add(this.getRelationship(relationsList.item(i)));
        }

        return relationsVector.toArray(new RelationShip[0]);
    }

    /**
     * Recupera una relaci&oacute;n individual de un nodo RelationShip.<br/>
     * Un nodo RelationShip tiene estas propiedades:
     * <ul><li><b>Nombre:</b> RelationShip</li>
     * <li><b>Atributos:</b> id, type y target</li></ul>
     * @param node Nodo xml RelationShip.
     * @return Objeto de relaci&oacute;n.
     * @throws AOException Cuando el nodo no encaja con el patr&oacute;n RelationShip.
     */
    private RelationShip getRelationship(Node node) throws AOException {

        // Comprobamos que sea un nodo de relacion
        if(!node.getNodeName().equals("Relationship"))
            throw new AOException("Se ha encontrado un nodo que es de relacion: "+node.getNodeName());

        // Compriobamos que tenga todos sus tributos
        NamedNodeMap attributes = node.getAttributes();
       
        if(attributes.getNamedItem("Id") == null ||
                attributes.getNamedItem("Type") == null ||
                attributes.getNamedItem("Target") == null)
            throw new AOException("Se ha encontrado un nodo de relacion que no disponia de todos sus atributos");

        // Creamos la relacion
        return new RelationShip(
                attributes.getNamedItem("Id").getNodeValue(),
                attributes.getNamedItem("Type").getNodeValue(),
                attributes.getNamedItem("Target").getNodeValue());
    }
    
//    public static void main(String[] args) {
//        
//        try {
//        
//            InputStream is = AOUtil.loadFile(AOUtil.createURI("C:/.rels"), null, false);
//            
//            RelationshipsParser parser = new RelationshipsParser(is);
//            RelationShip rs = parser.getRelation("rId1");
//            System.out.println("Id: "+rs.getId());
//            System.out.println("Type: "+rs.getType());
//            System.out.println("Target: "+rs.getTarget());
//            System.out.println("-------");
//            
//            System.out.println("Main Id: "+parser.getRelationshipId(RelationShip.DOCUMENT_RELATIONSHIP_TYPE));
//            System.out.println("------");
//            
//            RelationShip[] rss = parser.getRelationships();
//            System.out.println("N rs: "+rss.length);
//            for(RelationShip r : rss) {
//                System.out.println(r);
//            }
//            System.out.println("------");
//            
//            is.close();
//        
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//    }
}
