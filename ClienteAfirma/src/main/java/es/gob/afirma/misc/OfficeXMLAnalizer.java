/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Clase para el an&aacute;lisis de ficheros OOXML.
 */
public final class OfficeXMLAnalizer {

    private static final String ZIP_MIMETYPE = "application/zip";
    
    /** MimeTypes reconocidos del formato OOXML. */
    private static final Set<String> OOXML_MIMETYPES = new HashSet<String>(17);
    
    /** MimeTypes reconocidos del formato ODF. */
    private static final Set<String> ODF_MIMETYPES = new HashSet<String>(15);
    
    static {
        // MimeTypes reconocidos del formato OOXML
        OOXML_MIMETYPES.add("application/vnd.ms-word.document.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.openxmlformats-officedocument.wordprocessingml.document");
        OOXML_MIMETYPES.add("application/vnd.ms-word.template.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.openxmlformats-officedocument.wordprocessingml.template");
        OOXML_MIMETYPES.add("application/vnd.ms-powerpoint.template.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.openxmlformats-officedocument.presentationml.template");
        OOXML_MIMETYPES.add("application/vnd.ms-powerpoint.addin.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.ms-powerpoint.slideshow.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.openxmlformats-officedocument.presentationml.slideshow");
        OOXML_MIMETYPES.add("application/vnd.ms-powerpoint.presentation.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.openxmlformats-officedocument.presentationml.presentation");
        OOXML_MIMETYPES.add("application/vnd.ms-excel.addin.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.ms-excel.sheet.binary.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.ms-excel.sheet.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        OOXML_MIMETYPES.add("application/vnd.ms-excel.template.macroEnabled.12");
        OOXML_MIMETYPES.add("application/vnd.openxmlformats-officedocument.spreadsheetml.template");
    
        // MimeTypes reconocidos del formato ODF
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.text");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.text-template");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.text-web");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.text-master");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.graphics");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.graphics-template");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.presentation");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.presentation-template");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.spreadsheet");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.spreadsheet-template");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.chart");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.formula");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.database");
        ODF_MIMETYPES.add("application/vnd.oasis.opendocument.image");
        ODF_MIMETYPES.add("application/vnd.openofficeorg.extension");
    }
    
    /**
     * Devuelve el MimeType correspondiente al documento ofim&aacute;tico
     * proporcionado (ODF u OOXML). Si el fichero no se corresponde con ninguno
     * de ellos per es un Zip se devolvolvera el MimeType del Zip (application/zip)
     * y si no es Zip se devolver&aacute; <code>null</code>
     * @param zipData Fichero OOXML
     * @return MimeType.
     */
    public static String getMimeType(byte[] zipData) {
        
        ZipFile zipFile;
        try {
            zipFile = AOFileUtils.createTempZipFile(zipData);
        } catch (ZipException e1) {
            Logger.getLogger("es.gob.afirma").warning("El fichero indicado no es un ZIP");
            return null;
        } catch (Throwable e1) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo leer el fichero, se considerara que es un zip");
            return ZIP_MIMETYPE;
        }
        
        String mimetype = ZIP_MIMETYPE;
        try {
            String tempMimetype = null;
            if(isODFFile(zipFile)) {
                tempMimetype = getODFMimeType(zipFile.getInputStream(zipFile.getEntry("mimetype")));
            } else if(isOOXMLFile(zipFile)) {
                tempMimetype = getOOXMLMimeType(zipFile.getInputStream(zipFile.getEntry("[Content_Types].xml")));
            }
            if(tempMimetype != null)
                mimetype = tempMimetype;
        } catch (Throwable e) {}
        
        return mimetype;
        
    }
    
    /**
     * Indica si un fichero tiene la estructura de un documento OOXML.
     * @param document Fichero a analizar
     * @return Devuelve <code>true</code> si el fichero era un OOXML,
     * <code>false</code> en caso contrario.
     */
    public static boolean isOOXMLDocument(byte[] document) {
    	ZipFile zipFile;
        try {
            zipFile = AOFileUtils.createTempZipFile(document);
        } catch (Throwable e1) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo leer el fichero, se considerara que no es un documento OOXML");
            return false;
        }
        return isOOXMLFile(zipFile);
    }
    
    /**
     * Indica si un fichero Zip tiene la estructura de un documento OOXML soportado.
     * @param zipFile Fichero zip que deseamos comprobar.
     * @return Devuelve <code>true</code> si el fichero era un OOXML soportado,
     * <code>false</code> en caso contrario.
     */
    private static boolean isOOXMLFile(ZipFile zipFile) {
        // Comprobamos si estan todos los ficheros principales del documento
        return zipFile.getEntry("[Content_Types].xml") != null &&
                zipFile.getEntry("_rels/.rels") != null &&
                zipFile.getEntry("docProps/app.xml") != null &&
                zipFile.getEntry("docProps/core.xml") != null;
    }
    
    
    /**
     * Recupera el MimeType del XML "[Content_Type].xml" de un OOXML. Si el documento no es
     * correcto o no se reconoce el Mimetype se devuelve null.
     * @param contentTypeIs XML "[Content_Type].xml".
     * @return Devuelve el MimeType del OOXML o, si no es un OOXML reconocido, devuelve {@code null}.
     */
    public static String getOOXMLMimeType(InputStream contentTypeIs) {
        
        Document doc = null;
        try {
            doc = DocumentBuilderFactory.newInstance()
            .newDocumentBuilder().parse(contentTypeIs);
        } catch (Throwable e) {
            return null;
        }
        
        // Obtenemos la raiz
        Element root = doc.getDocumentElement();
        if(!root.getNodeName().equalsIgnoreCase("Types")) {
            return null;
        }
        
        Node node = null;
        NodeList nodes = root.getChildNodes();
        for(int i=0; i<nodes.getLength(); i++) {
            node = nodes.item(i);
            if(node.getNodeName().equalsIgnoreCase("Override")) {
                NamedNodeMap nodeAttributes = node.getAttributes();
                Node nodeAttribute = null;
                for(int j=0; j<nodeAttributes.getLength(); j++) {
                    if(nodeAttributes.item(j).getNodeName().equalsIgnoreCase("ContentType")) {
                        nodeAttribute = nodeAttributes.item(j);
                        break;
                    }
                }
                
                if(nodeAttribute != null) {
                    String value = nodeAttribute.getNodeValue();
                    if(value.indexOf('.') != -1) value = value.substring(0, value.lastIndexOf('.'));
                    if(OOXML_MIMETYPES.contains(value)) return value;
                }
            }
        }
        return null;
    }

    /**
     * Indica si un fichero tiene la estructura de un documento ODF.
     * @param document Fichero a analizar
     * @return Devuelve <code>true</code> si el fichero era un ODF,
     * <code>false</code> en caso contrario.
     */
    public static boolean isODFDocument(byte[] document) {
    	ZipFile zipFile;
        try {
            zipFile = AOFileUtils.createTempZipFile(document);
        } catch (Throwable e1) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo leer el fichero, se considerara que no es un documento ODF");
            return false;
        }
        return isODFFile(zipFile);
    }
    
    /**
     * Indica si un fichero Zip tiene la estructura de un documento ODF soportado.
     * @param zipFile Fichero zip que deseamos comprobar.
     * @return Devuelve <code>true</code> si el fichero era un ODF soportado,
     * <code>false</code> en caso contrario.
     */
    private static boolean isODFFile(ZipFile zipFile) {
        
        // Comprobamos si estan todos los ficheros principales del documento
        return zipFile.getEntry("mimetype") != null &&
                zipFile.getEntry("content.xml") != null &&
                zipFile.getEntry("meta.xml") != null &&
                zipFile.getEntry("settings.xml") != null &&
                zipFile.getEntry("styles.xml") != null &&
                zipFile.getEntry("META-INF/manifest.xml") != null;
    }
    
    /**
     * Recupera la extensi&oacute;n apropiada para un documento ODF. Si el fichero
     * no era un documento ODF soportado, se devolver&aacute; <code>null</code>.
     * @param file Fichero del que deseamos obtener la extensi&oacute;n.
     * @return Extensi&oacute;n del documento.
     */
    private static String getODFMimeType(InputStream contentTypeIs)  {
        String contentTypeData;
        try {
            contentTypeData = new String(AOUtil.getDataFromInputStream(contentTypeIs));
        } catch (Throwable e) {
            return null;
        }
        if (ODF_MIMETYPES.contains(contentTypeData)) return contentTypeData;
        return null;
    }
}
