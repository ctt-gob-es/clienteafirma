/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.xml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URI;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilter2ParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.crypto.dsig.spec.XPathType;
import javax.xml.crypto.dsig.spec.XPathType.Filter;
import javax.xml.crypto.dsig.spec.XSLTTransformParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;

import com.sun.org.apache.xerces.internal.dom.DOMOutputImpl;
import com.sun.org.apache.xml.internal.security.utils.Base64;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.ui.AOUIFactory;

/** Utilidades para las firmas XML. */
@SuppressWarnings("restriction")
public final class Utils {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    private Utils() {
        // No permitimos la instanciacion
    }

    /** Hoja de estilo local (rutal local no dereferenciable) a un XML */
    public static final class IsInnerlException extends Exception {
        private static final long serialVersionUID = -8769490831203570286L;
        
        /** Construye la excepci&oacute;n que indica que una referencia apunta al interior del mismo XML.
         * @param e Excepci&oacute;n anterior en la cadena */
        public IsInnerlException(final Throwable e) {
            super(e);
        }
        
    }

    /** No se puede dereferenciar la hoja de estilo. */
    public static final class CannotDereferenceException extends Exception {

        private static final long serialVersionUID = 5883820163272098664L;

        /** Construye una excepci&oacute;n que indica la imposibilidad de
         * dereferenciar una hoja de estilo.
         * @param s
         *        Mesaje de excepci&oacute;n */
        CannotDereferenceException(final String s) {
            super(s);
        }

        /** Construye una excepci&oacute;n que indica la imposibilidad de
         * dereferenciar una hoja de estilo.
         * @param s
         *        Mesaje de excepci&oacute;n
         * @param e
         *        Excepci&oacute;n anterior en la cadena */
        CannotDereferenceException(final String s, final Exception e) {
            super(s, e);
        }
    }

    /** La referencia de hoja de estilo apunta a un no-XML. */
    public static final class ReferenceIsNotXMLException extends Exception {
        private static final long serialVersionUID = 8076672806350530425L;
        ReferenceIsNotXMLException(final Throwable e) {
            super(e);
        }
    }

    /** Dereferencia una joja de estilo en forma de Documento DOM.
     * @param id
     *        Identificador de la hoja de estilo
     * @param headLess
     *        <code>true</code> si <b>no</b> se desea que se pregunte al
     *        usuario para dereferenciar las hojas de estilo enlazadas con
     *        rutas relativas
     * @return Documento DOM con la hoja de estilo
     * @throws CannotDereferenceException
     *         Si no se puede dereferenciar
     * @throws IsInnerlException
     *         Si no se puede dereferenciar por ser una referencia local
     * @throws ReferenceIsNotXMLException
     *         Si el objeto dereferenciado no puede transformarse en un
     *         Documento DOM */
    public static Document dereferenceStyleSheet(final String id, final boolean headLess) throws CannotDereferenceException,
                                                                                                 IsInnerlException,
                                                                                                 ReferenceIsNotXMLException {
        if (id == null || "".equals(id)) { //$NON-NLS-1$
            throw new CannotDereferenceException("La hoja de estilo era nula o vacia"); //$NON-NLS-1$ 
        }

        byte[] xml = null;

        // Intentamos dereferenciar directamente, cosa que funciona con
        // http://, https:// y file://
        try {
            final URI styleURI = AOUtil.createURI(id);
            if (styleURI.getScheme().equals("file")) { //$NON-NLS-1$
                throw new UnsupportedOperationException("No se aceptan dereferenciaciones directas con file://"); //$NON-NLS-1$
            }
            xml = AOUtil.getDataFromInputStream(AOUtil.loadFile(styleURI));
        }
        catch (final Exception e) {

            // Si no dereferencia puede ser por tres cosas, porque es una
            // referencia interna,
            // porque es una referencia local
            // o porque directamente no se puede dereferenciar

            // Miramos si la referencia es local
            final String[] idParts = id.replace(File.separator, "/").split("/"); //$NON-NLS-1$ //$NON-NLS-2$
            final String fileName = idParts[idParts.length - 1];

            if (fileName.startsWith("#")) { //$NON-NLS-1$
                throw new IsInnerlException(e); 
            }
            else if (id.startsWith("file://")) { //$NON-NLS-1$
                // Preguntamos al usuario para la dereferenciacion
                if (AOUIFactory.showConfirmDialog(null, 
                                                  XMLMessages.getString("Utils.5"), //$NON-NLS-1$
                                                  XMLMessages.getString("Utils.6"), //$NON-NLS-1$
                                                  AOUIFactory.OK_CANCEL_OPTION,
                                                  AOUIFactory.INFORMATION_MESSAGE) == AOUIFactory.OK_OPTION) {
                    
                    final File xmlStyleFile = AOUIFactory.getLoadFile(
                       XMLMessages.getString("Utils.7"), //$NON-NLS-1$
                       fileName,
                       XMLMessages.getString("Utils.8", fileName), //$NON-NLS-1$
                       null
                    );
                    
                    if (xmlStyleFile == null) {
                        throw new CannotDereferenceException("No se ha podido dereferenciar la hoja de estilo", e); //$NON-NLS-1$
                    }
                    try {
                        final InputStream is = new FileInputStream(xmlStyleFile);
                        xml = AOUtil.getDataFromInputStream(is);
                        try {
                            is.close();
                        }
                        catch (final Exception ex) {
                            // Ignoramos los errores en el cierre
                        }
                    }
                    catch (final Exception ex) {
                        throw new CannotDereferenceException("No se ha podido dereferenciar la hoja de estilo", ex); //$NON-NLS-1$
                    }
                }
            }
            else {
                throw new CannotDereferenceException("No se ha podido dereferenciar la hoja de estilo: " + id, e); //$NON-NLS-1$
            }
        }

        try {
            if (xml != null) {
                return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(xml));
            }
            throw new CannotDereferenceException("No se ha dereferenciado la hoja de estilo"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new ReferenceIsNotXMLException(e);
        }
    }

    /** A&ntilde;ade la cabecera de hoja de estilo a un XML dado.
     * @param xml
     *        XML origen
     * @param tpy
     *        Tipo de hoja de estilo
     * @param href
     *        Reeferncia a la hoja de estilo
     * @return XML con la cabecera de declaraci&oacute;n de hoja de estilo
     *         a&ntilde;adida */
    private static String addStyleSheetHeader(final String xml, String tpy, final String href) {
        if (href == null) {
            return xml;
        }
                
        final String type = (tpy != null) ? tpy : "text/xsl"; //$NON-NLS-1$

        if (xml == null || "".equals(xml)) { //$NON-NLS-1$
            return "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><?xml-stylesheet type=\"" + //$NON-NLS-1$
                   type
                   + "\" href=\"" + //$NON-NLS-1$
                   href
                   + "\"?>"; //$NON-NLS-1$
        }
        return xml.replaceFirst(">", //$NON-NLS-1$
                                ">\r\n<?xml-stylesheet type=\"" + //$NON-NLS-1$
                                        type
                                        + "\" href=\"" + //$NON-NLS-1$
                                        href
                                        + "\"?>" //$NON-NLS-1$
        );
    }

    /** Obtiene los par&aacute;metros de la cabecera de definici&oacute;n de la
     * hoja de estilo de un XML.
     * @param inputXML
     *        XML de entrada
     * @return Properties con los par&aacute;metros encontrados en la cabecera,
     *         o un Properties vac&iacute;o si el XML no declaraba una hoja de
     *         estilo */
    public static Properties getStyleSheetHeader(final String inputXML) {
        final Properties ret = new Properties();
        if (inputXML == null) {
            return ret;
        }
        final int startPos = inputXML.indexOf("<?xml-stylesheet "); //$NON-NLS-1$
        if (startPos == -1) {
            return ret;
        }
        
        String xml = inputXML.substring(startPos);
        xml = xml.substring(0, xml.indexOf('>') + 1)
                   .replace("<?xml-stylesheet ", "").replace("?>", "").replace(" ", "\n").replace("\"", "").replace("'", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$
        try {
            ret.load(new ByteArrayInputStream(xml.getBytes()));
        }
        catch (final Exception e) {
            LOGGER.severe(
              "No se ha podido analizar correctamente la cabecera de definicion de la hora de estilo del XML: " + e //$NON-NLS-1$
            );
        }
        return ret;
    }

    /** A&ntilde;ade transformaciones seg&uacute; la sintaxis de
     * par&aacute;metros adicionales en fichero de propiedades del Cliente @firma
     * a una lista pre-existente.
     * @param transforms
     *        Lista a la que a&ntilde;adir las transformaciones
     * @param xParams
     *        Informaci&oacute;n sobre las transformaciones a a&ntilde;adir
     * @param xmlSignaturePrefix
     *        Prefijo XMLDSig */
    public static void addCustomTransforms(final List<Transform> transforms, final Properties xParams, final String xmlSignaturePrefix) {

        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$

        final List<Transform> transformList = (transforms != null) ? transforms : new ArrayList<Transform>();
        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        // primero compruebo si hay transformaciones a medida
        final int numTransforms = Integer.parseInt(extraParams.getProperty("xmlTransforms", "0")); //$NON-NLS-1$ //$NON-NLS-2$
        String transformType;
        String transformBody;
        String transformSubtype;
        XPathType.Filter xPath2TransformFilter;
        TransformParameterSpec transformParam;

        for (int i = 0; i < numTransforms; i++) {
            transformType = extraParams.getProperty("xmlTransform" + Integer.toString(i) + "Type"); //$NON-NLS-1$ //$NON-NLS-2$
            transformBody = extraParams.getProperty("xmlTransform" + Integer.toString(i) + "Body"); //$NON-NLS-1$ //$NON-NLS-2$

            if (Transform.XPATH.equals(transformType) && transformBody != null) {
                try {
                    transformParam = new XPathFilterParameterSpec(transformBody, Collections.singletonMap(xmlSignaturePrefix, XMLSignature.XMLNS));
                }
                catch (final Exception e) {
                    LOGGER.warning(
                       "No se han podido crear los parametros para una transformacion XPATH, se omitira: " + e //$NON-NLS-1$
                    );
                    continue;
                }
            }
            else if (Transform.XPATH2.equals(transformType) && transformBody != null) {
                transformSubtype = extraParams.getProperty("xmlTransform" + Integer.toString(i) + "Subtype"); //$NON-NLS-1$ //$NON-NLS-2$
                if ("subtract".equals(transformSubtype)) { //$NON-NLS-1$
                    xPath2TransformFilter = Filter.SUBTRACT; 
                }
                else if ("intersect".equals(transformSubtype)) { //$NON-NLS-1$
                    xPath2TransformFilter = Filter.INTERSECT;
                }
                else if ("union".equals(transformSubtype)) { //$NON-NLS-1$
                    xPath2TransformFilter = Filter.UNION;
                }
                else {
                    LOGGER.warning(
                       "Se ha solicitado aplicar una transformacion XPATH2 de un tipo no soportado: " + transformSubtype //$NON-NLS-1$
                    );
                    continue;
                }
                try {
                    transformParam = new XPathFilter2ParameterSpec(Collections.singletonList(new XPathType(transformBody, xPath2TransformFilter)));
                }
                catch (final Exception e) {
                    LOGGER.warning(
                       "No se han podido crear los parametros para una transformacion XPATH2, se omitira: " + e //$NON-NLS-1$
                    );
                    continue;
                }
            }
            else if (Transform.XSLT.equals(transformType) && transformBody != null) {
                try {
                    transformParam =
                            new XSLTTransformParameterSpec(new DOMStructure(DocumentBuilderFactory.newInstance()
                                                                                                  .newDocumentBuilder()
                                                                                                  .parse(new ByteArrayInputStream(transformBody.getBytes()))
                                                                                                  .getDocumentElement()));
                }
                catch (final Exception e) {
                    LOGGER.warning(
                       "No se han podido crear los parametros para una transformacion XSLT, se omitira: " + e //$NON-NLS-1$
                    );
                    continue;
                }
            }
            else if (Transform.BASE64.equals(transformType)) {
                // La transformacion Base64 no necesita parametros
                transformParam = null;
            }
            else if (Transform.ENVELOPED.equals(transformType)) {
                // La transformacion Enveloped no necesita parametros
                transformParam = null;
            }
            else {
                LOGGER.warning("Tipo de transformacion no soportada: " + transformType); //$NON-NLS-1$
                continue;
            }

            // Llegados a este punto tenemos ya la transformacion, asi que la
            // anadimos
            try {
                transformList.add(fac.newTransform(transformType, transformParam));
            }
            catch (final Exception e) {
                LOGGER.warning("No se ha podido aplicar la transformacion '" + transformType + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }

        }
    }

    /** Obtiene de un nodo de referencia de tipo Object la lista de
     * transformaciones definidas. Si no tiene transfgormaciones definidas,
     * devuelve {@code null}.
     * @param referenceNode
     *        Nodo de tipo referencia.
     * @param namespacePrefix
     *        Prefijo del namespace de la firma (opcional).
     * @return Listado de transformaciones.
     * @throws InvalidAlgorithmParameterException
     *         Cuando se encuentre un par&aacute;metro inv&aacute;lido para
     *         el algoritmo de transformaci&oacute;n.
     * @throws NoSuchAlgorithmException
     *         Cuando se encuentre un algoritmo de transformaci&oacurte;n no
     *         soportado. */
    public static List<Transform> getObjectReferenceTransforms(final Node referenceNode, final String namespacePrefix) throws NoSuchAlgorithmException,
                                                                                                                        InvalidAlgorithmParameterException {

        final ArrayList<Transform> transformList = new ArrayList<Transform>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$

        // El nodo de referencia puede contener un nodo "Transforms" que a su
        // vez contiene
        // las distintas transformaciones
        final NodeList tmpNodeList = referenceNode.getChildNodes();

        for (int i = 0; i < tmpNodeList.getLength(); i++) {

            // Buscamos el nodo Transforms a secas o precedido por el namespace
            if ("Transforms".equals(tmpNodeList.item(i).getNodeName()) || (namespacePrefix + ":Transforms").equals(tmpNodeList.item(i).getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$

                final NodeList transformsNodeList = tmpNodeList.item(i).getChildNodes();

                for (int j = 0; j < transformsNodeList.getLength(); j++) {

                    transformList.add(fac.newTransform(getTransformAlgorithm(transformsNodeList.item(j)),
                                                       getTransformParameterSpec(transformsNodeList.item(j), namespacePrefix)));
                }
                break; // Si ya hemos encontrado el nodo "Transforms" dejamos de
                       // buscar
            }
        }

        return transformList;
    }

    /** Recupera el identificador del algoritmo de un nodo de
     * transformaci&oacute;n. Si no existe el atributo de algoritmo, se devuelve {@code null}.
     * @param transformNode
     *        Nodo de transformaci&oacute;n.
     * @return Algoritmo de transformaci&oacute;n. */
    private static String getTransformAlgorithm(final Node transformNode) {
        if (transformNode == null) {
            return null;
        }
        final Node algorithmNode = transformNode.getAttributes().getNamedItem("Algorithm"); //$NON-NLS-1$
        if (algorithmNode != null) {
            return algorithmNode.getNodeValue();
        }
        return null;
    }

    /** Recupera los par&aacute;metros de una transformaci&opacute;n. En el caso
     * de las transformaciones XPATH y XPATH2, se devolveran los
     * par&aacute;metros especificados y, en las transformacion Base64,
     * Enveloped y de Canonicalizaci&oacute;n (que no reciben par&aacute;metros)
     * se devolver&aacute; {@code null}, al igual que cuando no se reconozca el
     * tipo de transformaci&oacute;n.
     * @param transformNode
     *        Nodo de transformaci&oacute;n.
     * @return Par&aacute;metros de la transformaci&oacute;n.
     * @throws InvalidAlgorithmParameterException
     *         Cuando no se especifiquen correctamente los
     *         par&aacute;mnetros de las transformaciones XPATH y XPATH2. */
    private static TransformParameterSpec getTransformParameterSpec(final Node transformNode, final String namespacePrefix) throws InvalidAlgorithmParameterException {

        TransformParameterSpec params = null;
        final String algorithm = getTransformAlgorithm(transformNode);

        // Comprobamos que la transformacion sea de tipo XPATH o XPATH2, unicos
        // casos en los que
        // la transformacion recibe parametros
        if (algorithm != null && (Transform.XPATH.equals(algorithm) || Transform.XPATH2.equals(algorithm))) {

            // Si es una transformacion XPATH solo tenemos que recoger el cuerpo
            if (Transform.XPATH.equals(algorithm)) {

                final NodeList xpathTransforms = transformNode.getChildNodes();
                for (int i = 0; i < xpathTransforms.getLength(); i++) {
                    final Node xpathTransformNode = xpathTransforms.item(i);

                    // Probamos a encontrar un nodo XPath sin namespace y con el
                    // namespace indicado
                    if ("XPath".equals(xpathTransformNode.getNodeName()) || (namespacePrefix + ":XPath").equals(xpathTransformNode.getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$

                        if (namespacePrefix == null || "".equals(namespacePrefix)) { //$NON-NLS-1$
                            params = new XPathFilterParameterSpec(xpathTransformNode.getTextContent());
                        }
                        else {
                            params =
                                    new XPathFilterParameterSpec(xpathTransformNode.getTextContent(), Collections.singletonMap(namespacePrefix,
                                                                                                                               XMLSignature.XMLNS));
                        }

                        break;
                    }
                }

                if (params == null) {
                    throw new InvalidAlgorithmParameterException("No se ha indicado un cuerpo para una transformacion XPATH declarada"); //$NON-NLS-1$
                }

            }
            // Si la transformacion es XPATH2 debemos tomar el cuerpo y el
            // subtipo
            else if (Transform.XPATH2.equals(algorithm)) {

                final NodeList xpathTransforms = transformNode.getChildNodes();
                for (int i = 0; i < xpathTransforms.getLength(); i++) {
                    final Node xpathTransformNode = xpathTransforms.item(i);
                    if ("XPath".equals(xpathTransformNode.getNodeName()) || (namespacePrefix + ":XPath").equals(xpathTransformNode.getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$

                        Filter filter;
                        final Node filterNode = xpathTransformNode.getAttributes().getNamedItem("Filter"); //$NON-NLS-1$
                        if (filterNode != null) {
                            final String filterName = filterNode.getNodeValue();
                            if (filterName.equals("subtract")) { //$NON-NLS-1$
                                filter = Filter.SUBTRACT;
                            }
                            else if (filterName.equals("intersect")) { //$NON-NLS-1$
                                filter = Filter.INTERSECT;
                            }
                            else if (filterName.equals("union")) { //$NON-NLS-1$
                                filter = Filter.UNION;
                            }
                            else {
                                throw new InvalidAlgorithmParameterException("El subtipo '" + filterName + "' de la transformacion XPATH2 no es valido"); //$NON-NLS-1$ //$NON-NLS-2$
                            }
                        }
                        else {
                            throw new InvalidAlgorithmParameterException("No se ha declarado un subtipo para la transformacion XPATH2"); //$NON-NLS-1$
                        }

                        params = new XPathFilter2ParameterSpec(Collections.singletonList(new XPathType(xpathTransformNode.getTextContent(), filter)));
                        break;
                    }
                }

                if (params == null) {
                    throw new InvalidAlgorithmParameterException("No se ha indicado un cuerpo para una transformacion XPATH2 declarada"); //$NON-NLS-1$
                }
            }

        }

        return params;
    }

    /** Comprueba si hay alguna incorrecci&oacute;n en los par&aacute;metros
     * principales de firma.
     * @param format
     *        Formato de firma
     * @param mode
     *        Modo de firma
     * @param uri
     *        URI del objeto a firmar
     * @param externallyDetachedHashAlgorithm
     *        Algoritmo de huella digital en el caso de estar esta
     *        pre-calculada
     * @param xades
     *        <code>true</code> si la firma es XAdES, <code>false</code> si
     *        es XMLDSig */
    public static void checkIllegalParams(final String format,
                                          final String mode,
                                          final URI uri,
                                          final String externallyDetachedHashAlgorithm,
                                          final boolean xades) {
        if (!mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT) && !mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
            throw new UnsupportedOperationException("El modo de firma '" + mode + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        if (xades) { // XAdES
            if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
                throw new UnsupportedOperationException("No se puede realizar una firma XML Enveloped con contenido Explicito" //$NON-NLS-1$
                );
            }
            if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED) && mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
                throw new UnsupportedOperationException("No se puede realizar una firma XML Externally Detached con contenido Implicito" //$NON-NLS-1$
                );
            }
            if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED) && uri == null && externallyDetachedHashAlgorithm == null) {
                throw new UnsupportedOperationException("La firma XML Externally Detached necesita un Message Digest precalculado o una URI accesible" //$NON-NLS-1$
                );
            }
            if (!format.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED) && !format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)
                && !format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
                && !format.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
                throw new UnsupportedOperationException("El formato de firma '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        else { // XMLDSig
               // Combinaciones prohibidas
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
                throw new UnsupportedOperationException("No se puede realizar una firma enveloped sobre un contenido explícito"); //$NON-NLS-1$
            }
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED) && mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
                throw new UnsupportedOperationException("No se puede realizar una firma XML Externally Detached con contenido Implicito" //$NON-NLS-1$
                );
            }
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED) && uri == null && externallyDetachedHashAlgorithm == null) {
                throw new UnsupportedOperationException("La firma XML Externally Detached necesita un Message Digest precalculado o una URI accesible" //$NON-NLS-1$
                );
            }

            // Formatos y modos soportados
            if (!format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED) && !format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
                && !format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
                && !format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)) {
                throw new UnsupportedOperationException("El formato de firma '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
            }

        }

    }

    /** Intenta determinar la URL de declaraci&oacute;n de espacio de nombres de
     * XAdES de una firma XAdES.
     * @param el
     *        Firma XAdES
     * @return URL de la declaraci&oacute;n del espacio de nombres */
    public static String guessXAdESNamespaceURL(final Node el) {

        final String latest = "\"http://uri.etsi.org/01903#\""; //$NON-NLS-1$
        final String xades122 = "\"http://uri.etsi.org/01903/v1.2.2#\""; //$NON-NLS-1$
        final String xades132 = "\"http://uri.etsi.org/01903/v1.3.2#\""; //$NON-NLS-1$
        final String xades141 = "\"http://uri.etsi.org/01903/v1.4.1#\""; //$NON-NLS-1$

        final String signatureText = new String(writeXML(el, null, null, null));

        final int numLatest = countSubstring(signatureText, latest);
        final int numXades122 = countSubstring(signatureText, xades122);
        final int numXades132 = countSubstring(signatureText, xades132);
        final int numXades141 = countSubstring(signatureText, xades141);
        
        // Prioridad: xades132 > latest > xades141 > xades122
        if (numXades132 >= numLatest && numXades132 >= numXades141 && numXades132 >= numXades122) {
            return xades132.replace("\"", ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (numLatest >= numXades132 && numLatest >= numXades141 && numLatest >= numXades122) {
            return latest.replace("\"", ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (numXades141 >= numLatest && numXades141 >= numXades132 && numXades141 >= numXades122) {
            return xades141.replace("\"", ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (numXades122 >= numXades132 && numXades122 >= numLatest && numXades122 >= numXades141) {
            return xades122.replace("\"", ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        
        return xades132.replace("\"", ""); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Intenta determinar la URL de declaraci&oacute;n de espacio de nombres de
     * XMLDSig de un XML.
     * @param el
     *        Firma XMLDSig
     * @return URL de la declaraci&oacute;n del espacio de nombres */
    public static String guessXMLDSigNamespaceURL(final Element el) {

        final String signatureText = new String(writeXML(el, null, null, null));

        // Por defecto "http://www.w3.org/2000/09/xmldsig#"

        final int numXmlDsig = countSubstring(signatureText, "http://www.w3.org/2000/09/xmldsig#"); //$NON-NLS-1$
        final int numXmlDsig11 = countSubstring(signatureText, "http://www.w3.org/2009/xmldsig11#"); //$NON-NLS-1$

        return numXmlDsig >= numXmlDsig11 ? "http://www.w3.org/2000/09/xmldsig#" : "http://www.w3.org/2009/xmldsig11#"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Intenta determinar el prefijo del espacio de nombres de la firma.
     * @param el
     *        Firma XMLDSig
     * @return Prefijo del espacio de nombres */
    public static String guessXMLDSigNamespacePrefix(final Element el) {

        final String signatureText = new String(writeXML(el, null, null, null));

        final int numEmpty = countSubstring(signatureText, "<Signature"); //$NON-NLS-1$
        final int numDs = countSubstring(signatureText, "<ds:Signature"); //$NON-NLS-1$
        final int numDsig = countSubstring(signatureText, "<dsig:Signature"); //$NON-NLS-1$
        final int numDsig11 = countSubstring(signatureText, "<dsig11:Signature"); //$NON-NLS-1$

        // Prioridad: ds > "" > dsig > dsig11
        if (numDs >= numEmpty && numDs >= numDsig && numDs >= numDsig11) {
            return "ds"; //$NON-NLS-1$
        }
        if (numEmpty >= numDs && numEmpty >= numDsig && numEmpty >= numDsig11) {
            return ""; //$NON-NLS-1$
        }
        if (numDsig >= numEmpty && numDsig >= numDs && numDsig >= numDsig11) {
            return "dsig"; //$NON-NLS-1$
        }
        if (numDsig11 >= numEmpty && numDsig11 >= numDsig && numDsig11 >= numDs) {
            return "dsig11"; //$NON-NLS-1$
        }
        return "ds"; //$NON-NLS-1$
    }

    /** Cuenta las repeticiones de una subcadena dentro de una cadena. Las
     * subcadenas no pueden estar acopladas.
     * @param text
     *        Texto en el que realizar la b&uacute;squeda.
     * @param substring
     *        Subcadena que deseamos buscar.
     * @return N&uacute;mero de coincidencias. */
    private static int countSubstring(final String text, final String substring) {
        int count = 0;
        for (int i = 0; i <= (text.length() - substring.length()); i++) {
            if ((substring.charAt(0) == text.charAt(i)) && (substring.equals(text.substring(i, i + substring.length())))) {
                count++;
                i += substring.length() - 1;
            }
        }
        return count;
    }

    /** En una lista de referencias, se eliminan las transformaciones Base64 de
     * aquellas que tengan el identificador nulo.
     * @param referenceList
     *        Lista de referencias original (no se modifica)
     * @return Nueva lista de referencias */
    public static List<Reference> cleanReferencesList(final List<Reference> referenceList) {

        final List<Reference> newList = new ArrayList<Reference>();
        if (referenceList == null) {
            return newList;
        }
        List<Transform> trans;
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
        boolean needsReconReference;

        for (final Reference r : referenceList) {
            if (r.getId() == null) {
                // Por cada referencia guardamos sus transformaciones que no son
                // Base64 por si hay que
                // reconstruirla
                trans = null;
                needsReconReference = false;
                for (final Object t : r.getTransforms()) {
                    if (t instanceof Transform) {
                        if (!"http://www.w3.org/2000/09/xmldsig#base64".equals(((Transform) t).getAlgorithm())) { //$NON-NLS-1$
                            // Si el ID es nulo y hay una transformacion Base64
                            // reconstruimos la referencia
                            // pero quitando esa transformacion Base64
                            if (trans == null) {
                                trans = new ArrayList<Transform>();
                            }
                            trans.add((Transform) t);
                        }
                        else {
                            needsReconReference = true;
                        }
                    }
                }
                // Ya tenemos las referencias, si se necesita reconstruir
                // la reconstruimos y la anadimos
                if (needsReconReference) {
                    newList.add(fac.newReference(r.getURI(), r.getDigestMethod(), trans, r.getType(), r.getId()));
                }
                // Si no, la referencia es buena y la podemos anadir
                // directamente
                else {
                    newList.add(r);
                }
            }
            else {
                newList.add(r);
            }
        }
        return newList;
    }

    /** Escribe un XML como texto.
     * @param node
     *        Nodo XML que queremos pasar a texto
     * @param props
     *        Propiedades del XML (<i>version</i>, <i>encoding</i>,
     *        <i>standalone</i>)
     * @param styleHref
     *        Referencia (enlace) a la hoja de estilo del XML (puede ser
     *        nulo)
     * @param styleType
     *        Tipo de la hoja de estilo del XML (puede ser nulo)
     * @return Cadena de texto con el XML en forma de array de octetos */
    public static byte[] writeXML(final Node node, final Map<String, String> props, final String styleHref, final String styleType) {

        final Map<String, String> xmlProps = (props != null) ? props : new Hashtable<String, String>(0);

        // La codificacion por defecto sera UTF-8
        final String xmlEncoding = xmlProps.containsKey(OutputKeys.ENCODING) ? xmlProps.get(OutputKeys.ENCODING) : "UTF-8"; //$NON-NLS-1$

        // Primero creamos un writer
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Writer writer = null;
        try {
            writer = new OutputStreamWriter(baos, xmlEncoding);
        }
        catch (final UnsupportedEncodingException e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            writer = new OutputStreamWriter(baos);
        }

        // Ahora escribimos el XML usando XALAN, para el control de los
        // namespaces
        writeXMLwithXALAN(writer, node, xmlEncoding);

        // Volvemos a cargar el XML en un DOM. Esto se hace porque las
        // bibliotecas de XALAN hacen
        // aparecer en ciertas ocasiones unos saltos de pagina unicode en el
        // base 64 de los
        // datos contenidos en la firma. Por motivos estilisticos, volvemos a
        // cargar el XML y a
        // guardarlo mediante DOM para que no aparezcan.
        final Document docum;
        try {
            docum = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(baos.toByteArray()));
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido recargar el XML para insertar los atributos de la cabecera, quizas la codificacion se vea afectada: " + e); //$NON-NLS-1$
            return baos.toByteArray();
        }

        // Escribimos por segunda vez el XML, pero ahora con el JRE
        baos = new ByteArrayOutputStream();
        try {
            writer = new OutputStreamWriter(baos, xmlEncoding);
        }
        catch (final Exception e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            writer = new OutputStreamWriter(baos);
        }

        writeXMLwithJRE(writer, docum.getDocumentElement(), false, xmlProps);

        // Y devolvemos el resultado como array de bytes, insertando antes la
        // cabecera de hoja de estilo
        try {
            return Utils.addStyleSheetHeader(new String(baos.toByteArray(), xmlEncoding), styleType, styleHref).getBytes(xmlEncoding);
        }
        catch (final Exception e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto del sistema: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return Utils.addStyleSheetHeader(new String(baos.toByteArray()), styleType, styleHref).getBytes();
        }
    }

    private static void writeXMLwithXALAN(final Writer writer, final Node node, final String xmlEncoding) {
        final LSSerializer serializer = ((DOMImplementationLS) node.getOwnerDocument().getImplementation()).createLSSerializer();
        serializer.getDomConfig().setParameter("namespaces", Boolean.FALSE); //$NON-NLS-1$
        final DOMOutputImpl output = new DOMOutputImpl();
        output.setCharacterStream(writer);
        if (xmlEncoding != null) {
            output.setEncoding(xmlEncoding);
        }
        serializer.write(node, output);
    }

    private static void writeXMLwithJRE(final Writer writer, 
                                              final Node node, 
                                              final boolean indent, 
                                              final Map<String, String> props) {
        try {
            final DOMSource domSource = new DOMSource(node);
            final StreamResult streamResult = new StreamResult(writer);
            final TransformerFactory tf = TransformerFactory.newInstance();
            final Transformer serializer = tf.newTransformer();

            final Map<String, String> properties = (props != null) ? props : new Hashtable<String, String>();

            // Por defecto, si no hay eclarada una codificacion, se utiliza
            // UTF-8
            if (!properties.containsKey(OutputKeys.ENCODING) || "".equals(properties.get(OutputKeys.ENCODING))) { //$NON-NLS-1$
                properties.put(OutputKeys.ENCODING, "UTF-8"); //$NON-NLS-1$
            }
            for (final String key : properties.keySet()) {
                serializer.setOutputProperty(key, properties.get(key));
            }
            if (indent) {
                serializer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
            }
            serializer.transform(domSource, streamResult);
        }
        catch (final Exception e) {
            LOGGER.severe("Error escribiendo el XML: " + e); //$NON-NLS-1$
        }
    }

    /** Genera un objeto descriptor de firma.
     * @param namespace
     *        Espacio de nombres utilizado para la recuperaci&oacute;n de
     *        atributos XAdES.
     * @param signature
     *        Nodo de firma.
     * @return Objeto descriptor de firma. */
    public static AOSimpleSignInfo getSimpleSignInfoNode(final String namespace, final Element signature) {

        // Recupera la fecha de firma
        Date signingTime = null;
        if (namespace != null) {
            try {
                signingTime =
                        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").parse(((Element) signature.getElementsByTagNameNS(namespace, "SigningTime") //$NON-NLS-1$ //$NON-NLS-2$
                                                                                                .item(0)).getTextContent());
            }
            catch (final Exception e) {
                LOGGER.warning("No se ha podido recuperar la fecha de firma: " + e); //$NON-NLS-1$
            }
        }

        final AOSimpleSignInfo ssi = new AOSimpleSignInfo(new X509Certificate[] {
            Utils.getCertificate(signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Certificate").item(0)) //$NON-NLS-1$
        }, signingTime);
        ssi.setSignAlgorithm(((Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, "SignatureMethod").item(0)).getAttribute("Algorithm")); //$NON-NLS-1$ //$NON-NLS-2$

        byte[] pkcs1;
        try {
            pkcs1 = Base64.decode(((Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, "SignatureValue").item(0)).getTextContent()); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning("No se pudo extraer el PKCS#1 de una firma"); //$NON-NLS-1$
            pkcs1 = null;
        }
        ssi.setPkcs1(pkcs1);

        return ssi;
    }

    /** Obtiene el CN del certificado de una firma.
     * @param signature
     *        Nodo de firma.
     * @return CN del certificado de firma. */
    public static String getStringInfoNode(final Element signature) {
        return AOUtil.getCN(Utils.getCertificate(signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Certificate").item(0))); //$NON-NLS-1$
    }

    /** Genera un certificado X.509 a partir de un nodo de certificado de firma.
     * @param certificateNode
     *        Nodo "X509Certificate" de la firma.
     * @return Certificado de firma. */
    public static X509Certificate getCertificate(final Node certificateNode) {
        return createCert(certificateNode.getTextContent().trim().replace("\r", "").replace("\n", "").replace(" ", "").replace("\t", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    }

    /** Recupera el identificador (id) de la firma sobre la que se ha realizado
     * una contrafirma. Si no se encuentra la firma a la que se referencia se
     * devuelve cadena vac&iacute;a.
     * @param signature
     *        Nodo de la contrafirma.
     * @param signatureValues
     *        Listado con todos los SignatureValue del documento de firma.
     * @return Identificador de la firma (Signature) referenciada. */
    public static String getCounterSignerReferenceId(final Element signature, final NodeList signatureValues) {
        // Tomamos la URI de la primera referencia (la del objeto firmado),
        // evitando el primer caracter de la URI que sera la almohadilla (#)
        final String uri = ((Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Reference").item(0)).getAttribute("URI").substring(1); //$NON-NLS-1$ //$NON-NLS-2$
        String signatureId = ""; //$NON-NLS-1$
        for (int j = 0; j < signatureValues.getLength(); j++) {
            final Element signatureValue = (Element) signatureValues.item(j);
            if (signatureValue.getAttribute("Id").equals(uri)) { //$NON-NLS-1$
                signatureId = ((Element) signatureValue.getParentNode()).getAttribute("Id"); //$NON-NLS-1$
                break;
            }
        }
        return signatureId;
    }
    
    /** Crea un X509Certificate a partir de un certificado en Base64.
     * @param b64Cert
     *        Certificado en Base64. No debe incluir <i>Bag Attributes</i>
     * @return Certificado X509 o <code>null</code> si no se pudo crear */
    public static X509Certificate createCert(final String b64Cert) {
        if (b64Cert == null || "".equals(b64Cert)) { //$NON-NLS-1$
            LOGGER.severe("Se ha proporcionado una cadena nula o vacia, se devolvera null"); //$NON-NLS-1$
            return null;
        }
        final X509Certificate cert;
        try {
            final InputStream isCert = new ByteArrayInputStream(Base64.decode(b64Cert));
            cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(isCert); //$NON-NLS-1$
            try {
                isCert.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
            }
        }
        catch (final Exception e) {
            LOGGER.severe("No se pudo decodificar el certificado en Base64, se devolvera null: " + e); //$NON-NLS-1$
            return null;
        }
        return cert;
    }
}
