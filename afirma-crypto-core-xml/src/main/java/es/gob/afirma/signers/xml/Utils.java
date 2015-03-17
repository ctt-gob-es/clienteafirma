/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URI;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
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

import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilter2ParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.crypto.dsig.spec.XPathType;
import javax.xml.crypto.dsig.spec.XPathType.Filter;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSimpleSignInfo;

/** Utilidades para las firmas XML. */
public final class Utils {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private Utils() {
        // No permitimos la instanciacion
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
    private static String addStyleSheetHeader(final String xml, final String tpy, final String href) {
        if (href == null) {
            return xml;
        }

        final String type = tpy != null ? tpy : "text/xsl"; //$NON-NLS-1$

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

        final List<Transform> transformList = transforms != null ? transforms : new ArrayList<Transform>();
        final Properties extraParams = xParams != null ? xParams : new Properties();

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
                    transformParam = new XPathFilter2ParameterSpec(
                		Collections.singletonList(
            				new XPathType(transformBody, xPath2TransformFilter)
        				)
            		);
                }
                catch (final Exception e) {
                    LOGGER.warning(
                       "No se han podido crear los parametros para una transformacion XPATH2, se omitira: " + e //$NON-NLS-1$
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
                transformList.add(Utils.getDOMFactory().newTransform(transformType, transformParam));
            }
            catch (final Exception e) {
                LOGGER.warning("No se ha podido aplicar la transformacion '" + transformType + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }

        }
    }

    /** Obtiene de un nodo de referencia de tipo <i>Object</i> la lista de
     * transformaciones definidas. Si no tiene transfgormaciones definidas,
     * devuelve {@code null}.
     * @param referenceNode Nodo de tipo referencia.
     * @param namespacePrefix Prefijo del espacio de nombres de la firma (opcional).
     * @return Listado de transformaciones.
     * @throws InvalidAlgorithmParameterException Cuando se encuentre un par&aacute;metro inv&aacute;lido para
     *                                            el algoritmo de transformaci&oacute;n.
     * @throws NoSuchAlgorithmException Cuando se encuentre un algoritmo de transformaci&oacute;n no soportado. */
    public static List<Transform> getObjectReferenceTransforms(final Node referenceNode,
    		                                                   final String namespacePrefix) throws NoSuchAlgorithmException,
                                                                                                    InvalidAlgorithmParameterException {
        final ArrayList<Transform> transformList = new ArrayList<Transform>();

        // El nodo de referencia puede contener un nodo "Transforms" que a su
        // vez contiene
        // las distintas transformaciones
        final NodeList tmpNodeList = referenceNode.getChildNodes();

        for (int i = 0; i < tmpNodeList.getLength(); i++) {

            // Buscamos el nodo Transforms a secas o precedido por el namespace
            if ("Transforms".equals(tmpNodeList.item(i).getNodeName()) || (namespacePrefix + ":Transforms").equals(tmpNodeList.item(i).getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$

                final NodeList transformsNodeList = tmpNodeList.item(i).getChildNodes();

                for (int j = 0; j < transformsNodeList.getLength(); j++) {

                    transformList.add(Utils.getDOMFactory().newTransform(getTransformAlgorithm(transformsNodeList.item(j)),
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

    /** Recupera los par&aacute;metros de una transformaci&oacute;n. En el caso
     * de las transformaciones XPATH y XPATH2, se devolveran los
     * par&aacute;metros especificados y, en las transformacion Base64,
     * Enveloped y de Canonicalizaci&oacute;n (que no reciben par&aacute;metros)
     * se devolver&aacute; {@code null}, al igual que cuando no se reconozca el
     * tipo de transformaci&oacute;n.
     * @param transformNode Nodo de transformaci&oacute;n.
     * @param namespacePrefix Prefijo del espacio de nombres XML
     * @return Par&aacute;metros de la transformaci&oacute;n.
     * @throws InvalidAlgorithmParameterException
     *         Cuando no se especifiquen correctamente los
     *         par&aacute;mnetros de las transformaciones XPATH y XPATH2. */
    private static TransformParameterSpec getTransformParameterSpec(final Node transformNode,
    		                                                        final String namespacePrefix) throws InvalidAlgorithmParameterException {

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
                            params = new XPathFilterParameterSpec(
                        		xpathTransformNode.getTextContent(),
                        		Collections.singletonMap(
                    				namespacePrefix,
                                    XMLSignature.XMLNS
                                )
                            );
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
                throw new UnsupportedOperationException("No se puede realizar una firma enveloped sobre un contenido explicito"); //$NON-NLS-1$
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

    /** Intenta determinar el prefijo del espacio de nombres de XAdES.
     * @param el Firma XAdES
     * @return Prefijo del espacio de nombres */
    public static String guessXAdESNamespacePrefix(final Element el) {
        final String signatureText = new String(writeXML(el, null, null, null));
        if (signatureText.contains("xmlns:etsi=\"http://uri.etsi.org/")) { //$NON-NLS-1$
            return "etsi"; //$NON-NLS-1$
        }
        return "xades"; //$NON-NLS-1$
    }

    /** Intenta determinar el prefijo del espacio de nombres de la firma XMLDSig.
     * @param el Firma XMLDSig
     * @return Prefijo del espacio de nombres */
    public static String guessXmlDSigNamespacePrefix(final Element el) {

        final String signatureText = new String(Utils.writeXML(el, null, null, null));

        final int numEmpty = Utils.countSubstring(signatureText, "<Signature"); //$NON-NLS-1$
        final int numDs = Utils.countSubstring(signatureText, "<ds:Signature"); //$NON-NLS-1$
        final int numDsig = Utils.countSubstring(signatureText, "<dsig:Signature"); //$NON-NLS-1$
        final int numDsig11 = Utils.countSubstring(signatureText, "<dsig11:Signature"); //$NON-NLS-1$

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
     * @param text Texto en el que realizar la b&uacute;squeda.
     * @param substring Subcadena que deseamos buscar.
     * @return N&uacute;mero de coincidencias. */
    public static int countSubstring(final String text, final String substring) {
        int count = 0;
        for (int i = 0; i <= text.length() - substring.length(); i++) {
            if (substring.charAt(0) == text.charAt(i) && substring.equals(text.substring(i, i + substring.length()))) {
                count++;
                i += substring.length() - 1;
            }
        }
        return count;
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

        final Map<String, String> xmlProps = props != null ? props : new Hashtable<String, String>(0);

        // La codificacion por defecto sera UTF-8
        final String xmlEncoding = xmlProps.containsKey(OutputKeys.ENCODING) ? xmlProps.get(OutputKeys.ENCODING) : "UTF-8"; //$NON-NLS-1$

        // Primero creamos un writer
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Writer writer = null;
        try {
            writer = new OutputStreamWriter(baos, xmlEncoding);
        }
        catch (final UnsupportedEncodingException e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            writer = new OutputStreamWriter(baos);
        }

        // Ahora escribimos el XML usando XALAN
        writeXMLwithXALAN(writer, node, xmlEncoding);

        try {
            DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(baos.toByteArray()));
        }
        catch (final Exception e) {
            LOGGER.severe(
        		"No se ha podido recargar el XML para insertar los atributos de la cabecera, quizas la codificacion se vea afectada: " + e //$NON-NLS-1$
    		);
            return baos.toByteArray();
        }

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
        final com.sun.org.apache.xerces.internal.dom.DOMOutputImpl output = new com.sun.org.apache.xerces.internal.dom.DOMOutputImpl();
        output.setCharacterStream(writer);
        if (xmlEncoding != null) {
            output.setEncoding(xmlEncoding);
        }
        serializer.write(node, output);
    }

    /** Genera un objeto descriptor de firma.
     * @param namespace Espacio de nombres utilizado para la recuperaci&oacute;n de
     *                  atributos XAdES.
     * @param signature Nodo de firma.
     * @return Objeto descriptor de firma. */
    public static AOSimpleSignInfo getSimpleSignInfoNode(final String namespace, final Element signature) {

        // Recupera la fecha de firma
        Date signingTime = null;
        if (namespace != null) {
            try {
                signingTime = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").parse( //$NON-NLS-1$
            		((Element) signature.getElementsByTagNameNS(namespace, "SigningTime").item(0)).getTextContent() //$NON-NLS-1$
        		);
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

    /** Recupera la factor&iacute;a de firmas XML preferente.
     * @return Factor&iacute;a de firmas XML */
    public static XMLSignatureFactory getDOMFactory() {
		final XMLSignatureFactory fac;
		try {
			// Primero comprobamos si hay una version nueva de XMLSec accesible, en cuyo caso, podria
			// provocar un error el no usarla. Normalmente, ClassCastException al recuperar la factoria.
			fac =  XMLSignatureFactory.getInstance(
				"DOM", //$NON-NLS-1$
				(Provider) Class.forName("org.apache.jcp.xml.dsig.internal.dom.XMLDSigRI").newInstance() //$NON-NLS-1$
			);
			LOGGER.info("Se usara la factoria XML del XMLSec instalado"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			return XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
		}
		return fac;
    }

    /** Instala el proveedor de firmas XMLDSig para el entorno de ejecuci&oacute;n de Java en uso. */
    public static void installXmlDSigProvider() {
    	if (Security.getProvider("XMLDSig") == null) { //$NON-NLS-1$
        	LOGGER.info("Instalamos el proveedor de firma XML de Apache"); //$NON-NLS-1$
        	try {
        		Security.addProvider((Provider) Class.forName("org.apache.jcp.xml.dsig.internal.dom.XMLDSigRI").newInstance()); //$NON-NLS-1$
        	}
        	catch (final Exception e) {
        		LOGGER.info("No se encontro el proveedor de firma XML de Apache, se instalara el de Sun: " + e); //$NON-NLS-1$
        		try {
        			Security.addProvider((Provider)
        					Class.forName("org.jcp.xml.dsig.internal.dom.XMLDSigRI").newInstance()); //$NON-NLS-1$
        		}
        		catch (final Exception e2) {
        			LOGGER.warning("No se ha podido agregar el proveedor de firma XMLDSig de Sun para firmas XML: " + e2); //$NON-NLS-1$
        		}
        	}
        }
    }
}
