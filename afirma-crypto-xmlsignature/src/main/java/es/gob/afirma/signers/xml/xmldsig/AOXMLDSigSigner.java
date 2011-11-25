/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.xml.xmldsig;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.utils.Base64;
import com.sun.org.apache.xml.internal.security.utils.XMLUtils;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.Utils.CannotDereferenceException;
import es.gob.afirma.signers.xml.Utils.IsInnerlException;
import es.gob.afirma.signers.xml.Utils.ReferenceIsNotXMLException;
import es.gob.afirma.signers.xml.XMLConstants;

/** Manejador de firmas XML en formato XMLDSig.
 * @version 0.2 */
@SuppressWarnings("restriction")
public final class AOXMLDSigSigner implements AOSigner {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** URI que define la versi&oacute;n por defecto de XAdES. */
    private static final String XADESNS = "http://uri.etsi.org/01903#"; //$NON-NLS-1$

    /** URI que define una referencia de tipo OBJECT. */
    private static final String OBJURI = "http://www.w3.org/2000/09/xmldsig#Object"; //$NON-NLS-1$
    
    private static final String SIGNATURE_STR = "Signature"; //$NON-NLS-1$
    private static final String MIMETYPE_STR = "MimeType"; //$NON-NLS-1$
    private static final String ENCODING_STR = "Encoding"; //$NON-NLS-1$
    private static final String REFERENCE_STR = "Reference"; //$NON-NLS-1$
    
    private static final String HTTP_PROTOCOL_PREFIX = "http://"; //$NON-NLS-1$
    private static final String HTTPS_PROTOCOL_PREFIX = "https://"; //$NON-NLS-1$
    
    private static final String STYLE_REFERENCE_PREFIX = "StyleReference-"; //$NON-NLS-1$

    private static final String CSURI = "http://uri.etsi.org/01903#CountersignedSignature"; //$NON-NLS-1$
    private static final String AFIRMA = "AFIRMA"; //$NON-NLS-1$
    private static final String XML_SIGNATURE_PREFIX = "ds"; //$NON-NLS-1$

    private static final String DETACHED_CONTENT_ELEMENT_NAME = "CONTENT"; //$NON-NLS-1$
    private static final String DETACHED_STYLE_ELEMENT_NAME = "STYLE"; //$NON-NLS-1$

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    private static final String DIGEST_METHOD = DigestMethod.SHA1;

    private String algo;
    private Document doc;

    static {
        if (Security.getProvider("XMLDSig") == null) { //$NON-NLS-1$
            try {
                Security.addProvider(new org.jcp.xml.dsig.internal.dom.XMLDSigRI());
            }
            catch (final Exception e) {
                LOGGER.warning("No se ha podido agregar el proveedor de firma XMLDSig necesario para firmas XML: " + e); //$NON-NLS-1$
            }
        }
    }

    /** Firma datos en formato XMLDSig 1.0 (XML Digital Signature).
     * <p>
     *  En el caso de que se firma un fichero con formato XML que contenga hojas de estilo
     *  XSL, y siempre que no se haya establecido el par&aacute;metro <i>ignoreStyleSheets</i> a
     *  <i>true</i>, se sigue la siguiente convenci&oacute;n para la firma es estas:
     * </p>
     * <ul>
     *  <li>Firmas XML Enveloped</li>
     *  <ul>
     *   <li>Hoja de estilo con ruta relativa</li>
     *   <ul>
     *    <li>No se firma.</li>
     *   </ul>
     *   <li>Hola de estilo remota con ruta absoluta</li>
     *   <ul>
     *    <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original.</li>
     *    <li>Se firma una referencia (canonicalizada) a esta hoja remota.</li>
     *   </ul>
     *   <li>Hoja de estilo empotrada</li>
     *   <ul>
     *    <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original.</li>
     *   </ul>
     *  </ul>
     *  <li>Firmas XML Externally Detached</li>
     *  <ul>
     *   <li>Hoja de estilo con ruta relativa</li>
     *   <ul>
     *    <li>No se firma.</li>
     *   </ul>
     *   <li>Hola de estilo remota con ruta absoluta</li>
     *   <ul>
     *    <li>Se firma una referencia (canonicalizada) a esta hoja remota.</li>
     *   </ul>
     *   <li>Hoja de estilo empotrada</li>
     *   <ul>
     *    <li>No es necesaria ninguna acci&oacute;n adicional.</li>
     *   </ul>
     *  </ul>
     *  <li>Firmas XML Enveloping</li>
     *  <ul>
     *   <li>Hoja de estilo con ruta relativa</li>
     *   <ul>
     *    <li>No se firma.</li>
     *   </ul>
     *   <li>Hola de estilo remota con ruta absoluta</li>
     *   <ul>
     *    <li>Se firma una referencia (canonicalizada) a esta hoja remota.</li>
     *   </ul>
     *   <li>Hoja de estilo empotrada</li>
     *   <ul>
     *    <li>No es necesaria ninguna acci&oacute;n adicional.</li>
     *   </ul>
     *  </ul>
     *  <li>Firmas XML Internally Detached</li>
     *  <ul>
     *   <li>Hoja de estilo con ruta relativa</li>
     *   <ul>
     *    <li>No se firma.</li>
     *   </ul>
     *   <li>Hola de estilo remota con ruta absoluta</li>
     *   <ul>
     *    <li>Se firma una referencia (canonicalizada) a esta hoja remota.</li>
     *   </ul>
     *   <li>Hoja de estilo empotrada</li>
     *   <ul>
     *    <li>No es necesaria ninguna acci&oacute;n adicional</li>
     *   </ul>
     *  </ul> 
     * </ul>
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i><br>(<code>AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA</code>)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i><br>(<code>AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA</code>)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i><br>(<code>AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA</code>)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i><br>(<code>AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA</code>)</li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>uri</i></b></dt>
     *   <dd>URI en la que se encuentra el documento, necesario en el caso de modo expl&iacute;cito y formato detached</dd>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. Se admiten los siguientes valores:
     *    <ul>
     *     <li>
     *      &nbsp;&nbsp;&nbsp;<i>explicit</i><br>(<code>AOSignConstants.SIGN_MODE_EXPLICIT</code>)<br>
     *      <b>
     *       <br>Importante: Las firmas XMLDSig expl&iacute;citas no se adec&uacute;an a ninguna normativa,
     *       y pueden ser rechazadas por sistemas de validaci&oacute;n de firmas.
     *      </b>
     *     </li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>implicit</i><br>(<code>AOSignConstants.SIGN_MODE_IMPLICIT</code>)</li>
     *    </ul>
     *   </dd>
     *  <dt><b><i>xmlSignaturePrefix</i></b></dt>
     *   <dd>
     *    Prefijo de espacio de nombres XML para los nodos de firma. Si no se especifica este par&aacute;metro
     *    se usa el valor por defecto (<i>ds</i>).
     *   </dd>
     *  <dt><b><i>format</i></b></dt>
     *   <dd>
     *    Formato en que se realizar&aacute; la firma. Se admiten los siguientes valores:
     *    <ul>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Detached</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED</code>)</li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Externally Detached</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED</code>)</li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Enveloped</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED</code>)</li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Enveloping</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING</code>)</li>
     *    </ul>
     *   </dd>
     *  <dt><b><i>precalculatedHashAlgorithm</i></b></dt>
     *   <dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
     *  <dt><b><i>xmlTransforms</i></b></dt>
     *   <dd>N&uacute;mero de transformaciones a aplicar al XML antes de firmarlo</dd>
     *  <dt><b><i>xmlTransform</i>n<i>Type</i></b></dt>
     *   <dd>Tipo de la transformaci&oacute;n <i>n</i> (debe ser la URL del algoritmo segun define W3C)</dd>
     *  <dt><b><i>xmlTransform<i>n</i>Subtype</i></b></dt>
     *   <dd>Subtipo de la transformaci&oacute;n <i>n</i> (por ejemplo, "intersect", "subtract" o "union" para XPATH2)</dd>
     *  <dt><b><i>xmlTransform<i>n</i>Body</i></b></dt>
     *   <dd>Cuerpo de la transformaci&oacute;n <i>n</i></dd>
     *  <dt><b><i>referencesDigestMethod</i></b></dt>
     *   <dd>Algoritmo de huella digital a usar en las referencias XML</dd>
     *  <dt><b><i>canonicalizationAlgorithm</i></b></dt>
     *   <dd>Algoritmo de canonicalizaci&oacute;n<i>n</i></dd>
     *  <dt><b><i>ignoreStyleSheets</i></b></dt>
     *   <dd>Ignora las hojas de estilo externas de los XML (no las firma) si se establece a <code>true</code>, si se establece a <code>false</code> s&iacute; las firma</dd>
     *  <dt><b><i>mimeType</i></b></dt>
     *   <dd>MIME-Type de los datos a firmar</dd>
     *  <dt><b><i>encoding</i></b></dt>
     *   <dd>Codificaci&oacute;n de los datos a firmar</dd>
     *  <dt><b><i>avoidBase64Transforms</i></b></dt>
     *   <dd>
     *    No declara transformaciones Base64 incluso si son necesarias si se establece a <code>true</code>, si se establece a <code>false</code>
     *    act&uacute;a normalmente (s&iacute; las declara)
     *   </dd> 
     *  <dt><b><i>headLess</i></b></dt>
     *   <dd>
     *    Evita cualquier interacci&oacute;n con el usuraio si se establece a <code>true</code>, si se establece a <code>false</code> act&uacute;a
     *    normalmente (puede mostrar di&aacute;logos, por ejemplo, para la dereferenciaci&oacute;n de hojas de estilo enlazadas con rutas relativas).
     *    &Uacute;til para los procesos desatendidos y por lotes
     *   </dd> 
     * </dl>
     * @return Firma en formato XMLDSig 1.0
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */  
    public byte[] sign(final byte[] data, 
                       final String algorithm, 
                       final PrivateKeyEntry keyEntry, 
                       final Properties xParams) throws AOException {

        final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String format = extraParams.getProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING); //$NON-NLS-1$
        final String mode = extraParams.getProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD); //$NON-NLS-1$
        final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$
        final boolean ignoreStyleSheets = Boolean.parseBoolean(extraParams.getProperty("ignoreStyleSheets", "true")); //$NON-NLS-1$ //$NON-NLS-2$
        final boolean avoidBase64Transforms = Boolean.parseBoolean(extraParams.getProperty("avoidBase64Transforms", "false")); //$NON-NLS-1$ //$NON-NLS-2$
        final boolean headLess = Boolean.parseBoolean(extraParams.getProperty("headLess", "true")); //$NON-NLS-1$ //$NON-NLS-2$
        String mimeType = extraParams.getProperty("mimeType"); //$NON-NLS-1$
        String encoding = extraParams.getProperty("encoding"); //$NON-NLS-1$
        if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
            encoding = XMLConstants.BASE64_ENCODING;
        }
        final String xmlSignaturePrefix = extraParams.getProperty("xmlSignaturePrefix", XML_SIGNATURE_PREFIX); //$NON-NLS-1$
        
        URI uri = null;
        try {
            uri = new URI(extraParams.getProperty("uri")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            // Se ignora, puede estar ausente
        }

        final String precalculatedHashAlgorithm = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$

        Utils.checkIllegalParams(format, mode, uri, precalculatedHashAlgorithm, false);

        // Un externally detached con URL permite los datos nulos o vacios
        if ((data == null || data.length == 0) && !(format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED) && uri != null)) {
            throw new AOException("No se han podido leer los datos a firmar"); //$NON-NLS-1$
        }

        // Propiedades del documento XML original
        final Map<String, String> originalXMLProperties = new Hashtable<String, String>();

        // carga el documento xml
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Elemento de datos
        Element dataElement;

        final String contentId = DETACHED_CONTENT_ELEMENT_NAME + "-" + UUID.randomUUID().toString() + "-" + DETACHED_CONTENT_ELEMENT_NAME; //$NON-NLS-1$ //$NON-NLS-2$
        final String styleId = DETACHED_STYLE_ELEMENT_NAME + "-" + UUID.randomUUID().toString() + "-" + DETACHED_STYLE_ELEMENT_NAME; //$NON-NLS-1$ //$NON-NLS-2$
        boolean isBase64 = false;
        boolean wasEncodedToBase64 = false;

        // Elemento de estilo
        Element styleElement = null;
        String styleType = null;
        String styleHref = null;
        String styleEncoding = null;

        if (mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
            try {
                // Obtenemos el objeto XML y su codificacion
                final Document docum = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(data));

                // Obtenemos la hoja de estilo del XML
                try {
                    final Properties p;
                    if (!ignoreStyleSheets) {
                        p = Utils.getStyleSheetHeader(new String(data));
                    }
                    else {
                        p = new Properties();
                    }
                    styleType = p.getProperty("type"); //$NON-NLS-1$
                    styleHref = p.getProperty("href"); //$NON-NLS-1$

                    if (styleType != null && styleHref != null) {

                        LOGGER.info("Se ha encontrado una hoja de estilo asociada al XML a firmar: tipo=" + styleType //$NON-NLS-1$
                                                               + ", referencia=" //$NON-NLS-1$
                                                               + styleHref);

                        LOGGER.info("Dereferenciando la hoja de estilo"); //$NON-NLS-1$
                        try {
                            final Document tmpDoc = Utils.dereferenceStyleSheet(
                                TransformerFactory.newInstance().getAssociatedStylesheet(
                                     new StreamSource(new ByteArrayInputStream(data)),
                                     null,
                                     null,
                                     null
                                 ).getSystemId(),
                                 headLess
                            );

                            // Cuidado!! Solo rellenamos el Elemento DOM si no
                            // es HTTP o HTTPS, porque si es accesible
                            // remotamente no necesito el elemento, ya que se
                            // firma via referencia Externally Detached
                            if (!styleHref.startsWith(HTTP_PROTOCOL_PREFIX) && !styleHref.startsWith(HTTPS_PROTOCOL_PREFIX)) {
                                styleElement = tmpDoc.getDocumentElement();
                            }

                            styleEncoding = tmpDoc.getXmlEncoding();
                        }
                        catch (final IsInnerlException ex) {
                            LOGGER
                                  .info("La hoja de estilo esta referenciada internamente, por lo que no se necesita dereferenciar"); //$NON-NLS-1$
                        }
                        catch (final ReferenceIsNotXMLException ex) {
                            LOGGER
                                  .warning("La hoja de estilo referenciada no es XML o no se ha dereferenciado apropiadamente"); //$NON-NLS-1$
                        }
                        catch (final CannotDereferenceException ex) {
                            LOGGER
                                  .warning("La hoja de estilo no ha podido dereferenciar, probablemente sea un enlace relativo local"); //$NON-NLS-1$
                        }
                        catch (final Exception ex) {
                            LOGGER.severe("Error intentando dereferenciar la hoja de estilo: " + ex); //$NON-NLS-1$
                        }
                    }
                }
                catch (final Exception e) {
                    LOGGER.info("No se ha encontrado ninguna hoja de estilo asociada al XML a firmar"); //$NON-NLS-1$
                }

                // Si no hay asignado un MimeType o es el por defecto
                // establecemos el de XML
                if (mimeType == null || XMLConstants.DEFAULT_MIMETYPE.equals(mimeType)) {
                    mimeType = "text/xml"; //$NON-NLS-1$
                }

                if (encoding == null) {
                    encoding = docum.getXmlEncoding();
                }

                // Ademas del encoding, sacamos otros datos del doc XML original

                // Hacemos la comprobacion del base64 por si se establecido
                // desde fuera
                if (encoding != null && !XMLConstants.BASE64_ENCODING.equals(encoding)) {
                    originalXMLProperties.put(OutputKeys.ENCODING, encoding);
                }
                String tmpXmlProp = docum.getXmlVersion();
                if (tmpXmlProp != null) {
                    originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
                }
                final DocumentType dt = docum.getDoctype();
                if (dt != null) {
                    tmpXmlProp = dt.getSystemId();
                    if (tmpXmlProp != null) {
                        originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
                    }
                }

                if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED)) {
                    dataElement = docum.createElement(DETACHED_CONTENT_ELEMENT_NAME);
                    dataElement.setAttributeNS(null, "Id", contentId); //$NON-NLS-1$
                    dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType); 
                    dataElement.setAttributeNS(null, ENCODING_STR, encoding); 
                    dataElement.appendChild(docum.getDocumentElement());

                    // Tambien el estilo
                    if (styleElement != null) {
                        try {
                            final Element tmpStyleElement = docum.createElement(DETACHED_STYLE_ELEMENT_NAME);
                            tmpStyleElement.setAttributeNS(null, "Id", styleId); //$NON-NLS-1$
                            if (styleType != null) {
                                tmpStyleElement.setAttributeNS(null, MIMETYPE_STR, styleType); 
                            }
                            tmpStyleElement.setAttributeNS(null, ENCODING_STR, styleEncoding); 
                            tmpStyleElement.appendChild(docum.adoptNode(styleElement.cloneNode(true)));
                            styleElement = tmpStyleElement;
                        }
                        catch (final Exception e) {
                            LOGGER
                                  .warning("No ha sido posible crear el elemento DOM para incluir la hoja de estilo del XML como Internally Detached: " + e); //$NON-NLS-1$
                            styleElement = null;
                        }
                    }
                }
                else {
                    dataElement = docum.getDocumentElement();
                }

            }
            // captura de error en caso de no ser un documento xml
            catch (final Exception e) {
                if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
                    throw new AOFormatFileException("El modo Enveloped solo permite firmar datos XML", e); //$NON-NLS-1$
                }
                // para los formatos de firma internally detached y enveloping
                // se trata de convertir el documento a base64
                try {
                    LOGGER.info("El documento no es un XML valido. Se convertira a Base64: " + e); //$NON-NLS-1$

                    // crea un nuevo nodo xml para contener los datos en base 64
                    final Document docFile = dbf.newDocumentBuilder().newDocument();
                    dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);
                    uri = null;
                    encoding = XMLConstants.BASE64_ENCODING;
                    if (mimeType == null) {
                        mimeType = XMLConstants.DEFAULT_MIMETYPE;
                    }

                    dataElement.setAttributeNS(null, "Id", contentId); //$NON-NLS-1$

                    // Si es base 64, lo firmamos indicando como contenido el
                    // dato pero, ya que puede
                    // poseer un formato particular o caracteres valido pero
                    // extranos para el XML,
                    // realizamos una decodificacion y recodificacion para asi
                    // homogenizar el formato.
                    if (AOUtil.isBase64(data) && (XMLConstants.BASE64_ENCODING.equals(encoding) || ((encoding != null) ? encoding : "").toLowerCase().equals("base64"))) { //$NON-NLS-1$ //$NON-NLS-2$
                        LOGGER.info("El documento se ha indicado como Base64, se insertara como tal en el XML"); //$NON-NLS-1$

                        // Adicionalmente, si es un base 64 intentamos obtener
                        // el tipo del contenido
                        // decodificado para asi reestablecer el MimeType.
                        final byte[] decodedData = Base64.decode(data);
                        final MimeHelper mimeTypeHelper = new MimeHelper(decodedData);
                        final String tempMimeType = mimeTypeHelper.getMimeType();
                        mimeType = tempMimeType != null ? tempMimeType : XMLConstants.DEFAULT_MIMETYPE;
                        dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType); 
                        dataElement.setTextContent(Base64.encode(decodedData));
                    }
                    else {
                        if (XMLConstants.BASE64_ENCODING.equals(encoding)) {
                            LOGGER.info("El documento se ha indicado como Base64, pero no es un Base64 valido. Se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion"); //$NON-NLS-1$
                        }
                        else {
                            LOGGER.info("El documento se considera binario, se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion"); //$NON-NLS-1$
                        }
                        // Usamos el MimeType identificado
                        dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType); 
                        dataElement.setTextContent(Base64.encode(data));
                        wasEncodedToBase64 = true;
                    }
                    isBase64 = true;
                    encoding = XMLConstants.BASE64_ENCODING;
                    dataElement.setAttributeNS(null, ENCODING_STR, encoding); 
                }
                catch (final Exception ex) {
                    throw new AOException("Error al convertir los datos a base64", ex); //$NON-NLS-1$
                }
            }
        }

        // Firma Explicita
        else {
            // ESTE BLOQUE CONTIENE EL PROCESO A SEGUIR EN EL MODO EXPLICITO,
            // ESTO ES, NO FIRMAMOS LOS DATOS SINO SU HASH
            byte[] digestValue = null;
            // Si la URI no es nula recogemos los datos de fuera
            if (uri != null) {
                byte[] tmpData = null;
                try {
                    tmpData = AOUtil.getDataFromInputStream(AOUtil.loadFile(uri));
                }
                catch (final Exception e) {
                    throw new AOException("No se han podido obtener los datos de la URI externa", e); //$NON-NLS-1$
                }
                // Vemos si hemos obtenido bien los datos de la URI
                if (tmpData != null && tmpData.length > 0) {
                    try {
                        digestValue = MessageDigest.getInstance("SHA1").digest(tmpData); //$NON-NLS-1$
                    }
                    catch (final Exception e) {
                        throw new AOException("No se ha podido obtener el SHA1 de los datos de la URI externa", e); //$NON-NLS-1$
                    }
                }
            }
            // Si no tenemos URI y se nos inserto directamente el hash de los
            // datos
            else if (precalculatedHashAlgorithm != null) {
                digestValue = data;
            }
            // Si solo tenemos los datos
            else {
                try {
                    digestValue = MessageDigest.getInstance("SHA1").digest(data); //$NON-NLS-1$
                }
                catch (final Exception e) {
                    throw new AOException("No se ha podido obtener el SHA1 de los datos proporcionados", e); //$NON-NLS-1$
                }
            }

            if (digestValue == null || digestValue.length < 1) {
                throw new AOException("Error al obtener la huella SHA1 de los datos"); //$NON-NLS-1$
            }

            final Document docFile;
            try {
                docFile = dbf.newDocumentBuilder().newDocument();
            }
            catch (final Exception e) {
                throw new AOException("No se ha podido crear el documento XML contenedor", e); //$NON-NLS-1$
            }
            dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);

            encoding = XMLConstants.BASE64_ENCODING;
            // En el caso de la firma explicita, se firma el Hash de los datos
            // en lugar de los propios datos.
            // En este caso, los indicaremos a traves del MimeType en donde
            // establecemos un tipo especial
            // que designa al hash. Independientemente del algoritmo de firma
            // utilizado, el Hash de las firmas
            // explicitas de datos siempre sera SHA1, salvo que el hash se haya
            // establecido desde fuera.
            String hashAlgoUri;
            if (precalculatedHashAlgorithm != null) {
                mimeType = "hash/" + precalculatedHashAlgorithm.toLowerCase(); //$NON-NLS-1$
                hashAlgoUri = XMLConstants.MESSAGEDIGEST_ALGOS_URI.get(precalculatedHashAlgorithm.toLowerCase());
            }
            else {
                mimeType = "hash/sha1"; //$NON-NLS-1$
                hashAlgoUri = XMLConstants.MESSAGEDIGEST_ALGOS_URI.get("sha1"); //$NON-NLS-1$
            }

            dataElement.setAttributeNS(null, "Id", contentId); //$NON-NLS-1$
            dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType); 
            dataElement.setAttributeNS(null, ENCODING_STR, encoding); 
            if (hashAlgoUri != null) {
                dataElement.setAttributeNS(null, "hashAlgorithm", hashAlgoUri); // TODO: Aqui se agrega el atributo a la Detached Explicita //$NON-NLS-1$
            }

            dataElement.setTextContent(Base64.encode(digestValue));
            isBase64 = true;

            // FIN BLOQUE EXPLICITO
        }

        // ***************************************************
        // ***************************************************

        final String tmpUri = "#" + contentId; //$NON-NLS-1$
        final String tmpStyleUri = "#" + styleId; //$NON-NLS-1$

        // Crea el nuevo documento de firma
        Document docSignature = null;
        try {
            docSignature = dbf.newDocumentBuilder().newDocument();
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
                docSignature.appendChild(docSignature.adoptNode(dataElement));
            }
            else {
                docSignature.appendChild(docSignature.createElement(AFIRMA));
            }
        }
        catch (final Exception e) {
            throw new AOException("Error al crear la firma en formato " + format + ", modo " + mode, e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final List<Reference> referenceList = new ArrayList<Reference>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        final String referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
        final String referenceStyleId = STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString(); 

        final List<Transform> transformList = new ArrayList<Transform>();

        // Primero anadimos las transformaciones a medida
        Utils.addCustomTransforms(transformList, extraParams, xmlSignaturePrefix);

        // Solo canonicalizo si es XML
        if (!isBase64) {
            try {
                // Transformada para la canonicalizacion inclusiva
                transformList.add(fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null));
            }
            catch (final Exception e) {
                LOGGER
                      .severe("No se puede encontrar el algoritmo de canonicalizacion, la referencia no se canonicalizara: " + e); //$NON-NLS-1$
            }
        }
        // Si no era XML y tuve que convertir a Base64 yo mismo declaro la
        // transformacion
        else if (wasEncodedToBase64 && !avoidBase64Transforms) {
            try {
                transformList.add(fac.newTransform(Transform.BASE64, (TransformParameterSpec) null));
            }
            catch (final Exception e) {
                LOGGER.severe("No se puede encontrar el algoritmo transformacion Base64, esta no se declarara: " + e); //$NON-NLS-1$
            }
        }

        // crea una referencia al documento insertado en un nodo Object para la
        // firma enveloping y a el estilo
        XMLObject envelopingObject = null;
        XMLObject envelopingStyleObject = null;

        if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)) {
            try {
                // crea el nuevo elemento Object que contiene el documento a
                // firmar
                final List<XMLStructure> structures = new ArrayList<XMLStructure>(1);

                // Si los datos se han convertido a base64, bien por ser
                // binarios o explicitos
                if (isBase64) {
                    structures.add(new DOMStructure(dataElement.getFirstChild()));
                }
                else {
                    structures.add(new DOMStructure(dataElement));
                }

                final String objectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
                envelopingObject = fac.newXMLObject(structures, objectId, mimeType, encoding);

                // crea la referencia al nuevo elemento Object
                referenceList.add(fac.newReference("#" + objectId, digestMethod, transformList, OBJURI, referenceId)); //$NON-NLS-1$

                // Vamos con la hoja de estilo
                if (styleElement != null) {
                    final String objectStyleId = "StyleObject-" + UUID.randomUUID().toString(); //$NON-NLS-1$
                    envelopingStyleObject =
                            fac.newXMLObject(Collections.singletonList(new DOMStructure(styleElement)), objectStyleId, styleType, styleEncoding);
                    referenceList.add(fac.newReference("#" + objectStyleId, //$NON-NLS-1$
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       OBJURI,
                                                       referenceStyleId));

                }
            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato enveloping", e); //$NON-NLS-1$
            }

            // Hojas de estilo para enveloping en Externally Detached. Comprobamos si la referencia al estilo es externa
            if ((styleHref != null && styleElement == null) && (styleHref.startsWith(HTTP_PROTOCOL_PREFIX) || styleHref.startsWith(HTTPS_PROTOCOL_PREFIX))) {
                try {
                    referenceList.add(fac.newReference(styleHref,
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       null,
                                                       referenceStyleId));
                }
                catch (final Exception e) {
                    LOGGER.severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e); //$NON-NLS-1$
                }
            }

        }

        // crea una referencia al documento mediante la URI hacia el
        // identificador del nodo CONTENT
        else if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED)) {
            try {
                if (dataElement != null) {
                    // inserta en el nuevo documento de firma el documento a
                    // firmar
                    docSignature.getDocumentElement().appendChild(docSignature.adoptNode(dataElement));
                    // crea la referencia a los datos firmados que se
                    // encontraran en el mismo documento
                    referenceList.add(fac.newReference(tmpUri, digestMethod, transformList, null, referenceId));
                }
                if (styleElement != null) {
                    // inserta en el nuevo documento de firma la hoja de estilo
                    docSignature.getDocumentElement().appendChild(docSignature.adoptNode(styleElement));
                    // crea la referencia a los datos firmados que se
                    // encontraran en el mismo documento
                    referenceList.add(fac.newReference(tmpStyleUri,
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       null,
                                                       referenceStyleId));
                }

            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato detached implicito", e); //$NON-NLS-1$
            }

            // Hojas de estilo remotas para detached. Comprobamos si la referencia al estilo es externa
            if ((styleHref != null && styleElement == null) && (styleHref.startsWith(HTTP_PROTOCOL_PREFIX) || styleHref.startsWith(HTTPS_PROTOCOL_PREFIX))) {
                try {
                    referenceList.add(fac.newReference(styleHref,
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       null,
                                                       referenceStyleId));
                }
                catch (final Exception e) {
                    LOGGER.severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e); //$NON-NLS-1$
                }
            }

        }

        // Crea una referencia al documento mediante la URI externa si la
        // tenemos o usando un Message Digest
        // precalculado si no tenemos otro remedio
        else if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)) {
            Reference ref = null;
            // No tenemos uri, suponemos que los datos son el message digest
            if (precalculatedHashAlgorithm != null && (uri == null || uri.getScheme().equals("") || uri.getScheme().equals("file"))) { //$NON-NLS-1$ //$NON-NLS-2$
                DigestMethod dm = null;
                try {
                    // Convertimos el algo del Message Digest externo a la
                    // nomenclatura XML
                    if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA1")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.SHA1, null);
                    }
                    else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-256")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.SHA256, null);
                    }
                    else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-512")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.SHA512, null);
                    }
                    else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("RIPEMD160")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.RIPEMD160, null);
                    }
                }
                catch (final Exception e) {
                    throw new AOException("No se ha podido crear el metodo de huella digital para la referencia Externally Detached", e); //$NON-NLS-1$
                }
                if (dm == null) {
                    throw new AOException("Metodo de Message Digest para la referencia Externally Detached no soportado: " + precalculatedHashAlgorithm); //$NON-NLS-1$
                }
                ref = fac.newReference("", dm, null, null, referenceId, data); //$NON-NLS-1$
            }
            // Tenemos URI y no nos han establecido algoritmo de message digest,
            // por lo que es una referencia externa accesible
            else {
                // Si es una referencia de tipo file:// obtenemos el fichero y
                // creamos una referencia solo con
                // el message digest
                if (uri != null && uri.getScheme().equals("file")) { //$NON-NLS-1$
                    try {
                        ref =
                                fac.newReference("", //$NON-NLS-1$
                                                 digestMethod,
                                                 null,
                                                 null,
                                                 referenceId,
                                                 MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(digestMethodAlgorithm))
                                                              .digest(AOUtil.getDataFromInputStream(AOUtil.loadFile(uri))));
                    }
                    catch (final Exception e) {
                        throw new AOException("No se ha podido crear la referencia XML a partir de la URI local (" + uri.toASCIIString() + ")", e); //$NON-NLS-1$ //$NON-NLS-2$
                    }
                }
                // Si es una referencia distinta de file:// suponemos que es
                // dereferenciable de forma universal
                // por lo que dejamos que Java lo haga todo
                else if (uri != null) {
                    try {
                        ref = fac.newReference(uri.toASCIIString(), digestMethod);
                    }
                    catch (final Exception e) {
                        throw new AOException("No se ha podido crear la referencia Externally Detached, probablemente por no obtenerse el metodo de digest", //$NON-NLS-1$
                                              e);
                    }
                }
            }
            if (ref == null) {
                throw new AOException("Error al generar la firma Externally Detached, no se ha podido crear la referencia externa"); //$NON-NLS-1$
            }
            referenceList.add(ref);

            // Hojas de estilo remotas en Externally Detached
            if (styleHref != null && styleElement == null) {
                // Comprobamos que la URL es valida
                if (styleHref.startsWith(HTTP_PROTOCOL_PREFIX) || styleHref.startsWith(HTTPS_PROTOCOL_PREFIX)) { 
                    try {
                        referenceList.add(fac.newReference(styleHref,
                                                           digestMethod,
                                                           Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                      (TransformParameterSpec) null)),
                                                           null,
                                                           referenceStyleId));
                    }
                    catch (final Exception e) {
                        LOGGER.severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e); //$NON-NLS-1$
                    }
                }
                else {
                    LOGGER.warning("Se necesita una referencia externa HTTP o HTTPS a la hoja de estilo para referenciarla en firmas XML Externally Detached"); //$NON-NLS-1$
                }
            }

        }

        // crea una referencia indicando que se trata de una firma enveloped
        else if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
            try {

                // Transformacion enveloped
                // La enveloped siempre la primera, para que no se quede sin
                // nodos Signature por haber
                // ejecutado antes otra transformacion
                transformList.add(fac.newTransform(Transform.ENVELOPED, (TransformParameterSpec) null));

                // Transformacion XPATH para eliminar el resto de firmas del
                // documento
                transformList.add(
                  fac.newTransform(
                    Transform.XPATH,
                    new XPathFilterParameterSpec("not(ancestor-or-self::" + xmlSignaturePrefix + ":Signature)", //$NON-NLS-1$ //$NON-NLS-2$
                    Collections.singletonMap(xmlSignaturePrefix, XMLSignature.XMLNS))
                  )
                );

                // crea la referencia
                referenceList.add(fac.newReference("", digestMethod, transformList, null, referenceId)); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato enveloped", e); //$NON-NLS-1$
            }

            // Hojas de estilo remotas para enveloped. Comprobamos si la referencia al estilo es externa
            if ((styleHref != null && styleElement == null) && (styleHref.startsWith(HTTP_PROTOCOL_PREFIX) || styleHref.startsWith(HTTPS_PROTOCOL_PREFIX))) {
                try {
                    referenceList.add(fac.newReference(styleHref,
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       null,
                                                       referenceStyleId));
                }
                catch (final Exception e) {
                    LOGGER.severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e); //$NON-NLS-1$
                }
            }

        }

        // definicion de identificadores
        final String id = UUID.randomUUID().toString();
        final String keyInfoId = "KeyInfo-" + id; //$NON-NLS-1$

        try {

            // se anade una referencia a KeyInfo
            referenceList.add(fac.newReference("#" + keyInfoId, digestMethod, transformList, null, null)); //$NON-NLS-1$

            // KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final List<Object> content = new ArrayList<Object>();
            final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
            content.add(kif.newKeyValue(cert.getPublicKey()));
            // content.add(kif.newX509Data(Collections.singletonList(cert)));
            // //TODO: Con esto solo agregamos el certificado de firma
            Certificate[] certs = keyEntry.getCertificateChain();
            if (certs == null) {
                certs = new Certificate[] {
                    keyEntry.getCertificate()
                };
            }
            content.add(kif.newX509Data(Arrays.asList(certs)));

            // Object
            final List<XMLObject> objectList = new ArrayList<XMLObject>();

            // en el caso de formato enveloping se inserta el elemento Object
            // con el documento a firmar
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING) && (envelopingObject != null)) {
                objectList.add(envelopingObject);
                if (envelopingStyleObject != null) {
                    objectList.add(envelopingStyleObject);
                }
            }

            // Si es enveloped hay que anadir la hoja de estilo dentro de la
            // firma y
            // referenciarla
            if ((format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) && (styleElement != null)) {
                objectList.add(fac.newXMLObject(Collections.singletonList(new DOMStructure(styleElement)), styleId, styleType, styleEncoding));
                try {
                    referenceList.add(fac.newReference(tmpStyleUri,
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       null,
                                                       referenceStyleId));
                }
                catch (final Exception e) {
                    LOGGER
                          .severe("No se ha podido anadir una referencia a la hoja de estilo, esta se incluira dentro de la firma, pero no estara firmada: " + e); //$NON-NLS-1$
                }
            }

            // genera la firma
            final XMLSignature signature =
                    fac.newXMLSignature(fac.newSignedInfo(fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec) null),
                                                          fac.newSignatureMethod(algoUri, null),
                                                          Utils.cleanReferencesList(referenceList)),
                                        kif.newKeyInfo(content, keyInfoId),
                                        objectList,
                                        "Signature-" + id, //$NON-NLS-1$
                                        "SignatureValue-" + id); //$NON-NLS-1$

            final DOMSignContext signContext = new DOMSignContext(keyEntry.getPrivateKey(), docSignature.getDocumentElement());
            signContext.putNamespacePrefix(XMLConstants.DSIGNNS, xmlSignaturePrefix);
            signature.sign(signContext);
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la firma XMLdSig", e); //$NON-NLS-1$
        }

        final String signatureNodeName = (xmlSignaturePrefix == null || "".equals(xmlSignaturePrefix) ? "" : xmlSignaturePrefix + ":") + SIGNATURE_STR; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        // Si se esta realizando una firma enveloping simple no tiene sentido el
        // nodo raiz,
        // asi que sacamos el nodo de firma a un documento aparte
        if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)) {
            try {
                if (docSignature.getElementsByTagName(signatureNodeName).getLength() == 1) {
                    final Document newdoc = dbf.newDocumentBuilder().newDocument();
                    newdoc.appendChild(newdoc.adoptNode(docSignature.getElementsByTagName(signatureNodeName).item(0)));
                    docSignature = newdoc;
                }
            }
            catch (final Exception e) {
                LOGGER.info("No se ha eliminado el nodo padre '<AFIRMA>': " + e); //$NON-NLS-1$
            }
        }

        // Si no es enveloped quito los valores para que no se inserte la
        // cabecera de hoja de estilo
        if (!format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
            styleHref = null;
            styleType = null;
        }
        return Utils.writeXML(docSignature.getDocumentElement(), originalXMLProperties, styleHref, styleType);

    }

    /** Comprueba si la firma es detached.
     * @param element
     *        Elemento que contiene el nodo ra&iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es detached */
    private boolean isDetached(final Element element) {
        if (element == null) {
            return false;
        }
        if (element.getFirstChild().getLocalName() != null && element.getFirstChild().getLocalName().equals(DETACHED_CONTENT_ELEMENT_NAME)) {
            return true;
        }
        return false;
    }

    /** Comprueba si la firma es enveloped
     * @param element
     *        Elemento que contiene el nodo ra&iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es enveloped */
    private boolean isEnveloped(final Element element) {
        final NodeList transformList = element.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Transform"); //$NON-NLS-1$
        for (int i = 0; i < transformList.getLength(); i++) {
            if (((Element) transformList.item(i)).getAttribute("Algorithm").equals(Transform.ENVELOPED)){ //$NON-NLS-1$
                return true;
            }
        }
        return false;
    }

    /** Comprueba si la firma es enveloping
     * @param element
     *        Elemento que contiene el nodo ra&iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es enveloping */
    private boolean isEnveloping(final Element element) {
        if (element == null) {
            return false;
        }
        if (element.getLocalName().equals(SIGNATURE_STR) || (element.getLocalName().equals(AFIRMA) && element.getFirstChild() 
                                                                                                           .getLocalName()
                                                                                                           .equals(SIGNATURE_STR))) { 
            return true;
        }

        return false;
    }

    /** {@inheritDoc} */
    public byte[] getData(final byte[] sign) throws AOInvalidFormatException {
        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        final Element rootSig;
        Element elementRes = null;
        try {
            // comprueba que sea una documento de firma valido
            if (!isSign(sign)) {
                throw new AOInvalidFormatException("El documento no es un documento de firmas valido."); //$NON-NLS-1$
            }

            // obtiene la raiz del documento de firmas
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            // si es detached
            if (this.isDetached(rootSig)) {
                final Element firstChild = (Element) rootSig.getFirstChild();
                // si el documento es un xml se extrae como tal
                if (firstChild.getAttribute(MIMETYPE_STR).equals("text/xml")) { //$NON-NLS-1$ 
                    elementRes = (Element) firstChild.getFirstChild();
                }
                // Si el MimeType es de tipo Hash (tipo creado para el cliente
                // afirma) asi que la firma no tiene datos
                // else if
                // (firstChild.getAttribute(MIMETYPE_STR).startsWith("hash/")) {
                // elementRes = null;
                // }
                // si el documento es binario se deshace la codificacion en
                // Base64
                else {
                    return Base64.decode(firstChild.getTextContent());
                }
            }

            // si es enveloped
            else if (this.isEnveloped(rootSig)) {
                // obtiene las firmas y las elimina
                final NodeList signatures = rootSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 
                final int numSignatures = signatures.getLength();
                for (int i = 0; i < numSignatures; i++) {
                    rootSig.removeChild(signatures.item(0));
                }
                elementRes = rootSig;
            }

            // si es enveloping
            else if (this.isEnveloping(rootSig)) {
                // obtiene el nodo Object de la primera firma
                final Element object = (Element) rootSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Object").item(0); //$NON-NLS-1$
                // si el documento es un xml se extrae como tal
                if (object.getAttribute(MIMETYPE_STR).equals("text/xml")) { //$NON-NLS-1$ 
                    elementRes = (Element) object.getFirstChild();
                }
                else {
                    return Base64.decode(object.getTextContent());
                }
            }
        }
        catch (final Exception ex) {
            throw new AOInvalidFormatException("Error al leer el fichero de firmas", ex); //$NON-NLS-1$
        }

        // si no se ha recuperado ningún dato se devuelve null
        if (elementRes == null) {
            return null;
        }

        // convierte el documento obtenido en un array de bytes
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        XMLUtils.outputDOM(elementRes, baosSig);
        return baosSig.toByteArray();
    }

    /** {@inheritDoc} */
    public byte[] cosign(final byte[] data, 
                         final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties xParams) throws AOException {

        final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD); //$NON-NLS-1$
        final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$
        final String xmlSignaturePrefix = extraParams.getProperty("xmlSignaturePrefix", XML_SIGNATURE_PREFIX); //$NON-NLS-1$

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Propiedades del documento XML original
        final Map<String, String> originalXMLProperties = new Hashtable<String, String>();

        // carga el documento XML de firmas y su raiz
        Document docSig;
        Element rootSig;
        try {
            docSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
            rootSig = docSig.getDocumentElement();

            // si el documento contiene una firma simple se inserta como raiz el
            // nodo AFIRMA
            if (rootSig.getNodeName().equals((xmlSignaturePrefix == null || "".equals(xmlSignaturePrefix) ? "" : xmlSignaturePrefix + ":"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                docSig = insertarNodoAfirma(docSig);
                rootSig = docSig.getDocumentElement();
            }
        }
        catch (final ParserConfigurationException pcex) {
            throw new AOException("Formato de documento de firmas incorrecto", pcex); //$NON-NLS-1$
        }
        catch (final SAXException saxex) {
            throw new AOException("Formato de documento de firmas incorrecto", saxex); //$NON-NLS-1$
        }
        catch (final IOException ioex) {
            throw new AOException("Error al leer el documento de firmas", ioex); //$NON-NLS-1$
        }
        catch (final IllegalArgumentException iaex) {
            throw new AOException("Parametro de entrada incorrecto", iaex); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido leer el documento XML de firmas", e); //$NON-NLS-1$
        }

        final List<Reference> referenceList = new ArrayList<Reference>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Localizamos la primera firma (primer nodo SIGNATURE_STR) en profundidad
        // en el arbol de firma.
        // Se considera que todos los objetos SIGNATURE_STR del documento firman
        // (referencian) los mismos
        // objetos, por lo que podemos extraerlos de cualquiera de las firmas
        // actuales.
        // Buscamos dentro de ese Signature todas las referencias que apunten a
        // datos para firmarlas
        final ArrayList<String> referencesIds = new ArrayList<String>();
        Node currentElement;
        final NodeList nl = ((Element) docSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR).item(0)).getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR); 

        // Se considera que la primera referencia de la firma son los datos que
        // debemos firmar, ademas
        // de varias referencias especiales
        for (int i = 0; i < nl.getLength(); i++) {
            currentElement = nl.item(i);

            // Firmamos la primera referencia (que seran los datos firmados) y
            // las hojas de estilo que
            // tenga asignadas. Las hojas de estilo tendran un identificador que
            // comience por STYLE_REFERENCE_PREFIX.
            // TODO: Identificar las hojas de estilo de un modo generico.
            final NamedNodeMap currentNodeAttributes = currentElement.getAttributes();
            if (i == 0 || (currentNodeAttributes.getNamedItem("Id") != null && currentNodeAttributes.getNamedItem("Id") //$NON-NLS-1$ //$NON-NLS-2$
                                                                                                    .getNodeValue()
                                                                                                    .startsWith(STYLE_REFERENCE_PREFIX))) { 

                // Buscamos las transformaciones declaradas en la Referencia,
                // para anadirlas
                // tambien en la nueva
                List<Transform> currentTransformList;
                try {
                    currentTransformList = Utils.getObjectReferenceTransforms(currentElement, xmlSignaturePrefix);
                }
                catch (final NoSuchAlgorithmException e) {
                    Logger.getLogger("Se ha declarado una transformacion personalizada de un tipo no soportado: " + e); //$NON-NLS-1$
                    throw new AOException("Se ha declarado una transformacion personalizada de un tipo no soportado", e); //$NON-NLS-1$
                }
                catch (final InvalidAlgorithmParameterException e) {
                    Logger.getLogger("Se han especificado parametros erroneos para una transformacion personalizada: " + e); //$NON-NLS-1$
                    throw new AOException("Se han especificado parametros erroneos para una transformacion personalizada", e); //$NON-NLS-1$
                }

                // Creamos un identificador de referencia para el objeto a
                // firmar y la almacenamos
                // para mantener un listado con todas. En el caso de las hojas
                // de estilo lo creamos con un
                // identificador descriptivo
                String referenceId = null;
                if ((currentNodeAttributes.getNamedItem("Id") != null && currentNodeAttributes.getNamedItem("Id") //$NON-NLS-1$ //$NON-NLS-2$
                                                                                              .getNodeValue()
                                                                                              .startsWith(STYLE_REFERENCE_PREFIX))) { 
                    referenceId = STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString(); 
                }
                else {
                    referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
                }
                referencesIds.add(referenceId);

                // Creamos la propia referencia con las transformaciones de la
                // original
                referenceList.add(fac.newReference(((Element) currentElement).getAttribute("URI"), digestMethod, currentTransformList, // Lista de //$NON-NLS-1$
                                                                                                                                       // transformaciones
                                                   null,
                                                   referenceId));
            }
        }

        // definicion de identificadores
        final String id = UUID.randomUUID().toString();
        final String signatureId = "Signature-" + id; //$NON-NLS-1$
        final String signatureValueId = "SignatureValue-" + id; //$NON-NLS-1$
        final String keyInfoId = "KeyInfo-" + id; //$NON-NLS-1$

        try {

            // CanonicalizationMethod
            final CanonicalizationMethod cm = fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec) null);

            // se anade una referencia a KeyInfo
            final List<Transform> transformList = new ArrayList<Transform>();
            final Transform trCanonicalization = fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null);
            transformList.add(trCanonicalization);
            referenceList.add(fac.newReference("#" + keyInfoId, digestMethod, transformList, null, null)); //$NON-NLS-1$

            // SignatureMethod
            final SignatureMethod sm = fac.newSignatureMethod(algoUri, null);

            // KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final List<Object> x509Content = new ArrayList<Object>();
            final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
            x509Content.add(cert);

            final List<Object> content = new ArrayList<Object>();
            content.add(kif.newKeyValue(cert.getPublicKey()));
            content.add(kif.newX509Data(x509Content));

            final DOMSignContext signContext = new DOMSignContext(keyEntry.getPrivateKey(), rootSig);
            signContext.putNamespacePrefix(XMLConstants.DSIGNNS, xmlSignaturePrefix);

            fac.newXMLSignature(fac.newSignedInfo(cm, sm, referenceList), // SignedInfo
                                kif.newKeyInfo(content, keyInfoId), // KeyInfo
                                new ArrayList<Object>(),
                                signatureId,
                                signatureValueId).sign(signContext);

        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la cofirma XMLdSig", e); //$NON-NLS-1$
        }

        return Utils.writeXML(rootSig, originalXMLProperties, null, null);
    }

    /** {@inheritDoc} */
    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // carga la raiz del documento XML de firmas
        // y crea un nuevo documento que contendra solo los datos sin firmar
        Element rootSig;
        Element rootData;
        try {
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            final Document docData = dbf.newDocumentBuilder().newDocument();
            rootData = (Element) docData.adoptNode(rootSig.cloneNode(true));

            // Obtiene las firmas y las elimina. Para evitar eliminar firmas de
            // las que cuelgan otras
            // y despues intentar eliminar estas, las buscamos y eliminamos de
            // una en una
            NodeList signatures = rootData.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 
            while (signatures.getLength() > 0) {
                rootData.removeChild(signatures.item(0));
                signatures = rootData.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 
            }

            docData.appendChild(rootData);
        }
        catch (final ParserConfigurationException pcex) {
            throw new AOException("Formato de documento de firmas incorrecto", pcex); //$NON-NLS-1$
        }
        catch (final SAXException saxex) {
            throw new AOException("Formato de documento de firmas incorrecto", saxex); //$NON-NLS-1$
        }
        catch (final IOException ioex) {
            throw new AOException("Error al leer el documento de firmas", ioex); //$NON-NLS-1$
        }
        catch (final IllegalArgumentException iaex) {
            throw new AOException("Parametro de entrada incorrecto", iaex); //$NON-NLS-1$
        }

        // convierte el documento de firmas en un InputStream
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        XMLUtils.outputDOM(rootSig, baosSig);

        // convierte el documento a firmar en un InputStream
        final ByteArrayOutputStream baosData = new ByteArrayOutputStream();
        XMLUtils.outputDOM(rootData, baosData);

        return cosign(baosData.toByteArray(), baosSig.toByteArray(), algorithm, keyEntry, extraParams);
    }

    /** {@inheritDoc} */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties xParams) throws AOException {

        final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD); //$NON-NLS-1$
        final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$
        String encoding = extraParams.getProperty("encoding"); //$NON-NLS-1$
        if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
            encoding = XMLConstants.BASE64_ENCODING;
        }
        final String xmlSignaturePrefix = extraParams.getProperty("xmlSignaturePrefix", XML_SIGNATURE_PREFIX); //$NON-NLS-1$

        this.algo = algorithm;

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // se carga el documento XML y su raiz
        final Map<String, String> originalXMLProperties = new Hashtable<String, String>();
        Element root;
        try {
            this.doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));

            // Tomamos la configuracion del XML que contrafirmamos
            if (encoding == null) {
                encoding = this.doc.getXmlEncoding();
            }

            // Ademas del encoding, sacamos otros datos del doc XML original
            // Hacemos la comprobacion del base64 por si se establecido desde
            // fuera
            if (encoding != null && !XMLConstants.BASE64_ENCODING.equalsIgnoreCase(encoding)) {
                originalXMLProperties.put(OutputKeys.ENCODING, encoding);
            }
            String tmpXmlProp = this.doc.getXmlVersion();
            if (tmpXmlProp != null) {
                originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
            }
            final DocumentType dt = this.doc.getDoctype();
            if (dt != null) {
                tmpXmlProp = dt.getSystemId();
                if (tmpXmlProp != null) {
                    originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
                }
            }

            root = this.doc.getDocumentElement();

            // si el documento contiene una firma simple se inserta como raiz el
            // nodo AFIRMA
            if (root.getNodeName().equals((xmlSignaturePrefix == null || "".equals(xmlSignaturePrefix) ? "" : xmlSignaturePrefix + ":"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                this.doc = insertarNodoAfirma(this.doc);
                root = this.doc.getDocumentElement();
            }

            if (targetType == CounterSignTarget.TREE) {
                this.countersignTree(root, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
            else if (targetType == CounterSignTarget.LEAFS) {
                this.countersignLeafs(root, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
            else if (targetType == CounterSignTarget.NODES) {
                this.countersignNodes(root, targets, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
            else if (targetType == CounterSignTarget.SIGNERS) {
                this.countersignSigners(root, targets, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }

        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
        }

        // convierte el xml resultante para devolverlo como byte[]
        return Utils.writeXML(this.doc.getDocumentElement(), originalXMLProperties, null, null);
    }

    /** Realiza la contrafirma de todos los nodos del &aacute;rbol.
     * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void countersignTree(final Element root,
                                 final PrivateKeyEntry keyEntry,
                                 final String refsDigestMethod,
                                 final String canonicalizationAlgorithm,
                                 final String xmlSignaturePrefix) throws AOException {

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 
        final int numSignatures = signatures.getLength();

        final Element[] nodes = new Element[numSignatures];
        for (int i = 0; i < numSignatures; i++) {
            nodes[i] = (Element) signatures.item(i);
        }

        // y crea sus contrafirmas
        try {
            for (int i = 0; i < numSignatures; i++) {
                this.cs(nodes[i], keyEntry, refsDigestMethod, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
        }
    }

    /** Realiza la contrafirma de todos los nodos hoja del &aacute;rbol.
     * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void countersignLeafs(final Element root,
                                  final PrivateKeyEntry keyEntry,
                                  final String refsDigestMethod,
                                  final String canonicalizationAlgorithm,
                                  final String xmlSignaturePrefix) throws AOException {

        // obtiene todas las firmas y las referencias
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 
        final NodeList references = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR); 

        final int numSignatures = signatures.getLength();
        final int numReferences = references.getLength();

        // comprueba cuales son hojas
        try {
            for (int i = 0; i < numSignatures; i++) {
                final Element signature = (Element) signatures.item(i);
                final String refURI = "#" + signature.getAttribute("Id") + "Value"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

                boolean isLeaf = true;

                // si la firma esta referenciada por otra firma entonces no es
                // hoja
                for (int j = 0; j < numReferences; j++) {
                    if (((Element) references.item(j)).getAttribute("URI").equals(refURI)) { //$NON-NLS-1$
                        isLeaf = false;
                    }
                }

                // y crea sus contrafirmas
                if (isLeaf) {
                    this.cs(signature, keyEntry, refsDigestMethod, canonicalizationAlgorithm, xmlSignaturePrefix);
                }
            }
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
        }
    }

    /** Realiza la contrafirma de los nodos indicados en el par&aacute;metro
     * targets.
     * @param root Elemento raiz del documento xml que contiene las firmas
     * @param tgts Array con las posiciones de los nodos a contrafirmar
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void countersignNodes(final Element root,
                                  Object[] tgts,
                                  final PrivateKeyEntry keyEntry,
                                  final String refsDigestMethod,
                                  final String canonicalizationAlgorithm,
                                  final String xmlSignaturePrefix) throws AOException {

        if (tgts == null) {
            throw new IllegalArgumentException("La lista de nodos a contrafirmar no puede ser nula"); //$NON-NLS-1$
        }
        
        // descarta las posiciones que esten repetidas
        final List<Integer> targetsList = new ArrayList<Integer>();
        for (int i = 0; i < tgts.length; i++) {
            if (!targetsList.contains(tgts[i])) {
                targetsList.add((Integer) tgts[i]);
            }
        }
        final Object[] targets = targetsList.toArray();

        final AOTreeNode tree = new AOTreeNode("AFIRMA"); //$NON-NLS-1$

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 

        final int numSignatures = signatures.getLength();

        final String[] arrayIds = new String[numSignatures];
        final String[] arrayRef = new String[numSignatures];
        final AOTreeNode[] arrayNodes = new AOTreeNode[numSignatures];

        // genera un arbol con las firmas para conocer su posicion
        for (int i = 0; i < numSignatures; i++) {
            final Element signature = (Element) signatures.item(i);
            final String sigId = signature.getAttribute("Id"); //$NON-NLS-1$

            final AOTreeNode node = new AOTreeNode(signature);
            arrayIds[i] = sigId;
            arrayNodes[i] = node;

            final String typeReference = ((Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR).item(0)).getAttribute("Type"); //$NON-NLS-1$ 
            if (typeReference.equals(CSURI)) {
                final String uri = ((Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR).item(0)).getAttribute("URI"); //$NON-NLS-1$ 
                arrayRef[i] = uri.substring(1, uri.length() - 5);
            }
            else {
                arrayRef[i] = ""; //$NON-NLS-1$
            }
        }

        for (int i = numSignatures - 1; i > 0; i--) {
            for (int j = 0; j < numSignatures; j++) {
                if (arrayRef[i].equals(arrayIds[j])) {
                    arrayNodes[j].add(arrayNodes[i]);
                }
            }
        }

        for (int i = 0; i < numSignatures; i++){
            if ("".equals(arrayRef[i])) { //$NON-NLS-1$
                tree.add(arrayNodes[i]);
            }
        }

        // introduce en una lista los nodos del arbol recorrido en preorden
        final List<Element> listNodes = new ArrayList<Element>();
        final Enumeration<AOTreeNode> enumTree = tree.preorderEnumeration();
        enumTree.nextElement();
        while (enumTree.hasMoreElements()) {
            listNodes.add((Element) enumTree.nextElement().getUserObject());
        }

        // obtiene los nodos indicados en targets
        final Element[] nodes = new Element[targets.length];
        try {
            for (int i = 0; i < targets.length; i++) {
                nodes[i] = listNodes.get(((Integer) targets[i]).intValue());
            }
        }
        catch (final ClassCastException e) {
            throw new AOException("Valor de nodo no valido", e); //$NON-NLS-1$
        }
        catch (final IndexOutOfBoundsException e) {
            throw new AOException("Posicion de nodo no valida", e); //$NON-NLS-1$
        }

        // y crea sus contrafirmas
        try {
            for (final Element node : nodes) {
                this.cs(node, keyEntry, refsDigestMethod, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
        }
    }

    /** Realiza la contrafirma de los firmantes indicados en el par&aacute;metro
     * targets.
     * @param root
     *        Elemento ra&iacute;z del documento xml que contiene las firmas
     * @param targets
     *        Array con el nombre de los firmantes de los nodos a
     *        contrafirmar
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    private void countersignSigners(final Element root,
                                    final Object[] targets,
                                    final PrivateKeyEntry keyEntry,
                                    final String refsDigestMethod,
                                    final String canonicalizationAlgorithm,
                                    final String xmlSignaturePrefix) throws AOException {

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 
        final int numSignatures = signatures.getLength();

        final List<Object> signers = Arrays.asList(targets);
        final List<Element> nodes = new ArrayList<Element>();

        // obtiene los nodos de los firmantes indicados en targets
        for (int i = 0; i < numSignatures; i++) {
            final Element node = (Element) signatures.item(i);
            if (signers.contains(AOUtil.getCN(Utils.getCertificate(node.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Certificate").item(0))))) { //$NON-NLS-1$
                nodes.add(node);
            }
        }

        // y crea sus contrafirmas
        final Iterator<Element> i = nodes.iterator();
        while (i.hasNext()) {
            this.cs(i.next(), keyEntry, refsDigestMethod, canonicalizationAlgorithm, xmlSignaturePrefix);
        }
    }

    /** Realiza la contrafirma de la firma pasada por par&aacute;metro.
     * @param signature Elemento con el nodo de la firma a contrafirmar
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void cs(final Element signature, 
                    final PrivateKeyEntry keyEntry, 
                    final String refsDigestMethod, 
                    final String canonicalizationAlgorithm,
                    final String xmlSignaturePrefix) throws AOException {

        // obtiene el nodo SignatureValue
        final Element signatureValue = (Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, "SignatureValue").item(0); //$NON-NLS-1$

        // crea la referencia a la firma que se contrafirma
        final List<Reference> referenceList = new ArrayList<Reference>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(refsDigestMethod, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + refsDigestMethod + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        final String referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$

        try {
            // Transformada para la canonicalizacion inclusiva con comentarios
            final List<Transform> transformList = new ArrayList<Transform>();
            transformList.add(fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null));
            referenceList.add(fac.newReference("#" + signatureValue.getAttribute("Id"), digestMethod, transformList, CSURI, referenceId)); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
        }

        // definicion de identificadores
        final String id = UUID.randomUUID().toString();
        final String signatureId = "Signature-" + id; //$NON-NLS-1$
        final String signatureValueId = "SignatureValue-" + id; //$NON-NLS-1$
        final String keyInfoId = "KeyInfo-" + id; //$NON-NLS-1$

        try {

            // se anade una referencia a KeyInfo
            referenceList.add(fac.newReference("#" + keyInfoId, digestMethod)); //$NON-NLS-1$

            // KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final List<Object> x509Content = new ArrayList<Object>();
            final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
            x509Content.add(cert);

            final List<Object> content = new ArrayList<Object>();
            content.add(kif.newKeyValue(cert.getPublicKey()));
            content.add(kif.newX509Data(x509Content));

            final XMLSignature sign =
                    fac.newXMLSignature(fac.newSignedInfo(fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec) null),
                                                          fac.newSignatureMethod(XMLConstants.SIGN_ALGOS_URI.get(this.algo), null),
                                                          referenceList), kif.newKeyInfo(content, keyInfoId), null, signatureId, signatureValueId);

            final DOMSignContext signContext = new DOMSignContext(keyEntry.getPrivateKey(), signature.getOwnerDocument().getDocumentElement());
            signContext.putNamespacePrefix(XMLConstants.DSIGNNS, xmlSignaturePrefix);

            sign.sign(signContext);
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + this.algo + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {

        // recupera la raiz del documento de firmas
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Element root;
        final String completePrefix;
        try {
            this.doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
            root = this.doc.getDocumentElement();

            // Identificamos el prefijo que se utiliza en los nodos de firma
            String xmlDSigNSPrefix = Utils.guessXMLDSigNamespacePrefix(root);
            if (xmlDSigNSPrefix == null) {
                xmlDSigNSPrefix = XML_SIGNATURE_PREFIX;
            }
            completePrefix = ("".equals(xmlDSigNSPrefix) ? "" : xmlDSigNSPrefix + ":"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

            // Si el documento tiene como nodo raiz el nodo de firma, se agrega
            // un nodo raiz previo para que la lectura de las firmas del
            // documento
            // se haga correctamente
            if (root.getNodeName().equals(completePrefix + SIGNATURE_STR)) { 
                this.doc = insertarNodoAfirma(this.doc);
                root = this.doc.getDocumentElement();
            }
        }
        catch (final Exception e) {
            LOGGER.warning("Se ha producido un error al obtener la estructura de firmas. " + e); //$NON-NLS-1$
            return null;
        }

        final AOTreeNode tree = new AOTreeNode("Datos"); //$NON-NLS-1$

        // Obtenemos todas las firmas y los signature value
        final NodeList signatures = root.getElementsByTagName(completePrefix + SIGNATURE_STR); 
        final NodeList signatureValues = root.getElementsByTagName(completePrefix + "SignatureValue"); //$NON-NLS-1$

        final int numSignatures = signatures.getLength();
        final String[] arrayIds = new String[numSignatures];
        final String[] arrayRef = new String[numSignatures];
        final AOTreeNode[] arrayNodes = new AOTreeNode[numSignatures];

        for (int i = 0; i < numSignatures; i++) {

            final Element signature = (Element) signatures.item(i);

            arrayIds[i] = signature.getAttribute("Id"); //$NON-NLS-1$

            arrayNodes[i] = new AOTreeNode(asSimpleSignInfo ? Utils.getSimpleSignInfoNode(XADESNS, signature) : Utils.getStringInfoNode(signature));

            // Recogemos el identificador de la firma a la que se referencia (si
            // no es contrafirma sera cadena vacia)
            final String typeReference = ((Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR).item(0)).getAttribute("Type"); //$NON-NLS-1$ 
            if (typeReference.equals(CSURI)) {
                arrayRef[i] = Utils.getCounterSignerReferenceId(signature, signatureValues);
            }
            else {
                arrayRef[i] = ""; //$NON-NLS-1$
            }
        }

        // Se buscan las contrafirmas de cada firma o cofirma
        for (int i = 0; i < numSignatures; i++) {
            if ("".equals(arrayRef[i])) { //$NON-NLS-1$
                tree.add(generaArbol(i, numSignatures - 1, arrayNodes, arrayIds, arrayRef)[i]);
            }
        }

        return new AOTreeModel(tree, numSignatures);
    }

    /** M&eacute;todo recursivo para la obtenci&oacute;n de la estructura de
     * &aacute;rbol.
     * @param i Inicio de lectura del array de identificadores
     * @param j Inicio de lectura inversa del array de referencias
     * @param arrayNodes Array de objetos AOTreeNode
     * @param arrayIds Array de identificadores
     * @param arrayRef Array de referencias
     * @return Array de objetos AOTreeNode */
    private AOTreeNode[] generaArbol(final int i, final int j, final AOTreeNode arrayNodes[], final String arrayIds[], final String arrayRef[]) {
        final int max = arrayIds.length;
        if (i < max && j > 0) {
            if (arrayIds[i].equals(arrayRef[j])) {
                generaArbol(i + 1, j - 1, arrayNodes, arrayIds, arrayRef);
            }
            if (i < j) {
                generaArbol(i, j - 1, arrayNodes, arrayIds, arrayRef);
            }
            if (!arrayIds[i].equals(arrayRef[j])) {
                return arrayNodes;
            }
            generaArbol(j, max - 1, arrayNodes, arrayIds, arrayRef);
            arrayNodes[i].add(arrayNodes[j]);
        }
        return arrayNodes;
    }

    /** {@inheritDoc} */
    public boolean isSign(final byte[] sign) {

        if (sign == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        try {
            // Carga el documento a validar
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            final Document signDoc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
            final Element rootNode = signDoc.getDocumentElement();

            final ArrayList<Node> signNodes = new ArrayList<Node>();
            if (rootNode.getNodeName().equals((XML_SIGNATURE_PREFIX == null || "".equals(XML_SIGNATURE_PREFIX) ? "" : XML_SIGNATURE_PREFIX + ":"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                signNodes.add(rootNode);
            }

            final NodeList signatures = rootNode.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_STR); 
            for (int i = 0; i < signatures.getLength(); i++) {
                signNodes.add(signatures.item(i));
            }

            // Si no se encuentran firmas, no es un documento de firma
            if (signNodes.size() == 0) {
                return false;
            }

        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    public String getSignedName(final String originalName, final String inText) {
        return originalName + (inText != null ? inText : "") + ".xsig"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Devuelve un nuevo documento con ra&iacute;z "AFIRMA" y conteniendo al
     * documento pasado por par&aacute;metro.
     * @param docu Documento que estar&aacute; contenido en el nuevo documento
     * @return Documento con ra&iacute;z "AFIRMA"
     * @throws ParserConfigurationException */
    private Document insertarNodoAfirma(final Document docu) throws ParserConfigurationException {

        // Nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Crea un nuevo documento con la raiz "AFIRMA"
        final Document docAfirma = dbf.newDocumentBuilder().newDocument();
        final Element rootAfirma = docAfirma.createElement(AFIRMA);

        // Inserta el documento pasado por parametro en el nuevo documento
        rootAfirma.appendChild(docAfirma.adoptNode(docu.getDocumentElement()));
        docAfirma.appendChild(rootAfirma);

        return docAfirma;
    }

    /** {@inheritDoc} */
    public AOSignInfo getSignInfo(final byte[] sign) throws AOException {
        if (sign == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(sign)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        final AOSignInfo signInfo = new AOSignInfo(AOSignConstants.SIGN_FORMAT_XMLDSIG);

        // Analizamos mas en profundidad la firma para obtener el resto de datos

        // Tomamos la raiz del documento
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Element rootSig = null;
        try {
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();
        }
        catch (final Exception e) {
            LOGGER.warning("Error al analizar la firma: " + e); //$NON-NLS-1$
            rootSig = null;
        }

        // Establecemos la variante de firma
        if (rootSig != null) {
            if (isDetached(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED);
            }
            else if (isEnveloped(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED);
            }
            else if (isEnveloping(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING);
            }
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros
        // datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return signInfo;
    }

}
