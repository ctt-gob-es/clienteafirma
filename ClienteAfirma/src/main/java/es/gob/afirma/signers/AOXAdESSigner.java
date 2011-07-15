/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers;

import static es.gob.afirma.misc.AOConstants.DEFAULT_MIMETYPE;
import static es.gob.afirma.misc.AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
import static es.gob.afirma.misc.AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
import static es.gob.afirma.misc.AOConstants.SIGN_ALGOS_URI;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_DETACHED;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_ENVELOPED;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_ENVELOPING;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED;
import static es.gob.afirma.misc.AOConstants.SIGN_MODE_IMPLICIT;
import static es.gob.afirma.signers.xmlhelper.XMLConstants.DSIGNNS;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.security.AccessController;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.Vector;
import java.util.logging.Logger;

import javax.activation.MimeType;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import net.java.xades.security.xml.XAdES.DataObjectFormat;
import net.java.xades.security.xml.XAdES.DataObjectFormatImpl;
import net.java.xades.security.xml.XAdES.ObjectIdentifier;
import net.java.xades.security.xml.XAdES.ObjectIdentifierImpl;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifier;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifierImpl;
import net.java.xades.security.xml.XAdES.SignatureProductionPlace;
import net.java.xades.security.xml.XAdES.SignatureProductionPlaceImpl;
import net.java.xades.security.xml.XAdES.SignerRole;
import net.java.xades.security.xml.XAdES.SignerRoleImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;
import net.java.xades.util.XMLUtils;

import org.ietf.jgss.Oid;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.MimeHelper;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;
import es.gob.afirma.signers.beans.AOSignInfo;
import es.gob.afirma.signers.xmlhelper.AOXMLAdvancedSignature;
import es.gob.afirma.signers.xmlhelper.Utils;
import es.gob.afirma.signers.xmlhelper.Utils.CannotDereferenceException;
import es.gob.afirma.signers.xmlhelper.Utils.IsInnerlException;
import es.gob.afirma.signers.xmlhelper.Utils.ReferenceIsNotXMLException;

/** Operaciones de firmas en formato XAdES.
 * <p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 * <dt>uri</dt>
 * <dd>URI en la que se encuentra el documento, necesario en el caso de modo expl&iacute;cito y formato detached</dd>
 * <dt>mode</dt>
 * <dd>Modo de firma a usar (Expl&iacute;cita o Impl&iacute;cita)</dd>
 * <dt>format</dt>
 * <dd>Formato en que se realizar&aacute; la firma</dd>
 * <dt>policyIdentifier</dt>
 * <dd>URL identificadora de la pol&iacute;tica de firma (normalmente una URL hacia el documento que describe la pol&iacute;tica)</dd>
 * <dt>policyDescription</dt>
 * <dd>Descripci&oacute;n de la pol&iacute;tica</dd>
 * <dt>policyQualifier</dt>
 * <dd>OID calificador de la pol&iacute;tica de firma</dd>
 * <dt>signerClaimedRole</dt>
 * <dd>Cargo atribuido para el firmante</dd>
 * <dt>signerCertifiedRole</dt>
 * <dd>Cargo confirmado para el firmante</dd>
 * <dt>precalculatedHashAlgorithm</dt>
 * <dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 * <dt>signatureProductionCity</dt>
 * <dd>Ciudad en la que se realiza la firma</dd>
 * <dt>signatureProductionProvince</dt>
 * <dd>Provincia en la que se realiza la firma</dd>
 * <dt>signatureProductionPostalCode</dt>
 * <dd>C&oacute;digo postal en el que se realiza la firma</dd>
 * <dt>signatureProductionCountry</dt>
 * <dd>Pa&iacute;s en el que se realiza la firma</dd>
 * <dt>xmlTransforms</dt>
 * <dd>N&uacute;mero de transformaciones a aplicar al XML antes de firmarlo</dd>
 * <dt>xmlTransform<i>n</i>Type</dt>
 * <dd>Tipo de la transformaci&oacute;n <i>n</i> (debe ser la URL del algoritmo segun define W3C)</dd>
 * <dt>xmlTransform<i>n</i>Subtype</dt>
 * <dd>Subtipo de la transformaci&oacute;n <i>n</i> (por ejemplo, "intersect", "subtract" o "union" para XPATH2)</dd>
 * <dt>xmlTransform<i>n</i>Body</dt>
 * <dd>Cuerpo de la transformaci&oacute;n <i>n</i></dd>
 * <dt>referencesDigestMethod</dt>
 * <dd>Algoritmo de huella digital a usar en las referencias XML (referencesDigestMethod)</dd>
 * <dt>canonicalizationAlgorithm</dt>
 * <dd>Algoritmo de canonicalizaci&oacute;n</dd>
 * <dt>xadesNamespace</dt>
 * <dd>URL de definici&oacute;n del espacio de nombres de XAdES (y por extensi&oacute;n, versi&oacute;n de XAdES)</dd> <!--
 * <dt>xmlDSigNamespacePrefix</dt>
 * <dd>Prefijo del espacio de nombres de XMLDSig (normalmente "dsig" o "ds")</dd> -->
 * <dt>ignoreStyleSheets</dt>
 * <dd>Ignora las hojas de estilo externas de los XML (no las firma) si se establece a <code>true</code>, si se establece a <code>false</code>
 * act&uacute;a normalmente (s&iacute; las firma)</dd>
 * <dt>avoidBase64Transforms</dt>
 * <dd>No declara transformaciones Base64 incluso si son necesarias si se establece a <code>true</code>, si se establece a <code>false</code>
 * act&uacute;a normalmente (s&iacute; las declara)</dd> <!--
 * <dt>headLess</dt>
 * <dd>Evita cualquier interacci&oacute;n con el usuario si se establece a <code>true</code>, si se establece a <code>false</code> act&uacute;a
 * normalmente (puede mostrar di&aacute;logos, por ejemplo, para la dereferenciaci&oacute;n de hojas de estilo enlazadas con rutas relativas).
 * &Uacute;til para los procesos desatendidos y por lotes</dd> -->
 * </dl>
 * <p>
 * Tratamiento de las hojas de estilo en firmas XML:
 * <ul>
 * <li>Firmas XML Enveloped</li>
 * <ul>
 * <li>Hoja de estilo con ruta relativa</li>
 * <ul>
 * <li>No se firma.</li>
 * </ul>
 * <li>Hola de estilo remota con ruta absoluta</li>
 * <ul>
 * <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original</li>
 * <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 * </ul>
 * <li>Hoja de estilo empotrada</li>
 * <ul>
 * <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original</li>
 * </ul>
 * </ul>
 * <li>Firmas XML Externally Detached</li>
 * <ul>
 * <li>Hoja de estilo con ruta relativa</li>
 * <ul>
 * <li>No se firma.</li>
 * </ul>
 * <li>Hola de estilo remota con ruta absoluta</li>
 * <ul>
 * <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 * </ul>
 * <li>Hoja de estilo empotrada</li>
 * <ul>
 * <li>No es necesaria ninguna acci&oacute;n</li>
 * </ul>
 * </ul>
 * <li>Firmas XML Enveloping</li>
 * <ul>
 * <li>Hoja de estilo con ruta relativa</li>
 * <ul>
 * <li>No se firma.</li>
 * </ul>
 * <li>Hola de estilo remota con ruta absoluta</li>
 * <ul>
 * <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 * </ul>
 * <li>Hoja de estilo empotrada</li>
 * <ul>
 * <li>No es necesaria ninguna acci&oacute;n</li>
 * </ul>
 * </ul>
 * <li>Firmas XML Internally Detached</li>
 * <ul>
 * <li>Hoja de estilo con ruta relativa</li>
 * <ul>
 * <li>No se firma.</li>
 * </ul>
 * <li>Hola de estilo remota con ruta absoluta</li>
 * <ul>
 * <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 * </ul>
 * <li>Hoja de estilo empotrada</li>
 * <ul>
 * <li>No es necesaria ninguna acci&oacute;n</li>
 * </ul>
 * </ul> </ul>
 * </p>
 * @version 0.3 */
public final class AOXAdESSigner implements AOSigner {

    /** URI que define la versi&oacute;n por defecto de XAdES. */
    private static final String XADESNS = "http://uri.etsi.org/01903/v1.3.2#";

    /** URI que define una referencia de tipo OBJECT. */
    private static final String OBJURI = "http://www.w3.org/2000/09/xmldsig#Object";

    private static final String CSURI = "http://uri.etsi.org/01903#CountersignedSignature";
    private static final String AFIRMA = "AFIRMA";
    private static final String XML_SIGNATURE_PREFIX = "ds";
    private static final String XADES_SIGNATURE_PREFIX = "xades";
    private static final String SIGNATURE_NODE_NAME = XML_SIGNATURE_PREFIX + ":Signature";
    private static final String DETACHED_CONTENT_ELEMENT_NAME = "CONTENT";
    private static final String DETACHED_STYLE_ELEMENT_NAME = "STYLE";

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    private static final String DIGEST_METHOD = DigestMethod.SHA1;

    private String algo;
    private Document doc;

    // Esta variable solo aplica a XAdES 1.3.2, esta deprecada en XAdES1.4.1
    private String dataObjectFormatDescription = null;
    private ObjectIdentifier objectIdentifier = null;

    private String mimeType = null;
    private String encoding = null;

    static {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                if (Platform.getJavaVersion().equals(Platform.JREVER.J5)) {
                    try {
                        Security.addProvider(new org.jcp.xml.dsig.internal.dom.XMLDSigRI());
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma")
                              .warning("No se ha podido agregar el proveedor de firma XMLDSig necesario para firmas XML: " + e);
                    }
                }
                return null;
            }
        });
    }

    public byte[] sign(final byte[] data, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        // Algoritmos de firma con nombres alternativos
        if (algorithm.equalsIgnoreCase("RSA")) {
            algorithm = SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) {
            algorithm = SIGN_ALGORITHM_SHA1WITHDSA;
        }

        final String algoUri = SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'");
        }

        if (extraParams == null) {
            extraParams = new Properties();
        }
        final String format = extraParams.getProperty("format", AOConstants.SIGN_FORMAT_XADES_ENVELOPING);
        final String mode = extraParams.getProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);
        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD);
        final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE);
        final String xadesNamespace = extraParams.getProperty("xadesNamespace", XADESNS);
        final boolean ignoreStyleSheets = Boolean.parseBoolean(extraParams.getProperty("ignoreStyleSheets", "true"));
        final boolean avoidBase64Transforms = Boolean.parseBoolean(extraParams.getProperty("avoidBase64Transforms", "false"));
        final boolean headLess = Boolean.parseBoolean(extraParams.getProperty("headLess", "true"));
        final String precalculatedHashAlgorithm = extraParams.getProperty("precalculatedHashAlgorithm");

        URI uri = null;
        try {
            uri = new URI(extraParams.getProperty("uri"));
        }
        catch (final Exception e) {}

        Utils.checkIllegalParams(format, mode, uri, precalculatedHashAlgorithm, true);

        // Un externally detached con URL permite los datos nulos o vacios
        if ((data == null || data.length == 0) && !(format.equals(SIGN_FORMAT_XADES_EXTERNALLY_DETACHED) && uri != null)) {
            throw new AOException("No se han podido leer los datos a firmar");
        }

        // Propiedades del documento XML original
        final Hashtable<String, String> originalXMLProperties = new Hashtable<String, String>();

        // carga el documento xml
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Elemento de datos
        Element dataElement;

        final String contentId = DETACHED_CONTENT_ELEMENT_NAME + "-" + UUID.randomUUID().toString();
        final String styleId = DETACHED_STYLE_ELEMENT_NAME + "-" + UUID.randomUUID().toString();
        boolean isBase64 = false;
        boolean wasEncodedToBase64 = false;

        // Elemento de estilo
        Element styleElement = null;
        String styleType = null;
        String styleHref = null;
        String styleEncoding = null;

        if (mode.equals(SIGN_MODE_IMPLICIT)) {
            try {

                // Obtenemos el objeto XML
                final Document docum = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(data));

                // Obtenemos la hoja de estilo del XML
                try {
                    Properties p;
                    if (!ignoreStyleSheets) {
                        p = Utils.getStyleSheetHeader(new String(data));
                    }
                    else {
                        p = new Properties();
                    }
                    styleType = p.getProperty("type");
                    styleHref = p.getProperty("href");

                    if (styleType != null && styleHref != null) {

                        Logger.getLogger("es.gob.afirma").info("Se ha encontrado una hoja de estilo asociada al XML a firmar: tipo=" + styleType
                                                               + ", referencia="
                                                               + styleHref);

                        Logger.getLogger("es.gob.afirma").info("Dereferenciando la hoja de estilo");
                        try {
                            final Document tmpDoc =
                                    Utils.dereferenceStyleSheet(TransformerFactory.newInstance()
                                                                                  .getAssociatedStylesheet(new StreamSource(new ByteArrayInputStream(data)),
                                                                                                           null,
                                                                                                           null,
                                                                                                           null)
                                                                                  .getSystemId(),
                                                                headLess);

                            // Cuidado!! Solo rellenamos el Elemento DOM si no
                            // es HTTP o HTTPS, porque si es accesible
                            // remotamente no necesito el elemento, ya que se
                            // firma via referencia Externally Detached
                            if (!styleHref.startsWith("http://") && !styleHref.startsWith("https://")) {
                                styleElement = tmpDoc.getDocumentElement();
                            }

                            styleEncoding = tmpDoc.getXmlEncoding();
                        }
                        catch (final IsInnerlException ex) {
                            Logger.getLogger("es.gob.afirma")
                                  .info("La hoja de estilo esta referenciada internamente, por lo que no se necesita dereferenciar");
                        }
                        catch (final ReferenceIsNotXMLException ex) {
                            Logger.getLogger("es.gob.afirma")
                                  .warning("La hoja de estilo referenciada no es XML o no se ha dereferenciado apropiadamente");
                        }
                        catch (final CannotDereferenceException ex) {
                            Logger.getLogger("es.gob.afirma")
                                  .warning("La hoja de estilo no ha podido dereferenciar, probablemente sea un enlace relativo local");
                        }
                        catch (final Exception ex) {
                            Logger.getLogger("es.gob.afirma").severe("Error intentando dereferenciar la hoja de estilo: " + ex);
                        }
                    }
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").info("No se ha encontrado ninguna hoja de estilo asociada al XML a firmar");
                }

                // Si no hay asignado un MimeType o es el por defecto
                // establecemos el de XML
                if (mimeType == null || DEFAULT_MIMETYPE.equals(mimeType)) {
                    mimeType = "text/xml";
                }

                // Obtenemos el encoding del documento original
                if (encoding == null) {
                    encoding = docum.getXmlEncoding();
                }

                // Hacemos la comprobacion del base64 por si se establecido
                // desde fuera
                if (encoding != null && !AOConstants.BASE64_ENCODING.equals(encoding)) {
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

                if (format.equals(SIGN_FORMAT_XADES_DETACHED)) {
                    dataElement = docum.createElement(DETACHED_CONTENT_ELEMENT_NAME);
                    dataElement.setAttributeNS(null, "Id", contentId);
                    dataElement.setAttributeNS(null, "MimeType", mimeType);
                    dataElement.setAttributeNS(null, "Encoding", encoding);
                    dataElement.appendChild(docum.getDocumentElement());

                    // Tambien el estilo
                    if (styleElement != null) {
                        try {
                            final Element tmpStyleElement = docum.createElement(DETACHED_STYLE_ELEMENT_NAME);
                            tmpStyleElement.setAttributeNS(null, "Id", styleId);
                            if (styleType != null) {
                                tmpStyleElement.setAttributeNS(null, "MimeType", styleType);
                            }
                            tmpStyleElement.setAttributeNS(null, "Encoding", styleEncoding);

                            tmpStyleElement.appendChild(docum.adoptNode(styleElement.cloneNode(true)));

                            styleElement = tmpStyleElement;
                        }
                        catch (final Exception e) {
                            Logger.getLogger("es.gob.afirma")
                                  .warning("No ha sido posible crear el elemento DOM para incluir la hoja de estilo del XML como Internally Detached: " + e);
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
                if (format.equals(SIGN_FORMAT_XADES_ENVELOPED)) {
                    throw new AOFormatFileException("El modo Enveloped solo permite firmar datos XML");
                }
                // para los formatos de firma internally detached y enveloping
                // se trata de convertir el documento a base64
                Logger.getLogger("es.gob.afirma").info("El documento no es un XML valido. Se convertira a Base64: " + e);

                try {
                    // crea un nuevo nodo xml para contener los datos en base 64
                    final Document docFile = dbf.newDocumentBuilder().newDocument();
                    dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);
                    uri = null;
                    encoding = AOConstants.BASE64_ENCODING;
                    if (mimeType == null) {
                        mimeType = DEFAULT_MIMETYPE;
                    }

                    dataElement.setAttributeNS(null, "Id", contentId);
                    dataElement.setAttributeNS(null, "Encoding", encoding);

                    // Si es base 64, lo firmamos indicando como contenido el
                    // dato pero, ya que puede
                    // poseer un formato particular o caracteres valido pero
                    // extranos para el XML,
                    // realizamos una decodificacion y recodificacion para asi
                    // homogenizar el formato.
                    if (AOUtil.isBase64(data)) {
                        Logger.getLogger("es.gob.afirma").info("El documento se considera Base64, se insertara como tal en el XML");

                        // Adicionalmente, si es un base 64 intentamos obtener
                        // el tipo del contenido
                        // decodificado para asi reestablecer el MimeType.
                        final byte[] decodedData = AOCryptoUtil.decodeBase64(data);
                        final MimeHelper mimeTypeHelper = new MimeHelper(decodedData);
                        final String tempMimeType = mimeTypeHelper.getMimeType();
                        mimeType = tempMimeType != null ? tempMimeType : DEFAULT_MIMETYPE;
                        dataElement.setAttributeNS(null, "MimeType", mimeType);

                        dataElement.setTextContent(AOCryptoUtil.encodeBase64(decodedData, true));
                    }
                    else {
                        Logger.getLogger("es.gob.afirma")
                              .info("El documento se considera binario, se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion");

                        // Usamos el MimeType identificado
                        dataElement.setAttributeNS(null, "MimeType", mimeType);

                        dataElement.setTextContent(AOCryptoUtil.encodeBase64(data, true));
                        wasEncodedToBase64 = true;
                    }
                    isBase64 = true;
                }
                catch (final Exception ex) {
                    throw new AOException("Error al convertir los datos a base64", ex);
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

                final byte[] tmpData;
                try {
                    tmpData = AOUtil.getDataFromInputStream(AOUtil.loadFile(uri, null, false));
                }
                catch (final Exception e) {
                    throw new AOException("No se han podido obtener los datos de la URI externa '" + uri + "'", e);
                }
                // Vemos si hemos obtenido bien los datos de la URI
                if (tmpData != null && tmpData.length > 0) {
                    try {
                        digestValue = MessageDigest.getInstance("SHA1").digest(tmpData);
                    }
                    catch (final Exception e) {
                        throw new AOException("No se ha podido obtener el SHA1 de los datos de la URI externa", e);
                    }
                }
            }
            // Si se nos ha introducido el messageDigest, firmamos este como si
            // fuesen los datos
            else if (precalculatedHashAlgorithm != null) {
                digestValue = data;
            }
            // El hash de los datos, ni una URI a traves de la que calcularlos,
            // entonces lo calculamos
            // a traves de los datos introducidos (Siempre se calcula el SHA-1
            // de los datos)
            else {
                try {
                    digestValue = MessageDigest.getInstance("SHA1").digest(data);
                }
                catch (final Exception e) {
                    throw new AOException("No se ha podido obtener el SHA1 de los datos proporcionados", e);
                }
            }

            if (digestValue == null || digestValue.length < 1) {
                throw new AOException("Error al obtener la huella SHA1 de los datos");
            }

            final Document docFile;
            try {
                docFile = dbf.newDocumentBuilder().newDocument();
            }
            catch (final Exception e) {
                throw new AOException("No se ha podido crear el documento XML contenedor", e);
            }
            dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);

            encoding = AOConstants.BASE64_ENCODING;
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
                mimeType = "hash/" + precalculatedHashAlgorithm.toLowerCase();
                hashAlgoUri = AOConstants.MESSAGEDIGEST_ALGOS_URI.get(precalculatedHashAlgorithm.toLowerCase());
            }
            else {
                mimeType = "hash/sha1";
                hashAlgoUri = AOConstants.MESSAGEDIGEST_ALGOS_URI.get("sha1");
            }

            dataElement.setAttributeNS(null, "Id", contentId);
            dataElement.setAttributeNS(null, "MimeType", mimeType);
            dataElement.setAttributeNS(null, "Encoding", encoding);
            if (hashAlgoUri != null) {
                dataElement.setAttributeNS(null, "hashAlgorithm", hashAlgoUri);
            }
            dataElement.setTextContent(AOCryptoUtil.encodeBase64(digestValue, false));
            isBase64 = true;

            // FIN BLOQUE EXPLICITO
        }

        // ***************************************************
        // ***************************************************

        final String tmpUri = "#" + contentId;
        final String tmpStyleUri = "#" + styleId;

        // Crea el nuevo documento de firma
        Document docSignature = null;
        try {
            docSignature = dbf.newDocumentBuilder().newDocument();
            if (format.equals(SIGN_FORMAT_XADES_ENVELOPED)) {
                docSignature.appendChild(docSignature.adoptNode(dataElement));
            }
            else {
                docSignature.appendChild(docSignature.createElement(AFIRMA));
            }
        }
        catch (final Exception e) {
            throw new AOException("Error al crear la firma en formato " + format + ", modo " + mode, e);
        }

        final List<Reference> referenceList = new ArrayList<Reference>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e);
        }
        final String referenceId = "Reference-" + UUID.randomUUID().toString();
        final String referenceStyleId = "StyleReference-" + UUID.randomUUID().toString();

        final List<Transform> transformList = new ArrayList<Transform>();

        // Primero anadimos las transformaciones a medida
        Utils.addCustomTransforms(transformList, extraParams, XML_SIGNATURE_PREFIX);

        // Solo canonicalizo si es XML
        if (!isBase64) {
            try {
                // Transformada para la canonicalizacion inclusiva
                transformList.add(fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null));
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma")
                      .severe("No se puede encontrar el algoritmo de canonicalizacion, la referencia no se canonicalizara: " + e);
            }
        }
        // Si no era XML y tuve que convertir a Base64 yo mismo declaro la
        // transformacion
        else if (wasEncodedToBase64 && !avoidBase64Transforms) {
            try {
                transformList.add(fac.newTransform(Transform.BASE64, (TransformParameterSpec) null));
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe("No se puede encontrar el algoritmo transformacion Base64, esta no se declarara: " + e);
            }
        }

        // crea una referencia al documento insertado en un nodo Object para la
        // firma enveloping y a el estilo
        XMLObject envelopingObject = null;
        XMLObject envelopingStyleObject = null;

        if (format.equals(SIGN_FORMAT_XADES_ENVELOPING)) {
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

                final String objectId = "Object-" + UUID.randomUUID().toString();
                envelopingObject = fac.newXMLObject(structures, objectId, mimeType, encoding);

                // crea la referencia al nuevo elemento Object
                referenceList.add(fac.newReference("#" + objectId, digestMethod, transformList, OBJURI, referenceId));

                // Vamos con la hoja de estilo
                if (styleElement != null) {
                    final String objectStyleId = "StyleObject-" + UUID.randomUUID().toString();
                    envelopingStyleObject =
                            fac.newXMLObject(Collections.singletonList(new DOMStructure(styleElement)), objectStyleId, styleType, styleEncoding);
                    referenceList.add(fac.newReference("#" + objectStyleId,
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       OBJURI,
                                                       referenceStyleId));

                }
            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato enveloping", e);
            }

            // Hojas de estilo para enveloping en Externally Detached
            if (styleHref != null && styleElement == null) {
                // Comprobamos si la referencia al estilo es externa
                if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
                    try {
                        referenceList.add(fac.newReference(styleHref,
                                                           digestMethod,
                                                           Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                      (TransformParameterSpec) null)),
                                                           null,
                                                           referenceStyleId));
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.agob.afirma")
                              .severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e);
                    }
                }
            }

        }

        // crea una referencia al documento mediante la URI hacia el
        // identificador del nodo CONTENT
        else if (format.equals(SIGN_FORMAT_XADES_DETACHED)) {
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
                throw new AOException("Error al generar la firma en formato detached implicito", e);
            }

            // Hojas de estilo remotas para detached
            if (styleHref != null && styleElement == null) {
                // Comprobamos si la referencia al estilo es externa
                if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
                    try {
                        referenceList.add(fac.newReference(styleHref,
                                                           digestMethod,
                                                           Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                      (TransformParameterSpec) null)),
                                                           null,
                                                           referenceStyleId));
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.agob.afirma")
                              .severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e);
                    }
                }
            }

        }

        // Crea una referencia al documento mediante la URI externa si la
        // tenemos o usando un Message Digest
        // precalculado si no tenemos otro remedio
        else if (format.equals(SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
            Reference ref = null;
            // No tenemos uri, suponemos que los datos son el message digest
            if (precalculatedHashAlgorithm != null && (uri == null || uri.getScheme().equals("") || uri.getScheme().equals("file"))) {
                DigestMethod dm = null;
                try {
                    // Convertimos el algo del Message Digest externo a la
                    // nomenclatura XML
                    if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA1")) {
                        dm =
                                fac.newDigestMethod(DigestMethod.SHA1, null);
                    }
                    else if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-256")) {
                        dm =
                                fac.newDigestMethod(DigestMethod.SHA256, null);
                    }
                    else if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-512")) {
                        dm =
                                fac.newDigestMethod(DigestMethod.SHA512, null);
                    }
                    else if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("RIPEMD160")) {
                        dm =
                                fac.newDigestMethod(DigestMethod.RIPEMD160, null);
                    }
                }
                catch (final Exception e) {
                    throw new AOException("No se ha podido crear el metodo de huella digital para la referencia Externally Detached", e);
                }
                if (dm == null) {
                    throw new AOException("Metodo de Message Digest para la referencia Externally Detached no soportado: " + precalculatedHashAlgorithm);
                }
                ref = fac.newReference("", dm, null, null, referenceId, data);
            }
            // Tenemos URI y no nos han establecido algoritmo de message digest,
            // por lo que es una referencia externa accesible
            else {
                // Si es una referencia de tipo file:// obtenemos el fichero y
                // creamos una referencia solo con
                // el message digest
                if (uri != null && uri.getScheme().equals("file")) {
                    try {
                        ref =
                                fac.newReference("",
                                                 digestMethod,
                                                 null,
                                                 null,
                                                 referenceId,
                                                 MessageDigest.getInstance(AOCryptoUtil.getDigestAlgorithmName(digestMethodAlgorithm))
                                                              .digest(AOUtil.getDataFromInputStream(AOUtil.loadFile(uri, null, false))));
                    }
                    catch (final Exception e) {
                        throw new AOException("No se ha podido crear la referencia XML a partir de la URI local (" + uri.toASCIIString() + ")", e);
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
                        throw new AOException("No se ha podido crear la referencia Externally Detached, probablemente por no obtenerse el metodo de digest",
                                              e);
                    }
                }
            }
            if (ref == null) {
                throw new AOException("Error al generar la firma Externally Detached, no se ha podido crear la referencia externa");
            }
            referenceList.add(ref);

            // Hojas de estilo remotas en Externally Detached
            if (styleHref != null && styleElement == null) {
                // Comprobamos que la URL es valida
                if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
                    try {
                        referenceList.add(fac.newReference(styleHref,
                                                           digestMethod,
                                                           Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                      (TransformParameterSpec) null)),
                                                           null,
                                                           referenceStyleId));
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.agob.afirma")
                              .severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e);
                    }
                }
                else {
                    Logger.getLogger("es.gob.afirma")
                               .warning("Se necesita una referencia externa HTTP o HTTPS a la hoja de estilo para referenciarla en firmas XML Externally Detached");
                }
            }

        }

        // crea una referencia indicando que se trata de una firma enveloped
        else if (format.equals(SIGN_FORMAT_XADES_ENVELOPED)) {
            try {

                // Transformacion enveloped
                // La enveloped siempre la primera, para que no se quede sin
                // nodos Signature por haber
                // ejecutado antes otra transformacion
                transformList.add(fac.newTransform(Transform.ENVELOPED, (TransformParameterSpec) null));

                // Transformacion XPATH para eliminar el resto de firmas del
                // documento
                transformList.add(fac.newTransform(Transform.XPATH,
                                                   new XPathFilterParameterSpec("not(ancestor-or-self::" + XML_SIGNATURE_PREFIX + ":Signature)",
                                                                                Collections.singletonMap(XML_SIGNATURE_PREFIX, XMLSignature.XMLNS))));

                // crea la referencia
                referenceList.add(fac.newReference("", digestMethod, transformList, null, referenceId));
            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato enveloped", e);
            }

            // Hojas de estilo remotas para enveloped
            if (styleHref != null && styleElement == null) {
                // Comprobamos si la referencia al estilo es externa
                if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
                    try {
                        referenceList.add(fac.newReference(styleHref,
                                                           digestMethod,
                                                           Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                      (TransformParameterSpec) null)),
                                                           null,
                                                           referenceStyleId));
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.agob.afirma")
                              .severe("No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e);
                    }
                }
            }

        }

        // Instancia XADES_EPES
        final XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(XAdES.EPES, // XAdES
                                                          xadesNamespace, // XAdES NameSpace
                                                          XADES_SIGNATURE_PREFIX, // XAdES Prefix
                                                          XML_SIGNATURE_PREFIX, // XMLDSig Prefix
                                                          digestMethodAlgorithm, // DigestMethod
                                                          docSignature.getDocumentElement() // Element
        );

        // SigningCertificate
        final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
        xades.setSigningCertificate(cert);

        // SignaturePolicyIdentifier
        final SignaturePolicyIdentifier spi =
                getPolicy(extraParams.getProperty("policyIdentifier"),
                          extraParams.getProperty("policyDescription"),
                          extraParams.getProperty("policyQualifier"));
        if (spi != null) {
            xades.setSignaturePolicyIdentifier(spi);
        }

        // SignatureProductionPlace
        final SignatureProductionPlace spp =
                getSignatureProductionPlace(extraParams.getProperty("signatureProductionCity"),
                                            extraParams.getProperty("signatureProductionProvince"),
                                            extraParams.getProperty("signatureProductionPostalCode"),
                                            extraParams.getProperty("signatureProductionCountry"));
        if (spp != null) {
            xades.setSignatureProductionPlace(spp);
        }

        // SignerRole
        SignerRole signerRole = null;
        try {
            final String claimedRole = extraParams.getProperty("signerClaimedRole");
            final String certifiedRole = extraParams.getProperty("signerCertifiedRole");
            signerRole = new SignerRoleImpl();
            if (claimedRole != null) {
                signerRole.addClaimedRole(claimedRole);
            }
            if (certifiedRole != null) {
                signerRole.addCertifiedRole(certifiedRole);
            }
        }
        catch (final Exception e) {}
        if (signerRole != null) {
            xades.setSignerRole(signerRole);
        }

        // SigningTime
        if (Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true"))) {
            xades.setSigningTime(new Date());
        }

        // DataObjectFormat
        final ArrayList<DataObjectFormat> objectFormats = new ArrayList<DataObjectFormat>();
        final DataObjectFormat objectFormat = new DataObjectFormatImpl(
        // TODO: Establecer la variable en XAdES 1.3.2, esta deprecado
        // en XAdES 1.4.1
                                                                 dataObjectFormatDescription,
                                                                 objectIdentifier,
                                                                 mimeType,
                                                                 encoding,
                                                                 "#" + referenceId);

        objectFormats.add(objectFormat);
        xades.setDataObjectFormats(objectFormats);

        final AOXMLAdvancedSignature xmlSignature;
        try {
            xmlSignature = AOXMLAdvancedSignature.newInstance(xades);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido instanciar la firma XML Avanzada de JXAdES", e);
        }
        try {
            xmlSignature.setDigestMethod(digestMethodAlgorithm);
            xmlSignature.setCanonicalizationMethod(canonicalizationAlgorithm);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se ha podido establecer el algoritmo de huella digital (" + algoUri
                                                     + "), es posible que el usado en la firma difiera del indicado: "
                                                     + e);
        }

        // en el caso de formato enveloping se inserta el elemento Object con el
        // documento a firmar
        if (format.equals(SIGN_FORMAT_XADES_ENVELOPING)) {
            xmlSignature.addXMLObject(envelopingObject);
            if (envelopingStyleObject != null) {
                xmlSignature.addXMLObject(envelopingStyleObject);
            }
        }

        // Si es enveloped hay que anadir la hoja de estilo dentro de la firma y
        // referenciarla
        if (format.equals(SIGN_FORMAT_XADES_ENVELOPED)) {
            if (styleElement != null) {
                xmlSignature.addStyleSheetEnvelopingOntoSignature(styleElement, styleType, styleEncoding, styleId);

                try {
                    referenceList.add(fac.newReference(tmpStyleUri,
                                                       digestMethod,
                                                       Collections.singletonList(fac.newTransform(canonicalizationAlgorithm,
                                                                                                  (TransformParameterSpec) null)),
                                                       null,
                                                       referenceStyleId));
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma")
                          .severe("No se ha podido anadir una referencia a la hoja de estilo, esta se incluira dentro de la firma, pero no estara firmada: " + e);
                }
            }
        }

        // genera la firma
        try {
            xmlSignature.sign(cert, keyEntry.getPrivateKey(), algoUri, referenceList, "Signature-" + UUID.randomUUID().toString(), null /* TSA */
            );
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'", e);
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la firma XAdES", e);
        }

        // Si se esta realizando una firma enveloping simple no tiene sentido el
        // nodo raiz,
        // asi que sacamos el nodo de firma a un documento aparte
        if (format.equals(SIGN_FORMAT_XADES_ENVELOPING)) {
            try {
                if (docSignature.getElementsByTagNameNS(DSIGNNS, "Signature").getLength() == 1) {
                    final Document newdoc = dbf.newDocumentBuilder().newDocument();
                    newdoc.appendChild(newdoc.adoptNode(docSignature.getElementsByTagNameNS(DSIGNNS, "Signature").item(0)));
                    docSignature = newdoc;
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").info("No se ha eliminado el nodo padre '<AFIRMA>': " + e);
            }
        }

        encoding = null;
        mimeType = null;

        // Si no es enveloped quito los valores para que no se inserte la
        // cabecera de hoja de estilo
        if (!format.equals(SIGN_FORMAT_XADES_ENVELOPED)) {
            styleHref = null;
            styleType = null;
        }

        return Utils.writeXML(docSignature.getDocumentElement(), originalXMLProperties, styleHref, styleType);

    }

    /** Comprueba si la firma es detached
     * @param element
     *        Elemento que contiene el nodo ra$iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es detached */
    public boolean isDetached(final Element element) {
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
     *        Elemento que contiene el nodo ra$iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es enveloped */
    public boolean isEnveloped(final Element element) {
        final NodeList transformList = element.getElementsByTagNameNS(DSIGNNS, "Transform");
        for (int i = 0; i < transformList.getLength(); i++) {
            if (((Element) transformList.item(i)).getAttribute("Algorithm").equals(Transform.ENVELOPED)) {
                return true;
            }
        }
        return false;
    }

    /** Comprueba si la firma es enveloping
     * @param element
     *        Elemento que contiene el nodo ra$iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es enveloping */
    public boolean isEnveloping(final Element element) {
        if (element.getLocalName().equals("Signature") || (element.getLocalName().equals(AFIRMA) && element.getFirstChild()
                                                                                                           .getLocalName()
                                                                                                           .equals("Signature"))) {
            return true;
        }
        return false;
    }

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
                throw new AOInvalidFormatException("El documento no es un documento de firmas valido.");
            }

            // obtiene la raiz del documento de firmas
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            // si es detached
            if (this.isDetached(rootSig)) {

                final Element firstChild = (Element) rootSig.getFirstChild();
                // si el documento es un xml se extrae como tal
                if (firstChild.getAttribute("MimeType").equals("text/xml")) {
                    elementRes = (Element) firstChild.getFirstChild();
                }
                // Si el MimeType es de tipo Hash (tipo creado para el cliente
                // afirma) asi que la firma no tiene datos
                // else if
                // (firstChild.getAttribute("MimeType").startsWith("hash/")) {
                // elementRes = null;
                // }
                // si el documento es binario se deshace la codificacion en
                // Base64
                else {
                    return AOCryptoUtil.decodeBase64(firstChild.getTextContent());
                }
            }

            // si es enveloped
            else if (this.isEnveloped(rootSig)) {

                // TODO: Revisar si es conveniente eliminar las firmas a traves
                // de transformadas

                // ByteArrayOutputStream baos = new ByteArrayOutputStream();
                // final XMLSignatureFactory fac =
                // XMLSignatureFactory.getInstance("DOM");
                // Transform t = fac.newTransform(
                // Transform.XPATH,
                // new XPathFilterParameterSpec(
                // "not(ancestor-or-self::" + XML_SIGNATURE_PREFIX +
                // ":Signature)",
                // Collections.singletonMap(XML_SIGNATURE_PREFIX,
                // XMLSignature.XMLNS)
                // )
                // );
                //
                // t.transform(new OctetStreamData(new
                // ByteArrayInputStream(sign)), null, baos);
                //
                // return baos.toByteArray();

                // Transformer xmlTranformer =
                // TransformerFactory.newInstance().newTransformer(source);
                // xmlTranformer.transform(new StreamSource(new
                // ByteArrayInputStream(sign)), new StreamResult(baos));
                // elementRes =
                // DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new
                // ByteArrayInputStream(baos.toByteArray());

                // obtiene las firmas y las elimina
                final NodeList signatures = rootSig.getElementsByTagNameNS(DSIGNNS, "Signature");
                for (int i = 0; i < signatures.getLength(); i++) {
                    rootSig.removeChild(signatures.item(0));
                }

                elementRes = rootSig;
            }

            // si es enveloping
            else if (this.isEnveloping(rootSig)) {

                // obtiene el nodo Object de la primera firma
                final Element object = (Element) rootSig.getElementsByTagNameNS(DSIGNNS, "Object").item(0);
                // si el documento es un xml se extrae como tal
                if (object.getAttribute("MimeType").equals("text/xml")) {
                    elementRes = (Element) object.getFirstChild();
                }
                // Si el MimeType es de tipo Hash (tipo creado para el cliente
                // afirma) asi que la firma no tiene datos
                // else if (object.getAttribute("MimeType").startsWith("hash/"))
                // {
                // elementRes = null;
                // }
                // si el documento es binario se deshace la codificacion en
                // Base64
                else {
                    return AOCryptoUtil.decodeBase64(object.getTextContent());
                }
            }
        }
        catch (final Exception ex) {
            throw new AOInvalidFormatException("Error al leer el fichero de firmas", ex);
        }

        // si no se ha recuperado ningun dato se devuelve null
        if (elementRes == null) {
            return null;
        }

        // convierte el documento obtenido en un array de bytes
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        XMLUtils.writeXML(baosSig, elementRes, false);

        return baosSig.toByteArray();
    }

    private SignatureProductionPlace getSignatureProductionPlace(final String city,
                                                                 final String province,
                                                                 final String postalCode,
                                                                 final String country) {
        if (city == null && province == null && postalCode == null && country == null) {
            return null;
        }
        return new SignatureProductionPlaceImpl(city, province, postalCode, country);
    }

    private SignaturePolicyIdentifier getPolicy(final String identifier, final String description, final String qualifier) {
        if (identifier == null) {
            return null;
        }
        final SignaturePolicyIdentifier spi = new SignaturePolicyIdentifierImpl(false);
        try {
            spi.setIdentifier(identifier);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido acceder al identificador ('" + identifier
                                                      + "') de la politica "
                                                      + "de firma, no se anadira este campo");
            return null;
        }
        // FIXME: Error en JXAdES. Si la descripcion es nula toda la firma
        // falla.
        final String desc = (description != null ? description : "");
        spi.setDescription(desc);

        if (qualifier != null) {
            spi.setQualifier(qualifier);
        }
        return spi;
    }

    public void setDataObjectFormat(final String description, final Oid objectIdentifier, final MimeType mimeType, final String encoding) {
        // La descripcion ya no es valida en XAdES 1.3 y 1.4
        // TODO: Habilitar solo para XAdES 1.3
        if (description != null) {
            this.dataObjectFormatDescription = description;
        }
        if (mimeType != null) {
            this.mimeType = mimeType.getBaseType();
        }
        if (encoding != null && (!"".equals(encoding))) {
            this.encoding = encoding;
        }
        if (this.encoding.equalsIgnoreCase("base64")) {
            this.encoding = AOConstants.BASE64_ENCODING;
        }
        if (objectIdentifier != null) {
            this.objectIdentifier = new ObjectIdentifierImpl("OIDAsURN", "urn:oid:" + objectIdentifier, null, new ArrayList<String>(0));
        }
    }

    public byte[] cosign(final byte[] data, final byte[] sign, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        if (algorithm.equalsIgnoreCase("RSA")) {
            algorithm = SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) {
            algorithm = SIGN_ALGORITHM_SHA1WITHDSA;
        }

        final String algoUri = SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'");
        }

        if (extraParams == null) {
            extraParams = new Properties();
        }
        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD);
        final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE);
        final String xadesNamespace = extraParams.getProperty("xadesNamespace", XADESNS);

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Propiedades del documento XML original
        final Hashtable<String, String> originalXMLProperties = new Hashtable<String, String>();

        // carga el documento XML de firmas y su raiz
        Document docSig;
        Element rootSig;
        try {
            docSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
            rootSig = docSig.getDocumentElement();

            // Si el documento contiene una firma simple se inserta como raiz el
            // nodo AFIRMA
            if (rootSig.getNodeName().equals(SIGNATURE_NODE_NAME)) {
                docSig = insertarNodoAfirma(docSig);
                rootSig = docSig.getDocumentElement();
            }
        }
        catch (final ParserConfigurationException pcex) {
            throw new AOException("Formato de documento de firmas incorrecto.", pcex);
        }
        catch (final SAXException saxex) {
            throw new AOException("Formato de documento de firmas incorrecto", saxex);
        }
        catch (final IOException ioex) {
            throw new AOException("Error al leer el documento de firmas", ioex);
        }
        catch (final IllegalArgumentException iaex) {
            throw new AOException("Parametro de entrada incorrecto", iaex);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido leer el documento XML de firmas", e);
        }

        final List<Reference> referenceList = new ArrayList<Reference>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e);
        }

        // Localizamos la primera firma (primer nodo "Signature") en profundidad
        // en el arbol de firma.
        // Se considera que todos los objetos "Signature" del documento firman
        // (referencian) los mismos
        // objetos, por lo que podemos extraerlos de cualquiera de las firmas
        // actuales.
        // Buscamos dentro de ese Signature todas las referencias que apunten a
        // datos para firmarlas
        final Vector<String> referencesIds = new Vector<String>();
        Node currentElement;
        final NodeList nl = ((Element) docSig.getElementsByTagNameNS(DSIGNNS, "Signature").item(0)).getElementsByTagNameNS(DSIGNNS, "Reference");

        // Se considera que la primera referencia de la firma son los datos que
        // debemos firmar, ademas
        // de varias referencias especiales
        for (int i = 0; i < nl.getLength(); i++) {
            currentElement = nl.item(i);

            // Firmamos la primera referencia (que seran los datos firmados) y
            // las hojas de estilo que
            // tenga asignadas. Las hojas de estilo tendran un identificador que
            // comience por "StyleReference-".
            // TODO: Identificar las hojas de estilo de un modo generico.
            final NamedNodeMap currentNodeAttributes = currentElement.getAttributes();
            if (i == 0 || (currentNodeAttributes.getNamedItem("Id") != null && currentNodeAttributes.getNamedItem("Id")
                                                                                                    .getNodeValue()
                                                                                                    .startsWith("StyleReference-"))) {

                // Buscamos las transformaciones declaradas en la Referencia,
                // para anadirlas
                // tambien en la nueva
                final Vector<Transform> currentTransformList;
                try {
                    currentTransformList = Utils.getObjectReferenceTransforms(currentElement, XML_SIGNATURE_PREFIX);
                }
                catch (final NoSuchAlgorithmException e) {
                    throw new AOException("Se ha declarado una transformacion personalizada de un tipo no soportado", e);
                }
                catch (final InvalidAlgorithmParameterException e) {
                    throw new AOException("Se han especificado parametros erroneos para una transformacion personalizada", e);
                }

                // Creamos un identificador de referencia para el objeto a
                // firmar y la almacenamos
                // para mantener un listado con todas. En el caso de las hojas
                // de estilo lo creamos con un
                // identificador descriptivo
                String referenceId = null;
                if ((currentNodeAttributes.getNamedItem("Id") != null && currentNodeAttributes.getNamedItem("Id")
                                                                                              .getNodeValue()
                                                                                              .startsWith("StyleReference-"))) {
                    referenceId = "StyleReference-" + UUID.randomUUID().toString();
                }
                else {
                    referenceId = "Reference-" + UUID.randomUUID().toString();
                }
                referencesIds.add(referenceId);

                // Creamos la propia referencia con las transformaciones de la
                // original
                referenceList.add(fac.newReference(((Element) currentElement).getAttribute("URI"), digestMethod, currentTransformList, // Lista de
                                                                                                                                       // transformaciones
                                                   null,
                                                   referenceId));
            }
        }

        final XAdES_EPES xades =
                (XAdES_EPES) XAdES.newInstance(XAdES.EPES,
                                               xadesNamespace,
                                               XADES_SIGNATURE_PREFIX,
                                               XML_SIGNATURE_PREFIX,
                                               digestMethodAlgorithm,
                                               rootSig);

        // establece el certificado
        final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
        xades.setSigningCertificate(cert);

        // SignaturePolicyIdentifier
        final SignaturePolicyIdentifier spi =
                getPolicy(extraParams.getProperty("policyIdentifier"),
                          extraParams.getProperty("policyDescription"),
                          extraParams.getProperty("policyQualifier"));
        if (spi != null) {
            xades.setSignaturePolicyIdentifier(spi);
        }

        // SignatureProductionPlace
        final SignatureProductionPlace spp =
                getSignatureProductionPlace(extraParams.getProperty("signatureProductionCity"),
                                            extraParams.getProperty("signatureProductionProvince"),
                                            extraParams.getProperty("signatureProductionPostalCode"),
                                            extraParams.getProperty("signatureProductionCountry"));
        if (spp != null) {
            xades.setSignatureProductionPlace(spp);
        }

        // SignerRole
        SignerRole signerRole = null;
        try {
            final String claimedRole = extraParams.getProperty("signerClaimedRole");
            final String certifiedRole = extraParams.getProperty("signerCertifiedRole");
            signerRole = new SignerRoleImpl();
            if (claimedRole != null) {
                signerRole.addClaimedRole(claimedRole);
            }
            if (certifiedRole != null) {
                signerRole.addCertifiedRole(certifiedRole);
            }
        }
        catch (final Exception e) {}

        if (signerRole != null) {
            xades.setSignerRole(signerRole);
        }

        // SigningTime
        if (Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true"))) {
            xades.setSigningTime(new Date());
        }

        // crea la firma
        final AOXMLAdvancedSignature xmlSignature;
        try {
            xmlSignature = AOXMLAdvancedSignature.newInstance(xades);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido instanciar la firma Avanzada XML JXAdES", e);
        }

        try {
            xmlSignature.setDigestMethod(digestMethodAlgorithm);
            xmlSignature.setCanonicalizationMethod(canonicalizationAlgorithm);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se ha podido establecer el algoritmo de huella digital (" + algoUri
                                                     + "), es posible que el usado en la firma difiera del indicado: "
                                                     + e);
        }

        try {
            xmlSignature.sign(cert, keyEntry.getPrivateKey(), algoUri, referenceList, "Signature-" + UUID.randomUUID().toString(), null/*TSA*/);
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'", e);
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la cofirma", e);
        }

        return Utils.writeXML(rootSig, originalXMLProperties, null, null);
    }

    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // carga la raiz del documento XML de firmas
        // y crea un nuevo documento que contendra solo los datos sin firmar
        final Element rootSig;
        final Element rootData;
        try {
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            final Document docData = dbf.newDocumentBuilder().newDocument();
            rootData = (Element) docData.adoptNode(rootSig.cloneNode(true));

            // Obtiene las firmas y las elimina. Para evitar eliminar firmas de
            // las que cuelgan otras
            // y despues intentar eliminar estas, las buscamos y eliminamos de
            // una en una
            NodeList signatures = rootData.getElementsByTagNameNS(DSIGNNS, "Signature");
            while (signatures.getLength() > 0) {
                rootData.removeChild(signatures.item(0));
                signatures = rootData.getElementsByTagNameNS(DSIGNNS, "Signature");
            }

            docData.appendChild(rootData);
        }
        catch (final ParserConfigurationException pcex) {
            throw new AOException("Formato de documento de firmas incorrecto", pcex);
        }
        catch (final SAXException saxex) {
            throw new AOException("Formato de documento de firmas incorrecto", saxex);
        }
        catch (final IOException ioex) {
            throw new AOException("Error al leer el documento de firmas", ioex);
        }
        catch (final IllegalArgumentException iaex) {
            throw new AOException("Parametro de entrada incorrecto", iaex);
        }

        // convierte el documento de firmas en un InputStream
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        XMLUtils.writeXML(baosSig, rootSig, false);

        // convierte el documento a firmar en un InputStream
        final ByteArrayOutputStream baosData = new ByteArrayOutputStream();
        XMLUtils.writeXML(baosData, rootData, false);

        return cosign(baosData.toByteArray(), baosSig.toByteArray(), algorithm, keyEntry, extraParams);
    }

    public byte[] countersign(final byte[] sign,
                              String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              Properties extraParams) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }

        if (sign == null) {
            throw new NullPointerException("El objeto de firma no puede ser nulo");
        }

        if (algorithm.equalsIgnoreCase("RSA")) {
            algorithm = SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) {
            algorithm = SIGN_ALGORITHM_SHA1WITHDSA;
        }

        final String algoUri = SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'");
        }

        this.algo = algorithm;

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // flag que indica si el documento tiene una firma simple o esta
        // cofirmado
        // por defecto se considera que es un documento cofirmado
        boolean esFirmaSimple = false;

        // se carga el documento XML y su raiz
        final Hashtable<String, String> originalXMLProperties = new Hashtable<String, String>();
        Element root;
        try {
            doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));

            // Si no hay asignado un MimeType o es el por defecto establecemos
            // el de XML
            if (mimeType == null || DEFAULT_MIMETYPE.equals(mimeType)) {
                mimeType = "text/xml";
            }
            if (encoding == null) {
                encoding = doc.getXmlEncoding();
            }

            // Ademas del encoding, sacamos otros datos del doc XML original.
            // Hacemos la comprobacion del base64 por si se establecido desde
            // fuera
            if (encoding != null && !AOConstants.BASE64_ENCODING.equals(encoding)) {
                originalXMLProperties.put(OutputKeys.ENCODING, encoding);
            }
            String tmpXmlProp = doc.getXmlVersion();
            if (tmpXmlProp != null) {
                originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
            }
            final DocumentType dt = doc.getDoctype();
            if (dt != null) {
                tmpXmlProp = dt.getSystemId();
                if (tmpXmlProp != null) {
                    originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
                }
            }

            // Nos aseguramos que la configuracion no afectara a operaciones
            // futuras
            encoding = null;
            mimeType = null;

            root = doc.getDocumentElement();

            // si no es un documento cofirma se anade temporalmente el nodo raiz
            // AFIRMA
            // para que las operaciones de contrafirma funcionen correctamente
            if (root.getNodeName().equals(SIGNATURE_NODE_NAME)) {
                esFirmaSimple = true;
                doc = insertarNodoAfirma(doc);
                root = doc.getDocumentElement();
            }
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e);
        }

        try {
            if (targetType == CounterSignTarget.Tree) {
                this.countersignTree(root, keyEntry, extraParams, algorithm);
            }
            else if (targetType == CounterSignTarget.Leafs) {
                this.countersignLeafs(root, keyEntry, extraParams, algorithm);
            }
            else if (targetType == CounterSignTarget.Nodes) {
                this.countersignNodes(root, targets, keyEntry, extraParams, algorithm);
            }
            else if (targetType == CounterSignTarget.Signers) {
                this.countersignSigners(root, targets, keyEntry, extraParams, algorithm);
            }
        }
        catch (final UnsupportedOperationException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la contrafirma", e);
        }

        // si el documento recibido no estaba cofirmado se elimina el nodo raiz
        // temporal AFIRMA
        // y se vuelve a dejar como raiz el nodo Signature original
        if (esFirmaSimple) {
            try {
                final Document newdoc = dbf.newDocumentBuilder().newDocument();
                newdoc.appendChild(newdoc.adoptNode(doc.getElementsByTagNameNS(DSIGNNS, "Signature").item(0)));
                doc = newdoc;
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").info("No se ha eliminado el nodo padre '<AFIRMA>': " + e);
            }
        }

        return Utils.writeXML(doc.getDocumentElement(), originalXMLProperties, null, null);
    }

    /** Realiza la contrafirma de todos los nodos del arbol
     * @param root
     *        Elemento ra&iacute;z del documento xml que contiene las firmas
     * @param algorithm
     *        Algoritmo de firma XML
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    private void countersignTree(final Element root, final PrivateKeyEntry keyEntry, final Properties extraParams, final String algorithm) throws AOException {

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");
        final int numSignatures = signatures.getLength();

        final Element[] nodes = new Element[numSignatures];
        for (int i = 0; i < numSignatures; i++) {
            nodes[i] = (Element) signatures.item(i);
        }

        // y crea sus contrafirmas
        try {
            for (int i = 0; i < numSignatures; i++) {
                this.cs(nodes[i], keyEntry, extraParams, algorithm);
            }
        }
        catch (final UnsupportedOperationException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma del arbol", e);
        }
    }

    /** Realiza la contrafirma de todos los nodos hoja del arbol
     * @param root
     *        Elemento ra&iacute;z del documento xml que contiene las firmas
     * @param algorithm
     *        Algoritmo de firma XML
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    private void countersignLeafs(final Element root, final PrivateKeyEntry keyEntry, final Properties extraParams, final String algorithm) throws AOException {

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");
        int numSignatures = signatures.getLength();

        // comprueba cuales son hojas
        try {
            for (int i = 0; i < numSignatures; i++) {
                final Element signature = (Element) signatures.item(i);
                final int children = signature.getElementsByTagNameNS(DSIGNNS, "Signature").getLength();

                // y crea sus contrafirmas
                if (children == 0) {
                    this.cs(signature, keyEntry, extraParams, algorithm);
                    numSignatures++;
                    i++;
                }
            }
        }
        catch (final UnsupportedOperationException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma de hojas", e);
        }
    }

    /** Realiza la contrafirma de los nodos indicados en el par&aacute;metro
     * targets
     * @param root
     *        Elemento raiz del documento xml que contiene las firmas
     * @param targets
     *        Array con las posiciones de los nodos a contrafirmar
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    private void countersignNodes(final Element root,
                                  Object[] targets,
                                  final PrivateKeyEntry keyEntry,
                                  final Properties extraParams,
                                  final String algorithm) throws AOException {

        // descarta las posiciones que esten repetidas
        final List<Integer> targetsList = new ArrayList<Integer>();
        for (int i = 0; i < targets.length; i++) {
            if (!targetsList.contains(targets[i])) {
                targetsList.add((Integer) targets[i]);
            }
        }
        targets = targetsList.toArray();

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");

        // obtiene los nodos indicados en targets
        final Element[] nodes = new Element[targets.length];
        try {
            for (int i = 0; i < targets.length; i++) {
                nodes[i] = (Element) signatures.item((Integer) targets[i]);
                if (nodes[i] == null) {
                    throw new AOException("Posicion de nodo no valida.");
                }
            }
        }
        catch (final ClassCastException e) {
            throw new AOException("Valor de nodo no valido", e);
        }

        // y crea sus contrafirmas
        try {
            for (final Element node : nodes) {
                this.cs(node, keyEntry, extraParams, algorithm);
            }
        }
        catch (final UnsupportedOperationException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma de nodos", e);
        }
    }

    /** Realiza la contrafirma de los firmantes indicados en el par&aacute;metro
     * targets
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
                                    final Properties extraParams,
                                    final String algorithm) throws AOException {

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");

        final List<Object> signers = Arrays.asList(targets);
        final List<Element> nodes = new ArrayList<Element>();

        // obtiene los nodos de los firmantes indicados en targets
        for (int i = 0; i < signatures.getLength(); i++) {
            final Element node = (Element) signatures.item(i);
            if (signers.contains(AOUtil.getCN(Utils.getCertificate(node.getElementsByTagNameNS(DSIGNNS, "X509Certificate").item(0))))) {
                nodes.add(node);
            }
        }

        // y crea sus contrafirmas
        final Iterator<Element> i = nodes.iterator();
        while (i.hasNext()) {
            this.cs(i.next(), keyEntry, extraParams, algorithm);
        }
    }

    /** Realiza la contrafirma de la firma pasada por par&aacute;metro
     * @param signature
     *        Elemento con el nodo de la firma a contrafirmar
     * @param algorithm
     *        Algoritmo de firma XML
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    private void cs(final Element signature, final PrivateKeyEntry keyEntry, Properties extraParams, final String algorithm) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }
        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD);
        final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE);
        final String xadesNamespace = extraParams.getProperty("xadesNamespace", XADESNS);

        // crea un nodo CounterSignature
        final Element counterSignature = doc.createElement(XADES_SIGNATURE_PREFIX + ":CounterSignature");

        // recupera o crea un nodo UnsignedSignatureProperties
        final NodeList usp = signature.getElementsByTagNameNS(xadesNamespace, "UnsignedSignatureProperties");
        Element unsignedSignatureProperties;
        if (usp.getLength() == 0) {
            unsignedSignatureProperties = doc.createElement(XADES_SIGNATURE_PREFIX + ":UnsignedSignatureProperties");
        }
        else {
            unsignedSignatureProperties = (Element) usp.item(0);
        }

        unsignedSignatureProperties.appendChild(counterSignature);

        // recupera o crea un nodo UnsignedProperties
        final NodeList up = signature.getElementsByTagNameNS(xadesNamespace, "UnsignedProperties");
        final Element unsignedProperties;
        if (up.getLength() == 0) {
            unsignedProperties = doc.createElement(XADES_SIGNATURE_PREFIX + ":UnsignedProperties");
        }
        else {
            unsignedProperties = (Element) up.item(0);
        }

        unsignedProperties.appendChild(unsignedSignatureProperties);

        // inserta el nuevo nodo en QualifyingProperties
        final Node qualifyingProperties = signature.getElementsByTagNameNS(xadesNamespace, "QualifyingProperties").item(0);
        qualifyingProperties.appendChild(unsignedProperties);

        // obtiene el nodo SignatureValue
        final Element signatureValue = (Element) signature.getElementsByTagNameNS(DSIGNNS, "SignatureValue").item(0);

        // crea la referencia a la firma que se contrafirma
        final List<Reference> referenceList = new ArrayList<Reference>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e);
        }
        final String referenceId = "Reference-" + UUID.randomUUID().toString();

        try {
            // Transformada para la canonicalizacion inclusiva con comentarios
            final List<Transform> transformList = new ArrayList<Transform>();
            transformList.add(fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null));

            // Aunque el metodo utilizado para generar las contrafirmas hacen
            // que no sea necesario
            // indicar el tipo de la referencia, lo agregamos por si resultase
            // de utilidad
            referenceList.add(fac.newReference("#" + signatureValue.getAttribute("Id"), digestMethod, transformList, CSURI, referenceId));
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e);
        }

        // nueva instancia XADES_EPES del nodo a contrafirmar
        final XAdES_EPES xades =
                (XAdES_EPES) XAdES.newInstance(XAdES.EPES,
                                               xadesNamespace,
                                               XADES_SIGNATURE_PREFIX,
                                               XML_SIGNATURE_PREFIX,
                                               digestMethodAlgorithm,
                                               counterSignature);

        // establece el certificado
        final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
        xades.setSigningCertificate(cert);

        // SignaturePolicyIdentifier
        final SignaturePolicyIdentifier spi =
                getPolicy(extraParams.getProperty("policyIdentifier"),
                          extraParams.getProperty("policyDescription"),
                          extraParams.getProperty("policyQualifier"));
        if (spi != null) {
            xades.setSignaturePolicyIdentifier(spi);
        }

        // SignatureProductionPlace
        final SignatureProductionPlace spp =
                getSignatureProductionPlace(extraParams.getProperty("signatureProductionCity"),
                                            extraParams.getProperty("signatureProductionProvince"),
                                            extraParams.getProperty("signatureProductionPostalCode"),
                                            extraParams.getProperty("signatureProductionCountry"));
        if (spp != null) {
            xades.setSignatureProductionPlace(spp);
        }

        // SignerRole
        SignerRole signerRole = null;
        try {
            final String claimedRole = extraParams.getProperty("signerClaimedRole");
            final String certifiedRole = extraParams.getProperty("signerCertifiedRole");
            signerRole = new SignerRoleImpl();
            if (claimedRole != null) {
                signerRole.addClaimedRole(claimedRole);
            }
            if (certifiedRole != null) {
                signerRole.addCertifiedRole(certifiedRole);
            }
        }
        catch (final Exception e) {}
        if (signerRole != null) {
            xades.setSignerRole(signerRole);
        }

        // SigningTime
        if (Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true"))) {
            xades.setSigningTime(new Date());
        }

        // crea la firma
        final AOXMLAdvancedSignature xmlSignature;
        try {
            xmlSignature = AOXMLAdvancedSignature.newInstance(xades);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido instanciar la firma Avanzada XML JXAdES", e);
        }
        try {
            xmlSignature.setDigestMethod(digestMethodAlgorithm);
            xmlSignature.setCanonicalizationMethod(canonicalizationAlgorithm);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se ha podido establecer el algoritmo de huella digital (" + SIGN_ALGOS_URI.get(algo)
                                                     + "), es posible que el usado en la firma difiera del indicado: "
                                                     + e);
        }
        try {
            xmlSignature.sign(cert, keyEntry.getPrivateKey(), SIGN_ALGOS_URI.get(algorithm), referenceList, "Signature-" + UUID.randomUUID()
                                                                                                                               .toString(), null /* TSA */
            );
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algo + "'", e);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma", e);
        }
    }

    public TreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {

        // Obtenemos el arbol del documento
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        final Document signDoc;
        try {
            signDoc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("Se ha producido un error al obtener la estructura de firmas: " + e);
            return null;
        }

        // Obtenemos todas las firmas del documento y el SignatureValue de cada
        // una de ellas
        final NodeList signatures = signDoc.getElementsByTagNameNS(DSIGNNS, "Signature");

        // Mantendremos 3 listas: la de identificadores de firma, la de
        // identificadores a las
        // que referencia cada firma (cadena vacia salvo para las contrafirmas)
        // y los objetos
        // con los que representaremos cada uno de los nodos de firma.
        final Vector<String> arrayIds = new Vector<String>();
        final Vector<String> arrayRef = new Vector<String>();
        final Vector<TreeNode> arrayNodes = new Vector<TreeNode>();

        // Rellenamos cada las listas con los datos de las firmas del documento
        for (int i = 0; i < signatures.getLength(); i++) {
            final Element signature = (Element) signatures.item(i);

            // Recogemos el identificador del nodo de firma
            arrayIds.add(signature.getAttribute("Id"));

            // Recogemos los objetos que identificaran a los nodos de firma
            arrayNodes.add(new TreeNode(asSimpleSignInfo ? Utils.getSimpleSignInfoNode(Utils.guessXAdESNamespaceURL(signDoc.getDocumentElement()),
                                                                                       signature) : Utils.getStringInfoNode(signature)));

            // Recogemos el identificador de la firma a la que se referencia (si
            // no es contrafirma sera cadena vacia)
            if (signature.getParentNode().getNodeName().equals(XADES_SIGNATURE_PREFIX + ":CounterSignature")) {
                arrayRef.add(Utils.getCounterSignerReferenceId(signature, signDoc.getElementsByTagNameNS(DSIGNNS, "SignatureValue")));
            }
            else {
                arrayRef.add("");
            }
        }

        // Se crea el que sera el nodo raiz del arbol
        final TreeNode treeRoot = new TreeNode("Datos");

        // Se crea el arbol componiendo las subrama de cada firma directa de los
        // datos
        for (int i = 0; i < arrayRef.size(); i++) {
            if (arrayRef.elementAt(i).equals("")) {
                treeRoot.add(generateSignsTree(i, signatures.getLength() - 1, arrayNodes, arrayIds, arrayRef)[i]);
            }
        }
        return new TreeModel(treeRoot, signatures.getLength());
    }

    /** M&eacute;todo recursivo para la obtenci&oacute;n de la estructura de
     * &aacute;rbol
     * @param i
     *        Inicio de lectura del array de identificadores
     * @param j
     *        Inicio de lectura inversa del array de referencias
     * @param arrayNodes
     *        Array de objetos TreeNode
     * @param arrayIds
     *        Array de identificadores
     * @param arrayRef
     *        Array de referencias
     * @return Array de objetos TreeNode */
    private TreeNode[] generateSignsTree(final int i,
                                         final int j,
                                         final Vector<TreeNode> arrayNodes,
                                         final Vector<String> arrayIds,
                                         final Vector<String> arrayRef) {

        final int max = arrayIds.size();

        if (i < max && j > 0) {
            if (arrayIds.get(i).equals(arrayRef.get(j))) {
                generateSignsTree(i + 1, j - 1, arrayNodes, arrayIds, arrayRef);
            }

            if (i < j) {
                generateSignsTree(i, j - 1, arrayNodes, arrayIds, arrayRef);
            }

            if (!arrayIds.get(i).equals(arrayRef.get(j))) {
                return arrayNodes.toArray(new TreeNode[0]);
            }

            generateSignsTree(j, max - 1, arrayNodes, arrayIds, arrayRef);

            arrayNodes.get(i).add(arrayNodes.get(j));
        }

        return arrayNodes.toArray(new TreeNode[0]);
    }

    public boolean isSign(final byte[] sign) {

        if (sign == null) {
            Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
            return false;
        }

        try {
            // Carga el documento a validar
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);

            // JXades no captura un nodo de firma si se pasa este como raiz del
            // arbol de firmas, asi
            // que nos vemos obligados a crear un nodo padre, del que colgara
            // todo el arbol de firmas,
            // para que lo detecte correctamente
            final Document signDoc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
            final Element rootNode = signDoc.getDocumentElement();

            final ArrayList<Node> signNodes = new ArrayList<Node>();
            if (rootNode.getNodeName().equals(SIGNATURE_NODE_NAME)) {
                signNodes.add(rootNode);
            }

            final NodeList signatures = rootNode.getElementsByTagNameNS(DSIGNNS, "Signature");
            for (int i = 0; i < signatures.getLength(); i++) {
                signNodes.add(signatures.item(i));
            }

            // Si no se encuentran firmas, no es un documento de firma
            if (signNodes.size() == 0 || !checkSignNodes(rootNode, signNodes)) {
                return false;
            }

            // final List<Node> envelopedSig = new ArrayList<Node>();
            //
            // // Se identifican las firmas enveloped del documento
            // for (int i = 0; i < signatures.getLength(); i++) {
            // Element signature = (Element)signatures.item(i);
            // NodeList transforms = signature.getElementsByTagNameNS(DSIGNNS,
            // "Transform");
            // if(((Element)transforms.item(transforms.getLength()-1))
            // .getAttribute("Algorithm").equals(Transform.ENVELOPED))
            // envelopedSig.add(signature);
            // }
            //
            // XAdES_EPES xades;
            // XMLAdvancedSignature xmlSignature;
            // List<SignatureStatus> ssList;
            //
            // // Comprobaremos las firmas enveloped 1 a 1, por eso, si hay mas
            // de una firma, las recorremos
            // // eliminando el resto para cada comprobacion individual
            // if (envelopedSig.size() > 1) {
            // //se eliminan todas las firmas enveloped salvo la que se quiere
            // verificar
            // for (int i = 0; i < envelopedSig.size(); i++) {
            // Node removedChild = null;
            // for (int j = 0; j < envelopedSig.size(); j++) {
            // if (i != j) {
            // removedChild = envelopedSig.get(j);
            // rootNode.removeChild(removedChild);
            // }
            // }
            //
            // //instancia XADES_EPES
            // xades = (XAdES_EPES) XAdES.newInstance(
            // XAdES.EPES,
            // xadesNamespace,
            // XADES_SIGNATURE_PREFIX,
            // XML_SIGNATURE_PREFIX,
            // DIGEST_METHOD,
            // rootNode
            // );
            //
            // // Comprueba si las firmas del documento son validas
            // xmlSignature = XMLAdvancedSignature.newInstance(xades);
            // ssList = xmlSignature.validate();
            //
            // // Si alguna firma fuera erronea devuelve false, en caso
            // contrario es true
            // if (!SignatureStatus.isValid(ssList))
            // return false;
            //
            // //vuelve a incluir la firma eliminada
            // rootNode.appendChild(removedChild);
            // }
            //
            // return true;
            // }
            //
            //
            // // A partir de aqui se comprueban las firmas no enveloped y
            // enveloped simples (sin multifirma)
            //
            // // Instancia XADES_EPES
            // xades = (XAdES_EPES) XAdES.newInstance(
            // XAdES.EPES,
            // xadesNamespace,
            // XADES_SIGNATURE_PREFIX,
            // XML_SIGNATURE_PREFIX,
            // DIGEST_METHOD,
            // rootNode
            // );
            //
            // // Comprueba si las firmas del documento son validas
            // xmlSignature = XMLAdvancedSignature.newInstance(xades);
            // ssList = xmlSignature.validate();
            //
            // // Si alguna firma fuera erronea devuelve false, en caso
            // contrario es true
            // return SignatureStatus.isValid(ssList);
        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }

    /** Comprueba que los nodos de firma proporcionados sean firmas en el formato
     * dato.
     * @param rootNode
     *        Nodo r&iacute;z del documento de firma.
     * @param signNodes
     *        Listado de nodos de firma.
     * @return Devuelve {@code true} cuando todos los nodos sean firmas en este
     *         formato. */
    private boolean checkSignNodes(final Node rootNode, final List<Node> signNodes) {

        final String xadesNamespace = Utils.guessXAdESNamespaceURL(rootNode);
        for (final Node signNode : signNodes) {
            if (((Element) signNode).getElementsByTagNameNS(xadesNamespace, "QualifyingProperties").getLength() == 0) {
                return false;
            }
        }

        return true;
    }

    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
            return false;
        }
        return true;
    }

    public String getSignedName(final String originalName, final String inText) {
        return originalName + (inText != null ? inText : "") + ".xsig";
    }

    /** Devuelve un nuevo documento con ra&iacute;z "AFIRMA" del que cuelga el
     * documento especificado.
     * @param doc
     *        Documento que estar&aacute; contenido en el nuevo documento.
     * @return Documento con ra&iacute;z "AFIRMA".
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

    public AOSignInfo getSignInfo(final byte[] sign) throws AOInvalidFormatException, AOException {
        if (sign == null) {
            throw new NullPointerException("No se han introducido datos para analizar");
        }

        if (!isSign(sign)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
        }

        final AOSignInfo signInfo = new AOSignInfo(AOConstants.SIGN_FORMAT_XADES);

        // Analizamos mas en profundidad la firma para obtener el resto de datos

        // Tomamos la raiz del documento
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Element rootSig = null;
        try {
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("Error al analizar la firma: " + e);
            rootSig = null;
        }

        // Establecemos la variante de firma
        if (rootSig != null) {
            if (isDetached(rootSig)) {
                signInfo.setVariant(AOConstants.SIGN_FORMAT_XADES_DETACHED);
            }
            else if (isEnveloped(rootSig)) {
                signInfo.setVariant(AOConstants.SIGN_FORMAT_XADES_ENVELOPED);
            }
            else if (isEnveloping(rootSig)) {
                signInfo.setVariant(AOConstants.SIGN_FORMAT_XADES_ENVELOPING);
            }
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros
        // datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return signInfo;
    }

    public String getDataMimeType(final byte[] sign) throws AOUnsupportedSignFormatException {

        String mType = null;

        // Si no hay datos a analizar
        if (sign == null) {
            throw new NullPointerException("No se han introducido datos para analizar");
        }

        // Si no es una firma valida
        if (!isSign(sign)) {
            throw new AOUnsupportedSignFormatException("Los datos introducidos no se corresponden con un objeto de firma");
        }

        // Obtiene el documento y su raiz
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Document tmpDoc = null;
        Element rootSig = null;
        try {
            tmpDoc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
            rootSig = tmpDoc.getDocumentElement();
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("Error al analizar la firma: " + e);
            rootSig = null;
        }

        if (rootSig != null) {
            // si es enveloped trata de obtener el MimeType del nodo raiz, que
            // corresponde a los datos
            if (isEnveloped(rootSig)) {
                mType = rootSig.getAttribute("MimeType");
            }
            // si es enveloping
            else if (isEnveloping(rootSig)) {
                // si el documento no tiene como nodo raiz AFIRMA se anade este
                // para que la lectura de las firmas del documento se haga
                // correctamente
                if (rootSig.getNodeName().equals(SIGNATURE_NODE_NAME)) {
                    try {
                        tmpDoc = insertarNodoAfirma(tmpDoc);
                    }
                    catch (final Exception e) {
                        throw new AOUnsupportedSignFormatException("Error al analizar la firma", e);
                    }
                    rootSig = tmpDoc.getDocumentElement();
                }

                // obtiene el nodo de firma y el elemento que contiene los datos
                final NodeList signatures = rootSig.getElementsByTagNameNS(DSIGNNS, "Signature");
                final NodeList objects = ((Element) signatures.item(0)).getElementsByTagNameNS(DSIGNNS, "Object");

                mType = ((Element) objects.item(0)).getAttribute("MimeType");
            }
            // si es detached
            else if (isDetached(rootSig)) {
                mType = ((Element) rootSig.getFirstChild()).getAttribute("MimeType");
            }
        }

        if (mType == null || mType.equals("")) {
            return null;
        }

        return mType;
    }
}
