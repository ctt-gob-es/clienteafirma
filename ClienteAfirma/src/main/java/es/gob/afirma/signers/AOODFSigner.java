/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignatureProperties;
import javax.xml.crypto.dsig.SignatureProperty;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.ietf.jgss.Oid;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.Init;
import com.sun.org.apache.xml.internal.security.c14n.Canonicalizer;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;
import es.gob.afirma.signers.beans.AOSignInfo;
import es.gob.afirma.signers.xmlhelper.Utils;

/** Clase para la firma electr&oacute;nica de ficheros ODF en OpenOffice.org 3 y
 * superiores. Par&aacute;metros adicionales aceptados para las operaciones de
 * firma:<br>
 * <dl>
 * <dt>useOpenOffice31Mode</dt>
 * <dd>Genera firmas compatibles OpenOffice.org 3.1 e inferiores si se establece a <code>true</code>, y firmas compatibles con LibreOffice y
 * OpenOffice 3.2 y superiores si no se establece o se establece a <code>false</code></dd> <!--
 * <dt>referencesDigestMethod</dt>
 * <dd>Algoritmo de huella digital a usar en las referencias XML (en formato URL segun W3C)</dd> -->
 * </dl>
 * @version 0.2 */
public final class AOODFSigner implements AOSigner {

    private static String OPENOFFICE = "urn:oasis:names:tc:opendocument:xmlns:digitalsignature:1.0";

    /** Mimetypes de los formatos ODF soportados. */
    private static final Set<String> supportedFormats;

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    private static final String DIGEST_METHOD = DigestMethod.SHA1;

    static {
        supportedFormats = new HashSet<String>();
        supportedFormats.add("application/vnd.oasis.opendocument.text");
        supportedFormats.add("application/vnd.oasis.opendocument.spreadsheet");
        supportedFormats.add("application/vnd.oasis.opendocument.presentation");
    }

    /** Firma o cofirma un documento OpenOffice de tipo ODT, ODS y ODG.<br/>
     * @param data
     *        Datos a firmar
     * @param algorithm
     *        No necesario. Se utiliza siempre el algoritmo SHA1withRSA
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso
     * @since 1.6.0_10 */
    public byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        if (algorithm != null && !algorithm.equalsIgnoreCase("SHA1withRSA")) {
            Logger.getLogger("es.gob.afirma").warning("Las firmas ODF s\u00F3lo soportan el algoritmo de firma SHA1withRSA");
        }

        if (extraParams == null) {
            extraParams = new Properties();
        }
        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD);
        final boolean useOpenOffice31Mode = "true".equalsIgnoreCase(extraParams.getProperty("useOpenOffice31Mode"));

        if (!isValidDataFile(data)) {
            throw new AOFormatFileException("El fichero introducido no es un documento ODF");
        }

        String fullPath = "META-INF/manifest.xml";
        boolean isCofirm = false;

        try {
            // genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip");
            final FileOutputStream fos = new FileOutputStream(zipFile);
            fos.write(data);
            try {
                fos.flush();
            }
            catch (final Exception e) {}
            try {
                fos.close();
            }
            catch (final Exception e) {}

            // carga el fichero zip
            final ZipFile zf = new ZipFile(zipFile);

            // obtiene el archivo manifest.xml, que indica los ficheros que
            // contiene el ODF
            final InputStream manifest = zf.getInputStream(zf.getEntry(fullPath));
            final byte[] manifestData = AOUtil.getDataFromInputStream(manifest);

            if (manifest != null) try {
                manifest.close();
            }
            catch (final Exception t) {}

            // obtiene el documento manifest.xml y su raiz
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            final Document docManifest = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(manifestData));
            final Element rootManifest = docManifest.getDocumentElement();

            // recupera todos los nodos de manifest.xml
            final NodeList listFileEntry = rootManifest.getElementsByTagName("manifest:file-entry");

            //
            // Datos necesarios para la firma
            //

            // MessageDigest
            final MessageDigest md = MessageDigest.getInstance("SHA1");

            // XMLSignatureFactory
            final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");

            // DigestMethod
            DigestMethod dm;
            try {
                dm = fac.newDigestMethod(digestMethodAlgorithm, null);
            }
            catch (Exception e) {
                if (DIGEST_METHOD.equals(digestMethodAlgorithm)) {
                    throw new AOException("No se ha podido obtener un generador de huellas digitales", e);
                }
                Logger.getLogger("es.gob.afirma")
                      .warning("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm
                               + "', se intentara con el algoritmo por defecto '"
                               + DIGEST_METHOD
                               + "': "
                               + e);
                dm = fac.newDigestMethod(DIGEST_METHOD, null);
            }

            // Transforms
            final List<Transform> transformList = new ArrayList<Transform>(1);
            transformList.add(fac.newTransform(Canonicalizer.ALGO_ID_C14N_OMIT_COMMENTS, (TransformParameterSpec) null));

            // References
            final List<Reference> referenceList = new ArrayList<Reference>();

            Init.init();

            final Canonicalizer canonicalizer = Canonicalizer.getInstance(Canonicalizer.ALGO_ID_C14N_OMIT_COMMENTS);

            //
            // Anadimos tambien referencias manualmente al propio manifest.xml y
            // al mimetype
            //

            // manifest tiene una canonicalizacion. Solo en OOo 3.2 y superiores
            if (!useOpenOffice31Mode) {

                // mimetype es una referencia simple, porque no es XML
                referenceList.add(fac.newReference("mimetype", dm, null, null, null, md.digest(AOUtil.getDataFromInputStream(
                // Recupera el fichero
                zf.getInputStream(zf.getEntry("mimetype"))))));

                referenceList.add(fac.newReference("META-INF/manifest.xml",
                                                   dm,
                                                   transformList,
                                                   null,
                                                   null,
                                                   md.digest(canonicalizer.canonicalizeSubtree(
                                                   // Recupera el fichero y su raiz
                                                   dbf.newDocumentBuilder().parse(new ByteArrayInputStream(manifestData)).getDocumentElement()))));
            }

            // para cada nodo de manifest.xml
            Reference reference;
            for (int i = 0; i < listFileEntry.getLength(); i++) {
                fullPath = ((Element) listFileEntry.item(i)).getAttribute("manifest:full-path");

                // si es un archivo
                if (!fullPath.endsWith("/")) {

                    // y es uno de los siguientes archivos xml
                    if (fullPath.equals("content.xml") || fullPath.equals("meta.xml")
                        || fullPath.equals("styles.xml")
                        || fullPath.equals("settings.xml")) {

                        // crea la referencia
                        reference = fac.newReference(fullPath.replaceAll(" ", "%20"), dm, transformList, null, null,
                        // Obtiene su forma canonica y su DigestValue
                                                     md.digest(canonicalizer.canonicalizeSubtree(
                                                     // Recupera el fichero y su raiz
                                                     dbf.newDocumentBuilder().parse(zf.getInputStream(zf.getEntry(fullPath))).getDocumentElement())));

                    }

                    // si no es uno de los archivos xml
                    else {

                        // crea la referencia
                        reference = fac.newReference(fullPath.replaceAll(" ", "%20"), dm, null, null, null, md.digest(AOUtil.getDataFromInputStream(
                        // Recupera el fichero
                        zf.getInputStream(zf.getEntry(fullPath)))));

                    }

                    // si no se trata del documento de firmas se anade la
                    // referencia
                    if (!fullPath.equals("META-INF/documentsignatures.xml")) {
                        referenceList.add(reference);
                    }

                    // si existe el documento de firmas, entonces sera una
                    // cofirma.
                    else isCofirm = true;
                }
            }

            final Document docSignatures;
            final Element rootSignatures;
            // si es cofirma
            if (isCofirm) {
                // recupera el documento de firmas y su raiz
                docSignatures = dbf.newDocumentBuilder().parse(zf.getInputStream(zf.getEntry("META-INF/documentsignatures.xml")));
                rootSignatures = docSignatures.getDocumentElement();
            }
            else {
                // crea un nuevo documento de firmas
                docSignatures = dbf.newDocumentBuilder().newDocument();
                rootSignatures = docSignatures.createElement("document-signatures");
                rootSignatures.setAttribute("xmlns", OPENOFFICE);
                docSignatures.appendChild(rootSignatures);

                // En OpenOffice 3.2 y superiores no anadimos la propia firma al
                // manifest
                // para evitar referencias circulares
                if (useOpenOffice31Mode) {
                    final Element nodeDocumentSignatures = docManifest.createElement("manifest:file-entry");
                    nodeDocumentSignatures.setAttribute("manifest:media-type", "");
                    nodeDocumentSignatures.setAttribute("manifest:full-path", "META-INF/documentsignatures.xml");
                    rootManifest.appendChild(nodeDocumentSignatures);

                    // nuevo elemento de META-INF
                    final Element nodeMetaInf = docManifest.createElement("manifest:file-entry");
                    nodeMetaInf.setAttribute("manifest:media-type", "");
                    nodeMetaInf.setAttribute("manifest:full-path", "META-INF/");
                    rootManifest.appendChild(nodeMetaInf);
                }
            }

            // Ids de Signature y SignatureProperty
            final String signatureId = UUID.randomUUID().toString();
            final String signaturePropertyId = UUID.randomUUID().toString();

            // referencia a SignatureProperty
            referenceList.add(fac.newReference("#" + signaturePropertyId, dm));

            // contenido de SignatureProperty
            final Element content = docSignatures.createElement("dc:date");
            content.setAttribute("xmlns:dc", "http://purl.org/dc/elements/1.1/");
            content.setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss,SS").format(new Date()));
            final List<XMLStructure> contentList = new ArrayList<XMLStructure>();
            contentList.add(new DOMStructure(content));

            // SignatureProperty
            final List<SignatureProperty> spList = new ArrayList<SignatureProperty>();
            spList.add(fac.newSignatureProperty(contentList, "#" + signatureId, signaturePropertyId));

            // SignatureProperties
            final List<SignatureProperties> spsList = new ArrayList<SignatureProperties>();
            spsList.add(fac.newSignatureProperties(spList, null));

            // Object
            final List<XMLObject> objectList = new ArrayList<XMLObject>();
            objectList.add(fac.newXMLObject(spsList, null, null, null));

            /*
             * Si se solicito una firma explicita, advertimos no son compatibles
             * con ODF y se ignorara esta configuracion
             */
            if (extraParams.containsKey("mode") && extraParams.getProperty("mode").equals(AOConstants.SIGN_MODE_EXPLICIT)) {
                Logger.getLogger("es.gob.afirma")
                      .warning("El formato de firma ODF no soporta el modo de firma explicita, " + "se ignorara esta configuracion");
            }

            // Preparamos el KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final List<Object> x509Content = new ArrayList<Object>();
            final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
            x509Content.add(cert.getSubjectX500Principal().getName());
            x509Content.add(cert);

            // genera la firma
            fac.newXMLSignature(
            // SignedInfo
            fac.newSignedInfo(
            // CanonicalizationMethod
            fac.newCanonicalizationMethod(CanonicalizationMethod.INCLUSIVE, (C14NMethodParameterSpec) null),
                              // SignatureMethod
                              fac.newSignatureMethod(SignatureMethod.RSA_SHA1, null),
                              referenceList),
                                // KeyInfo
                                kif.newKeyInfo(Collections.singletonList(kif.newX509Data(x509Content)), null),
                                objectList,
                                signatureId,
                                null).sign(new DOMSignContext(keyEntry.getPrivateKey(), rootSignatures));

            // crea un nuevo fichero zip
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final ZipOutputStream zos = new ZipOutputStream(baos);

            // copia el contenido del zip original en el nuevo excepto el
            // documento de firmas y manifest.xml
            final Enumeration<? extends ZipEntry> e = zf.entries();
            ZipEntry ze;
            ZipEntry zeOut;
            while (e.hasMoreElements()) {
                ze = e.nextElement();
                zeOut = new ZipEntry(ze.getName());
                if (!ze.getName().equals("META-INF/documentsignatures.xml") && !ze.getName().equals("META-INF/manifest.xml")) {
                    zos.putNextEntry(zeOut);
                    zos.write(AOUtil.getDataFromInputStream(zf.getInputStream(ze)));
                }
            }

            // anade el documento de firmas
            zos.putNextEntry(new ZipEntry("META-INF/documentsignatures.xml"));
            final ByteArrayOutputStream baosXML = new ByteArrayOutputStream();
            writeXML(baosXML, rootSignatures, false);
            zos.write(baosXML.toByteArray());
            zos.closeEntry();

            // anade manifest.xml
            zos.putNextEntry(new ZipEntry("META-INF/manifest.xml"));
            final ByteArrayOutputStream baosManifest = new ByteArrayOutputStream();
            writeXML(baosManifest, rootManifest, false);
            zos.write(baosManifest.toByteArray());
            zos.closeEntry();

            try {
                zos.close();
            }
            catch (final Exception t) {}

            return baos.toByteArray();

        }
        catch (final IOException ioex) {
            throw new AOFormatFileException("No es posible abrir el fichero. " + fullPath + ": " + ioex);
        }
        catch (final SAXException saxex) {
            throw new AOFormatFileException("Estructura de archivo no valida: " + fullPath + ": " + saxex);
        }
        catch (final Exception e) {
            throw new AOException("No ha sido posible generar la firma ODF", e);
        }

    }

    public byte[] cosign(final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }

    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }

    /** M&eacute;todo no implementado. No es posible realizar contrafirmas de
     * documentos ODF. Lanza una <code>UnsupportedOperationException</code>. */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros ODF");
    }

    public TreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {

        try {
            // genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip");
            final FileOutputStream fos = new FileOutputStream(zipFile);
            fos.write(sign);
            try {
                fos.flush();
            }
            catch (final Exception e) {}
            try {
                fos.close();
            }
            catch (final Exception e) {}

            // carga el fichero zip
            final ZipFile zf = new ZipFile(zipFile);

            // obtiene el archivo de firmas
            final InputStream signIs = zf.getInputStream(zf.getEntry("META-INF/documentsignatures.xml"));

            // recupera la raiz del documento de firmas
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            final Element root = dbf.newDocumentBuilder().parse(signIs).getDocumentElement();

            // obtiene todas las firmas
            final NodeList signatures = root.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Signature");

            int numSignatures = signatures.getLength();

            final String[] arrayIds = new String[numSignatures];
            final String[] arrayRef = new String[numSignatures];
            final TreeNode[] arrayNodes = new TreeNode[numSignatures];

            for (int i = 0; i < numSignatures; i++) {
                final Element signature = (Element) signatures.item(i);
                final String sigId = signature.getAttribute("Id");

                final String strCert = signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "X509Certificate").item(0).getTextContent();
                TreeNode node;

                if (asSimpleSignInfo) {
                    node = new TreeNode(Utils.getSimpleSignInfoNode(null, signature));
                }
                else {
                    node = new TreeNode(AOUtil.getCN(AOCryptoUtil.createCert(strCert)));
                }
                arrayIds[i] = sigId;
                arrayNodes[i] = node;

                final String typeReference =
                        ((Element) signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Reference").item(0)).getAttribute("Type");
                if (typeReference.equals("http://uri.etsi.org/01903#CountersignedSignature")) {
                    final String uri =
                            ((Element) signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Reference").item(0)).getAttribute("URI");
                    arrayRef[i] = uri.substring(1, uri.length() - 5);
                }
                else {
                    arrayRef[i] = "";
                }
            }

            final TreeNode tree = new TreeNode("Datos");

            for (int i = numSignatures - 1; i > 0; i--)
                for (int j = 0; j < numSignatures; j++) {
                    if (arrayRef[i].equals(arrayIds[j])) {
                        arrayNodes[j].add(arrayNodes[i]);
                    }
                }

            for (int i = 0; i < numSignatures; i++) {
                if (arrayRef[i] == "") {
                    tree.add(arrayNodes[i]);
                }
            }

            try {
                zipFile.delete();
            }
            catch (final Exception e) {}

            return new TreeModel(tree, tree.getChildCount());
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("Se ha producido un error al obtener la estructura de firmas: " + e);
            return new TreeModel(new TreeNode("Ra\u00EDz"));
        }
    }

    /** Indica si el fichero indicado es una firma v&aacute;lida del tipo de
     * signer concreto. En el caso concreto del formato ODF, un documento, este
     * firmado o no, puede considerarse una firma ya que es apto para ser
     * cofirmado y, de cualquier forma, la contrafirma no es posible sobre este
     * tipo de fichero.
     * @param signData
     *        Firma que deseamos comprobar.
     * @return Devuelve <code>true</code> si el fichero es una firma reconocida
     *         por este signer, <code>false</code> en caso contrario. */
    public boolean isSign(final byte[] signData) {
        return isValidDataFile(signData);
    }

    public boolean isValidDataFile(final byte[] data) {

        // Si el mimetype del fichero no se ajusta a alguno de los MimeTypes
        // soportados
        // para firma ODF se lanzara una excepcion, en ese caso deducimos que no
        // es un
        // fichero valido
        String mimetype = null;
        try {
            mimetype = this.getODFMimeType(data);
        }
        catch (Exception e) {
            return false;
        }

        // Sera valido si el mimetype coincide con alguno de los formatos ODF
        // soportados
        return mimetype != null && supportedFormats.contains(mimetype);
    }

    public String getSignedName(final String originalName, final String inText) {

        final String inTextInt = (inText != null ? inText : "");

        if (originalName == null) {
            return inTextInt + ".odf";
        }
        final String originalNameLC = originalName.toLowerCase();
        if (originalNameLC.length() <= 4) {
            return originalName + inTextInt + ".odf";
        }
        if (originalNameLC.endsWith(".odt")) {
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".odt";
        }
        if (originalNameLC.endsWith(".odp")) {
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".odp";
        }
        if (originalNameLC.endsWith(".ods")) {
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".ods";
        }
        return originalName + inTextInt + ".odf";
    }

    /** M&eacute;todo no implementado. */
    public void setDataObjectFormat(final String description, final Oid objectIdentifier, final javax.activation.MimeType mimeType, final String encoding) {}

    private static void writeXML(final OutputStream outStream, final Node node, final boolean indent) {
        writeXML(new BufferedWriter(new OutputStreamWriter(outStream, Charset.forName("UTF-8"))), node, indent);
    }

    private static void writeXML(final Writer writer, final Node node, final boolean indent) {
        try {
            Transformer serializer = TransformerFactory.newInstance().newTransformer();
            serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");

            if (indent) {
                serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            }
            serializer.transform(new DOMSource(node), new StreamResult(writer));
        }
        catch (Exception ex) {
            Logger.getLogger("es.gob.afirma").severe("Error al escribir el cuerpo del XML: " + ex);
        }
    }

    public byte[] getData(final byte[] signData) throws AOInvalidFormatException {

        // Si no es una firma ODF valida, lanzamos una excepcion
        if (!isSign(signData)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida");
        }

        // TODO: Por ahora, devolveremos el propio ODF firmado.
        return signData;
    }

    public AOSignInfo getSignInfo(final byte[] signData) throws AOInvalidFormatException, AOException {
        if (signData == null) {
            throw new NullPointerException("No se han introducido datos para analizar");
        }
        if (!isSign(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
        }
        return new AOSignInfo(AOConstants.SIGN_FORMAT_ODF);
    }

    public String getDataMimeType(final byte[] signData) throws AOUnsupportedSignFormatException {
        final String mimetype = getODFMimeType(signData);
        if (mimetype == null || !supportedFormats.contains(mimetype)) {
            throw new AOUnsupportedSignFormatException("La firma introducida no es un documento ODF");
        }
        return mimetype;
    }

    private String getODFMimeType(byte[] signData) {
        String mimetype = null;
        try {
            // Genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip");
            final FileOutputStream fos = new FileOutputStream(zipFile);

            fos.write(signData);

            try {
                fos.flush();
            }
            catch (final Exception e) {}
            try {
                fos.close();
            }
            catch (final Exception e) {}

            // carga el fichero zip
            final ZipFile zf;
            try {
                zf = new ZipFile(zipFile);
            }
            catch (ZipException e) {
                // Si detectamos que no es un fichero Zip, devolvemos null
                return null;
            }

            // obtiene el archivo mimetype
            final ZipEntry entry = zf.getEntry("mimetype");
            if (entry != null) {
                mimetype = new String(AOUtil.getDataFromInputStream(zf.getInputStream(entry)));
            }

            try {
                zipFile.delete();
            }
            catch (Exception e) {}

        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error al analizar el fichero de firma: " + e);
            return null;
        }
        return mimetype;
    }

}
