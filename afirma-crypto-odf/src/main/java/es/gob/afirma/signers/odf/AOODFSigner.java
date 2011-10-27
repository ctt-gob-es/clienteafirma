/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.odf;

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

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.Init;
import com.sun.org.apache.xml.internal.security.c14n.Canonicalizer;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;


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
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static String OPENOFFICE = "urn:oasis:names:tc:opendocument:xmlns:digitalsignature:1.0"; //$NON-NLS-1$

    /** Mimetypes de los formatos ODF soportados. */
    private static final Set<String> SUPPORTED_FORMATS;

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    private static final String DIGEST_METHOD = DigestMethod.SHA1;

    static {
        SUPPORTED_FORMATS = new HashSet<String>();
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.text"); //$NON-NLS-1$
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.spreadsheet"); //$NON-NLS-1$
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.presentation"); //$NON-NLS-1$
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
    public byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, final Properties xParams) throws AOException {

        if (algorithm != null && !algorithm.equalsIgnoreCase("SHA1withRSA")) { //$NON-NLS-1$
            LOGGER.warning("Las firmas ODF s\u00F3lo soportan el algoritmo de firma SHA1withRSA"); //$NON-NLS-1$
        }

        final Properties extraParams = (xParams != null) ? xParams : new Properties();
        
        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD); //$NON-NLS-1$
        final boolean useOpenOffice31Mode = "true".equalsIgnoreCase(extraParams.getProperty("useOpenOffice31Mode")); //$NON-NLS-1$ //$NON-NLS-2$

        if (!isValidDataFile(data)) {
            throw new AOFormatFileException("El fichero introducido no es un documento ODF"); //$NON-NLS-1$
        }

        String fullPath = "META-INF/manifest.xml"; //$NON-NLS-1$
        boolean isCofirm = false;

        try {
            // genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
            final FileOutputStream fos = new FileOutputStream(zipFile);
            fos.write(data);
            try {
                fos.flush();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el vaciado
            }
            try {
                fos.close();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el cierre
            }

            // carga el fichero zip
            final ZipFile zf = new ZipFile(zipFile);

            // obtiene el archivo manifest.xml, que indica los ficheros que
            // contiene el ODF
            final InputStream manifest = zf.getInputStream(zf.getEntry(fullPath));
            final byte[] manifestData = AOUtil.getDataFromInputStream(manifest);

            if (manifest != null) {
                try {
                    manifest.close();
                }
                catch (final Exception t) {
                 // Ignoramos los errores en el cierre
                }
            }

            // obtiene el documento manifest.xml y su raiz
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            final Document docManifest = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(manifestData));
            final Element rootManifest = docManifest.getDocumentElement();

            // recupera todos los nodos de manifest.xml
            final NodeList listFileEntry = rootManifest.getElementsByTagName("manifest:file-entry"); //$NON-NLS-1$

            //
            // Datos necesarios para la firma
            //

            // MessageDigest
            final MessageDigest md = MessageDigest.getInstance("SHA1"); //$NON-NLS-1$

            // XMLSignatureFactory
            final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$

            // DigestMethod
            DigestMethod dm;
            try {
                dm = fac.newDigestMethod(digestMethodAlgorithm, null);
            }
            catch (final Exception e) {
                if (DIGEST_METHOD.equals(digestMethodAlgorithm)) {
                    throw new AOException("No se ha podido obtener un generador de huellas digitales", e); //$NON-NLS-1$
                }
                LOGGER.warning("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm //$NON-NLS-1$
                               + "', se intentara con el algoritmo por defecto '" //$NON-NLS-1$
                               + DIGEST_METHOD
                               + "': " //$NON-NLS-1$
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
                referenceList.add(fac.newReference("mimetype", dm, null, null, null, md.digest(AOUtil.getDataFromInputStream( //$NON-NLS-1$
                // Recupera el fichero
                zf.getInputStream(zf.getEntry("mimetype")))))); //$NON-NLS-1$

                referenceList.add(fac.newReference("META-INF/manifest.xml", //$NON-NLS-1$
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
                fullPath = ((Element) listFileEntry.item(i)).getAttribute("manifest:full-path"); //$NON-NLS-1$

                // si es un archivo
                if (!fullPath.endsWith("/")) { //$NON-NLS-1$

                    // y es uno de los siguientes archivos xml
                    if (fullPath.equals("content.xml") || fullPath.equals("meta.xml") //$NON-NLS-1$ //$NON-NLS-2$
                        || fullPath.equals("styles.xml") //$NON-NLS-1$
                        || fullPath.equals("settings.xml")) { //$NON-NLS-1$

                        // crea la referencia
                        reference = fac.newReference(fullPath.replaceAll(" ", "%20"), dm, transformList, null, null, //$NON-NLS-1$ //$NON-NLS-2$
                        // Obtiene su forma canonica y su DigestValue
                                                     md.digest(canonicalizer.canonicalizeSubtree(
                                                     // Recupera el fichero y su raiz
                                                     dbf.newDocumentBuilder().parse(zf.getInputStream(zf.getEntry(fullPath))).getDocumentElement())));

                    }

                    // si no es uno de los archivos xml
                    else {

                        // crea la referencia
                        reference = fac.newReference(fullPath.replaceAll(" ", "%20"), dm, null, null, null, md.digest(AOUtil.getDataFromInputStream( //$NON-NLS-1$ //$NON-NLS-2$
                        // Recupera el fichero
                        zf.getInputStream(zf.getEntry(fullPath)))));

                    }

                    // si no se trata del documento de firmas se anade la
                    // referencia
                    if (!fullPath.equals("META-INF/documentsignatures.xml")) { //$NON-NLS-1$
                        referenceList.add(reference);
                    }
                    else {
                        isCofirm = true;
                    }
                }
            }

            final Document docSignatures;
            final Element rootSignatures;
            // si es cofirma
            if (isCofirm) {
                // recupera el documento de firmas y su raiz
                docSignatures = dbf.newDocumentBuilder().parse(zf.getInputStream(zf.getEntry("META-INF/documentsignatures.xml"))); //$NON-NLS-1$
                rootSignatures = docSignatures.getDocumentElement();
            }
            else {
                // crea un nuevo documento de firmas
                docSignatures = dbf.newDocumentBuilder().newDocument();
                rootSignatures = docSignatures.createElement("document-signatures"); //$NON-NLS-1$
                rootSignatures.setAttribute("xmlns", OPENOFFICE); //$NON-NLS-1$
                docSignatures.appendChild(rootSignatures);

                // En OpenOffice 3.2 y superiores no anadimos la propia firma al
                // manifest
                // para evitar referencias circulares
                if (useOpenOffice31Mode) {
                    final Element nodeDocumentSignatures = docManifest.createElement("manifest:file-entry"); //$NON-NLS-1$
                    nodeDocumentSignatures.setAttribute("manifest:media-type", ""); //$NON-NLS-1$ //$NON-NLS-2$
                    nodeDocumentSignatures.setAttribute("manifest:full-path", "META-INF/documentsignatures.xml"); //$NON-NLS-1$ //$NON-NLS-2$
                    rootManifest.appendChild(nodeDocumentSignatures);

                    // nuevo elemento de META-INF
                    final Element nodeMetaInf = docManifest.createElement("manifest:file-entry"); //$NON-NLS-1$
                    nodeMetaInf.setAttribute("manifest:media-type", ""); //$NON-NLS-1$ //$NON-NLS-2$
                    nodeMetaInf.setAttribute("manifest:full-path", "META-INF/"); //$NON-NLS-1$ //$NON-NLS-2$
                    rootManifest.appendChild(nodeMetaInf);
                }
            }

            // Ids de Signature y SignatureProperty
            final String signatureId = UUID.randomUUID().toString();
            final String signaturePropertyId = UUID.randomUUID().toString();

            // referencia a SignatureProperty
            referenceList.add(fac.newReference("#" + signaturePropertyId, dm)); //$NON-NLS-1$

            // contenido de SignatureProperty
            final Element content = docSignatures.createElement("dc:date"); //$NON-NLS-1$
            content.setAttribute("xmlns:dc", "http://purl.org/dc/elements/1.1/"); //$NON-NLS-1$ //$NON-NLS-2$
            content.setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss,SS").format(new Date())); //$NON-NLS-1$
            final List<XMLStructure> contentList = new ArrayList<XMLStructure>();
            contentList.add(new DOMStructure(content));

            // SignatureProperty
            final List<SignatureProperty> spList = new ArrayList<SignatureProperty>();
            spList.add(fac.newSignatureProperty(contentList, "#" + signatureId, signaturePropertyId)); //$NON-NLS-1$

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
            if (extraParams.containsKey("mode") && extraParams.getProperty("mode").equals(AOSignConstants.SIGN_MODE_EXPLICIT)) { //$NON-NLS-1$ //$NON-NLS-2$
                LOGGER.warning("El formato de firma ODF no soporta el modo de firma explicita, " + "se ignorara esta configuracion"); //$NON-NLS-1$ //$NON-NLS-2$
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
                if (!ze.getName().equals("META-INF/documentsignatures.xml") && !ze.getName().equals("META-INF/manifest.xml")) { //$NON-NLS-1$ //$NON-NLS-2$
                    zos.putNextEntry(zeOut);
                    zos.write(AOUtil.getDataFromInputStream(zf.getInputStream(ze)));
                }
            }

            // anade el documento de firmas
            zos.putNextEntry(new ZipEntry("META-INF/documentsignatures.xml")); //$NON-NLS-1$
            final ByteArrayOutputStream baosXML = new ByteArrayOutputStream();
            writeXML(baosXML, rootSignatures, false);
            zos.write(baosXML.toByteArray());
            zos.closeEntry();

            // anade manifest.xml
            zos.putNextEntry(new ZipEntry("META-INF/manifest.xml")); //$NON-NLS-1$
            final ByteArrayOutputStream baosManifest = new ByteArrayOutputStream();
            writeXML(baosManifest, rootManifest, false);
            zos.write(baosManifest.toByteArray());
            zos.closeEntry();

            try {
                zos.close();
            }
            catch (final Exception t) {
                // Ignoramos los errores en el cierre
            }

            return baos.toByteArray();

        }
        catch (final IOException ioex) {
            throw new AOFormatFileException("No es posible abrir el fichero. " + fullPath + ": " + ioex); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final SAXException saxex) {
            throw new AOFormatFileException("Estructura de archivo no valida: " + fullPath + ": " + saxex); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new AOException("No ha sido posible generar la firma ODF", e); //$NON-NLS-1$
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
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros ODF"); //$NON-NLS-1$
    }

    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {

        try {
            // genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
            final FileOutputStream fos = new FileOutputStream(zipFile);
            fos.write(sign);
            try {
                fos.flush();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el vaciado
            }
            try {
                fos.close();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el cierre
            }

            // carga el fichero zip
            final ZipFile zf = new ZipFile(zipFile);

            // obtiene el archivo de firmas
            final InputStream signIs = zf.getInputStream(zf.getEntry("META-INF/documentsignatures.xml")); //$NON-NLS-1$

            // recupera la raiz del documento de firmas
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            final Element root = dbf.newDocumentBuilder().parse(signIs).getDocumentElement();

            // obtiene todas las firmas
            final NodeList signatures = root.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Signature"); //$NON-NLS-1$ //$NON-NLS-2$

            final int numSignatures = signatures.getLength();

            final String[] arrayIds = new String[numSignatures];
            final String[] arrayRef = new String[numSignatures];
            final AOTreeNode[] arrayNodes = new AOTreeNode[numSignatures];

            for (int i = 0; i < numSignatures; i++) {
                final Element signature = (Element) signatures.item(i);
                final String sigId = signature.getAttribute("Id"); //$NON-NLS-1$

                final String strCert = signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "X509Certificate").item(0).getTextContent(); //$NON-NLS-1$ //$NON-NLS-2$
                AOTreeNode node;

                if (asSimpleSignInfo) {
                    node = new AOTreeNode(Utils.getSimpleSignInfoNode(null, signature));
                }
                else {
                    node = new AOTreeNode(AOUtil.getCN(Utils.createCert(strCert)));
                }
                arrayIds[i] = sigId;
                arrayNodes[i] = node;

                final String typeReference =
                        ((Element) signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Reference").item(0)).getAttribute("Type"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                if (typeReference.equals("http://uri.etsi.org/01903#CountersignedSignature")) { //$NON-NLS-1$
                    final String uri =
                            ((Element) signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Reference").item(0)).getAttribute("URI"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    arrayRef[i] = uri.substring(1, uri.length() - 5);
                }
                else {
                    arrayRef[i] = ""; //$NON-NLS-1$
                }
            }

            final AOTreeNode tree = new AOTreeNode("Datos"); //$NON-NLS-1$

            for (int i = numSignatures - 1; i > 0; i--) {
                for (int j = 0; j < numSignatures; j++) {
                    if (arrayRef[i].equals(arrayIds[j])) {
                        arrayNodes[j].add(arrayNodes[i]);
                    }
                }
            }

            for (int i = 0; i < numSignatures; i++) {
                if (arrayRef[i] == "") { //$NON-NLS-1$
                    tree.add(arrayNodes[i]);
                }
            }

            try {
                zipFile.delete();
            }
            catch (final Exception e) {
                // Se ignoran los errores del borrado, es responsabilidad del usuario lipiar los temporales periodicamente
            }

            return new AOTreeModel(tree, tree.getChildCount());
        }
        catch (final Exception e) {
            LOGGER.warning("Se ha producido un error al obtener la estructura de firmas: " + e); //$NON-NLS-1$
            return new AOTreeModel(new AOTreeNode("Ra\u00EDz")); //$NON-NLS-1$
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
        catch (final Exception e) {
            return false;
        }

        // Sera valido si el mimetype coincide con alguno de los formatos ODF
        // soportados
        return mimetype != null && SUPPORTED_FORMATS.contains(mimetype);
    }

    public String getSignedName(final String originalName, final String inText) {

        final String inTextInt = (inText != null ? inText : ""); //$NON-NLS-1$

        if (originalName == null) {
            return inTextInt + ".odf"; //$NON-NLS-1$
        }
        final String originalNameLC = originalName.toLowerCase();
        if (originalNameLC.length() <= 4) {
            return originalName + inTextInt + ".odf"; //$NON-NLS-1$
        }
        if (originalNameLC.endsWith(".odt")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".odt"; //$NON-NLS-1$
        }
        if (originalNameLC.endsWith(".odp")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".odp"; //$NON-NLS-1$
        }
        if (originalNameLC.endsWith(".ods")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".ods"; //$NON-NLS-1$
        }
        return originalName + inTextInt + ".odf"; //$NON-NLS-1$
    }

    private static void writeXML(final OutputStream outStream, final Node node, final boolean indent) {
        writeXML(new BufferedWriter(new OutputStreamWriter(outStream, Charset.forName("UTF-8"))), node, indent); //$NON-NLS-1$
    }

    private static void writeXML(final Writer writer, final Node node, final boolean indent) {
        try {
            final Transformer serializer = TransformerFactory.newInstance().newTransformer();
            serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8"); //$NON-NLS-1$

            if (indent) {
                serializer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
            }
            serializer.transform(new DOMSource(node), new StreamResult(writer));
        }
        catch (final Exception ex) {
            LOGGER.severe("Error al escribir el cuerpo del XML: " + ex); //$NON-NLS-1$
        }
    }

    public byte[] getData(final byte[] signData) throws AOInvalidFormatException {

        // Si no es una firma ODF valida, lanzamos una excepcion
        if (!isSign(signData)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // TODO: Por ahora, devolveremos el propio ODF firmado.
        return signData;
    }

    public AOSignInfo getSignInfo(final byte[] signData) throws AOInvalidFormatException, AOException {
        if (signData == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }
        if (!isSign(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_ODF);
    }

    private String getODFMimeType(final byte[] signData) {
        String mimetype = null;
        try {
            // Genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
            final FileOutputStream fos = new FileOutputStream(zipFile);

            fos.write(signData);

            try {
                fos.flush();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el vaciado
            }
            try {
                fos.close();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el cierre
            }

            // carga el fichero zip
            final ZipFile zf;
            try {
                zf = new ZipFile(zipFile);
            }
            catch (final ZipException e) {
                // Si detectamos que no es un fichero Zip, devolvemos null
                return null;
            }

            // obtiene el archivo mimetype
            final ZipEntry entry = zf.getEntry("mimetype"); //$NON-NLS-1$
            if (entry != null) {
                mimetype = new String(AOUtil.getDataFromInputStream(zf.getInputStream(entry)));
            }

            try {
                zipFile.delete();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el cierre
            }

        }
        catch (final Exception e) {
            LOGGER.severe("Error al analizar el fichero de firma: " + e); //$NON-NLS-1$
            return null;
        }
        return mimetype;
    }

}
