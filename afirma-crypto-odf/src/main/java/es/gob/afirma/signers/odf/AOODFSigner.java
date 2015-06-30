/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

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
import java.lang.reflect.Method;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.security.PrivateKey;
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

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;


/** Manejador de firmas electr&oacute;nicas XML de ficheros ODF en formato compatible
 * con OpenOffice.org 3.2 y superiores.
 * @version 0.2 */
public final class AOODFSigner implements AOSigner {

	private static final String EXTENSION_ODT = ".odt"; //$NON-NLS-1$
	private static final String EXTENSION_ODP = ".odp"; //$NON-NLS-1$
	private static final String EXTENSION_ODS = ".ods"; //$NON-NLS-1$
	private static final String EXTENSION_ODF = ".odf"; //$NON-NLS-1$

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String OPENOFFICE = "urn:oasis:names:tc:opendocument:xmlns:digitalsignature:1.0"; //$NON-NLS-1$

    private static final String MANIFEST_PATH = "META-INF/manifest.xml"; //$NON-NLS-1$
    private static final String SIGNATURES_PATH = "META-INF/documentsignatures.xml"; //$NON-NLS-1$

    private static final String XMLDSIG_NAMESPACE = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$

    /** Mimetypes de los formatos ODF soportados. */
    private static final Set<String> SUPPORTED_FORMATS;

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    private static final String DIGEST_METHOD = DigestMethod.SHA1;

    private static final String DIGEST_METHOD_ALGORITHM_NAME = "SHA1"; //$NON-NLS-1$

    static {
        SUPPORTED_FORMATS = new HashSet<String>();
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.text"); //$NON-NLS-1$
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.spreadsheet"); //$NON-NLS-1$
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.presentation"); //$NON-NLS-1$
    }

    static {
        Utils.installXmlDSigProvider();
    }

    /** A&ntilde;ade una firma electr&oacute;nica a un documento ODF.
     * @param data Documento ODF a firmar
     * @param algorithm Se ignora el valor de este par&aacute;metro, se utiliza siempre el algoritmo SHA1withRSA
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>useOpenOffice31Mode</i></b></dt>
     *   <dd>
     *    Un valor <code>true</code> fuerza la generaci&oacute;n de firmas en formato OpenOffice.org 3.1. Las firmas en formato
     *    OpenOffice.org 3.1 no son compatibles ni con versiones anteriores ni con posteriores, incluyendo LibreOffice.
     *   </dd>
     * </dl>
     * @return Documento ODF con la nueva firma a&ntilde;adida
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] sign(final byte[] data,
                       final String algorithm,
                       final PrivateKey key,
                       final java.security.cert.Certificate[] certChain,
                       final Properties xParams) throws AOException {

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD); //$NON-NLS-1$
        final boolean useOpenOffice31Mode = "true".equalsIgnoreCase(extraParams.getProperty("useOpenOffice31Mode")); //$NON-NLS-1$ //$NON-NLS-2$

        if (!isValidDataFile(data)) {
            throw new AOFormatFileException("Los datos introducidos no se corresponden con un documento ODF"); //$NON-NLS-1$
        }

        String fullPath = MANIFEST_PATH;
        boolean isCofirm = false;

        try {
            // genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
            final FileOutputStream fos = new FileOutputStream(zipFile);
            fos.write(data);
            fos.flush();
            fos.close();

            // carga el fichero zip
            final ZipFile zf = new ZipFile(zipFile);

            // obtiene el archivo manifest.xml, que indica los ficheros que
            // contiene el ODF
            final InputStream manifest = zf.getInputStream(zf.getEntry(fullPath));
            final byte[] manifestData = AOUtil.getDataFromInputStream(manifest);

            if (manifest != null) {
                manifest.close();
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
            final MessageDigest md;
            try {
	            md = MessageDigest.getInstance(DIGEST_METHOD_ALGORITHM_NAME);
            }
            catch (final Exception e) {
            	zf.close();
            	throw new AOException(
        			"No se ha podido obtener un generador de huellas digitales con el algoritmo " + DIGEST_METHOD_ALGORITHM_NAME + ": " + e, e //$NON-NLS-1$ //$NON-NLS-2$
    			);
            }

            // XMLSignatureFactory
            final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$

            // DigestMethod
            final DigestMethod dm;
            try {
                dm = fac.newDigestMethod(digestMethodAlgorithm, null);
            }
            catch (final Exception e) {
                zf.close();
                throw new AOException(
                      "No se ha podido obtener un generador de huellas digitales con el algoritmo: " + digestMethodAlgorithm, e //$NON-NLS-1$
                );
            }

            // Configuramos mediante reflexion las transformaciones y referencias

            final Class<?> canonicalizerClass = Class.forName("com.sun.org.apache.xml.internal.security.c14n.Canonicalizer"); //$NON-NLS-1$
            final String algoIdC14nOmitComments =
            		(String) canonicalizerClass.getField("ALGO_ID_C14N_OMIT_COMMENTS").get(null); //$NON-NLS-1$

            // Transforms
            final List<Transform> transformList = new ArrayList<Transform>(1);
            transformList.add(
        		fac.newTransform(
    				algoIdC14nOmitComments,
    				(TransformParameterSpec) null
				)
    		);

            // References
            final List<Reference> referenceList = new ArrayList<Reference>();

            Class.forName("com.sun.org.apache.xml.internal.security.Init").getMethod("init").invoke(null); //$NON-NLS-1$ //$NON-NLS-2$

            final Object canonicalizer = canonicalizerClass.
            		getMethod("getInstance", String.class).invoke(null, algoIdC14nOmitComments); //$NON-NLS-1$

            final Method canonicalizeSubtreeMethod =
            		canonicalizerClass.getMethod("canonicalizeSubtree", org.w3c.dom.Node.class); //$NON-NLS-1$

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

                referenceList.add(
            		fac.newReference(
        				MANIFEST_PATH,
        				dm,
        				transformList,
        				null,
        				null,
        				md.digest(
        						(byte[]) canonicalizeSubtreeMethod.invoke(
        								canonicalizer,
        								// Recupera el fichero y su raiz
        								dbf.newDocumentBuilder().parse(
        										new ByteArrayInputStream(manifestData)
    									).getDocumentElement()
    							)
    						)
    				)
        		);
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
                        		md.digest(
                        				(byte[]) canonicalizeSubtreeMethod.invoke(
                        						canonicalizer,
                        						// Recupera el fichero y su raiz
                        						dbf.newDocumentBuilder().parse(zf.getInputStream(zf.getEntry(fullPath))).getDocumentElement()
                						)
                				)
                		);
                    }

                    // si no es uno de los archivos xml
                    else {

                        // crea la referencia
                        reference = fac.newReference(fullPath.replaceAll(" ", "%20"), dm, null, null, null, md.digest(AOUtil.getDataFromInputStream( //$NON-NLS-1$ //$NON-NLS-2$
                        // Recupera el fichero
                        zf.getInputStream(zf.getEntry(fullPath)))));

                    }

                    if (!fullPath.equals(SIGNATURES_PATH)) {
                    	referenceList.add(reference);
                    }
                    else {
                        // Para mantener la compatibilidad con OpenOffice 3.1?
                    	isCofirm = true;
                    }
                }
            }

            // Si se encuentra el fichero de firmas en el documento, la nueva firma
            // se debe agregar a el
            if (!isCofirm && zf.getEntry(SIGNATURES_PATH) != null) {
            	isCofirm = true;
            }

            final Document docSignatures;
            final Element rootSignatures;
            // si es cofirma
            if (isCofirm) {
                // recupera el documento de firmas y su raiz
                docSignatures = dbf.newDocumentBuilder().parse(zf.getInputStream(zf.getEntry(SIGNATURES_PATH)));
                rootSignatures = docSignatures.getDocumentElement();
            }
            else {
                // crea un nuevo documento de firmas
                docSignatures = dbf.newDocumentBuilder().newDocument();
                rootSignatures = docSignatures.createElement("document-signatures"); //$NON-NLS-1$
                rootSignatures.setAttribute("xmlns", OPENOFFICE); //$NON-NLS-1$
                docSignatures.appendChild(rootSignatures);

                // En OpenOffice 3.2 y superiores no anadimos la propia firma al
                // manifest para evitar referencias circulares
                if (useOpenOffice31Mode) {
                    final Element nodeDocumentSignatures = docManifest.createElement("manifest:file-entry"); //$NON-NLS-1$
                    nodeDocumentSignatures.setAttribute("manifest:media-type", ""); //$NON-NLS-1$ //$NON-NLS-2$
                    nodeDocumentSignatures.setAttribute("manifest:full-path", SIGNATURES_PATH); //$NON-NLS-1$
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

            // Preparamos el KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final List<Object> x509Content = new ArrayList<Object>();
            final X509Certificate cert = (X509Certificate) certChain[0];
            x509Content.add(cert.getSubjectX500Principal().getName());
            x509Content.add(cert);

            // genera la firma
            fac.newXMLSignature(
              // SignedInfo
              fac.newSignedInfo(
                // CanonicalizationMethod
                 fac.newCanonicalizationMethod(
                   CanonicalizationMethod.INCLUSIVE,
                   (C14NMethodParameterSpec) null),
                   fac.newSignatureMethod(SignatureMethod.RSA_SHA1, null),
                   referenceList
                 ),
                 // KeyInfo
                 kif.newKeyInfo(
                   Collections.singletonList(kif.newX509Data(x509Content)),
                   null
                 ),
                 objectList,
                 signatureId,
                 null
              ).sign(
                 new DOMSignContext(key, rootSignatures)
            );

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
                if (!ze.getName().equals(SIGNATURES_PATH) && !ze.getName().equals(MANIFEST_PATH)) {
                    zos.putNextEntry(zeOut);
                    zos.write(AOUtil.getDataFromInputStream(zf.getInputStream(ze)));
                }
            }

            // anade el documento de firmas
            zos.putNextEntry(new ZipEntry(SIGNATURES_PATH));
            final ByteArrayOutputStream baosXML = new ByteArrayOutputStream();
            writeXML(baosXML, rootSignatures, false);
            zos.write(baosXML.toByteArray());
            zos.closeEntry();

            // anade manifest.xml
            zos.putNextEntry(new ZipEntry(MANIFEST_PATH));
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
            zf.close();
            return baos.toByteArray();

        }
        catch (final SAXException saxex) {
            throw new AOFormatFileException("Estructura de archivo no valida: " + fullPath + ": " + saxex); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new AOException("No ha sido posible generar la firma ODF: " + e, e); //$NON-NLS-1$
        }
    }

    /** A&ntilde;ade una firma electr&oacute;nica a un documento ODF.
     * Este m&eacute;todo es completamente equivalente a <code>sign(byte[], String, PrivateKeyEntry, Properties)</code>.
     * @param data No usado, se ignora el valor de este par&aacute;metro
     * @param sign Documento ODF a firmar
     * @param algorithm Se ignora el valor de este par&aacute;metro, se utiliza siempre el algoritmo SHA1withRSA
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>useOpenOffice31Mode</i></b></dt>
     *   <dd>
     *    Un valor <code>true</code> fuerza la generaci&oacute;n de firmas en formato OpenOffice.org 3.1. Las firmas en formato
     *    OpenOffice.org 3.1 no son compatibles ni con versiones anteriores ni con posteriores, incluyendo LibreOffice.
     *   </dd>
     * </dl>
     * @return Documento ODF con la nueva firma a&ntilde;adida
     * @throws AOException Cuando ocurre cualquier problema durante el proceso
     * @throws IOException Cuando hay errores de entrada / salida */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties extraParams) throws AOException, IOException {
        return sign(sign, algorithm, key, certChain, extraParams);
    }

    /** A&ntilde;ade una firma electr&oacute;nica a un documento ODF.
     * Este m&eacute;todo es completamente equivalente a <code>sign(byte[], String, PrivateKeyEntry, Properties)</code>.
     * @param sign Documento ODF a firmar
     * @param algorithm Se ignora el valor de este par&aacute;metro, se utiliza siempre el algoritmo SHA1withRSA
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>useOpenOffice31Mode</i></b></dt>
     *   <dd>
     *    Un valor <code>true</code> fuerza la generaci&oacute;n de firmas en formato OpenOffice.org 3.1. Las firmas en formato
     *    OpenOffice.org 3.1 no son compatibles ni con versiones anteriores ni con posteriores, incluyendo LibreOffice.
     *   </dd>
     * </dl>
     * @return Documento ODF con la nueva firma a&ntilde;adida
     * @throws AOException Cuando ocurre cualquier problema durante el proceso
     * @throws IOException Cuando hay errores de entrada / salida */
    @Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties extraParams) throws AOException, IOException {
        return sign(sign, algorithm, key, certChain, extraParams);
    }

    /** M&eacute;todo no implementado. No es posible realizar contrafirmas de
     * documentos ODF. Lanza siempre una <code>UnsupportedOperationException</code>. */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final java.security.cert.Certificate[] certChain,
                              final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros ODF"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) throws AOInvalidFormatException, IOException {

    	if (!isSign(sign)) {
    		throw new AOInvalidFormatException("Los datos indicados no se corresponden con un ODF firmado"); //$NON-NLS-1$
    	}

        try {
            // genera el archivo zip temporal a partir del InputStream de
            // entrada
            final File zipFile = File.createTempFile("sign", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
            final FileOutputStream fos = new FileOutputStream(zipFile);
            fos.write(sign);
            fos.close();

            // carga el fichero zip
            final ZipFile zf = new ZipFile(zipFile);

            // obtiene el archivo de firmas
            final InputStream signIs = zf.getInputStream(zf.getEntry(SIGNATURES_PATH));

            // recupera la raiz del documento de firmas
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            final Element root = dbf.newDocumentBuilder().parse(signIs).getDocumentElement();

            // obtiene todas las firmas
            final NodeList signatures = root.getElementsByTagNameNS(XMLDSIG_NAMESPACE, "Signature"); //$NON-NLS-1$

            final int numSignatures = signatures.getLength();

            final String[] arrayIds = new String[numSignatures];
            final String[] arrayRef = new String[numSignatures];
            final AOTreeNode[] arrayNodes = new AOTreeNode[numSignatures];

            for (int i = 0; i < numSignatures; i++) {
                final Element signature = (Element) signatures.item(i);

                final String strCert = signature.getElementsByTagNameNS(XMLDSIG_NAMESPACE, "X509Certificate").item(0).getTextContent(); //$NON-NLS-1$
                final AOTreeNode node;

                if (asSimpleSignInfo) {
                    node = new AOTreeNode(Utils.getSimpleSignInfoNode(null, signature));
                }
                else {
                    node = new AOTreeNode(AOUtil.getCN(Utils.createCert(strCert)));
                }
                arrayIds[i] = signature.getAttribute("Id"); //$NON-NLS-1$
                arrayNodes[i] = node;

                final String typeReference = ((Element) signature.getElementsByTagNameNS(
            		XMLDSIG_NAMESPACE,
            		"Reference" //$NON-NLS-1$
        		).item(0)).getAttribute("Type"); //$NON-NLS-1$
                if ("http://uri.etsi.org/01903#CountersignedSignature".equals(typeReference)) { //$NON-NLS-1$
                    final String uri = ((Element) signature.getElementsByTagNameNS(XMLDSIG_NAMESPACE, "Reference").item(0)).getAttribute("URI"); //$NON-NLS-1$ //$NON-NLS-2$
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
                if ("".equals(arrayRef[i])) { //$NON-NLS-1$
                    tree.add(arrayNodes[i]);
                }
            }

            signIs.close();
            zf.close();

            try {
                zipFile.delete();
            }
            catch (final Exception e) {
                // Se ignoran los errores del borrado, es responsabilidad del usuario lipiar los temporales periodicamente
            }

            return new AOTreeModel(tree);
        }
        catch (final Exception e) {
            LOGGER.warning("Se ha producido un error al obtener la estructura de firmas: " + e); //$NON-NLS-1$
            return new AOTreeModel(new AOTreeNode("Ra\u00EDz")); //$NON-NLS-1$
        }
    }

    /** Indica si los datos indicados son un documento ODF susceptible de contener una firma
     * electr&oacute;nica.
     * @param signData Datos que deseamos comprobar.
     * @return Devuelve <code>true</code> si los datos indicados son un documento ODF susceptible de contener una firma
     * electr&oacute;nica, <code>false</code> en caso contrario.
     * @throws IOException Si ocurren problemas durante la lectura de la firma */
    @Override
	public boolean isSign(final byte[] signData) throws IOException {
        if(!isValidDataFile(signData)) {
        	return false;
        }

    	final File odfFile;
    	try {
    		odfFile = createTempFile(signData);
    		odfFile.deleteOnExit();
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se pudo crear una copia del fichero para su analisis, se devolvera false"); //$NON-NLS-1$
			return false;
		}

    	// carga el fichero zip
    	ZipFile zf = null;
    	try {
    		zf = new ZipFile(odfFile);
    	}
    	catch (final Exception e) {
    		if (zf != null) {
    			zf.close();
    		}
    		return false;
    	}

    	// obtiene el archivo mimetype
    	final boolean ret = zf.getEntry(AOODFSigner.SIGNATURES_PATH) != null;
    	zf.close();
    	return ret;
    }

    /** Indica si los datos son un documento ODF susceptible de ser firmado.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos son un documento ODF susceptible de ser firmado, <code>false</code> en caso contrario */
    @Override
	public boolean isValidDataFile(final byte[] data) {

    	final File odfFile;
    	try {
    		odfFile = createTempFile(data);
    		odfFile.deleteOnExit();
    	} catch (final Exception e) {
    		LOGGER.warning("No se pudo crear una copia del fichero para su analisis, se devolvera false"); //$NON-NLS-1$
			return false;
		}

        // Si el mimetype del fichero no se ajusta a alguno de los MimeTypes
        // soportados
        // para firma ODF se lanzara una excepcion, en ese caso deducimos que no
        // es un
        // fichero valido
        String mimetype = null;
        try {
            mimetype = AOODFSigner.getODFMimeType(odfFile);
        }
        catch (final Exception e) {
            return false;
        }

        // Sera valido si el mimetype coincide con alguno de los formatos ODF
        // soportados
        return mimetype != null && SUPPORTED_FORMATS.contains(mimetype);
    }

    /** {@inheritDoc} */
    @Override
	public String getSignedName(final String originalName, final String inText) {

        final String inTextInt = inText != null ? inText : ""; //$NON-NLS-1$

        if (originalName == null) {
            return inTextInt + EXTENSION_ODF;
        }
        final String originalNameLC = originalName.toLowerCase();
        if (originalNameLC.length() <= EXTENSION_ODF.length()) {
            return originalName + inTextInt + EXTENSION_ODF;
        }
        if (originalNameLC.endsWith(EXTENSION_ODT)) {
            return originalName.substring(0, originalName.length() - EXTENSION_ODT.length()) + inTextInt + EXTENSION_ODT;
        }
        if (originalNameLC.endsWith(EXTENSION_ODP)) {
            return originalName.substring(0, originalName.length() - EXTENSION_ODP.length()) + inTextInt + EXTENSION_ODP;
        }
        if (originalNameLC.endsWith(EXTENSION_ODS)) {
            return originalName.substring(0, originalName.length() - EXTENSION_ODS.length()) + inTextInt + EXTENSION_ODS;
        }
        return originalName + inTextInt + EXTENSION_ODF;
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

    /** Si la entrada es un documento ODF, devuelve el mismo documento sin ninguna modificaci&oacute;n.
     * @param signData Documento ODF
     * @return Documento de entrada si este es ODF, <code>null</code> en cualquier otro caso
     * @throws IOException Si ocurren problemas al leer la firma */
    @Override
	public byte[] getData(final byte[] signData) throws AOInvalidFormatException, IOException {

        // Si no es una firma ODF valida, lanzamos una excepcion
        if (!isSign(signData)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // TODO: Por ahora, devolveremos el propio ODF firmado.
        return signData;
    }

    /** {@inheritDoc} */
    @Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOException, IOException {
        if (signData == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }
        if (!isSign(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_ODF);
    }

    private static String getODFMimeType(final File odfFile) throws IOException {
        String mimetype = null;

        // carga el fichero zip
        final ZipFile zf;
        try {
            zf = new ZipFile(odfFile);
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
        zf.close();

        return mimetype;
    }

    /** Crea un fichero temporal con los datos.
     * @param data Datos del fichero.
     * @return Fichero generado.
     * @throws IOException Cuando se produce un error durante la generaci&oacute;n. */
    private static File createTempFile(final byte[] data) throws IOException {
    	// Genera el archivo zip temporal a partir del InputStream de entrada
        final File zipFile = File.createTempFile("sign", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
        final FileOutputStream fos = new FileOutputStream(zipFile);

        fos.write(data);
        fos.flush();
        fos.close();

        return zipFile;
    }

}
