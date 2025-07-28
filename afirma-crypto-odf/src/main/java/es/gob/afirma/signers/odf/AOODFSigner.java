/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
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
import java.nio.charset.StandardCharsets;
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
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
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
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
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
import es.gob.afirma.core.keystores.AOCancelledSMOperationException;
import es.gob.afirma.core.keystores.AuthenticationException;
import es.gob.afirma.core.keystores.LockedKeyStoreException;
import es.gob.afirma.core.keystores.PinException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.core.misc.SecureXmlTransformer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;

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

    private static final String CANONICAL_XML_ALGORITHM = "http://www.w3.org/TR/2001/REC-xml-c14n-20010315"; //$NON-NLS-1$

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    private static final String DEFAULT_DIGEST_METHOD = DigestMethod.SHA1;

    private static final String DIGEST_METHOD_ALGORITHM_NAME = "SHA1"; //$NON-NLS-1$

    private static final String ENTRY_MIMETYPE = "mimetype"; //$NON-NLS-1$

    private static final int THRESHOLD_FILE_SIZE = 1000000000; // 1 GB

    static {

        // Instalamos el proveedor de Apache. Esto es necesario para evitar problemas con los saltos de linea
        // de los Base 64
        XmlDSigProviderHelper.configureXmlDSigProvider();

        // Listado de formatos OpenDocument soportados
        SUPPORTED_FORMATS = new HashSet<>();
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.text"); //$NON-NLS-1$
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.spreadsheet"); //$NON-NLS-1$
        SUPPORTED_FORMATS.add("application/vnd.oasis.opendocument.presentation"); //$NON-NLS-1$
    }

    /** A&ntilde;ade una firma electr&oacute;nica a un documento ODF.
     * @param data Documento ODF a firmar.
     * @param algorithm Algoritmo de firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
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

    	if (data.length >= THRESHOLD_FILE_SIZE) {
    		throw new AOException("Los datos tienen un tamano superior al permitido"); //$NON-NLS-1$
    	}

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final String digestMethodAlgorithm = extraParams.getProperty(AOODFExtraParams.REFERENCES_DIGEST_METHOD, DEFAULT_DIGEST_METHOD);
        final boolean useOpenOffice31Mode = Boolean.parseBoolean(extraParams.getProperty(AOODFExtraParams.USE_OPEN_OFFICE_31_MODE));

        if (!isValidDataFile(data)) {
            throw new AOFormatFileException("Los datos introducidos no se corresponden con un documento ODF"); //$NON-NLS-1$
        }

        String fullPath = MANIFEST_PATH;
        boolean isCofirm = false;

        try {
            // Genera el archivo zip temporal a partir del InputStream de entrada
            final File zipFile = File.createTempFile("sign", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
            try (
        		final FileOutputStream fos = new FileOutputStream(zipFile);
    		) {
	            fos.write(data);
	            fos.flush();
            }
            zipFile.deleteOnExit();

            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            try (
	            // carga el fichero zip
	            final ZipFile zf = new ZipFile(zipFile);
    		) {
            	final byte[] manifestData;
            	try (
		            // obtiene el archivo manifest.xml, que indica los ficheros que
		            // contiene el ODF
		            final InputStream manifest = zf.getInputStream(zf.getEntry(fullPath));
    			) {
            		manifestData = AOUtil.getDataFromInputStream(manifest);
            	}

	            // obtiene el documento manifest.xml y su raiz
	            final Document docManifest = SecureXmlBuilder.getSecureDocumentBuilder().parse(new ByteArrayInputStream(manifestData));
	            final Element rootManifest = docManifest.getDocumentElement();

	            // recupera todos los nodos de manifest.xml
	            final NodeList listFileEntry = rootManifest.getElementsByTagName("manifest:file-entry"); //$NON-NLS-1$

	            // Datos necesarios para la firma

	            // MessageDigest
	            final MessageDigest md;
	            try {
		            md = MessageDigest.getInstance(DIGEST_METHOD_ALGORITHM_NAME);
	            }
	            catch (final Exception e) {
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
	                throw new AOException(
	                      "No se ha podido obtener un generador de huellas digitales con el algoritmo: " + digestMethodAlgorithm, e //$NON-NLS-1$
	                );
	            }

	            // Configuramos las transformaciones y referencias

	            // Transforms
	            final List<Transform> transformList = new ArrayList<>(1);
	            transformList.add(
	        		fac.newTransform(
	        			CANONICAL_XML_ALGORITHM,
	    				(TransformParameterSpec) null
					)
	    		);

	            // References
	            final List<Reference> referenceList = new ArrayList<>();

	            // Anadimos tambien referencias manualmente al propio manifest.xml y
	            // al mimetype

	            // manifest tiene una canonicalizacion. Solo en OOo 3.2 y superiores
	            if (!useOpenOffice31Mode) {

	                // mimetype es una referencia simple, porque no es XML
	            	try (
            			final InputStream zis = zf.getInputStream(
							zf.getEntry("mimetype") //$NON-NLS-1$
						)
        			) {
		                referenceList.add(
		            		fac.newReference(
		        				"mimetype", //$NON-NLS-1$
		        				dm,
		        				null,
		        				null,
		        				null,
		        				md.digest(
		    						AOUtil.getDataFromInputStream(
										zis
									)
								)
							)
						);
	            	}

	                referenceList.add(
	            		fac.newReference(
	        				MANIFEST_PATH,
	        				dm,
	        				transformList,
	        				null,
	        				null,
	        				md.digest(
	        					OdfXmlUtil.canonicalizeXml(
	        							Utils.getNewDocumentBuilder().parse(
											new ByteArrayInputStream(manifestData)
									).getDocumentElement(),
	    							CANONICAL_XML_ALGORITHM
								)
							)
	    				)
	        		);
	            }

	            // Para cada nodo de manifest.xml
	            Reference reference;
	            for (int i = 0; i < listFileEntry.getLength(); i++) {
	                fullPath = ((Element) listFileEntry.item(i)).getAttribute("manifest:full-path"); //$NON-NLS-1$

	                // Si es un archivo
	                if (!fullPath.endsWith("/")) { //$NON-NLS-1$

	                    // y es uno de los siguientes archivos xml
	                    if (fullPath.equals("content.xml") || //$NON-NLS-1$
	                    	fullPath.equals("meta.xml")    || //$NON-NLS-1$
	                        fullPath.equals("styles.xml")  || //$NON-NLS-1$
	                        fullPath.equals("settings.xml")) { //$NON-NLS-1$

	                        // Crea la referencia
	                    	try (
                    			final InputStream zis = zf.getInputStream(zf.getEntry(fullPath))
                			) {
		                        reference = fac.newReference(
	                        		fullPath.replace(" ", "%20"), //$NON-NLS-1$ //$NON-NLS-2$
	                        		dm,
	                        		transformList,
	                        		null,
	                        		null,
	                        		// Obtiene su forma canonica y su DigestValue
		                    		md.digest(
		                				OdfXmlUtil.canonicalizeXml(
		                						Utils.getNewDocumentBuilder().parse(zis).getDocumentElement(),
		            						CANONICAL_XML_ALGORITHM
		        						)
		            				)
		                		);
	                    	}
	                    }

	                    // Si no es uno de los archivos XML
	                    else {

	                        // Crea la referencia
	                    	try (
                				final InputStream zis = zf.getInputStream(zf.getEntry(fullPath))
                			) {
		                        reference = fac.newReference(
	                        		fullPath.replace(" ", "%20"), //$NON-NLS-1$ //$NON-NLS-2$
	                        		dm,
	                        		null,
	                        		null,
	                        		null,
	                        		md.digest(AOUtil.getDataFromInputStream(zis))
	                    		);
	                    	}

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
	            	try (
            			final InputStream zis = zf.getInputStream(zf.getEntry(SIGNATURES_PATH))
        			) {
		                docSignatures = Utils.getNewDocumentBuilder().parse(
	                		zis
	            		);
	            	}
	                rootSignatures = docSignatures.getDocumentElement();
	            }
	            else {
	                // crea un nuevo documento de firmas
	                docSignatures = Utils.getNewDocumentBuilder().newDocument();
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

	            // Tenemos que crear el elemento de datos cargandolo como un XML completo
	            // para evitar un error con Java 11
	            final String contentString = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss,SS") //$NON-NLS-1$
	            		.format(new Date());
	            final Document dateDocument = Utils.getNewDocumentBuilder().parse(
	            		new ByteArrayInputStream(
	            				("<dc:date xmlns:dc=\"http://purl.org/dc/elements/1.1/\">" + contentString + "</dc:date>") //$NON-NLS-1$ //$NON-NLS-2$
	            					.getBytes()));
	            final Element content = dateDocument.getDocumentElement();

	            final List<XMLStructure> contentList = new ArrayList<>();
	            contentList.add(new DOMStructure(content));

	            // SignatureProperty
	            final List<SignatureProperty> spList = new ArrayList<>();
	            spList.add(fac.newSignatureProperty(contentList, "#" + signatureId, signaturePropertyId)); //$NON-NLS-1$

	            // SignatureProperties
	            final List<SignatureProperties> spsList = new ArrayList<>();
	            spsList.add(fac.newSignatureProperties(spList, null));

	            // Object
	            final List<XMLObject> objectList = new ArrayList<>();
	            objectList.add(fac.newXMLObject(spsList, null, null, null));

	            // Preparamos el KeyInfo
	            final KeyInfoFactory kif = fac.getKeyInfoFactory();
	            final List<Object> x509Content = new ArrayList<>();
	            final X509Certificate cert = (X509Certificate) certChain[0];
	            x509Content.add(cert.getSubjectX500Principal().getName());
	            x509Content.add(cert);

	            // genera la firma
	            final javax.xml.crypto.dsig.XMLSignature xmlSignature = fac.newXMLSignature(
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
	            		);

	            final DOMSignContext context = new DOMSignContext(key, rootSignatures);
	            try {
	            	xmlSignature.sign(
	            			context
	            			);
	            }
	            catch (final XMLSignatureException e) {
	            	final Throwable cause = e.getCause() != null ? e.getCause() : null;
	            	if (cause != null) {
	            		String causeName = cause.getClass().getName();
	            		// Si JMulticard informa de un problema de autenticacion durante la firma
	            		if ("es.gob.jmulticard.jse.provider.SignatureAuthException".equals(causeName)) { //$NON-NLS-1$
	            			causeName = cause.getCause() != null ? cause.getCause().getClass().getName() : null;
	            			// Si la tarjeta esta bloqueada
	            			if ("es.gob.jmulticard.card.AuthenticationModeLockedException".equals(causeName)) { //$NON-NLS-1$
	            				throw new LockedKeyStoreException("El almacen de claves esta bloqueado", e); //$NON-NLS-1$
	            			}
	            			// Si se ha insertado un PIN incorrecto
	            			if ("es.gob.jmulticard.card.BadPinException".equals(causeName)) { //$NON-NLS-1$
	            				throw new PinException("La contrasena del almacen o certificado es incorrecta", e); //$NON-NLS-1$
	            			}
	            			throw new AuthenticationException("Ocurrio un error de autenticacion al utilizar la clave de firma", cause); //$NON-NLS-1$
	            		}
	            	}
	    			throw e;
	            }
	            catch (final Exception e) {
	          	  if ("es.gob.jmulticard.CancelledOperationException".equals(e.getClass().getName())) { //$NON-NLS-1$
	          		  throw new AOCancelledSMOperationException("Cancelacion del dialogo de JMulticard"); //$NON-NLS-1$
	          	  }
	          	  throw e;
	            }

	            try (
		            // crea un nuevo fichero zip
		            final ZipOutputStream zos = new ZipOutputStream(baos);
        		) {
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
		                    try (
	                    		final InputStream zis = zf.getInputStream(ze)
                    		) {
			                    zos.write(
		                    		AOUtil.getDataFromInputStream(
	                    				zis
	                				)
	                    		);
		                    }
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

	            } // try-with-resources de "zos"
            } // try-with-resources de "zf"

            return baos.toByteArray();
        }
        catch (final SAXException saxex) {
            throw new AOFormatFileException("Estructura de archivo no valida '" + fullPath + "': " + saxex); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final AOException e) {
            throw e;
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
     * @return Documento ODF con la nueva firma a&ntilde;adida.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso. */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties extraParams) throws AOException {
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
     * @return Documento ODF con la nueva firma a&ntilde;adida.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso. */
    @Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties extraParams) throws AOException {
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
                              final Properties extraParams) {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros ODF"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo)
			throws AOInvalidFormatException, IOException {
		return getSignersStructure(sign, null, asSimpleSignInfo);
	}

    /** {@inheritDoc} */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo) throws AOInvalidFormatException, IOException {

    	if (sign.length >= THRESHOLD_FILE_SIZE) {
    		throw new IOException("Los datos tienen un tamano superior al permitido."); //$NON-NLS-1$
    	}

    	if (!isSign(sign)) {
    		throw new AOInvalidFormatException("Los datos indicados no se corresponden con un ODF firmado"); //$NON-NLS-1$
    	}

        try {
            final AOTreeNode tree = new AOTreeNode("Datos"); //$NON-NLS-1$
            try (
            	ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(sign));
    		) {
            	final ZipEntry entry = getEntry(zis, SIGNATURES_PATH);
            	if (entry == null) {
            		return new AOTreeModel(tree);
            	}
	            // recupera la raiz del documento de firmas
	            final Element root = SecureXmlBuilder.getSecureDocumentBuilder().parse(zis).getDocumentElement();

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
		return isSign(signData, null);
	}

	/** {@inheritDoc} */
    @Override
	public boolean isSign(final byte[] signData, final Properties params) throws IOException {

        if(!isValidDataFile(signData)) {
        	return false;
        }

        ZipEntry entry = null;
        try (ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(signData))) {
	    	// Comprueba si existe el fichero de firmas
        	entry = getEntry(zis, AOODFSigner.SIGNATURES_PATH);
        }

        return entry != null;
    }

    /** Indica si los datos son un documento ODF susceptible de ser firmado.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos son un documento ODF susceptible de ser firmado, <code>false</code> en caso contrario */
    @Override
	public boolean isValidDataFile(final byte[] data) {

    	// Si no es un ZIP, no se trata de un ODF
    	if (!new MimeHelper(data).isZipData()) {
    		return false;
    	}

        // Si el mimetype del fichero no se ajusta a alguno de los MimeTypes soportados
        // para firma ODF se lanzara una excepcion, en ese caso deducimos que no es un
        // fichero valido
        String mimetype = null;
    	try (ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(data))) {
    		mimetype = AOODFSigner.getODFMimeType(zis);
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
        writeXML(new BufferedWriter(new OutputStreamWriter(outStream, StandardCharsets.UTF_8)), node, indent);
    }

    private static void writeXML(final Writer writer, final Node node, final boolean indent) {
        try {
            final Transformer serializer = SecureXmlTransformer.getSecureTransformer();
            serializer.setOutputProperty(OutputKeys.ENCODING, StandardCharsets.UTF_8.name());

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
     * @param sign Documento ODF
     * @return Documento de entrada si este es ODF, <code>null</code> en cualquier otro caso
     * @throws IOException Si ocurren problemas al leer la firma */
	@Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException, IOException, AOException {
		return getData(sign, null);
	}

    @Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOInvalidFormatException, IOException {

        // Si no es una firma ODF valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // TODO: Por ahora, devolveremos el propio ODF firmado.
        return sign;
    }

    /** {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException, IOException {
		return getSignInfo(data, null);
	}

    /** {@inheritDoc} */
    @Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties param) throws AOException, IOException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }
        if (!isSign(data)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_ODF);
    }

    private static String getODFMimeType(final ZipInputStream odfZis) throws IOException {
    	String mimetypeContent = null;
    	final ZipEntry entry = getEntry(odfZis, ENTRY_MIMETYPE);
    	if (entry != null) {
    		final byte[] content = AOUtil.getDataFromInputStream(odfZis);
    		mimetypeContent = content != null ? new String(content) : "";  //$NON-NLS-1$
    	}
    	return mimetypeContent;
    }

	/**
	 * Busca una entrada dentro de un ZIP en memoria.
	 * @param zis Flujo de datos del fichero comprimido.
	 * @param entryName Entrada a buscar.
	 * @return Entrada buscada o {@code null} si no se encontr&oacute;.
	 * @throws IOException Cuando ocurre un error durante la lectura del fichero.
	 */
	private static ZipEntry getEntry(final ZipInputStream zis, final String entryName) throws IOException {
		ZipEntry entry = null;
		boolean found = false;
		while (!found && (entry = zis.getNextEntry()) != null) {
			if (entry.getName().equals(entryName)) {
				found = true;
			}
		}
		return found ? entry : null;
	}

}
