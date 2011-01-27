/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
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
import java.security.MessageDigest;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignatureProperties;
import javax.xml.crypto.dsig.SignatureProperty;
import javax.xml.crypto.dsig.SignedInfo;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.keyinfo.X509Data;
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

import es.gob.afirma.beans.AOSignInfo;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.signers.xmlhelper.Utils;

/**
 * Clase para la firma electr&oacute;nica de ficheros ODF en OpenOffice.org 3 y superiores.
 */
public final class AOODFSigner implements AOSigner {
	
	private static String OPENOFFICE = "urn:oasis:names:tc:opendocument:xmlns:digitalsignature:1.0";
	
	/** Mimetypes de los formatos ODF soportados. */
	private static final HashSet<String> supportedFormats;
	
	static {
	    supportedFormats = new HashSet<String>();
	    supportedFormats.add("application/vnd.oasis.opendocument.text");
	    supportedFormats.add("application/vnd.oasis.opendocument.spreadsheet");
	    supportedFormats.add("application/vnd.oasis.opendocument.presentation");
	}
	
	/**
	 * Firma o cofirma un documento OpenOffice de tipo ODT, ODS y ODG.<br/>
	 * @param file Flujo de lectura de los datos a firmar
	 * @param algorithm No necesario. Se utiliza siempre el algoritmo SHA1withRSA
	 * @param keyEntry Clave privada a usar para firmar
	 * @param cert Certificado a usar para firmar (debe ser relativo a la clave privada)
	 * @return Contenido firmado
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 * @since 1.6.0_10
	 */
	public byte[] sign(final InputStream file, final String algorithm, final PrivateKeyEntry keyEntry, final X509Certificate cert, final Properties extraParams) throws AOException {
		
		if(algorithm != null && !algorithm.equalsIgnoreCase("SHA1withRSA")) {
			Logger.getLogger("es.gob.afirma").warning("Las firmas ODF s\u00F3lo soportan el algoritmo de firma SHA1withRSA");
		}
		
		byte[] data;
		try {
			data = AOUtil.getDataFromInputStream(file);
		}
		catch(Throwable e) {
			throw new AOException("No se han podido leer los datos de entrada", e);
		}
		
		if (!isValidDataFile(new ByteArrayInputStream(data)))
			throw new AOFormatFileException("El fichero introducido no es un documento ODF");
		
		String fullPath = "META-INF/manifest.xml";
		boolean isCofirm = false;
		
		try {
			//genera el archivo zip temporal a partir del InputStream de entrada
			File zipFile = File.createTempFile("sign", ".zip");
			FileOutputStream fos = new FileOutputStream(zipFile);
			fos.write(data);
			fos.close();
			
			//carga el fichero zip
			ZipFile zf = new ZipFile(zipFile);
			
			//obtiene el archivo manifest.xml, que indica los ficheros que contiene el ODF
			InputStream manifest = zf.getInputStream(zf.getEntry(fullPath));
			
			//obtiene el documento manifest.xml y su raiz
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
			Document docManifest = dbf.newDocumentBuilder().parse(manifest);
			Element rootManifest = docManifest.getDocumentElement();	

			//recupera todos los nodos de manifest.xml
			NodeList listFileEntry = rootManifest.getElementsByTagName("manifest:file-entry");						

            //Datos necesarios para la firma
			//MessageDigest
			MessageDigest md = MessageDigest.getInstance("SHA1");
			//XMLSignatureFactory
            XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
            //DigestMethod
            DigestMethod dm = fac.newDigestMethod(DigestMethod.SHA1, null);            
            //Transforms
            Transform transform = fac.newTransform(Canonicalizer.ALGO_ID_C14N_OMIT_COMMENTS, (TransformParameterSpec)null);
            List<Transform> transformList = new ArrayList<Transform>();
            transformList.add(transform);            
            //References
            List<Reference> referenceList = new ArrayList<Reference>();
            
            Init.init();
			
			//para cada nodo de manifest.xml
			for (int i = 0; i < listFileEntry.getLength(); i++) {
				fullPath = ((Element)listFileEntry.item(i)).getAttribute("manifest:full-path");
				byte[] digestValue;
				Reference reference;
				
				//si es un archivo
				if (!fullPath.endsWith("/")) {
					
					//y es uno de los siguientes archivos xml
					if (fullPath.equals("content.xml") || fullPath.equals("meta.xml") || 
							fullPath.equals("styles.xml") || fullPath.equals("settings.xml")) {
						
						//recupera el fichero y su raiz
						InputStream xmlFile = zf.getInputStream(zf.getEntry(fullPath));
						Element root = dbf.newDocumentBuilder().parse(xmlFile).getDocumentElement();
						
						//obtiene su forma canonica y su DigestValue
						Canonicalizer canonicalizer = Canonicalizer.getInstance(Canonicalizer.ALGO_ID_C14N_OMIT_COMMENTS);
						byte[] docCanonicalize = canonicalizer.canonicalizeSubtree(root);
						digestValue = md.digest(docCanonicalize);
						
						//crea la referencia
						reference = fac.newReference(fullPath.replaceAll(" ", "%20"), dm, transformList, null, null, digestValue);
					}
						
					//si no es uno de los archivos xml
					else {
						//recupera el fichero
						InputStream xmlFile = zf.getInputStream(zf.getEntry(fullPath));

						byte[] docBytes = AOUtil.getDataFromInputStream(xmlFile);
						digestValue = md.digest(docBytes);
						
						//crea la referencia
						reference = fac.newReference(fullPath.replaceAll(" ", "%20"), dm, null, null, null, digestValue);
					}
					
					//si no se trata del documento de firmas se anade la referencia
					if (!fullPath.equals("META-INF/documentsignatures.xml"))
						referenceList.add(reference);
					//si existe el documento de firmas, entonces sera una cofirma.
					else
						isCofirm = true;
				}
			}
			
			Document docSignatures;
			Element rootSignatures;	
			//si es cofirma
			if (isCofirm) {
				//recupera el documento de firmas y su raiz
				InputStream xmlFile = zf.getInputStream(zf.getEntry("META-INF/documentsignatures.xml"));
				docSignatures = dbf.newDocumentBuilder().parse(xmlFile);
				rootSignatures = docSignatures.getDocumentElement();
			}
			else {
				//crea un nuevo documento de firmas
	            docSignatures = dbf.newDocumentBuilder().newDocument();
	            rootSignatures = docSignatures.createElement("document-signatures");
	            rootSignatures.setAttribute("xmlns", OPENOFFICE);
	            docSignatures.appendChild(rootSignatures);
	            
	            //modifica manifest.xml para incluir el nuevo documento de firmas
	            //nuevo elemento del documento de firmas
	            Element nodeDocumentSignatures = docManifest.createElement("manifest:file-entry");
	            nodeDocumentSignatures.setAttribute("manifest:media-type", "");
	            nodeDocumentSignatures.setAttribute("manifest:full-path", "META-INF/documentsignatures.xml");
	            rootManifest.appendChild(nodeDocumentSignatures);
	            
	            //nuevo elemento de META-INF
	            Element nodeMetaInf = docManifest.createElement("manifest:file-entry");
	            nodeMetaInf.setAttribute("manifest:media-type", "");
	            nodeMetaInf.setAttribute("manifest:full-path", "META-INF/");
	            rootManifest.appendChild(nodeMetaInf);
			}
			
			//Ids de Signature y SignatureProperty
			String signatureId = UUID.randomUUID().toString();
			String signaturePropertyId = UUID.randomUUID().toString();
			
			//referencia a SignatureProperty
			Reference signaturePropertyReference = fac.newReference("#" + signaturePropertyId, dm);
			referenceList.add(signaturePropertyReference);
			
			//CanonicalizationMethod
	        CanonicalizationMethod cm = fac.newCanonicalizationMethod(CanonicalizationMethod.INCLUSIVE, (C14NMethodParameterSpec)null);        	        
	
	        //SignatureMethod
	        SignatureMethod sm = fac.newSignatureMethod(SignatureMethod.RSA_SHA1, null);
	        
	        //SignedInfo
	        SignedInfo si = fac.newSignedInfo(cm, sm, referenceList);
	        
	        //KeyInfo
	        X509Data cerData;
	        KeyInfoFactory kif = fac.getKeyInfoFactory();
	        List<Object> x509Content = new ArrayList<Object>();	        
	        x509Content.add(cert.getSubjectX500Principal().getName());
	        x509Content.add(cert);
	        cerData = kif.newX509Data(x509Content);        
	        KeyInfo ki = kif.newKeyInfo(Collections.singletonList(cerData), null);
	        
	        //contenido de SignatureProperty
	        Element content = docSignatures.createElement("dc:date");
	        content.setAttribute("xmlns:dc", "http://purl.org/dc/elements/1.1/");
	        content.setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss,SS").format(new Date()));
	        XMLStructure str = new DOMStructure(content);
	        List<XMLStructure> contentList = new ArrayList<XMLStructure>();
	        contentList.add(str);
	        
	        //SignatureProperty
	        List<SignatureProperty> spList = new ArrayList<SignatureProperty>();
	        spList.add(fac.newSignatureProperty(contentList, "#" + signatureId, signaturePropertyId));
	        
	        //SignatureProperties
	        List<SignatureProperties> spsList = new ArrayList<SignatureProperties>();
	        spsList.add(fac.newSignatureProperties(spList, null));
	        
	        //Object	        
	        List<XMLObject> objectList = new ArrayList<XMLObject>();
	        objectList.add(fac.newXMLObject(spsList, null, null, null));
	
	        /*
	         * Si se solicito una firma explicita, advertimos no son compatibles con ODF y se
	         * ignorara esta configuracion
	         */
	        if (extraParams != null && extraParams.containsKey("mode")
	        		&& extraParams.getProperty("mode").equals(AOConstants.SIGN_MODE_EXPLICIT)) {
	        			Logger.getLogger("es.gob.afirma").warning(
	        					"El formato de firma ODF no soporta el modo de firma explicita, " +
	        					"se ignorara esta configuracion");
	        }
	        
	        //genera la firma
	        XMLSignature signature = fac.newXMLSignature(si, ki, objectList, signatureId, null);	
	        signature.sign(new DOMSignContext(keyEntry.getPrivateKey(), rootSignatures));
	        
	        //crea un nuevo fichero zip
	        ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ZipOutputStream zos = new ZipOutputStream(baos);
            
            //copia el contenido del zip original en el nuevo excepto el documento de firmas y manifest.xml
            Enumeration<? extends ZipEntry> e = zf.entries();
            while(e.hasMoreElements()){
                ZipEntry ze = e.nextElement();   
                ZipEntry zeOut = new ZipEntry(ze.getName());
                if(!ze.getName().equals("META-INF/documentsignatures.xml") && !ze.getName().equals("META-INF/manifest.xml")){
                    zos.putNextEntry(zeOut);
                    InputStream is = zf.getInputStream(ze);
                    zos.write(AOUtil.getDataFromInputStream(is));
                }               
            }
            
            //anade el documento de firmas
            zos.putNextEntry(new ZipEntry("META-INF/documentsignatures.xml"));
            ByteArrayOutputStream baosXML = new ByteArrayOutputStream();
            writeXML(baosXML, rootSignatures, false);
            zos.write(baosXML.toByteArray());
            zos.closeEntry();
            
            //anade manifest.xml
            zos.putNextEntry(new ZipEntry("META-INF/manifest.xml"));
            ByteArrayOutputStream baosManifest = new ByteArrayOutputStream();
            writeXML(baosManifest, rootManifest, false);
            zos.write(baosManifest.toByteArray());
            zos.closeEntry();
            
            zos.close();            
	        	        
	        return baos.toByteArray();
			
		}
		catch (IOException ioex) {
			ioex.printStackTrace();
			throw new AOFormatFileException("No es posible abrir el fichero. " + fullPath + ". " + ioex.toString());
		}
		catch (SAXException saxex) {
			throw new AOFormatFileException("Estructura de archivo no valida: " + fullPath + ". " + saxex.toString());
		}		
		catch (Throwable e) {
			e.printStackTrace();
			throw new AOException("No ha sido posible generar la firma ODF. " + e.toString());
		}		
		
	}
	
	public byte[] cosign(InputStream file, InputStream signFile, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams) throws AOException {
		return sign(signFile, algorithm, keyEntry, cert, extraParams);
	}

	public byte[] cosign(InputStream signFile, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams) throws AOException {
		return sign(signFile, algorithm, keyEntry, cert, extraParams);
	}
	
	/**
	 * M&eacute;todo no implementado. No es posible realizar contrafirmas de
	 * documentos ODF. Lanza una <code>UnsupportedOperationException</code>.
	 */
	public byte[] countersign(final InputStream signFile, final String algorithm, final CounterSignTarget targetType, final Object[] targets, final PrivateKeyEntry keyEntry, final X509Certificate cert, final Properties extraParams) throws AOException {
		throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros ODF");
	}
	
	public TreeModel getSignersStructure(InputStream sign, boolean asSimpleSignInfo) {
		
		try {
			//genera el archivo zip temporal a partir del InputStream de entrada
			File zipFile = File.createTempFile("sign", ".zip");
			FileOutputStream fos = new FileOutputStream(zipFile);
			fos.write(AOUtil.getDataFromInputStream(sign));
			fos.flush();
			fos.close();
			
			//carga el fichero zip
			ZipFile zf = new ZipFile(zipFile);
			
			//obtiene el archivo de firmas
			sign = zf.getInputStream(zf.getEntry("META-INF/documentsignatures.xml"));
				
			//recupera la raiz del documento de firmas
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
			Element root = dbf.newDocumentBuilder().parse(sign).getDocumentElement();
			
			//obtiene todas las firmas
			NodeList signatures = root.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Signature");
			
			int numSignatures = signatures.getLength();
			
			String[] arrayIds = new String[numSignatures];
			String[] arrayRef = new String[numSignatures];
			DefaultMutableTreeNode[] arrayNodes = new DefaultMutableTreeNode[numSignatures];
			
			for (int i = 0; i < numSignatures; i++) {
				Element signature = (Element)signatures.item(i);
				String sigId = signature.getAttribute("Id");	
				
				String strCert = signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "X509Certificate").item(0).getTextContent();
				DefaultMutableTreeNode node;
				
				if (asSimpleSignInfo) node = new DefaultMutableTreeNode(Utils.getSimpleSignInfoNode(null, signature));
				else node = new DefaultMutableTreeNode(AOUtil.getCN(AOCryptoUtil.createCert(strCert)));			
				arrayIds[i] = sigId;
				arrayNodes[i] = node;
				
				String typeReference = ((Element)signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Reference").item(0)).getAttribute("Type");
				if (typeReference.equals("http://uri.etsi.org/01903#CountersignedSignature")) {
					String uri = ((Element)signature.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Reference").item(0)).getAttribute("URI");
					arrayRef[i] = uri.substring(1, uri.length() - 5);
				}
				else arrayRef[i] = "";
			}
						
			DefaultMutableTreeNode tree = new DefaultMutableTreeNode("Datos");
			
			for (int i = numSignatures - 1; i > 0; i--) for (int j = 0; j < numSignatures; j++) {
				if (arrayRef[i].equals(arrayIds[j])) arrayNodes[j].add(arrayNodes[i]);
			}
			
			for (int i = 0; i < numSignatures; i++) if (arrayRef[i] == "") {
				tree.add(arrayNodes[i]);
			}
			
			try {
				zipFile.delete();
			} catch(Throwable e) {}
			
			return new JTree(tree).getModel();
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").warning (
				"Se ha producido un error al obtener la estructura de firmas. " + e.toString() 
			);
			e.printStackTrace();
			return new DefaultTreeModel(new DefaultMutableTreeNode("Ra\u00EDz"));
		}	
	}
	
	/**
	 * Indica si el fichero indicado es una firma v&aacute;lida del tipo de signer concreto. En el caso
	 * concreto del formato ODF, un documento, este firmado o no, puede considerarse una firma ya que es
	 * apto para ser cofirmado y, de cualquier forma, la contrafirma no es posible sobre este tipo de fichero.
	 * @param is Fichero que deseamos comprobar.
	 * @return Devuelve <code>true</code> si el fichero es una firma reconocida por este signer,
	 * <code>false</code> en caso contrario.
	 */
	public boolean isSign(InputStream is) {
		return isValidDataFile(is);
	}
	
	public boolean isValidDataFile(InputStream is) {
		
	    // Si el mimetype del fichero no se ajusta a alguno de los MimeTypes soportados
	    // para firma ODF se lanzara una excepcion, en ese caso deducimos que no es un
	    // fichero valido
	    String mimetype = null;
	    try {
	        mimetype = this.getODFMimeType(is);
	    } catch (Throwable e) {
	        return false;
        }
	    
	    // Sera valido si el mimetype coincide con alguno de los formatos ODF soportados
	    return mimetype != null && supportedFormats.contains(mimetype);
	}
	
	public String getSignedName(String originalName, String inText) {
		
		String inTextInt = (inText != null ? inText : ""); 
		
		if (originalName == null) return inTextInt + ".odf";
		String originalNameLC = originalName.toLowerCase(); 
		if (originalNameLC.length() <= 4) return originalName + inTextInt + ".odf";
		if (originalNameLC.endsWith(".odt")) return originalName.substring(0, originalName.length()-4) + inTextInt + ".odt";
		if (originalNameLC.endsWith(".odp")) return originalName.substring(0, originalName.length()-4) + inTextInt + ".odp";
		if (originalNameLC.endsWith(".ods")) return originalName.substring(0, originalName.length()-4) + inTextInt + ".ods";
		return originalName + inTextInt + ".odf";
	}

	/** M&eacute;todo no implementado. */
	public void setDataObjectFormat(String description, Oid objectIdentifier, javax.activation.MimeType mimeType, String encoding) {}

	private static void writeXML(OutputStream outStream, Node node, boolean indent)	{
		writeXML(new BufferedWriter(new OutputStreamWriter(outStream, Charset.forName("UTF-8"))), node, indent);
	}

	private static void writeXML(Writer writer, Node node, boolean indent) {
		try {
			Transformer serializer = TransformerFactory.newInstance().newTransformer();
			serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");

			if (indent) {
				serializer.setOutputProperty(OutputKeys.INDENT, "yes");
			}
			serializer.transform(new DOMSource(node), new StreamResult(writer));
		}
		catch(Throwable ex) {
			Logger.getLogger("es.gob.afirma").severe("Error al escribir el cuerpo del XML: "+ex);
		}
	}

    public byte[] getData(InputStream signData) throws AOInvalidFormatException {
        
        // Leemos la firma
        byte[] sign = null;
        try {
            sign = AOUtil.getDataFromInputStream(signData);
        } catch (IOException e) {
            throw new AOInvalidFormatException("No se pudieron leer los datos de entrada", e);
        } 

        // Si no es una firma ODF valida, lanzamos una excepcion
        if(!isSign(new ByteArrayInputStream(sign))) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida");
        }
        
        // TODO: Por ahora, devolveremos el propio ODF firmado.
        return sign;
    }


    public AOSignInfo getSignInfo(InputStream signData) throws AOInvalidFormatException, AOException {
        if(signData == null)
            throw new NullPointerException("No se han introducido datos para analizar");
        
        byte[] signDataReaded;
        try {
            signDataReaded = AOUtil.getDataFromInputStream(signData);
        } catch (Throwable e) {
            throw new AOException("No se han podido leer los datos de firma: "+e);
        }
        
        if(!isSign(new ByteArrayInputStream(signDataReaded))) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
        }
        
        AOSignInfo signInfo = new AOSignInfo(AOConstants.SIGN_FORMAT_ODF); 
        //TODO: Aqui podria venir el analisis de la firma buscando alguno de los otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo
        
        
        return signInfo;
    }

    public String getDataMimeType(InputStream signData) throws AOUnsupportedSignFormatException {

        String mimetype = getODFMimeType(signData);
        if (mimetype == null || !supportedFormats.contains(mimetype))
            throw new AOUnsupportedSignFormatException("La firma introducida no es un documento ODF");
        
        return mimetype;
    }

    private String getODFMimeType(InputStream signData) {
        String mimetype = "";
        try {
            // Genera el archivo zip temporal a partir del InputStream de entrada
            File zipFile = File.createTempFile("sign", ".zip");
            FileOutputStream fos = new FileOutputStream(zipFile);
            
            fos.write(AOUtil.getDataFromInputStream(signData));
            
            try { fos.flush(); } catch (Exception e) { }
            try { fos.close(); } catch (Exception e) { }
            
            //carga el fichero zip
            ZipFile zf = null;
            try {
            	zf = new ZipFile(zipFile);
            } catch (ZipException e) {
            	// Si detectamos que no es un fichero Zip, devolvemos null
            	return null;
            }
            
            //obtiene el archivo mimetype
            ZipEntry entry = zf.getEntry("mimetype");
            if(entry != null)
            	mimetype = new String(AOUtil.getDataFromInputStream(zf.getInputStream(entry)));

            try {
                zipFile.delete();
            } catch(Throwable e) {}
            
        } catch (Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al analizar el fichero de firma: "+e);
            return null;
        }
        return mimetype;
    }
    
}
