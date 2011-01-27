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

import static es.gob.afirma.misc.AOConstants.DEFAULT_MIMETYPE;
import static es.gob.afirma.misc.AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
import static es.gob.afirma.misc.AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
import static es.gob.afirma.misc.AOConstants.SIGN_ALGOS_URI;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XMLDSIG;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED;
import static es.gob.afirma.misc.AOConstants.SIGN_MODE_IMPLICIT;
import static es.gob.afirma.signers.xmlhelper.XMLConstants.DSIGNNS;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.AccessController;
import java.security.InvalidAlgorithmParameterException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Security;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.Vector;
import java.util.logging.Logger;

import javax.activation.MimeType;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
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
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import net.java.xades.util.XMLUtils;

import org.ietf.jgss.Oid;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import sun.misc.BASE64Decoder;
import es.gob.afirma.beans.AOSignInfo;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.MimeHelper;
import es.gob.afirma.misc.AOCryptoUtil.RawBASE64Encoder;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.signers.xmlhelper.Utils;
import es.gob.afirma.signers.xmlhelper.Utils.CannotDereferenceException;
import es.gob.afirma.signers.xmlhelper.Utils.IsInnerlException;
import es.gob.afirma.signers.xmlhelper.Utils.ReferenceIsNotXMLException;

/**
 * Operaciones de firmas en formato XMLDSign.<p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 *  <dt>uri</dt>
 *  	<dd>URI en la que se encuentra el documento, necesario en el caso de modo expl&iacute;cito y formato detached</dd>
 *  <dt>mode</dt>
 *  	<dd>Modo de firma a usar (Expl&iacute;cita o Impl&iacute;cita)</dd>
 *  <dt>format</dt>
 *  	<dd>Formato en que se realizar&aacute; la firma</dd>
 *  <dt>precalculatedHashAlgorithm</dt>
 *  	<dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 *  <dt>xmlTransforms</dt>
 *  	<dd>N&uacute;mero de transformaciones a aplicar al XML antes de firmarlo</dd>
 *  <dt>xmlTransform<i>n</i>Type</dt>
 *  	<dd>Tipo de la transformaci&oacute;n <i>n</i> (debe ser la URL del algoritmo segun define W3C)</dd>
 *  <dt>xmlTransform<i>n</i>Subtype</dt>
 *  	<dd>Subtipo de la transformaci&oacute;n <i>n</i> (por ejemplo, "intersect", "subtract" o "union" para XPATH2)</dd>  
 *  <dt>xmlTransform<i>n</i>Body</dt>
 *      <dd>Cuerpo de la transformaci&oacute;n <i>n</i></dd>
 *  <dt>referencesDigestMethod</dt>
 *  	<dd>Algoritmo de huella digital a usar en las referencias XML</dd>
 *  <dt>canonicalizationAlgorithm</dt>
 *      <dd>Algoritmo de canonicalizaci&oacute;n<i>n</i></dd>
 *  <dt>ignoreStyleSheets</dt>
 *      <dd>Ignora las hojas de estilo externas de los XML (no las firma) si se establece a <code>true</code>, si se establece a <code>false</code> s&iacute; las firma</dd>
 *  <dt>avoidBase64Transforms</dt>
 *      <dd>No declara transformaciones Base64 incluso si son necesarias si se establece a <code>true</code>, si se establece a <code>false</code> act&uacute;a normalmente (s&iacute; las declara)</dd>
 *  <!-- <dt>headLess</dt>
 *      <dd>Evita cualquier interacci&oacute;n con el usuraio si se establece a <code>true</code>, si se establece a <code>false</code> act&uacute;a normalmente (puede mostrar di&aacute;logos, por ejemplo, para la dereferenciaci&oacute;n de hojas de estilo enlazadas con rutas relativas). &Uacute;til para los procesos desatendidos y por lotes</dd> -->            
 * </dl>
 * <p>
 *  Tratamiento de las hojas de estilo en firmas XML:
 *  <ul>
 *   <li>Firmas XML Enveloped</li>
 *   <ul>
 *    <li>Hoja de estilo con ruta relativa</li>
 *    <ul>
 *     <li>No se firma.</li>
 *    </ul>
 *    <li>Hola de estilo remota con ruta absoluta</li>
 *    <ul>
 *     <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original</li>
 *     <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 *    </ul>
 *    <li>Hoja de estilo empotrada</li>
 *    <ul>
 *     <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original</li>
 *    </ul>
 *   </ul>
 *   <li>Firmas XML Externally Detached</li>
 *   <ul>
 *    <li>Hoja de estilo con ruta relativa</li>
 *    <ul>
 *     <li>No se firma.</li>
 *    </ul>
 *    <li>Hola de estilo remota con ruta absoluta</li>
 *    <ul>
 *     <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 *    </ul>
 *    <li>Hoja de estilo empotrada</li>
 *    <ul>
 *     <li>No es necesaria ninguna acci&oacute;n</li>
 *    </ul>
 *   </ul>
 *   <li> Firmas XML Enveloping</li>
 *   <ul>
 *   <li>Hoja de estilo con ruta relativa</li>
 *    <ul>
 *     <li>No se firma.</li>
 *    </ul>
 *    <li>Hola de estilo remota con ruta absoluta</li>
 *    <ul>
 *     <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 *    </ul>
 *    <li>Hoja de estilo empotrada</li>
 *    <ul>
 *     <li>No es necesaria ninguna acci&oacute;n</li>
 *    </ul>
 *   </ul>
 *   <li>Firmas XML Internally Detached</li>
 *   <ul>
 *    <li>Hoja de estilo con ruta relativa</li>
 *    <ul>
 *     <li>No se firma.</li>
 *    </ul>
 *    <li>Hola de estilo remota con ruta absoluta</li>
 *    <ul>
 *     <li>Se firma una referencia (canonicalizada) a esta hoja remota</li>
 *    </ul>
 *    <li>Hoja de estilo empotrada</li>
 *    <ul>
 *     <li>No es necesaria ninguna acci&oacute;n</li>
 *    </ul>
 *   </ul>   
 *  </ul>
 * </p>
 *    
 * @version 0.2
 */
public final class AOXMLDSigSigner implements AOSigner {

	/** URI que define la versi&oacute;n por defecto de XAdES. */
	private static final String XADESNS = "http://uri.etsi.org/01903#";

	/** URI que define una referencia de tipo OBJECT. */
	private static final String OBJURI = "http://www.w3.org/2000/09/xmldsig#Object";

	private static final String CSURI = "http://uri.etsi.org/01903#CountersignedSignature";
	private static final String AFIRMA = "AFIRMA";
	private static final String XML_SIGNATURE_PREFIX = "dsig";
	private static final String SIGNATURE_NODE_NAME = (XML_SIGNATURE_PREFIX == null || "".equals(XML_SIGNATURE_PREFIX) ? "" : XML_SIGNATURE_PREFIX + ":") + "Signature";
	private static final String DETACHED_CONTENT_ELEMENT_NAME = "CONTENT";
	private static final String DETACHED_STYLE_ELEMENT_NAME = "STYLE";

	/** Algoritmo de huella digital por defecto para las referencias XML. */
	private static final String DIGEST_METHOD = DigestMethod.SHA1;

	private String algo;
	private Document doc;

	/** Codificaci&oacute;n del contenido a firmar. */
	private String encoding = null;

	/** Tipo de contenido a firmar. */
	private String mimeType = null;


	static {
		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				if (System.getProperty("java.version").startsWith("1.5")) {
					try {
						Security.addProvider(new org.jcp.xml.dsig.internal.dom.XMLDSigRI());
					} 
					catch (final Throwable e) {
						Logger.getLogger("es.gob.afirma").warning(
								"No se ha podido agregar el proveedor de firma XMLDSig necesario para firmas XML: " + e
						);
					}
				}
				return null;
			}
		});
	}	

	public byte[] sign(final InputStream file, String algorithm, final PrivateKeyEntry keyEntry, final X509Certificate cert, Properties extraParams) throws AOException {						

		// Algoritmos de firma con nombres alternativos
		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = SIGN_ALGORITHM_SHA1WITHDSA;

		String algoUri = SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '"+algorithm+"'");
		}
		
		if (extraParams == null) extraParams = new Properties();
		final String format = extraParams.getProperty("format", SIGN_FORMAT_XMLDSIG_ENVELOPING);
		final String mode   = extraParams.getProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);
		final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD);
		final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE);
		final boolean ignoreStyleSheets = Boolean.parseBoolean(extraParams.getProperty("ignoreStyleSheets", "true"));
		final boolean avoidBase64Transforms = Boolean.parseBoolean(extraParams.getProperty("avoidBase64Transforms", "false"));
		final boolean headLess = Boolean.parseBoolean(extraParams.getProperty("headLess", "true"));

		URI uri = null;
		try {
			uri = new URI(extraParams.getProperty("uri"));
		}
		catch(final Throwable e) {}

		final String precalculatedHashAlgorithm = extraParams.getProperty("precalculatedHashAlgorithm");

		Utils.checkIllegalParams(format, mode, uri, precalculatedHashAlgorithm, false);

		// Datos principales a firmar (que tambien pueden ser un message digest)
		byte[] mainData = null;
		try {
			mainData = AOUtil.getDataFromInputStream(file);
		}
		catch(final Throwable e) {
			// Un externally detached con URL permite los datos nulos o vacios
			if (!(format.equals(SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED) && uri != null)) 
				throw new AOException("No se han podido leer los datos a firmar");
		}

		// Propiedades del documento XML original
		final Hashtable<String, String> originalXMLProperties = new Hashtable<String, String>();

		//carga el documento xml
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		// Elemento de datos
		Element dataElement;

		final String contentId = DETACHED_CONTENT_ELEMENT_NAME + "-" + UUID.randomUUID().toString();
		final String styleId = DETACHED_STYLE_ELEMENT_NAME  + "-" + UUID.randomUUID().toString();
		boolean isBase64 = false;
		boolean wasEncodedToBase64 = false;

		// Elemento de estilo
		Element styleElement = null;
		String styleType = null;
		String styleHref = null;
		String styleEncoding = null;

		if (mode.equals(SIGN_MODE_IMPLICIT)) {			
			try {
				// Obtenemos el objeto XML y su codificacion
				final Document docum = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(mainData));

				// Obtenemos la hoja de estilo del XML
				try {
					Properties p;
					if (!ignoreStyleSheets) p = Utils.getStyleSheetHeader(new String(mainData));
					else p = new Properties(); 
					styleType = p.getProperty("type");
					styleHref = p.getProperty("href");

					if (styleType != null && styleHref != null) {

						Logger.getLogger("es.gob.afirma").info(
								"Se ha encontrado una hoja de estilo asociada al XML a firmar: tipo=" + styleType + ", referencia=" + styleHref
						);

						Logger.getLogger("es.gob.afirma").info("Dereferenciando la hoja de estilo");
						try {
							final Document tmpDoc = Utils.dereferenceStyleSheet(
									TransformerFactory.newInstance().getAssociatedStylesheet(
											new StreamSource(new ByteArrayInputStream(mainData)),
											null,
											null,
											null
									).getSystemId(),
									headLess
							); 

							// Cuidado!! Solo rellenamos el Elemento DOM si no es HTTP o HTTPS, porque si es accesible
							// remotamente no necesito el elemento, ya que se firma via referencia Externally Detached
							if (!styleHref.startsWith("http://") && !styleHref.startsWith("https://")) styleElement = tmpDoc.getDocumentElement();

							styleEncoding = tmpDoc.getXmlEncoding();
						}
						catch(final IsInnerlException ex) {
							Logger.getLogger("es.gob.afirma").info(
									"La hoja de estilo esta referenciada internamente, por lo que no se necesita dereferenciar"
							);
						}
						catch(final ReferenceIsNotXMLException ex) {
							Logger.getLogger("es.gob.afirma").warning(
									"La hoja de estilo referenciada no es XML o no se ha dereferenciado apropiadamente"
							);
						}
						catch(final CannotDereferenceException ex) {
							Logger.getLogger("es.gob.afirma").warning(
									"La hoja de estilo no ha podido dereferenciar, probablemente sea un enlace relativo local"
							);
						}
						catch(final Throwable ex) {
							Logger.getLogger("es.gob.afirma").severe(
									"Error intentando dereferenciar la hoja de estilo: " + ex
							);
						}
					}
				}
				catch(Throwable e) {
					Logger.getLogger("es.gob.afirma").info(
							"No se ha encontrado ninguna hoja de estilo asociada al XML a firmar"
					);
				}

				// Si no hay asignado un MimeType o es el por defecto establecemos el de XML
				if (mimeType == null || DEFAULT_MIMETYPE.equals(mimeType)) mimeType = "text/xml";

				if (encoding == null) encoding = docum.getXmlEncoding();

				// Ademas del encoding, sacamos otros datos del doc XML original

				// Hacemos la comprobacion del base64 por si se establecido desde fuera
				if (encoding != null && !encoding.equalsIgnoreCase("base64")) originalXMLProperties.put(OutputKeys.ENCODING, encoding);
				String tmpXmlProp = docum.getXmlVersion();
				if (tmpXmlProp != null) originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
				final DocumentType dt = docum.getDoctype();
				if (dt!=null) {
					tmpXmlProp = dt.getSystemId();
					if (tmpXmlProp != null) originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
				}

				if (format.equals(SIGN_FORMAT_XMLDSIG_DETACHED)) {
					dataElement = docum.createElement(DETACHED_CONTENT_ELEMENT_NAME);
					dataElement.setAttribute("Id", contentId);
					dataElement.setAttribute("MimeType", mimeType);
					dataElement.setAttribute("Encoding", encoding);
					dataElement.appendChild(docum.getDocumentElement());

					// Tambien el estilo
					if (styleElement != null) {
						try {
							final Element tmpStyleElement = docum.createElement(DETACHED_STYLE_ELEMENT_NAME);
							tmpStyleElement.setAttribute("Id", styleId);
							if (styleType != null) tmpStyleElement.setAttribute("MimeType", styleType);
							tmpStyleElement.setAttribute("Encoding", styleEncoding);

							tmpStyleElement.appendChild(
									docum.adoptNode(
											styleElement.cloneNode(true)
									)
							);

							styleElement = tmpStyleElement;
						}
						catch(final Throwable e) {
							Logger.getLogger("es.gob.afirma").warning(
									"No ha sido posible crear el elemento DOM para incluir la hoja de estilo del XML como Internally Detached: " + e
							);
							styleElement = null;
						}
					}
				}
				else dataElement = docum.getDocumentElement();

			}
			//captura de error en caso de no ser un documento xml
			catch (final Throwable e) {			
				if (format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
					throw new AOFormatFileException("El modo Enveloped solo permite firmar datos XML");
				}
				//para los formatos de firma internally detached y enveloping se trata de convertir el documento a base64				
				try {
					Logger.getLogger("es.gob.afirma").info(
							"El documento no es un XML valido. Se convertira a Base64: " + e
					);

					//crea un nuevo nodo xml para contener los datos en base 64
					final Document docFile = dbf.newDocumentBuilder().newDocument();
					dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);
					uri = null;
					encoding = "base64";
					if (mimeType == null) mimeType = DEFAULT_MIMETYPE;

					dataElement.setAttribute("Id", contentId);
					dataElement.setAttribute("Encoding", encoding);

					// Si es base 64, lo firmamos indicando como contenido el dato pero, ya que puede
					// poseer un formato particular o caracteres valido pero extranos para el XML,
					// realizamos una decodificacion y recodificacion para asi homogenizar el formato.
					if (AOUtil.isBase64(mainData)) {
						Logger.getLogger("es.gob.afirma").info("El documento se considera Base64, se insertara como tal en el XML");

						// Adicionalmente, si es un base 64 intentamos obtener el tipo del contenido
						// decodificado para asi reestablecer el MimeType.
						final byte[] decodedData = new BASE64Decoder().decodeBuffer(new String(mainData).trim());
						final MimeHelper mimeTypeHelper = new MimeHelper(decodedData);
						final String tempMimeType = mimeTypeHelper.getMimeType();
						mimeType = tempMimeType != null ? tempMimeType : DEFAULT_MIMETYPE;
						dataElement.setAttribute("MimeType", mimeType);

						dataElement.setTextContent(new RawBASE64Encoder().encode(decodedData));
					}
					else {
						Logger.getLogger("es.gob.afirma").info(
								"El documento se considera binario, se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion"
						);

						// Usamos el MimeType identificado
						dataElement.setAttribute("MimeType", mimeType);

						dataElement.setTextContent(new RawBASE64Encoder().encode(mainData));
						wasEncodedToBase64 = true;
					}
					isBase64 = true;
				}
				catch (final Throwable ex) {
					throw new AOException("Error al convertir los datos a base64: " + ex.toString());
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
					tmpData = AOUtil.getDataFromInputStream(AOUtil.loadFile(uri, null, false));
				}
				catch(final Throwable e) {
					throw new AOException(
							"No se han podido obtener los datos de la URI externa"
					);
				}
				// Vemos si hemos obtenido bien los datos de la URI
				if (tmpData != null && tmpData.length > 0) {
					try {
						digestValue = MessageDigest.getInstance("SHA1").digest(tmpData);
					}
					catch(final Throwable e) {
						throw new AOException(
								"No se ha podido obtener el SHA1 de los datos de la URI externa: " + e
						);
					}
				}
			}
			// Si no tenemos URI entonces calculamos el hash de los datos
			else {
				try {
					digestValue = MessageDigest.getInstance("SHA1").digest(mainData);
				}
				catch(Throwable e) {
					throw new AOException(
							"No se ha podido obtener el SHA1 de los datos proporcionados: " + e
					);
				}
			}
			if (digestValue == null || digestValue.length < 1) throw new AOException(
					"Error al obtener la huella SHA1 de los datos"
			);

			Document docFile;
			try {
				docFile = dbf.newDocumentBuilder().newDocument();
			}
			catch(final Throwable e) {
				throw new AOException("No se ha podido crear el documento XML contenedor: " + e);
			}
			dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);

			encoding = "base64";
			// En el caso de la firma explicita, se firma el Hash de los datos en lugar de los propios datos.
			// En este caso, los indicaremos a traves del MimeType en donde establecemos un tipo especial
			// que designa al hash. Independientemente del algoritmo de firma utilizado, el Hash de las firmas
			// explicitas de datos siempre sera SHA1, salvo que el hash se haya establecido desde fuera.
			if(precalculatedHashAlgorithm != null) {
				mimeType = "hash/"+precalculatedHashAlgorithm.toLowerCase();
			} else {
				mimeType = "hash/sha1";
			}

			dataElement.setAttribute("Id", contentId);
			dataElement.setAttribute("MimeType", mimeType);
			dataElement.setAttribute("Encoding", encoding);

			dataElement.setTextContent(new RawBASE64Encoder().encode(digestValue));
			isBase64 = true;

			// FIN BLOQUE EXPLICITO
		}

		//***************************************************
		//***************************************************

		final String tmpUri = "#" + contentId;
		final String tmpStyleUri = "#" + styleId;

		// Crea el nuevo documento de firma
		Document docSignature = null;
		try {
			docSignature = dbf.newDocumentBuilder().newDocument();
			if (format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
				docSignature.appendChild(docSignature.adoptNode(dataElement));
			}
			else {
				docSignature.appendChild(docSignature.createElement(AFIRMA));
			}
		}
		catch(final Throwable e) {
			throw new AOException("Error al crear la firma en formato " + format + ", modo " + mode + ": " + e);
		}

		final List<Reference> referenceList = new ArrayList<Reference>();
		final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
		DigestMethod digestMethod;
		try {
			digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
		}
		catch(Throwable e) {
			throw new AOException(
					"No se ha podido obtener un generador de huellas digitales para el algoritmo '" + 
					digestMethodAlgorithm + "': " + e
			);
		}
		final String referenceId = "Reference-" + UUID.randomUUID().toString();
		final String referenceStyleId = "StyleReference-" + UUID.randomUUID().toString();

		final List<Transform> transformList = new ArrayList<Transform>();

		//Primero anadimos las transformaciones a medida
		Utils.addCustomTransforms(transformList, extraParams, XML_SIGNATURE_PREFIX);

		// Solo canonicalizo si es XML
		if (!isBase64) {
			try {
				//Transformada para la canonicalizacion inclusiva
				transformList.add(
						fac.newTransform(
								canonicalizationAlgorithm, 
								(TransformParameterSpec)null
						)
				);
			}
			catch(final Throwable e) {
				Logger.getLogger("es.gob.afirma").severe(
						"No se puede encontrar el algoritmo de canonicalizacion, la referencia no se canonicalizara: " + e
				);
			}
		}
		// Si no era XML y tuve que convertir a Base64 yo mismo declaro la transformación 
		else if (wasEncodedToBase64 && !avoidBase64Transforms) {
			try {
				transformList.add(fac.newTransform(Transform.BASE64, (TransformParameterSpec)null));
			}
			catch(final Throwable e) {
				Logger.getLogger("es.gob.afirma").severe(
						"No se puede encontrar el algoritmo transformacion Base64, esta no se declarara: " + e
				);
			}
		}

		//crea una referencia al documento insertado en un nodo Object para la firma enveloping y a el estilo
		XMLObject envelopingObject = null;
		XMLObject envelopingStyleObject = null;

		if (format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPING)) {
			try {
				//crea el nuevo elemento Object que contiene el documento a firmar
				final List<XMLStructure> structures = new ArrayList<XMLStructure>(1);

				//Si los datos se han convertido a base64, bien por ser binarios o explicitos 
				if (isBase64)
					structures.add(new DOMStructure(dataElement.getFirstChild()));
				else
					structures.add(new DOMStructure(dataElement));

				final String objectId = "Object-" + UUID.randomUUID().toString();
				envelopingObject = fac.newXMLObject(structures, objectId, mimeType, encoding);
				
				//crea la referencia al nuevo elemento Object
				referenceList.add(
						fac.newReference(
								"#" + objectId, 
								digestMethod, 
								transformList, 
								OBJURI, 
								referenceId
						)
				);

				// Vamos con la hoja de estilo
				if (styleElement != null) {
					final String objectStyleId = "StyleObject-" + UUID.randomUUID().toString();
					envelopingStyleObject = fac.newXMLObject(
							Collections.singletonList(
									new DOMStructure(styleElement)
							), 
							objectStyleId, 
							styleType,
							styleEncoding
					);
					referenceList.add(
							fac.newReference(
									"#" + objectStyleId, 
									digestMethod,
									Collections.singletonList(
											fac.newTransform(
													canonicalizationAlgorithm, 
													(TransformParameterSpec)null
											)
									), 
									OBJURI, 
									referenceStyleId
							)
					);

				}
			}
			catch(final Throwable e) {
				throw new AOException("Error al generar la firma en formato enveloping: " + e);
			}

			// Hojas de estilo para enveloping en Externally Detached
			if (styleHref != null && styleElement == null) {
				// Comprobamos si la referencia al estilo es externa
				if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
					try {
						referenceList.add(
								fac.newReference(
										styleHref, 
										digestMethod,
										Collections.singletonList(
												fac.newTransform(
														canonicalizationAlgorithm, 
														(TransformParameterSpec)null
												)
										),
										null,
										referenceStyleId
								)
						);
					}
					catch(final Throwable e) {
						Logger.getLogger("es.agob.afirma").severe(
								"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e
						);
					}
				}
			}

		}

		//crea una referencia al documento mediante la URI hacia el identificador del nodo CONTENT
		else if (format.equals(SIGN_FORMAT_XMLDSIG_DETACHED)) {
			try {
				if (dataElement != null) {
					//inserta en el nuevo documento de firma el documento a firmar
					docSignature.getDocumentElement().appendChild(docSignature.adoptNode(dataElement));
					//crea la referencia a los datos firmados que se encontraran en el mismo documento
					referenceList.add(
							fac.newReference(
									tmpUri, 
									digestMethod, 
									transformList, 
									null, 
									referenceId
							)
					);
				}
				if (styleElement != null) {
					//inserta en el nuevo documento de firma la hoja de estilo
					docSignature.getDocumentElement().appendChild(docSignature.adoptNode(styleElement));
					//crea la referencia a los datos firmados que se encontraran en el mismo documento
					referenceList.add(
							fac.newReference(
									tmpStyleUri, 
									digestMethod, 
									Collections.singletonList(
											fac.newTransform(
													canonicalizationAlgorithm, 
													(TransformParameterSpec)null
											)
									), 
									null, 
									referenceStyleId
							)
					);
				}

			}
			catch(final Throwable e) {
				throw new AOException("Error al generar la firma en formato detached implicito: " + e);
			}

			// Hojas de estilo remotas para detached
			if (styleHref != null && styleElement == null) {
				// Comprobamos si la referencia al estilo es externa
				if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
					try {
						referenceList.add(
								fac.newReference(
										styleHref, 
										digestMethod,
										Collections.singletonList(
												fac.newTransform(
														canonicalizationAlgorithm, 
														(TransformParameterSpec)null
												)
										),
										null,
										referenceStyleId
								)
						);
					}
					catch(final Throwable e) {
						Logger.getLogger("es.agob.afirma").severe(
								"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e
						);
					}
				}
			}

		}

		// Crea una referencia al documento mediante la URI externa si la tenemos o usando un Message Digest
		// precalculado si no tenemos otro remedio
		else if (format.equals(SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)) {
			Reference ref = null;
			// No tenemos uri, suponemos que los datos son el message digest
			if (precalculatedHashAlgorithm != null &&(uri == null || uri.getScheme().equals("") || uri.getScheme().equals("file"))) {
				DigestMethod dm = null;
				try {
					// Convertimos el algo del Message Digest externo a la nomenclatura XML
					if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA1"))
						dm = fac.newDigestMethod(DigestMethod.SHA1, null);
					else if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-256"))
						dm = fac.newDigestMethod(DigestMethod.SHA256, null);
					else if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-512"))
						dm = fac.newDigestMethod(DigestMethod.SHA512, null);
					else if (AOCryptoUtil.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("RIPEMD160"))
						dm = fac.newDigestMethod(DigestMethod.RIPEMD160, null);
				}
				catch(final Throwable e) {
					throw new AOException(
							"No se ha podido crear el metodo de huella digital para la referencia Externally Detached: " + e
					);
				}
				if (dm == null) throw new AOException(
						"Metodo de Message Digest para la referencia Externally Detached no soportado: " + precalculatedHashAlgorithm
				);				
				ref = fac.newReference(
						"", 
						dm, 
						null, 
						null, 
						referenceId, 
						mainData
				);
			}
			// Tenemos URI y no nos han establecido algoritmo de message digest,
			// por lo que es una referencia externa accesible
			else {
				// Si es una referencia de tipo file:// obtenemos el fichero y creamos una referencia solo con
				// el message digest
				if (uri.getScheme().equals("file")) {
					try {
						ref = fac.newReference(
								"", 
								digestMethod, 
								null, 
								null, 
								referenceId, 
								MessageDigest.getInstance(AOCryptoUtil.getDigestAlgorithmName(digestMethodAlgorithm)).digest(
										AOUtil.getDataFromInputStream(AOUtil.loadFile(uri, null, false))
								)
						);
					}
					catch(final Throwable e) {
						throw new AOException(
								"No se ha podido crear la referencia XML a partir de la URI local (" + uri.toASCIIString() + "): " + e
						);
					}
				}
				// Si es una referencia distinta de file:// suponemos que es dereferenciable de forma universal
				// por lo que dejamos que Java lo haga todo
				else {
					try {
						ref = fac.newReference(
								uri.toASCIIString(),
								digestMethod
						);
					}
					catch(final Throwable e) {
						throw new AOException(
								"No se ha podido crear la referencia Externally Detached, probablemente por no obtenerse el metodo de digest: " + e
						);
					}
				}
			}
			if (ref == null) throw new AOException(
					"Error al generar la firma Externally Detached, no se ha podido crear la referencia externa"
			); 
			referenceList.add(ref);

			// Hojas de estilo remotas en Externally Detached
			if (styleHref != null && styleElement == null) {
				// Comprobamos que la URL es valida
				if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
					try {
						referenceList.add(
								fac.newReference(
										styleHref, 
										digestMethod,
										Collections.singletonList(
												fac.newTransform(
														canonicalizationAlgorithm, 
														(TransformParameterSpec)null
												)
										),
										null,
										referenceStyleId
								)
						);
					}
					catch(final Throwable e) {
						Logger.getLogger("es.agob.afirma").severe(
								"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e
						);
					}
				}
				else Logger.getLogger("es.gob.afirma").warning(
						"Se necesita una referencia externa HTTP o HTTPS a la hoja de estilo para referenciarla en firmas XML Externally Detached"
				);
			}

		}

		//crea una referencia indicando que se trata de una firma enveloped
		else if (format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
			try {

				// Transformacion XPATH para eliminar el resto de firmas del documento
				transformList.add(
						fac.newTransform(
								Transform.XPATH, 
								new XPathFilterParameterSpec(
										"not(ancestor-or-self::" + XML_SIGNATURE_PREFIX + ":Signature)", 
										Collections.singletonMap(XML_SIGNATURE_PREFIX, XMLSignature.XMLNS)
								)
						)
				);

				// Transformacion enveloped
				transformList.add(fac.newTransform(Transform.ENVELOPED, (TransformParameterSpec)null));

				//crea la referencia
				referenceList.add(
						fac.newReference(
								"", 
								digestMethod, 
								transformList, 
								null, 
								referenceId
						)
				);
			}
			catch(final Throwable e) {				
				throw new AOException("Error al generar la firma en formato enveloped: " + e);
			}

			// Hojas de estilo remotas para enveloped
			if (styleHref != null && styleElement == null) {
				// Comprobamos si la referencia al estilo es externa
				if (styleHref.startsWith("http://") || styleHref.startsWith("https://")) {
					try {
						referenceList.add(
								fac.newReference(
										styleHref, 
										digestMethod,
										Collections.singletonList(
												fac.newTransform(
														canonicalizationAlgorithm, 
														(TransformParameterSpec)null
												)
										),
										null,
										referenceStyleId
								)
						);
					}
					catch(final Throwable e) {
						Logger.getLogger("es.agob.afirma").severe(
								"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e
						);
					}
				}

			}

		}


		//definicion de identificadores
		final String id = UUID.randomUUID().toString();
		final String keyInfoId = "KeyInfo-" + id;

		try {

			//se anade una referencia a KeyInfo
			referenceList.add(fac.newReference("#" + keyInfoId, digestMethod, transformList, null, null));

			//KeyInfo
			final KeyInfoFactory kif = fac.getKeyInfoFactory();
			final List<Object> content = new ArrayList<Object>();
			content.add(kif.newKeyValue(cert.getPublicKey()));
			content.add(kif.newX509Data(Collections.singletonList(cert)));		//TODO: Con esto solo agregamos el certificado de firma
//			content.add(kif.newX509Data(Arrays.asList(keyEntry.getCertificateChain())));

			//Object
			final List<XMLObject> objectList = new ArrayList<XMLObject>();

			//en el caso de formato enveloping se inserta el elemento Object con el documento a firmar
			if (format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPING) && (envelopingObject != null)) {
				objectList.add(envelopingObject);
				if (envelopingStyleObject != null) objectList.add(envelopingStyleObject);
			}

			// Si es enveloped hay que anadir la hoja de estilo dentro de la firma y
			// referenciarla
			if (format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
				if (styleElement != null) { 
					objectList.add(fac.newXMLObject(
							Collections.singletonList(new DOMStructure(styleElement)), 
							styleId, 
							styleType, 
							styleEncoding
					));
					try {
						referenceList.add(
								fac.newReference(
										tmpStyleUri, 
										digestMethod, 
										Collections.singletonList(
												fac.newTransform(
														canonicalizationAlgorithm, 
														(TransformParameterSpec)null
												)
										), 
										null, 
										referenceStyleId
								)
						);
					}
					catch(final Throwable e) {
						Logger.getLogger("es.gob.afirma").severe(
								"No se ha podido anadir una referencia a la hoja de estilo, esta se incluira dentro de la firma, pero no estara firmada: " + e
						);
					}
				}
			}

			//genera la firma
			final XMLSignature signature = fac.newXMLSignature(
					fac.newSignedInfo(
							fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec)null), 
							fac.newSignatureMethod(algoUri, null), 
							Utils.cleanReferencesList(referenceList)
					), 
					kif.newKeyInfo(content, keyInfoId), 
					objectList, 
					"Signature-" + id, 
					"SignatureValue-" + id
			);

			final DOMSignContext signContext = new DOMSignContext(keyEntry.getPrivateKey(), docSignature.getDocumentElement());        
			signContext.putNamespacePrefix(DSIGNNS, XML_SIGNATURE_PREFIX);
			signature.sign(signContext);
		}
		catch(final NoSuchAlgorithmException e) {
			throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '"+algorithm+"'", e);
		}
		catch(final Throwable e) {
			//e.printStackTrace();
			throw new AOException("Error al generar la firma XMLdSig: " + e);
		}	


		// Si se esta realizando una firma enveloping simple no tiene sentido el nodo raiz,
		// asi que sacamos el nodo de firma a un documento aparte
		if(format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPING)) {
			try {
				if (docSignature.getElementsByTagName(SIGNATURE_NODE_NAME).getLength() == 1) {
					final Document newdoc = dbf.newDocumentBuilder().newDocument();
					newdoc.appendChild(newdoc.adoptNode(docSignature.getElementsByTagName(SIGNATURE_NODE_NAME).item(0)));
					docSignature = newdoc;
				}
			}
			catch(final Throwable e) {
				Logger.getLogger("es.gob.afirma").info("No se ha eliminado el nodo padre '<AFIRMA>': " + e);
			}
		}

		encoding = null;
		mimeType = null;

		// Si no es enveloped quito los valores para que no se inserte la cabecera de hoja de estilo
		if (!format.equals(SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
			styleHref = null;
			styleType = null;
		}
		return Utils.writeXML(docSignature.getDocumentElement(), originalXMLProperties, styleHref, styleType);

	}	

	/**
	 * Comprueba si la firma es detached
	 * @param element Elemento que contiene el nodo ra$iacute;z del documento que se quiere comprobar
	 * @return Valor booleano, siendo verdadero cuando la firma es detached
	 */
	private boolean isDetached(final Element element) {
		if (element == null) return false;
		if (element.getFirstChild().getLocalName() != null && 
				element.getFirstChild().getLocalName().equals(DETACHED_CONTENT_ELEMENT_NAME))
			return true;
		return false;
	}

	/**
	 * Comprueba si la firma es enveloped
	 * @param element Elemento que contiene el nodo ra$iacute;z del documento que se quiere comprobar
	 * @return Valor booleano, siendo verdadero cuando la firma es enveloped
	 */
	private boolean isEnveloped(final Element element) {
		NodeList transformList = element.getElementsByTagNameNS(DSIGNNS, "Transform");
		for (int i = 0; i < transformList.getLength(); i++)
			if (((Element)transformList.item(i)).getAttribute("Algorithm").equals(Transform.ENVELOPED))
				return true;

		return false;
	}

	/**
	 * Comprueba si la firma es enveloping
	 * @param element Elemento que contiene el nodo ra$iacute;z del documento que se quiere comprobar
	 * @return Valor booleano, siendo verdadero cuando la firma es enveloping
	 */
	private boolean isEnveloping(final Element element) {
		if (element == null) return false;
		if (element.getLocalName().equals("Signature") || 
				(element.getLocalName().equals(AFIRMA) && 
						element.getFirstChild().getLocalName().equals("Signature")))
			return true;

		return false;
	}

	public byte[] getData(final InputStream signData) throws AOInvalidFormatException {		
		//nueva instancia de DocumentBuilderFactory que permita espacio de nombres (necesario para XML)
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		Element rootSig;
		Element elementRes = null;
		try {
			//se mantiene una copia de signData
			byte[] mainData = AOUtil.getDataFromInputStream(signData);

			//comprueba que sea una documento de firma valido
			ByteArrayInputStream isSig = new ByteArrayInputStream(mainData);
			if (!isSign(isSig))
				throw new AOInvalidFormatException("El documento no es un documento de firmas válido.");

			//obtiene la raiz del documento de firmas
			ByteArrayInputStream is = new ByteArrayInputStream(mainData);
			rootSig = dbf.newDocumentBuilder().parse(is).getDocumentElement();

			//si es detached
			if (this.isDetached(rootSig)) {
				Element firstChild = (Element)rootSig.getFirstChild();
				//si el documento es un xml se extrae como tal
				if (firstChild.getAttribute("MimeType").equals("text/xml")) {
					elementRes = (Element)firstChild.getFirstChild();
				}
				// Si el MimeType es de tipo Hash (tipo creado para el cliente afirma) asi que la firma no tiene datos  
				//else if (firstChild.getAttribute("MimeType").startsWith("hash/")) {
				//	elementRes = null;
				//}
				//si el documento es binario se deshace la codificacion en Base64
				else {
					String content = firstChild.getTextContent();
					return new BASE64Decoder().decodeBuffer(content);
				}
			}

			//si es enveloped
			else if (this.isEnveloped(rootSig)) {
				//obtiene las firmas y las elimina
				NodeList signatures = rootSig.getElementsByTagNameNS(DSIGNNS, "Signature");
				int numSignatures = signatures.getLength();
				for (int i = 0; i < numSignatures; i++)
					rootSig.removeChild(signatures.item(0));

				elementRes = rootSig;
			}

			//si es enveloping
			else if (this.isEnveloping(rootSig)) {
				//obtiene el nodo Object de la primera firma 
				Element object = (Element)rootSig.getElementsByTagNameNS(DSIGNNS, "Object").item(0);
				//si el documento es un xml se extrae como tal
				if (object.getAttribute("MimeType").equals("text/xml")) {
					elementRes = (Element)object.getFirstChild();
				}
				// Si el MimeType es de tipo Hash (tipo creado para el cliente afirma) asi que la firma no tiene datos  
				//else if (object.getAttribute("MimeType").startsWith("hash/")) {
				//	elementRes = null;
				//}
				//si el documento es binario se deshace la codificacion en Base64
				else {
					String content = object.getTextContent();
					return new BASE64Decoder().decodeBuffer(content);
				}
			}
		}		
		catch(final Throwable ex) {
			throw new AOInvalidFormatException("Error al leer el fichero de firmas. " + ex.toString());
		}	

		//si no se ha recuperado ningún dato se devuelve null
		if (elementRes == null)
			return null;

		//convierte el documento obtenido en un array de bytes
		final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
		XMLUtils.writeXML(baosSig, elementRes, false);
		return baosSig.toByteArray();
	}

	public void setDataObjectFormat(final String description, final Oid objectIdentifier, final MimeType mime, final String enc) {
		mimeType = (mime != null ? mime.toString() : null);
		encoding = enc;
	}

	public byte[] cosign(final InputStream file, final InputStream signFile, String algorithm, final PrivateKeyEntry keyEntry, final X509Certificate cert, Properties extraParams) throws AOException {		

		if(algorithm.equalsIgnoreCase("RSA"))      algorithm = SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA")) algorithm = SIGN_ALGORITHM_SHA1WITHDSA;

		String algoUri = SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '"+algorithm+"'");
		}
		
		if (extraParams == null) extraParams = new Properties();
		final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD);
		final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE);

		//nueva instancia de DocumentBuilderFactory que permita espacio de nombres (necesario para XML)
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);				

		// Propiedades del documento XML original
		final Hashtable<String, String> originalXMLProperties = new Hashtable<String, String>();

		//carga el documento XML de firmas y su raiz
		Document docSig;
		Element rootSig;		
		try {
			docSig = dbf.newDocumentBuilder().parse(signFile);			
			rootSig = docSig.getDocumentElement();

			//si el documento contiene una firma simple se inserta como raiz el nodo AFIRMA
			if (rootSig.getNodeName().equals(SIGNATURE_NODE_NAME)) {
				docSig = insertarNodoAfirma(docSig);
				rootSig = docSig.getDocumentElement();
			}
		}
		catch (final ParserConfigurationException pcex) {
			throw new AOException("Formato de documento de firmas incorrecto.", pcex);
		}
		catch (final SAXException saxex) {
			throw new AOException("Formato de documento de firmas incorrecto.", saxex);
		}
		catch (final IOException ioex) {
			throw new AOException("Error al leer el documento de firmas.", ioex);
		}
		catch (final IllegalArgumentException iaex) {
			throw new AOException("Parametro de entrada incorrecto.", iaex);
		}		
		catch (final Throwable e) {
			throw new AOException("No se ha podido leer el documento XML de firmas.", e);
		}

		final List<Reference> referenceList = new ArrayList<Reference>();			
		final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
		final DigestMethod digestMethod;
		try {
			digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
		}
		catch(final Throwable e) {
			throw new AOException(
					"No se ha podido obtener un generador de huellas digitales para el algoritmo '" + 
					digestMethodAlgorithm + "': " + e
			);
		}

		// Localizamos la primera firma (primer nodo "Signature") en profundidad en el arbol de firma.
		// Se considera que todos los objetos "Signature" del documento firman (referencian) los mismos
		// objetos, por lo que podemos extraerlos de cualquiera de las firmas actuales.
		NodeList signatureNodes = docSig.getElementsByTagNameNS(DSIGNNS, "Signature");
		Element signatureNode = (Element)signatureNodes.item(0);

		// Buscamos dentro de ese Signature todas las referencias que apunten a datos para firmarlas
		final Vector<String> referencesIds = new Vector<String>();
		Node currentElement;
		final NodeList nl = signatureNode.getElementsByTagNameNS(DSIGNNS, "Reference");

		// Se considera que la primera referencia de la firma son los datos que debemos firmar, ademas
		// de varias referencias especiales
		for (int i=0;i<nl.getLength();i++) {
			currentElement = (Element)nl.item(i);

			// Firmamos la primera referencia (que seran los datos firmados) y las hojas de estilo que
			// tenga asignadas. Las hojas de estilo tendran un identificador que comience por "StyleReference-".
			// TODO: Identificar las hojas de estilo de un modo generico.
			NamedNodeMap currentNodeAttributes = currentElement.getAttributes();
			if (i == 0 ||
					(currentNodeAttributes.getNamedItem("Id") != null &&
							currentNodeAttributes.getNamedItem("Id").getNodeValue().startsWith("StyleReference-"))) {

				// Buscamos las transformaciones declaradas en la Referencia, para anadirlas
				// tambien en la nueva
				Vector<Transform> currentTransformList;
				try {
					currentTransformList = Utils.getObjectReferenceTransforms(currentElement, XML_SIGNATURE_PREFIX);
				} catch (NoSuchAlgorithmException e) {
					Logger.getLogger("Se ha declarado una transformacion personalizada de un tipo no soportado: "+e);
					throw new AOException("Se ha declarado una transformacion personalizada de un tipo no soportado", e);
				} catch (InvalidAlgorithmParameterException e) {
					Logger.getLogger("Se han especificado parametros erroneos para una transformacion personalizada: "+e);
					throw new AOException("Se han especificado parametros erroneos para una transformacion personalizada", e);
				}

				// Creamos un identificador de referencia para el objeto a firmar y la almacenamos
				// para mantener un listado con todas. En el caso de las hojas de estilo lo creamos con un
				// identificador descriptivo
				String referenceId = null;
				if((currentNodeAttributes.getNamedItem("Id") != null &&
						currentNodeAttributes.getNamedItem("Id").getNodeValue().startsWith("StyleReference-"))) {
					referenceId = "StyleReference-" + UUID.randomUUID().toString();
				} else {
					referenceId = "Reference-" + UUID.randomUUID().toString();
				}
				referencesIds.add(referenceId);

				// Creamos la propia referencia con las transformaciones de la original
				referenceList.add(
						fac.newReference(
								((Element)currentElement).getAttribute("URI"),
								digestMethod,
								currentTransformList, // Lista de transformaciones 
								null,
								referenceId
						)
				);
			}
		}

		//definicion de identificadores
		final String id = UUID.randomUUID().toString();
		final String signatureId =  "Signature-" + id;
		final String signatureValueId = "SignatureValue-" + id;
		final String keyInfoId = "KeyInfo-" + id;

		try {

			//CanonicalizationMethod
			CanonicalizationMethod cm = fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec)null);        

			//se anade una referencia a KeyInfo
			List<Transform> transformList = new ArrayList<Transform>();
			Transform trCanonicalization = fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec)null);
			transformList.add(trCanonicalization);
			referenceList.add(fac.newReference("#" + keyInfoId, digestMethod, transformList, null, null));

			//SignatureMethod
			SignatureMethod sm = fac.newSignatureMethod(algoUri, null);

			//SignedInfo
			SignedInfo si = fac.newSignedInfo(cm, sm, referenceList);

			//KeyInfo
			X509Data cerData;
			KeyInfoFactory kif = fac.getKeyInfoFactory();
			List<Object> x509Content = new ArrayList<Object>();	        
			x509Content.add(cert);
			cerData = kif.newX509Data(x509Content);   

			List<Object> content = new ArrayList<Object>();
			content.add(kif.newKeyValue(cert.getPublicKey()));
			content.add(cerData);

			KeyInfo ki = kif.newKeyInfo(content, keyInfoId);

			XMLSignature signature = fac.newXMLSignature(si, ki, new ArrayList<Object>(), signatureId, signatureValueId);

			DOMSignContext signContext = new DOMSignContext(keyEntry.getPrivateKey(), rootSig);
			signContext.putNamespacePrefix(DSIGNNS, XML_SIGNATURE_PREFIX);	
			signature.sign(signContext);
		}
		catch (final NoSuchAlgorithmException e) {
			throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '"+algorithm+"'", e);
		}
		catch (final Throwable e) {
			throw new AOException("Error al generar la cofirma XMLdSig. " + e.toString());
		}

		return Utils.writeXML(rootSig, originalXMLProperties, null, null);
	}

	public byte[] cosign(final InputStream signFile, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams) throws AOException {

		//nueva instancia de DocumentBuilderFactory que permita espacio de nombres (necesario para XML)
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		//carga la raiz del documento XML de firmas
		//y crea un nuevo documento que contendra solo los datos sin firmar
		Element rootSig;
		Element rootData;
		try {
			rootSig = dbf.newDocumentBuilder().parse(signFile).getDocumentElement();

			Document docData = dbf.newDocumentBuilder().newDocument();
			rootData = (Element)docData.adoptNode(rootSig.cloneNode(true));			

			//obtiene las firmas y las elimina
			NodeList signatures = rootData.getElementsByTagNameNS(DSIGNNS, "Signature");			
			for (int i = 0; i < signatures.getLength(); i++)
				rootData.removeChild(signatures.item(i));

			docData.appendChild(rootData);
		}
		catch (ParserConfigurationException pcex) {
			throw new AOException("Formato de documento de firmas incorrecto. " + pcex.toString());
		}
		catch (SAXException saxex) {
			throw new AOException("Formato de documento de firmas incorrecto. " + saxex.toString());
		}
		catch (IOException ioex) {
			throw new AOException("Error al leer el documento de firmas. " + ioex.toString());
		}
		catch (IllegalArgumentException iaex) {
			throw new AOException("Parametro de entrada incorrecto. " + iaex.toString());
		}

		//convierte el documento de firmas en un InputStream
		ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
		XMLUtils.writeXML(baosSig, rootSig, false);
		byte[] byteSig = baosSig.toByteArray();                
		InputStream isSig = new ByteArrayInputStream(byteSig);

		//convierte el documento a firmar en un InputStream
		ByteArrayOutputStream baosData = new ByteArrayOutputStream();
		XMLUtils.writeXML(baosData, rootData, false);
		byte[] byteData = baosData.toByteArray();
		InputStream isData = new ByteArrayInputStream(byteData);

		return cosign(isData, isSig, algorithm, keyEntry, cert, extraParams);
	}

	public byte[] countersign(final InputStream signFile, String algorithm, final CounterSignTarget targetType, final Object[] targets, final PrivateKeyEntry keyEntry, final X509Certificate cert, Properties extraParams) throws AOException {

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = SIGN_ALGORITHM_SHA1WITHDSA;

		String algoUri = SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '"+algorithm+"'");
		}
		
		if (extraParams == null) extraParams = new Properties();
		final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD);
		final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE);

		this.algo = algorithm;

		//nueva instancia de DocumentBuilderFactory que permita espacio de nombres (necesario para XML)
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		//se carga el documento XML y su raiz
		Hashtable<String, String> originalXMLProperties = new Hashtable<String, String>();
		Element root;		
		try {
			doc = dbf.newDocumentBuilder().parse(signFile);

			// === Tomamos la configuracion del XML que contrafirmamos ===
			// Si no hay asignado un MimeType o es el por defecto establecemos el de XML
			if (mimeType == null || DEFAULT_MIMETYPE.equals(mimeType)) mimeType = "text/xml";
			if (encoding == null) encoding = doc.getXmlEncoding();

			// Ademas del encoding, sacamos otros datos del doc XML original
			// Hacemos la comprobacion del base64 por si se establecido desde fuera
			if (encoding != null && !encoding.equalsIgnoreCase("base64")) originalXMLProperties.put(OutputKeys.ENCODING, encoding);
			String tmpXmlProp = doc.getXmlVersion();
			if (tmpXmlProp != null) originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
			DocumentType dt = doc.getDoctype();
			if (dt!=null) {
				tmpXmlProp = dt.getSystemId();
				if (tmpXmlProp != null) originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
			}

			// Nos aseguramos que la configuracion no afectara a operaciones futuras
			encoding = null;
			mimeType = null;

			root = doc.getDocumentElement();

			//si el documento contiene una firma simple se inserta como raiz el nodo AFIRMA
			if (root.getNodeName().equals(SIGNATURE_NODE_NAME)) {
				doc = insertarNodoAfirma(doc);
				root = doc.getDocumentElement();
			}

			if(targetType == CounterSignTarget.Tree)          this.countersignTree(root, cert, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm);
			else if (targetType == CounterSignTarget.Leafs)   this.countersignLeafs(root, cert, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm);
			else if (targetType == CounterSignTarget.Nodes)   this.countersignNodes(root, targets, cert, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm);
			else if (targetType == CounterSignTarget.Signers) this.countersignSigners(root, targets, cert, keyEntry, digestMethodAlgorithm, canonicalizationAlgorithm);

		} catch (final UnsupportedOperationException e) {
			throw e;
		}
		catch (final Throwable e) {
			throw new AOException("No se ha podido realizar la contrafirma. " + e.toString());
		}

		//convierte el xml resultante para devolverlo como byte[]
		return Utils.writeXML(doc.getDocumentElement(), originalXMLProperties, null, null);
	}

	/**
	 * Realiza la contrafirma de todos los nodos del arbol
	 * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	private void countersignTree(Element root, X509Certificate cert, PrivateKeyEntry keyEntry, String refsDigestMethod, final String canonicalizationAlgorithm) throws AOException {

		//obtiene todas las firmas
		NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");
		int numSignatures = signatures.getLength();

		Element[] nodes = new Element[numSignatures];		
		for (int i = 0; i < numSignatures; i++)
			nodes[i] = (Element)signatures.item(i);

		//y crea sus contrafirmas
		try {
			for (int i = 0; i < numSignatures; i++) this.cs(nodes[i], cert, keyEntry, refsDigestMethod, canonicalizationAlgorithm);
		}
		catch (UnsupportedOperationException e) {
			throw e;
		}
		catch (Throwable e) {
			throw new AOException("No se ha podido realizar la contrafirma: " + e);
		} 
	}

	/**
	 * Realiza la contrafirma de todos los nodos hoja del arbol
	 * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	private void countersignLeafs(final Element root, final X509Certificate cert, final PrivateKeyEntry keyEntry, final String refsDigestMethod, final String canonicalizationAlgorithm) throws AOException {

		//obtiene todas las firmas y las referencias 
		NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");
		NodeList references = root.getElementsByTagNameNS(DSIGNNS, "Reference");

		int numSignatures = signatures.getLength();
		int numReferences = references.getLength();

		//comprueba cuales son hojas
		try {
			for (int i = 0; i < numSignatures; i++) {
				Element signature = (Element)signatures.item(i);
				String refURI = "#" + signature.getAttribute("Id") + "Value";

				boolean isLeaf = true;

				//si la firma esta referenciada por otra firma entonces no es hoja
				for (int j = 0; j < numReferences; j++) {					
					if (((Element)references.item(j)).getAttribute("URI").equals(refURI))
						isLeaf = false;
				}

				//y crea sus contrafirmas
				if (isLeaf) {
					this.cs(signature, cert, keyEntry, refsDigestMethod, canonicalizationAlgorithm);
				}
			}
		}
		catch (UnsupportedOperationException e) {
			throw e;
		}
		catch (Throwable e) {
			throw new AOException("No se ha podido realizar la contrafirma. " + e.toString());
		}
	}

	/**
	 * Realiza la contrafirma de los nodos indicados en el par&aacute;metro targets
	 * @param root Elemento raiz del documento xml que contiene las firmas
	 * @param targets Array con las posiciones de los nodos a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	@SuppressWarnings("unchecked")
	private void countersignNodes(final Element root, Object[] targets, final X509Certificate cert, final PrivateKeyEntry keyEntry, final String refsDigestMethod, final String canonicalizationAlgorithm) throws AOException {

		//descarta las posiciones que esten repetidas
		List<Integer> targetsList = new ArrayList<Integer>();
		for (int i = 0; i < targets.length; i++) {
			if (!targetsList.contains(targets[i]))
				targetsList.add((Integer)targets[i]);
		}
		targets = targetsList.toArray();


		DefaultMutableTreeNode tree = new DefaultMutableTreeNode("AFIRMA");

		//obtiene todas las firmas
		NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");

		int numSignatures = signatures.getLength();

		String[] arrayIds = new String[numSignatures];
		String[] arrayRef = new String[numSignatures];
		DefaultMutableTreeNode[] arrayNodes = new DefaultMutableTreeNode[numSignatures];

		//genera un arbol con las firmas para conocer su posicion
		for (int i = 0; i < numSignatures; i++) {
			Element signature = (Element)signatures.item(i);
			String sigId = signature.getAttribute("Id");

			DefaultMutableTreeNode node = new DefaultMutableTreeNode(signature);
			arrayIds[i] = sigId;
			arrayNodes[i] = node;

			String typeReference = ((Element)signature.getElementsByTagNameNS(DSIGNNS, "Reference").item(0)).getAttribute("Type");
			if (typeReference.equals(CSURI)) {
				String uri = ((Element)signature.getElementsByTagNameNS(DSIGNNS, "Reference").item(0)).getAttribute("URI");
				arrayRef[i] = uri.substring(1, uri.length() - 5);
			}
			else
				arrayRef[i] = "";
		}

		for (int i = numSignatures - 1; i > 0; i--) {
			for (int j = 0; j < numSignatures; j++) {
				if (arrayRef[i].equals(arrayIds[j])) arrayNodes[j].add(arrayNodes[i]);
			}
		}

		for (int i = 0; i < numSignatures; i++) if (arrayRef[i] == "") tree.add(arrayNodes[i]);

		//introduce en una lista los nodos del arbol recorrido en preorden
		List<Element> listNodes = new ArrayList<Element>();
		Enumeration<DefaultMutableTreeNode> enumTree = tree.preorderEnumeration();
		enumTree.nextElement();
		while(enumTree.hasMoreElements()) listNodes.add((Element)enumTree.nextElement().getUserObject());

		//obtiene los nodos indicados en targets
		Element[] nodes = new Element[targets.length];
		try {
			for (int i = 0; i < targets.length; i++) nodes[i] = listNodes.get((Integer)targets[i]);
		}
		catch (ClassCastException e) {
			throw new AOException("Valor de nodo no valido: " + e);
		}
		catch (IndexOutOfBoundsException e) {
			throw new AOException("Posicion de nodo no valida");
		}

		//y crea sus contrafirmas
		try {
			for (int i = 0; i < nodes.length; i++) this.cs(nodes[i], cert, keyEntry, refsDigestMethod, canonicalizationAlgorithm);				
		}
		catch (UnsupportedOperationException e) {
			throw e;
		}
		catch (Throwable e) {
			throw new AOException("No se ha podido realizar la contrafirma. " + e.toString());
		}
	}

	/**
	 * Realiza la contrafirma de los firmantes indicados en el par&aacute;metro targets
	 * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
	 * @param targets Array con el nombre de los firmantes de los nodos a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	private void countersignSigners(final Element root, final Object[] targets, final X509Certificate cert, final PrivateKeyEntry keyEntry, final String refsDigestMethod, final String canonicalizationAlgorithm) throws AOException {

		//obtiene todas las firmas
		NodeList signatures = root.getElementsByTagNameNS(DSIGNNS, "Signature");
		int numSignatures = signatures.getLength();

		List<Object> signers = Arrays.asList(targets);				
		List<Element> nodes = new ArrayList<Element>();

		//obtiene los nodos de los firmantes indicados en targets
		for (int i = 0; i < numSignatures; i++) {
			Element node = (Element)signatures.item(i);
			if (signers.contains(
					AOUtil.getCN(Utils.getCertificate(node.getElementsByTagNameNS(DSIGNNS, "X509Certificate").item(0))))) {
				nodes.add(node);
			}
		}

		//y crea sus contrafirmas
		Iterator<Element> i = nodes.iterator();
		while (i.hasNext()) {
			this.cs(i.next(), cert, keyEntry, refsDigestMethod, canonicalizationAlgorithm);
		}
	}

	/**
	 * Realiza la contrafirma de la firma pasada por par&aacute;metro
	 * @param signature Elemento con el nodo de la firma a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	private void cs(Element signature, X509Certificate cert, PrivateKeyEntry keyEntry, String refsDigestMethod, final String canonicalizationAlgorithm) throws AOException {

		//obtiene el nodo SignatureValue
		Element signatureValue = (Element)signature.getElementsByTagNameNS(DSIGNNS, "SignatureValue").item(0);

		//crea la referencia a la firma que se contrafirma
		List<Reference> referenceList = new ArrayList<Reference>();
		XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
		DigestMethod digestMethod;
		try {
			digestMethod = fac.newDigestMethod(refsDigestMethod, null);
		}
		catch(Throwable e) {
			throw new AOException(
					"No se ha podido obtener un generador de huellas digitales para el algoritmo '" + 
					refsDigestMethod + "': " + e
			);
		}
		String referenceId = "Reference-" + UUID.randomUUID().toString();

		try {			
			//Transformada para la canonicalizacion inclusiva con comentarios
			List<Transform> transformList = new ArrayList<Transform>();
			TransformParameterSpec nullParams = null;
			Transform trCanonicalization = fac.newTransform(canonicalizationAlgorithm, nullParams);			
			transformList.add(trCanonicalization);

			referenceList.add(
					fac.newReference(
							"#" + signatureValue.getAttribute("Id"), 
							digestMethod, 
							transformList, 
							CSURI, 
							referenceId
					)
			);
		}
		catch (Throwable e) {
			throw new AOException("No se ha podido realizar la contrafirma. " + e.toString());
		}


		//definicion de identificadores
		String id = UUID.randomUUID().toString();
		String signatureId =  "Signature-" + id;
		String signatureValueId = "SignatureValue-" + id;
		String keyInfoId = "KeyInfo-" + id;

		try {

			//CanonicalizationMethod
			CanonicalizationMethod cm = fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec)null);        

			//se anade una referencia a KeyInfo
			referenceList.add(fac.newReference("#" + keyInfoId, digestMethod));

			//SignatureMethod	        
			SignatureMethod sm = fac.newSignatureMethod(SIGN_ALGOS_URI.get(algo), null);

			//SignedInfo
			SignedInfo si = fac.newSignedInfo(cm, sm, referenceList);

			//KeyInfo
			X509Data cerData;
			KeyInfoFactory kif = fac.getKeyInfoFactory();
			List<Object> x509Content = new ArrayList<Object>();	        
			x509Content.add(cert);
			cerData = kif.newX509Data(x509Content);   

			List<Object> content = new ArrayList<Object>();
			content.add(kif.newKeyValue(cert.getPublicKey()));
			content.add(cerData);

			KeyInfo ki = kif.newKeyInfo(content, keyInfoId);


			XMLSignature sign = fac.newXMLSignature(si, ki, null, signatureId, signatureValueId);

			DOMSignContext signContext = new DOMSignContext(keyEntry.getPrivateKey(), signature.getOwnerDocument().getDocumentElement());        
			signContext.putNamespacePrefix(DSIGNNS, XML_SIGNATURE_PREFIX);

			sign.sign(signContext);
		}
		catch (NoSuchAlgorithmException e) {
			throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '"+algo+"'", e);
		}
		catch (Throwable e) {
			throw new AOException("No se ha podido realizar la contrafirma. " + e.toString());
		}		
	}


	public TreeModel getSignersStructure(InputStream sign, boolean asSimpleSignInfo) {

		//recupera la raiz del documento de firmas
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		Element root = null;
		String completePrefix = null;
		try {
			doc = dbf.newDocumentBuilder().parse(sign);
			root = doc.getDocumentElement();

			// Identificamos el prefijo que se utiliza en los nodos de firma
			String xmlDSigNSPrefix = Utils.guessXMLDSigNamespacePrefix(root);
			if (xmlDSigNSPrefix == null) xmlDSigNSPrefix = XML_SIGNATURE_PREFIX;
			completePrefix = (xmlDSigNSPrefix == null || "".equals(xmlDSigNSPrefix) ? "" : xmlDSigNSPrefix + ":");
			
			// Si el documento tiene como nodo raiz el nodo de firma, se agrega
			// un nodo raiz previo para que la lectura de las firmas del documento
			// se haga correctamente
			if (root.getNodeName().equals(completePrefix + "Signature")) {
				doc = insertarNodoAfirma(doc);
				root = doc.getDocumentElement();
			}
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").warning (
					"Se ha producido un error al obtener la estructura de firmas. " + e.toString() 
			);
			return null;
		}

		DefaultMutableTreeNode tree = new DefaultMutableTreeNode("Datos");

		// Obtenemos todas las firmas y los signature value
		NodeList signatures = root.getElementsByTagName(completePrefix + "Signature");
		NodeList signatureValues = root.getElementsByTagName(completePrefix + "SignatureValue");

		int numSignatures = signatures.getLength();
		String[] arrayIds = new String[numSignatures];
		String[] arrayRef = new String[numSignatures];
		DefaultMutableTreeNode[] arrayNodes = new DefaultMutableTreeNode[numSignatures];

		for (int i = 0; i < numSignatures; i++) {

			Element signature = (Element)signatures.item(i);

			arrayIds[i] = signature.getAttribute("Id");

			arrayNodes[i] = new DefaultMutableTreeNode(
					asSimpleSignInfo ?
							Utils.getSimpleSignInfoNode(XADESNS, signature) :		
								Utils.getStringInfoNode(signature)
			);

			// Recogemos el identificador de la firma a la que se referencia (si no es contrafirma sera cadena vacia)
			String typeReference = ((Element)signature.getElementsByTagNameNS(DSIGNNS, "Reference").item(0)).getAttribute("Type");
			if (typeReference.equals(CSURI))
				arrayRef[i] = Utils.getCounterSignerReferenceId(signature, signatureValues);
			else
				arrayRef[i] = "";
		}

		//Se buscan las contrafirmas de cada firma o cofirma
		for (int i = 0; i < numSignatures; i++) {
			if (arrayRef[i] == "")
				tree.add(generaArbol(i, numSignatures-1, arrayNodes, arrayIds, arrayRef)[i]);
		}

		return new JTree(tree).getModel();
	}		

	/**
	 * M&eacute;todo recursivo para la obtenci&oacute;n de la estructura de &aacute;rbol 
	 * @param i Inicio de lectura del array de identificadores
	 * @param j Inicio de lectura inversa del array de referencias
	 * @param arrayNodes Array de objetos DefaultMutableTreeNode
	 * @param arrayIds Array de identificadores
	 * @param arrayRef Array de referencias
	 * @return Array de objetos DefaultMutableTreeNode
	 */
	private DefaultMutableTreeNode[] generaArbol(int i, int j, DefaultMutableTreeNode arrayNodes[], 
			String arrayIds[], String arrayRef[]) {

		int max = arrayIds.length;

		if (i < max && j > 0) {
			if (arrayIds[i].equals(arrayRef[j]))
				generaArbol(i+1, j-1, arrayNodes, arrayIds, arrayRef);

			if (i < j)
				generaArbol(i, j-1, arrayNodes, arrayIds, arrayRef);

			if (!arrayIds[i].equals(arrayRef[j]))
				return arrayNodes;

			generaArbol(j, max-1, arrayNodes, arrayIds, arrayRef);

			arrayNodes[i].add(arrayNodes[j]);
		}

		return arrayNodes;
	}

	public boolean isSign(final InputStream file) {

		if(file == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}

		try{
			// Carga el documento a validar
			final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);

			Document signDoc = dbf.newDocumentBuilder().parse(file); 
			Element signRoot = signDoc.getDocumentElement();

			// Si el documento no tiene como nodo raiz AFIRMA se añade este
			// para que la lectura de las firmas del documento se haga correctamente
			if (signRoot.getNodeName().equals(SIGNATURE_NODE_NAME)) {
				signDoc = insertarNodoAfirma(signDoc);
				signRoot = signDoc.getDocumentElement();
			}

			final NodeList signatures = signRoot.getElementsByTagNameNS(DSIGNNS, "Signature");

			//TODO: Comprobar cada una de las firmas

			if(signatures.getLength() == 0)
				return false;

			////			final List<Node> countersignatures = new ArrayList<Node>();
			////			
			////			// Se identifican las contrafirmas
			////			for (int i = 0; i < signatures.getLength(); i++) {
			////				if(((Element)((Element)signatures.item(i)).getElementsByTagNameNS(DSIGNNS, "Reference").item(0)).getAttribute("Type").equals(CSURI))
			////					countersignatures.add(signatures.item(i));
			////			}
			////			
			////			// Se eliminan las contrafirmas
			////			for (int i = 0; i < countersignatures.size(); i++)
			////				signRoot.removeChild(countersignatures.get(i));
			//			
			//			// Comprueba si las firmas del documento son validas
			//			final List<SignatureStatus> ssList = XMLAdvancedSignature.newInstance(
			//					XAdES.newInstance(XAdES.EPES, signDoc.getDocumentElement())).validate();
			//						
			//			// Si alguna firma fuera erronea devuelve false, en caso contrario es true
			//			if (ssList.size() > 0) {
			//				return SignatureStatus.isValid(ssList);
			//			}
		}
		catch (final Throwable e) {
			return false;
		}
		return true;	
	}


	public boolean isValidDataFile(InputStream is) {
		if(is == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
		return true;
	}

	public String getSignedName(String originalName, String inText) {
		return originalName + (inText != null ? inText : "") + ".xsig";
	}

	/**
	 * Devuelve un nuevo documento con ra&iacute;z "AFIRMA" y conteniendo al documento pasado por par&aacute;metro
	 * @param doc Documento que estar&aacute; contenido en el nuevo documento
	 * @return Documento con ra&iacute;z "AFIRMA"
	 * @throws ParserConfigurationException
	 */
	private Document insertarNodoAfirma(Document docu) throws ParserConfigurationException {

		//nueva instancia de DocumentBuilderFactory que permita espacio de nombres (necesario para XML)
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		//crea un nuevo documento con la raiz "AFIRMA"
		Document docAfirma = dbf.newDocumentBuilder().newDocument();
		Element rootAfirma = docAfirma.createElement(AFIRMA);

		//inserta el documento pasado por parametro en el nuevo documento
		rootAfirma.appendChild(docAfirma.adoptNode(docu.getDocumentElement()));
		docAfirma.appendChild(rootAfirma);

		return docAfirma;
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

		AOSignInfo signInfo = new AOSignInfo(SIGN_FORMAT_XMLDSIG); 

		// Analizamos mas en profundidad la firma para obtener el resto de datos

		// Tomamos la raiz del documento
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		Element rootSig = null;
		try {
			rootSig = dbf.newDocumentBuilder().parse(
					new ByteArrayInputStream(signDataReaded)).getDocumentElement();
		} catch (Throwable e) {
			Logger.getLogger("es.gob.afirma").warning("Error al analizar la firma: "+e);
			rootSig = null;
		}

		// Establecemos la variante de firma
		if(rootSig != null) {
			if(isDetached(rootSig)) {
				signInfo.setVariant(SIGN_FORMAT_XMLDSIG_DETACHED);
			} else if(isEnveloped(rootSig)) {
				signInfo.setVariant(SIGN_FORMAT_XMLDSIG_ENVELOPED);
			} else if(isEnveloping(rootSig)) {
				signInfo.setVariant(SIGN_FORMAT_XMLDSIG_ENVELOPING);
			}
		}

		// Aqui vendria el analisis de la firma buscando alguno de los otros datos de relevancia
		// que se almacenan en el objeto AOSignInfo

		return signInfo;
	}

	public String getDataMimeType(InputStream signData) throws AOUnsupportedSignFormatException {

		String mType = null;

		//Si no hay datos a analizar
		if(signData == null)
			throw new NullPointerException("No se han introducido datos para analizar");

		byte[] signDataReaded;
		try {
			signDataReaded = AOUtil.getDataFromInputStream(signData);
		} 
		catch (Throwable e) {
			throw new AOUnsupportedSignFormatException("No se han podido leer los datos de firma: "+e);
		}

		//Si no es una firma valida
		if(!isSign(new ByteArrayInputStream(signDataReaded))) {
			throw new AOUnsupportedSignFormatException("Los datos introducidos no se corresponden con un objeto de firma");
		}

		//Obtiene el documento y su raiz
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		Document tmpDoc = null;
		Element rootSig = null;
		try {
			tmpDoc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(signDataReaded));
			rootSig = tmpDoc.getDocumentElement();
		} 
		catch (Throwable e) {
			Logger.getLogger("es.gob.afirma").warning("Error al analizar la firma: " + e);
			rootSig = null;
		}

		if(rootSig != null) {
			//si es enveloped trata de obtener el MimeType del nodo raiz, que corresponde a los datos
			if(isEnveloped(rootSig)) {
				mType = rootSig.getAttribute("MimeType");
			} 
			//si es enveloping
			else if (isEnveloping(rootSig)){			
				//si el documento no tiene como nodo raiz AFIRMA se añade este
				//para que la lectura de las firmas del documento se haga correctamente
				if (rootSig.getNodeName().equals(SIGNATURE_NODE_NAME)) {
					try {
						tmpDoc = insertarNodoAfirma(tmpDoc);
					}
					catch (Throwable e) {
						throw new AOUnsupportedSignFormatException("Error al analizar la firma.");
					}
					rootSig = tmpDoc.getDocumentElement();
				}

				//obtiene el nodo de firma y el elemento que contiene los datos
				NodeList signatures = rootSig.getElementsByTagNameNS(DSIGNNS, "Signature");
				NodeList objects = ((Element)signatures.item(0)).getElementsByTagNameNS(DSIGNNS, "Object");

				mType = ((Element)objects.item(0)).getAttribute("MimeType");
			}
			//si es detached
			else if (isDetached(rootSig)) {
				Element content = (Element)rootSig.getFirstChild();
				mType = content.getAttribute("MimeType");
			}
		}    	

		if (mType.equals("")) return null;

		return mType;
	}
}
