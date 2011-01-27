/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers.xmlhelper;

import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_DETACHED;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_ENVELOPED;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_ENVELOPING;
import static es.gob.afirma.misc.AOConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED;
import static es.gob.afirma.misc.AOConstants.SIGN_MODE_EXPLICIT;
import static es.gob.afirma.misc.AOConstants.SIGN_MODE_IMPLICIT;

import static es.gob.afirma.signers.xmlhelper.XMLConstants.DSIGNNS;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URI;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilter2ParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.crypto.dsig.spec.XPathType;
import javax.xml.crypto.dsig.spec.XSLTTransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathType.Filter;
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

import sun.misc.BASE64Decoder;

import com.sun.org.apache.xerces.internal.dom.DOMOutputImpl;

import es.gob.afirma.Messages;
import es.gob.afirma.beans.AOSimpleSignInfo;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;

/** Utilidades para las firmas XML. */
public final class Utils {
	
	/** Hoja de estilo local (rutal local no dereferenciable) a un XML */
	public final static class IsInnerlException extends Exception {
		private static final long serialVersionUID = -8769490831203570286L;
	}
	
	/** No se puede dereferenciar la hoja de estilo. */
	public final static class CannotDereferenceException extends Exception {
		
		private static final long serialVersionUID = 5883820163272098664L;
		
		/**
		 * Construye una excepci&oacute;n que indica la imposibilidad de dereferenciar
		 * una hoja de estilo. 
		 * @param s Mesaje de excepci&oacute;n
		 */
		public CannotDereferenceException(final String s) { super(s); }
	}
	
	/** La referencia de hoja de estilo apunta a un no-XML. */
	public final static class ReferenceIsNotXMLException extends Exception {
		private static final long serialVersionUID = 8076672806350530425L;
	}
	
	/**
	 * Dereferencia una joja de estilo en forma de Documento DOM
	 * @param id Identificador de la hoja de estilo
	 * @param headLess <code>true</code> si <b>no</b> se desea que se pregunte al usuario para dereferenciar
	 *                 las hojas de estilo enlazadas con rutas relativas
	 * @return Documento DOM con la hoja de estilo
	 * @throws CannotDereferenceException Si no se puede dereferenciar
	 * @throws IsInnerlException Si no se puede dereferenciar por ser una referencia local
	 * @throws ReferenceIsNotXMLException Si el objeto dereferenciado no puede transformarse en un Documento DOM
	 */
	public static Document dereferenceStyleSheet(final String id, final boolean headLess) throws CannotDereferenceException, IsInnerlException, ReferenceIsNotXMLException {
		if (id == null || "".equals(id)) throw new CannotDereferenceException("La hoja de estilo era nula o vacia"); //$NON-NLS-1$ //$NON-NLS-2$
		
		byte[] xml = null;
		
		// Intentamos dereferenciar directamente, cosa que funciona con
		// http://, https:// y file://
		try {
			xml = AOUtil.getDataFromInputStream(
				AOUtil.loadFile(
					AOUtil.createURI(id), 
					null, 
					false
				)
			);
		}
		catch(final Throwable e) {
			
			// Si no dereferencia puede ser por tres cosas, porque es una referencia interna,
			// porque es una referencia local local
			// o porque directamente no se puede dereferenciar
			
			// Miramos si la referencia es local
			final String[] idParts = id.replace(File.separator, "/").split("/"); //$NON-NLS-1$ //$NON-NLS-2$
			final String fileName = idParts[idParts.length-1]; 
			
			if (fileName.startsWith("#")) throw new IsInnerlException(); //$NON-NLS-1$
			else if (id.startsWith("file://")) { //$NON-NLS-1$
				// Preguntamos al usuario para la dereferenciacion
				if (JOptionPane.showConfirmDialog(
					null,
					Messages.getString("Utils.5"), //$NON-NLS-1$
					Messages.getString("Utils.6"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION,
					JOptionPane.INFORMATION_MESSAGE
				) == JOptionPane.OK_OPTION) {
					final JFileChooser fc = new JFileChooser();
					fc.setDialogTitle(Messages.getString("Utils.7")); //$NON-NLS-1$
					fc.setFileFilter(new FileFilter() {
						@Override
						public boolean accept(final File f) {
							if (f == null) return false;
							if (f.getName().equalsIgnoreCase(fileName)) return true;
							return false;
						}
						@Override
						public String getDescription() {
							return Messages.getString("Utils.8") + fileName + Messages.getString("Utils.9"); //$NON-NLS-1$ //$NON-NLS-2$
						}}
					);
					if (fc.showOpenDialog(null) != JFileChooser.APPROVE_OPTION) throw new CannotDereferenceException(
						"No se ha podido dereferenciar la hoja de estilo" //$NON-NLS-1$
					);
					try {
						xml = AOUtil.getDataFromInputStream(new FileInputStream(fc.getSelectedFile()));
					}
					catch(final Throwable ex) {
						throw new CannotDereferenceException("No se ha podido dereferenciar la hoja de estilo: " + ex); //$NON-NLS-1$
					}
				}
			}
			else throw new CannotDereferenceException(
				"No se ha podido dereferenciar la hoja de estilo: " + id //$NON-NLS-1$
			);
		}
		
		try {
			if (xml != null) return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(xml));
			throw new CannotDereferenceException("No se ha dereferenciado la hoja de estilo"); //$NON-NLS-1$
		}
		catch(final Throwable e) {
			throw new ReferenceIsNotXMLException();
		}
	}

	/**
	 * A&ntilde;ade la cabecera de hoja de estilo a un XML dado.
	 * @param xml XML origen 
	 * @param type Tipo de hoja de estilo
	 * @param href Reeferncia a la hoja de estilo
	 * @return XML con la cabecera de declaraci&oacute;n de hoja de estilo a&ntilde;adida
	 */
	public static String addStyleSheetHeader(final String xml, String type, final String href) {
		if (href == null) return xml;
		if (type == null) type = "text/xsl"; //$NON-NLS-1$
		if (xml == null || "".equals(xml)) return  //$NON-NLS-1$
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><?xml-stylesheet type=\"" + //$NON-NLS-1$
			type +
			"\" href=\"" +  //$NON-NLS-1$
			href +
			"\"?>"; //$NON-NLS-1$
		return xml.replaceFirst(">",  //$NON-NLS-1$
			">\r\n<?xml-stylesheet type=\"" + //$NON-NLS-1$
			type +
			"\" href=\"" +  //$NON-NLS-1$
			href +
			"\"?>" //$NON-NLS-1$
		);
	}

	
	/**
	 * Obtiene los par&aacute;metros de la cabecera de definici&oacute;n de la hoja de estilo de un XML.
	 * @param xml XML de entrada
	 * @return Properties con los par&aacute;metros encontrados en la cabecera, 
	 *         o un Properties vac&iacute;o si el XML no declaraba una hoja de estilo
	 */
	public static Properties getStyleSheetHeader(String xml) {
		final Properties ret = new Properties();
		if (xml == null) return ret;
		final int startPos = xml.indexOf("<?xml-stylesheet "); //$NON-NLS-1$
		if (startPos == -1) return ret;
		xml = xml.substring(startPos);
		xml = xml.substring(0, xml.indexOf('>')+1).replace("<?xml-stylesheet ", "").replace("?>", "").replace(" ", "\n").replace("\"", "").replace("'", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$
		try {
			ret.load(new ByteArrayInputStream(xml.getBytes()));
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha podido analizar correctamente la cabecera de definicion de la hora de estilo del XML: " + e //$NON-NLS-1$
			); 
		}
		return ret;
	}
	
	/**
	 * A&ntilde;ade transformaciones seg&uacute; la sintaxis de par&aacute;metros adicionales
	 * en fichero de propiedades del Cliente @firma a una lista pre-existente.
	 * @param transformList Lista a la que a&ntilde;adir las transformaciones
	 * @param extraParams Informaci&oacute;n sobre las transformaciones a a&ntilde;adir
	 * @param xmlSignaturePrefix Prefijo XMLDSig
	 */
	public static void addCustomTransforms(List<Transform> transformList, Properties extraParams, String xmlSignaturePrefix) {
		
		XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
		if (transformList == null) transformList = new ArrayList<Transform>();
		if (extraParams == null) extraParams = new Properties();
		
		// primero compruebo si hay transformaciones a medida
		int numTransforms = Integer.parseInt(extraParams.getProperty("xmlTransforms", "0")); //$NON-NLS-1$ //$NON-NLS-2$
		String transformType;
		String transformBody;
		String transformSubtype;
		XPathType.Filter xPath2TransformFilter;
		TransformParameterSpec transformParam;
		
		for(int i=0;i<numTransforms;i++) {
			transformType = extraParams.getProperty("xmlTransform" + Integer.toString(i) + "Type"); //$NON-NLS-1$ //$NON-NLS-2$
			transformBody = extraParams.getProperty("xmlTransform" + Integer.toString(i) + "Body"); //$NON-NLS-1$ //$NON-NLS-2$
			
//			System.out.println("Tipos soportados:");
//			System.out.println("-----------------");
//			System.out.println("XPATH:     "+Transform.XPATH);
//			System.out.println("XPATH2:    "+Transform.XPATH2);
//			System.out.println("XSLT:      "+Transform.XSLT);
//			System.out.println("BASE64:    "+Transform.BASE64);
//			System.out.println("ENVELOPED: "+Transform.ENVELOPED);
//			System.out.println("-----------------");
//			System.out.println("Tipo establecido: "+transformType);
			
			
			if (Transform.XPATH.equals(transformType) && transformBody != null) {
				try {
					transformParam = new XPathFilterParameterSpec(
						transformBody, Collections.singletonMap(xmlSignaturePrefix, XMLSignature.XMLNS)
					);
				}
				catch(Throwable e) {
					Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se han podido crear los parametros para una transformacion XPATH, se omitira: " + e //$NON-NLS-1$
					);
					continue;
				}
			}
			else if (Transform.XPATH2.equals(transformType) && transformBody != null) {
				transformSubtype = extraParams.getProperty("xmlTransform" + Integer.toString(i) + "Subtype"); //$NON-NLS-1$ //$NON-NLS-2$
				if       ("subtract".equals(transformSubtype)) xPath2TransformFilter = Filter.SUBTRACT; //$NON-NLS-1$
				else if ("intersect".equals(transformSubtype)) xPath2TransformFilter = Filter.INTERSECT; //$NON-NLS-1$
				else if     ("union".equals(transformSubtype)) xPath2TransformFilter = Filter.UNION; //$NON-NLS-1$
				else {
					Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
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
				catch(Throwable e) {
					Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se han podido crear los parametros para una transformacion XPATH2, se omitira: " + e //$NON-NLS-1$
					);
					continue;
				}
			}
			else if (Transform.XSLT.equals(transformType) && transformBody != null) {
				try {
					transformParam = new XSLTTransformParameterSpec(
						new DOMStructure(
							DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(
							transformBody.getBytes()
						)).getDocumentElement()
						)
					);
				}
				catch(final Throwable e) {
					Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
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
				Logger.getLogger("es.gob.afirma").warning("Tipo de transformacion no soportada: " + transformType); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
		
			// Llegados a este punto tenemos ya la transformacion, asi que la anadimos
			try {
				transformList.add(
					fac.newTransform(
						transformType,
						transformParam
					)
				);
			}
			catch(Throwable e) {
				Logger.getLogger("es.gob.afirma").warning("No se ha podido aplicar la transformacion '" + transformType + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
				
		}
	}
	
	/**
	 * Obtiene de un nodo de referencia de tipo Object la lista de transformaciones definidas.
	 * Si no tiene transfgormaciones definidas, devuelve {@code null}.
	 * @param referenceNode Nodo de tipo referencia.
	 * @param namespacePrefix Prefijo del namespace de la firma (opcional).
	 * @return Listado de transformaciones.
	 * @throws InvalidAlgorithmParameterException Cuando se encuentre un par&aacute;metro inv&aacute;lido para el algoritmo de transformaci&oacute;n.
	 * @throws NoSuchAlgorithmException Cuando se encuentre un algoritmo de transformaci&oacurte;n no soportado. 
	 */
	public static Vector<Transform> getObjectReferenceTransforms(Node referenceNode, String namespacePrefix) throws NoSuchAlgorithmException, InvalidAlgorithmParameterException {
		
		Vector<Transform> transformList = new Vector<Transform>();
		final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
		
		// El nodo de referencia puede contener un nodo "Transforms" que a su vez contiene
		// las distintas transformaciones
		NodeList tmpNodeList = referenceNode.getChildNodes();
		
		for (int i=0; i<tmpNodeList.getLength(); i++) {
			
			// Buscamos el nodo Transforms a secas o precedido por el namespace
			if("Transforms".equals(tmpNodeList.item(i).getNodeName()) ||
					(namespacePrefix+":Transforms").equals(tmpNodeList.item(i).getNodeName())) {
				
				NodeList transformsNodeList = tmpNodeList.item(i).getChildNodes();
				
				for (int j=0; j<transformsNodeList.getLength(); j++) {
					
					transformList.add(
							fac.newTransform(
									getTransformAlgorithm(transformsNodeList.item(j)),
									getTransformParameterSpec(transformsNodeList.item(j), namespacePrefix)
							)
					);
				}
				break;	// Si ya hemos encontrado el nodo "Transforms" dejamos de buscar
			}
		}
		
		return transformList;
	}
	
	/**
	 * Recupera el identificador del algoritmo de un nodo de transformaci&oacute;n. Si
	 * no existe el atributo de algoritmo, se devuelve {@code null}. 
	 * @param transformNode Nodo de transformaci&oacute;n.
	 * @return Algoritmo de transformaci&oacute;n.
	 */
	private static String getTransformAlgorithm(Node transformNode) {
		
		String algorithm = null;
		Node algorithmNode = transformNode.getAttributes().getNamedItem("Algorithm");
		if(algorithmNode != null) {
			algorithm = algorithmNode.getNodeValue();
		}
		
		return algorithm;
	}
	
	/**
	 * Recupera los par&aacute;metros de una transformaci&opacute;n. En el caso de las
	 * transformaciones XPATH y XPATH2, se devolveran los par&aacute;metros especificados y,
	 * en las transformacion Base64, Enveloped y de Canonicalizaci&oacute;n (que no reciben
	 * par&aacute;metros) se devolver&aacute; {@code null}, al igual que cuando no se reconozca
	 * el tipo de transformaci&oacute;n.
	 * @param transformNode Nodo de transformaci&oacute;n.
	 * @return Par&aacute;metros de la transformaci&oacute;n.
	 * @throws InvalidAlgorithmParameterException Cuando no se especifiquen correctamente los
	 * par&aacute;mnetros de las transformaciones XPATH y XPATH2.
	 */
	private static TransformParameterSpec getTransformParameterSpec(Node transformNode, String namespacePrefix) throws InvalidAlgorithmParameterException {
		
		TransformParameterSpec params = null;
		String algorithm = getTransformAlgorithm(transformNode);
		
		// Comprobamos que la transformacion sea de tipo XPATH o XPATH2, unicos casos en los que
		// la transformacion recibe parametros
		if(algorithm != null && (Transform.XPATH.equals(algorithm) || Transform.XPATH2.equals(algorithm))) {
			
			// Si es una transformacion XPATH solo tenemos que recoger el cuerpo
			if(Transform.XPATH.equals(algorithm)) {

				NodeList xpathTransforms = transformNode.getChildNodes();
				for(int i=0; i<xpathTransforms.getLength(); i++) {
					Node xpathTransformNode = xpathTransforms.item(i);
					
					// Probamos a encontrar un nodo XPath sin namespace y con el namespace indicado
					if("XPath".equals(xpathTransformNode.getNodeName()) ||
							(namespacePrefix+":XPath").equals(xpathTransformNode.getNodeName())) {
						
						if(namespacePrefix == null || "".equals(namespacePrefix)) {
							params = new XPathFilterParameterSpec(
									xpathTransformNode.getTextContent()
							);
						}
						else { 
							params = new XPathFilterParameterSpec(
									xpathTransformNode.getTextContent(),
									Collections.singletonMap(namespacePrefix, XMLSignature.XMLNS)
							);
						}
						
						break;
					}
				}
				
				if(params == null) {
					throw new InvalidAlgorithmParameterException("No se ha indicado un cuerpo para una transformacion XPATH declarada");
				}
				
			}
			// Si la transformacion es XPATH2 debemos tomar el cuerpo y el subtipo
			else if(Transform.XPATH2.equals(algorithm)) {
				
				NodeList xpathTransforms = transformNode.getChildNodes();
				for(int i=0; i<xpathTransforms.getLength(); i++) {
					Node xpathTransformNode = xpathTransforms.item(i);
					if("XPath".equals(xpathTransformNode.getNodeName()) ||
							(namespacePrefix+":XPath").equals(xpathTransformNode.getNodeName())) {
						
						Filter filter = null;
						Node filterNode = xpathTransformNode.getAttributes().getNamedItem("Filter");
						if(filterNode != null) {
							String filterName = filterNode.getNodeValue();
							if(filterName.equals("subtract"))       filter = Filter.SUBTRACT;
							else if(filterName.equals("intersect")) filter = Filter.INTERSECT;
							else if(filterName.equals("union"))     filter = Filter.UNION;
							else throw new InvalidAlgorithmParameterException("El subtipo '"+filterName+"' de la transformacion XPATH2 no es valido");
						}
						else  throw new InvalidAlgorithmParameterException("No se ha declarado un subtipo para la transformacion XPATH2");
						
						params = new XPathFilter2ParameterSpec(
								Collections.singletonList(
										new XPathType(
												xpathTransformNode.getTextContent(),
												filter)
									)
						);
						break;
					}
				}
				
				if(params == null) {
					throw new InvalidAlgorithmParameterException("No se ha indicado un cuerpo para una transformacion XPATH2 declarada");
				}
			}
			
		}
		
		return params;
	}
	
	/**
	 * Comprueba si hay alguna incorrecci&oacute;n en los par&aacute;metros principales de firma.
	 * @param format Formato de firma
	 * @param mode Modo de firma
	 * @param uri URI del objeto a firmar
	 * @param externallyDetachedHashAlgorithm Algoritmo de huella digital en el caso de estar esta pre-calculada
	 * @param xades <code>true</code> si la firma es XAdES, <code>false</code> si es XMLDSig
	 * @throws AOException si hay incorrecciones o incompatibilidades en los par&aacute;metros
	 */
	public static void checkIllegalParams(final String format, final String mode, final URI uri, final String externallyDetachedHashAlgorithm, final boolean xades) throws AOException {
		if (!mode.equals(SIGN_MODE_IMPLICIT) && !mode.equals(SIGN_MODE_EXPLICIT)) {
			throw new UnsupportedOperationException("El modo de firma '" + mode + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		if (xades) {  // XAdES
			if (format.equals(SIGN_FORMAT_XADES_ENVELOPED) && mode.equals(SIGN_MODE_EXPLICIT)) {
				throw new UnsupportedOperationException(
					"No se puede realizar una firma XML Enveloped con contenido Explicito" //$NON-NLS-1$
				);
			}
			if (format.equals(SIGN_FORMAT_XADES_EXTERNALLY_DETACHED) && mode.equals(SIGN_MODE_IMPLICIT)) {
				throw new UnsupportedOperationException(
						"No se puede realizar una firma XML Externally Detached con contenido Implicito" //$NON-NLS-1$
				);
			}
			if (format.equals(SIGN_FORMAT_XADES_EXTERNALLY_DETACHED) && uri == null && externallyDetachedHashAlgorithm == null) {
				throw new UnsupportedOperationException(
					"La firma XML Externally Detached necesita un Message Digest precalculado o una URI accesible" //$NON-NLS-1$
				);
			}
			if (!format.equals(SIGN_FORMAT_XADES_DETACHED) && 
				!format.equals(SIGN_FORMAT_XADES_ENVELOPED) && 
				!format.equals(SIGN_FORMAT_XADES_ENVELOPING) &&
				!format.equals(SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
					throw new UnsupportedOperationException("El formato de firma '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		else { // XMLDSig
			// Combinaciones prohibidas
			if (format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {
				throw new UnsupportedOperationException("No se puede realizar una firma enveloped sobre un contenido explícito"); //$NON-NLS-1$
			}
	        if (format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED) && mode.equals(AOConstants.SIGN_MODE_IMPLICIT)) {
	            throw new UnsupportedOperationException(
	                "No se puede realizar una firma XML Externally Detached con contenido Implicito" //$NON-NLS-1$
	            );
	        }
			if (format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED) && uri == null && externallyDetachedHashAlgorithm == null) {
				throw new UnsupportedOperationException(
					"La firma XML Externally Detached necesita un Message Digest precalculado o una URI accesible" //$NON-NLS-1$
				);
			}
			
			// Formatos y modos soportados
			if (!format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED) && 
				!format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && 
				!format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING) &&
				!format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)) {
					throw new UnsupportedOperationException("El formato de firma '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			
		}

	}
	
    /**
     * Intenta determinar la URL de declaraci&oacute;n de espacio de nombres de
     * XAdES de una firma XAdES.
     * @param el Firma XAdES
     * @return URL de la declaraci&oacute;n del espacio de nombres
     */
    public static String guessXAdESNamespaceURL(final Node el) {

    	final String latest = "http://uri.etsi.org/01903#"; //$NON-NLS-1$
    	final String xades132 = "http://uri.etsi.org/01903/v1.3.2#"; //$NON-NLS-1$
    	final String xades141 = "http://uri.etsi.org/01903/v1.4.1#"; //$NON-NLS-1$
    	
    	String signatureText = new String(writeXML(el, null, null, null));
    	
    	int numLatest = countSubstring(signatureText, latest);
    	int numXades132 = countSubstring(signatureText, xades132);
    	int numXades141 = countSubstring(signatureText, xades141);
    	
    	//Prioridad: xades132 > latest > xades141
    	if(numXades132 >= numLatest && numXades132 >= numXades141) return xades132;
    	if(numLatest >= numXades132 && numLatest >= numXades141) return latest;
    	if(numXades141 >= numLatest && numXades141 >= numXades132) return xades141;
    	
    	return xades132;
    }
        
    /**
     * Intenta determinar la URL de declaraci&oacute;n de espacio de nombres de
     * XMLDSig de un XML.
     * @param el Firma XMLDSig
     * @return URL de la declaraci&oacute;n del espacio de nombres
     */
    public static String guessXMLDSigNamespaceURL(final Element el) {
    	
    	String signatureText = new String(writeXML(el, null, null, null));
    	
    	// Por defecto "http://www.w3.org/2000/09/xmldsig#"
    	
    	int numXmlDsig = countSubstring(signatureText, "http://www.w3.org/2000/09/xmldsig#");
    	int numXmlDsig11 = countSubstring(signatureText, "http://www.w3.org/2009/xmldsig11#");
    	
    	return numXmlDsig >= numXmlDsig11 ? "http://www.w3.org/2000/09/xmldsig#" : "http://www.w3.org/2009/xmldsig11#";
    }
    
    /**
     * Intenta determinar el prefijo del espacio de nombres de la firma.
     * @param el Firma XMLDSig
     * @return Prefijo del espacio de nombres
     */
    public static String guessXMLDSigNamespacePrefix(final Element el) {
    	
    	String signatureText = new String(writeXML(el, null, null, null));
    	
    	int numEmpty = countSubstring(signatureText, "<Signature");
    	int numDs = countSubstring(signatureText, "<ds:Signature");
    	int numDsig = countSubstring(signatureText, "<dsig:Signature");
    	int numDsig11 = countSubstring(signatureText, "<dsig11:Signature");
    	
    	//Prioridad: ds > "" > dsig > dsig11
    	if(numDs >= numEmpty && numDs >= numDsig && numDs >= numDsig11) return "ds";
    	if(numEmpty >= numDs && numEmpty >= numDsig && numEmpty >= numDsig11) return "";
    	if(numDsig >= numEmpty && numDsig >= numDs && numDsig >= numDsig11) return "dsig";
    	if(numDsig11 >= numEmpty && numDsig11 >= numDsig && numDsig11 >= numDs) return "dsig11";
    	
    	return "ds";
    }
    
    /**
     * Cuenta las repeticiones de una subcadena dentro de una cadena. Las subcadenas
     * no pueden estar acopladas. 
     * @param text Texto en el que realizar la b&uacute;squeda.
     * @param substring Subcadena que deseamos buscar.
     * @return N&uacute;mero de coincidencias.
     */
    private static int countSubstring(final String text, final String substring) {
    	
    	int count =0;
    	for(int i=0; i<=(text.length()-substring.length()); i++) {
    		if(substring.charAt(0) == text.charAt(i)) {
    			if(substring.equals(text.substring(i, i+substring.length()))) {
    				count++;
    				i += substring.length()-1;
    			}
    		}
    	}
    	return count;
    }
    
    /**
     * En una lista de referencias, se eliminan las transformaciones Base64 de aquellas que tengan el identificador nulo.
     * @param referenceList Lista de referencias original (no se modifica)
     * @return Nueva lista de referencias
     */
    public static List<Reference> cleanReferencesList(final List<Reference> referenceList) {
    	
    	final List<Reference> newList = new ArrayList<Reference>();
    	if (referenceList == null) return newList;
    	List<Transform> trans;
    	final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
    	boolean needsReconReference;
    	
    	for (Reference r : referenceList) {
    		if (r.getId() == null) {
    			// Por cada referencia guardamos sus transformaciones que no son Base64 por si hay que
    			// reconstruirla
    			trans = null;
    			needsReconReference = false;
    			for (Object t : r.getTransforms()) {
						if (t instanceof Transform) {
    					if (!"http://www.w3.org/2000/09/xmldsig#base64".equals(((Transform) t).getAlgorithm())) {
    						// Si el ID es nulo y hay una transformación Base64 reconstruimos la referencia
    						// pero quitando esa transformacion Base64
    						if (trans == null) trans = new ArrayList<Transform>();
    						trans.add((Transform)t);
    					}
    					else {
    						needsReconReference = true;
    					}
    				}
    			}
    			// Ya tenemos las referencias, si se necesita reconstruir
    			// la reconstruimos y la anadimos
    			if (needsReconReference) {
    				newList.add(
  						fac.newReference(
  							r.getURI(), 
  							r.getDigestMethod(), 
  							trans, 
  							r.getType(), 
  							r.getId()
  						)
  					);
    			}
    			// Si no, la referencia es buena y la podemos anadir directamente
    			else {
    				newList.add(r);
    			}
    		}
    		else newList.add(r);
    	}
    	return newList;
    }
    
    /**
     * Escribe un XML como texto. 
     * @param node Nodo XML que queremos pasar a trexto
     * @param xmlProps Propiedades del XML (<i>version</i>, <i>encoding</i>, <i>standalone</i>)
     * @param styleHref Referencia (enlace) a la hoja de estilo del XML (puede ser nulo)
     * @param styleType Tipo de la hoja de estilo del XML (puede ser nulo)
     * @return Cadena de texto con el XML en forma de array de octetos
     */
    public final static byte[] writeXML(final Node node, Hashtable<String, String> xmlProps, final String styleHref, final String styleType) {
    	
    	if (xmlProps == null) xmlProps = new Hashtable<String, String>(0);
    	
    	// La codificacion por defecto sera UTF-8
    	final String xmlEncoding = xmlProps.containsKey(OutputKeys.ENCODING) ? xmlProps.get(OutputKeys.ENCODING) : "UTF-8";
    	
    	// Primero creamos un writer
    	ByteArrayOutputStream baos = new ByteArrayOutputStream();
    	Writer writer = null;
    	try {
    		writer = new OutputStreamWriter(baos, xmlEncoding);
    	} 
    	catch(final UnsupportedEncodingException e) {
    		Logger.getLogger("es.gob.afirma").warning(
    				"La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto: " + e
    		);
    		writer = new OutputStreamWriter(baos);
    	}
 		
  		// Ahora escribimos el XML usando XALAN, para el control de los namespaces
  		writeXMLwithXALAN(writer, node, xmlEncoding);
  		
  		// Volvemos a cargar el XML en un DOM. Esto se hace porque las bibliotecas de XALAN hacen
  		// aparecer en ciertas ocasiones unos saltos de pagina unicode en el base 64 de los
  		// datos contenidos en la firma. Por motivos estilisticos, volvemos a cargar el XML y a
  		// guardarlo mediante DOM para que no aparezcan.
  		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
  		dbf.setNamespaceAware(true);
  		Document docum;
  		try {
  				docum = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(baos.toByteArray()));
  		}
  		catch(Throwable e) {
  			Logger.getLogger("es.gob.afirma").severe(
  				"No se ha podido recargar el XML para insertar los atributos de la cabecera, quizas la codificacion se vea afectada: " + e
  			);
  			return baos.toByteArray();
  		}
  		  		
  		// Escribimos por segunda vez el XML, pero ahora con el JRE
  		baos = new ByteArrayOutputStream();
  		try {
  			writer = new OutputStreamWriter(baos, xmlEncoding);
  		} 
  		catch (final Throwable e) {
  			Logger.getLogger("es.gob.afirma").warning(
  				"La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto: " + e
  			);
  			writer = new OutputStreamWriter(baos);
  		}

  		writeXMLwithJRE(writer, docum.getDocumentElement(), false, xmlProps);

  		
  		// Y devolvemos el resultado como array de bytes, insertando antes la cabecera de hoja de estilo
  		try {
  			return Utils.addStyleSheetHeader(new String(baos.toByteArray(), xmlEncoding), styleType, styleHref).getBytes(xmlEncoding);
  		}
  		catch(final Throwable e) {
  			Logger.getLogger("es.gob.afirma").warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto del sistema: " + e);
  			return Utils.addStyleSheetHeader(new String(baos.toByteArray()), styleType, styleHref).getBytes();
  		}
    }
    
    private final static void writeXMLwithXALAN(final Writer writer, final Node node, final String xmlEncoding) {
        Document document = node.getOwnerDocument();
        DOMImplementationLS domImplLS = (DOMImplementationLS) document.getImplementation();
        LSSerializer serializer = domImplLS.createLSSerializer();
        serializer.getDomConfig().setParameter("namespaces", false);
        DOMOutputImpl output = new DOMOutputImpl();
        output.setCharacterStream(writer);
        if(xmlEncoding != null)
        	output.setEncoding(xmlEncoding);
        serializer.write(node, output);
    }
    
    private final static void writeXMLwithJRE(final Writer writer, final Node node, final boolean indent, Hashtable<String, String> properties) {
    	try {
	        final DOMSource domSource = new DOMSource(node);
	        final StreamResult streamResult = new StreamResult(writer);
	        final TransformerFactory tf = TransformerFactory.newInstance();
	        final Transformer serializer = tf.newTransformer();
	        
	        if (properties == null) properties = new Hashtable<String, String>();
            // Por defecto, si no hay eclarada una codificacion, se utiliza UTF-8
            if (!properties.containsKey(OutputKeys.ENCODING) || "".equals(properties.get(OutputKeys.ENCODING))) properties.put(OutputKeys.ENCODING, "UTF-8");
            for (final String key : properties.keySet()) {
	        	serializer.setOutputProperty(key, properties.get(key));
	        }
	        if (indent) serializer.setOutputProperty(OutputKeys.INDENT, "yes");
	        serializer.transform(domSource, streamResult);
    	}
    	catch(final Throwable e) {
    		e.printStackTrace();
    		Logger.getLogger("es.gob.afirma").severe("Ocurrio un error escribiendo el XML: " + e);
    	}
    }

    /**
  	 * Genera un objeto descriptor de firma.
  	 * @param namespace Espacio de nombres utilizado para la recuperaci&oacute;n de atributos XAdES.
  	 * @param signature Nodo de firma.
  	 * @return Objeto descriptor de firma.
  	 */
  	public static AOSimpleSignInfo getSimpleSignInfoNode(String namespace, Element signature) {
  		
  		//Recupera la fecha de firma
  		Date signingTime = null;
  		if(namespace != null) {
  			try {
  				signingTime = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").parse(
  						((Element)signature.getElementsByTagNameNS(namespace, "SigningTime").item(0)).getTextContent()
  				);
  			}
  			catch (Throwable e) {
  				Logger.getLogger("es.gob.afirma").warning("No se ha podido recuperar la fecha de firma: " + e);
  			}
  		}
  		
  		final AOSimpleSignInfo ssi = new AOSimpleSignInfo(
  			new X509Certificate[] {
  					Utils.getCertificate(signature.getElementsByTagNameNS(DSIGNNS, "X509Certificate").item(0)) }, 
  			signingTime);
  		ssi.setSignAlgorithm(((Element)signature.getElementsByTagNameNS(DSIGNNS, "SignatureMethod").item(0)).getAttribute("Algorithm"));
  		
  		byte[] pkcs1;
  		try {
  			pkcs1 = new BASE64Decoder().decodeBuffer(
  					((Element)signature.getElementsByTagNameNS(DSIGNNS, "SignatureValue").item(0))
  					.getTextContent());
  		} catch (Exception e) {
  			Logger.getLogger("es.gob.afirma").warning("No se pudo extraer el PKCS#1 de una firma");
  			pkcs1 = null;
			}
  		ssi.setPkcs1(pkcs1);
  		
  		return ssi;		
  	}
  	
  	
  	/**
  	 * Obtiene el CN del certificado de una firma.
  	 * @param signature Nodo de firma.
  	 * @return CN del certificado de firma.
  	 */
  	public static String getStringInfoNode(Element signature) {
  		return AOUtil.getCN(Utils.getCertificate(
  				signature.getElementsByTagNameNS(DSIGNNS, "X509Certificate").item(0)));
  	}
  	
  	
  	/**
  	 * Genera un certificado X.509 a partir de un nodo de certificado de firma.
  	 * @param certificateNode Nodo "X509Certificate" de la firma.
  	 * @return Certificado de firma.
  	 */
  	public static X509Certificate getCertificate(Node certificateNode) {
  		return AOCryptoUtil.createCert(
  				certificateNode.getTextContent().trim().replace("\r", "").replace("\n", "").replace(" ", "").replace("\t", "")
  		);
  	}
  	
  	/**
  	 * Recupera el identificador (id) de la firma sobre la que se ha realizado una contrafirma.
  	 * Si no se encuentra la firma a la que se referencia se devuelve cadena vac&iacute;a.
  	 * @param signature Nodo de la contrafirma.
  	 * @param signatureValues Listado con todos los SignatureValue del documento de firma.
  	 * @return Identificador de la firma (Signature) referenciada.
  	 */
  	public static String getCounterSignerReferenceId( Element signature, NodeList signatureValues ) {
  		// Tomamos la URI de la primera referencia (la del objeto firmado),
  		// evitando el primer caracter de la URI que sera la almohadilla (#)
  		String uri = ((Element)signature.getElementsByTagNameNS(DSIGNNS, "Reference").item(0)).getAttribute("URI").substring(1);		
  		String signatureId = "";
  		for (int j = 0; j < signatureValues.getLength(); j++) {
  			Element signatureValue = (Element)signatureValues.item(j);
  			if (signatureValue.getAttribute("Id").equals(uri)) {
  				signatureId = ((Element)signatureValue.getParentNode()).getAttribute("Id");
  				break;
  			}
  		}
  		return signatureId;
  	}
}
