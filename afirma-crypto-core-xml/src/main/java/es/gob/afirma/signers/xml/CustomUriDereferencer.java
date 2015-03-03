package es.gob.afirma.signers.xml;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import javax.xml.crypto.Data;
import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.URIReference;
import javax.xml.crypto.URIReferenceException;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.dom.DOMURIReference;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/** Dereferenciador a medida de referencias XML DOM. */
public final class CustomUriDereferencer implements URIDereferencer {

	private static final String ID = "Id"; //$NON-NLS-1$

	private static final String DEFAULT_SUN_URI_DEREFERENCER_CLASSNAME = "org.jcp.xml.dsig.internal.dom.DOMURIDereferencer"; //$NON-NLS-1$
	private static final String DEFAULT_APACHE_URI_DEREFERENCER_CLASSNAME = "org.apache.jcp.xml.dsig.internal.dom.DOMURIDereferencer"; //$NON-NLS-1$


	private final URIDereferencer defaultUriDereferencer;

	/** Crea un dereferenciador a medida que act&uacute;a solo cuando falla el dereferenciador por defecto
	 * @param defaultDereferencer Dereferenciador por defecto */
	public CustomUriDereferencer(final URIDereferencer defaultDereferencer) {
		this.defaultUriDereferencer = defaultDereferencer;
	}

	/** Obtiene el dereferenciador a medida por defecto de Java.
	 * @return Dereferenciador a medida por defecto de Java
	 * @throws NoSuchFieldException Si falla la reflexi&oacute;n por cambios de las clases internas
	 *                              de Java
	 * @throws SecurityException Si no se tienen permisos para la reflexi&oacute;n
	 * @throws ClassNotFoundException Si falla la reflexi&oacute;n por desaparici&oacute;n de las clases internas
	 *                                de Java
	 * @throws IllegalAccessException Si falla la reflexi&oacute;n por fallos de visibilidad */
	public static URIDereferencer getDefaultDereferencer() throws NoSuchFieldException,
	                                                              ClassNotFoundException,
	                                                              IllegalAccessException {
    	// Obtenemos el dereferenciador por defecto por reflexion.
		// Este sera el de Apache si esta instalado o el de Sun si no lo esta
		try {
			final Field instanceField = Class.forName(DEFAULT_APACHE_URI_DEREFERENCER_CLASSNAME).getDeclaredField("INSTANCE"); //$NON-NLS-1$
	    	instanceField.setAccessible(true);
	    	return (URIDereferencer) instanceField.get(null);
		}
		catch (final Exception e) {
			final Field instanceField = Class.forName(DEFAULT_SUN_URI_DEREFERENCER_CLASSNAME).getDeclaredField("INSTANCE"); //$NON-NLS-1$
			instanceField.setAccessible(true);
			return (URIDereferencer) instanceField.get(null);
		}
	}

	@Override
	public Data dereference(final URIReference domRef, final XMLCryptoContext context) throws URIReferenceException {
		try {
			return this.defaultUriDereferencer.dereference(domRef, context);
		}
		catch(final Exception e) {

			// Aqui ha fallado el dereferenciador por defecto, probamos a dereferenciar nosotros
			final Attr uriAttr = (Attr) ((DOMURIReference)domRef).getHere();

			final Document doc = uriAttr.getOwnerDocument();
            final String uriValue = uriAttr.getNodeValue();
            Node targetNode = null;

            // Derreferenciacion de todo el XML en firmas enveloped
            if ("".equals(uriValue)) { //$NON-NLS-1$
            	targetNode = doc;
            }

            // Buscamos el nodo en todo el XML
            if (targetNode == null) {
            	String id = uriValue;
            	if (uriValue.length() > 0 && uriValue.charAt(0) == '#') {
            		id = uriValue.substring(1);
            	}
            	targetNode = getElementById(doc, id);
            }

            if (targetNode == null) {
            	throw new URIReferenceException(e);
            }

            try {
            	final com.sun.org.apache.xml.internal.security.signature.XMLSignatureInput in = new com.sun.org.apache.xml.internal.security.signature.XMLSignatureInput(targetNode);
            	if (in.isOctetStream()) {
            		try {
            			return new org.jcp.xml.dsig.internal.dom.ApacheOctetStreamData(in);
            		}
            		catch (final Exception ioe) {
            			throw new URIReferenceException(e);
            		}
            	}
            	return new org.jcp.xml.dsig.internal.dom.ApacheNodeSetData(in);
            }
            catch (final Exception e2) {

            	// Nuevo intento con otras clases de apache
            	try {
            		final Class<?> xmlSignatureInputClass = Class.forName("org.apache.xml.security.signature.XMLSignatureInput"); //$NON-NLS-1$
            		final Constructor<?> xmlSignatureInputConstructor = xmlSignatureInputClass.getConstructor(Node.class);
            		final Object in = xmlSignatureInputConstructor.newInstance(targetNode);

            		final Method isOctetStreamMethod = xmlSignatureInputClass.getMethod("isOctetStream"); //$NON-NLS-1$
            		if (((Boolean) isOctetStreamMethod.invoke(in)).booleanValue()) {
            			try {
            				final Class<?> apacheOctetStreamDataClass = Class.forName("org.apache.jcp.xml.dsig.internal.dom.ApacheOctetStreamData"); //$NON-NLS-1$
            				final Constructor<?> apacheOctetStreamDataConstructor = apacheOctetStreamDataClass.getConstructor(in.getClass());

            				return (Data) apacheOctetStreamDataConstructor.newInstance(in);
            			}
            			catch (final Exception ioe) {
            				throw new URIReferenceException(e);
            			}
            		}

            		final Class<?> apacheNodeSetDataClass = Class.forName("org.apache.jcp.xml.dsig.internal.dom.ApacheNodeSetData"); //$NON-NLS-1$
            		final Constructor<?> apacheNodeSetDataConstructor = apacheNodeSetDataClass.getConstructor(in.getClass());

            		return (Data) apacheNodeSetDataConstructor.newInstance(in);
            	}
            	catch (final Exception e3) {
            		throw new URIReferenceException("Error al derreferenciar la URL en todos los intentos:" + //$NON-NLS-1$
            				"\nIntento 1: " + e + //$NON-NLS-1$
            				"\nIntento 2: " + e2 + //$NON-NLS-1$
            				"\nIntento 3: " + e3); //$NON-NLS-1$
            	}
            }

//            final org.apache.xml.security.signature.XMLSignatureInput in = new org.apache.xml.security.signature.XMLSignatureInput(targetNode);
//            if (in.isOctetStream()) {
//            	try {
//            		return new org.apache.jcp.xml.dsig.internal.dom.ApacheOctetStreamData(in);
//            	}
//            	catch (final Exception ioe) {
//            		throw new URIReferenceException(e);
//            	}
//            }
//            return new org.apache.jcp.xml.dsig.internal.dom.ApacheNodeSetData(in);
		}
	}

	/** Busca el primer nodo de un documento XML que tenga un atributo con nombre
	 * <i>Id</i> cuyo valor sea el indicado o <code>null</code> si no se encuentra
	 * ninguno.
	 * @param doc Documento XML
	 * @param nodeId Valor del atributo <i>Id</i> del nodo a buscar
	 * @return Primer nodo de un documento XML que tenga un atributo <i>Id</i> con el
	 *         valor indicado o <code>null</code> si no se encuentra ninguno */
	public static Element getElementById(final Document doc, final String nodeId) {
		if (doc == null || nodeId == null) {
			return null;
		}
	    final NodeList nodeList = doc.getElementsByTagName("*"); //$NON-NLS-1$
	    for (int i = 0, len = nodeList.getLength(); i < len; i++) {
	        final Node node = nodeList.item(i);
	        if (node.getNodeType() == Node.ELEMENT_NODE) {
	        	// Buscamos un atributo 'Id'
	        	final NamedNodeMap nnm = node.getAttributes();
	        	for (int j = 0; j < nnm.getLength(); ++j) {
	        	    final Node attr = nnm.item(j);
	        	    if (ID.equalsIgnoreCase(attr.getNodeName()) && nodeId.equals(attr.getNodeValue()) && node instanceof Element) {
	        	    	return (Element) node;
	        	    }
	        	}
	        }
	    }
		return null;
	}

}
