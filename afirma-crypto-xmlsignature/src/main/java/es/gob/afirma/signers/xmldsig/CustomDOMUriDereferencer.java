/*
 * Copyright 2005 Sun Microsystems, Inc.  All Rights Reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * CA 95054 USA or visit www.sun.com if you need additional information or
 * have any questions.
 */
/*
 * $Id: DOMURIDereferencer.java,v 1.19 2005/09/23 20:09:34 mullan Exp $
 */
package es.gob.afirma.signers.xmldsig;

import javax.xml.crypto.Data;
import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.URIReference;
import javax.xml.crypto.URIReferenceException;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.dom.DOMCryptoContext;
import javax.xml.crypto.dom.DOMURIReference;
import javax.xml.crypto.dsig.XMLSignContext;

import org.jcp.xml.dsig.internal.dom.ApacheNodeSetData;
import org.jcp.xml.dsig.internal.dom.ApacheOctetStreamData;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.org.apache.xml.internal.security.Init;
import com.sun.org.apache.xml.internal.security.signature.XMLSignatureInput;
import com.sun.org.apache.xml.internal.security.utils.IdResolver;
import com.sun.org.apache.xml.internal.security.utils.resolver.ResourceResolver;

/** DOM-based implementation of URIDereferencer.
 * @author Sean Mullan */
@SuppressWarnings("restriction")
public final class CustomDOMUriDereferencer implements URIDereferencer {

    static final URIDereferencer INSTANCE = new CustomDOMUriDereferencer();

    /** Constructor. */
    public CustomDOMUriDereferencer() {
        Init.init();
    }

	@Override
	public Data dereference(final URIReference uriRef, final XMLCryptoContext context) throws URIReferenceException {

        if (uriRef == null) {
            throw new IllegalArgumentException("La referencia no puede ser nula"); //$NON-NLS-1$
        }
        if (context == null) {
            throw new IllegalArgumentException("El contexto no puede ser nulo"); //$NON-NLS-1$
        }

        final DOMURIReference domRef = (DOMURIReference) uriRef;
        final Attr uriAttr = (Attr) domRef.getHere();
        final String uri = uriRef.getURI();
        final DOMCryptoContext dcc = (DOMCryptoContext) context;

        // Check if same-document URI and register ID
        if (uri != null && uri.length() != 0 && uri.charAt(0) == '#') {
            String id = uri.substring(1);

            if (id.startsWith("xpointer(id(")) { //$NON-NLS-1$
                final int i1 = id.indexOf('\'');
                final int i2 = id.indexOf('\'', i1+1);
                id = id.substring(i1+1, i2);
            }

            // this is a bit of a hack to check for registered
            // IDRefs and manually register them with Apache's IdResolver
            // map which includes builtin schema knowledge of DSig/Enc IDs
            if (context instanceof XMLSignContext) {
                final Node referencedElem = dcc.getElementById(id);
                if (referencedElem != null) {
                    IdResolver.registerElementById((Element) referencedElem, id);
                }
            }
        }

        try {
            final String baseURI = context.getBaseURI();
            final ResourceResolver apacheResolver = ResourceResolver.getInstance(uriAttr, baseURI);
            final XMLSignatureInput in = apacheResolver.resolve(uriAttr, baseURI);
            if (in.isOctetStream()) {
                return new ApacheOctetStreamData(in);
            }
			return new ApacheNodeSetData(in);
        }
        catch (final Exception e) {

        	// Si el derreferenciador de Apache no ha podido obtener la referencia, la buscamos
        	// nosotros en el XML

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
            	targetNode = findNodeById(id, doc);
            }

            if (targetNode == null) {
            	throw new URIReferenceException(e);
            }

            final XMLSignatureInput in = new XMLSignatureInput(targetNode);
            if (in.isOctetStream()) {
            	try {
            		return new ApacheOctetStreamData(in);
            	} catch (final Exception ioe) {
            		throw new URIReferenceException(e);
            	}
            }
			return new ApacheNodeSetData(in);
        }
    }

    /**
     * Comprueba si un nodo o alguno de sus nodos hijos tiene el identificador indicado.
     * @param id Identificador buscado.
     * @param xmlNode Nodo sobre el que se realiza la b&uacute;squeda.
     * @return Nodo con el identificador indicado o {@code null} si no se encuentra.
     */
    private Node findNodeById(final String id, final Node xmlNode) {

    	final NamedNodeMap attrs = xmlNode.getAttributes();
        if (attrs != null && attrs.getNamedItem("Id") != null && id.equals(attrs.getNamedItem("Id").getNodeValue())) { //$NON-NLS-1$ //$NON-NLS-2$
        	return xmlNode;
        }

        Node targetNode = null;
        final NodeList childNodes = xmlNode.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++) {
        	if (childNodes.item(i).getNodeType() == Node.ELEMENT_NODE) {
        		targetNode = findNodeById(id, childNodes.item(i));
        		if (targetNode != null) {
        			return targetNode;
        		}
        	}
        }
        return null;
    }
}
