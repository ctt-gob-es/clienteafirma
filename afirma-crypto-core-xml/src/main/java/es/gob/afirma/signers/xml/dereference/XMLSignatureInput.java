/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package es.gob.afirma.signers.xml.dereference;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import es.gob.afirma.core.misc.AOUtil;
import nu.xom.converters.DOMConverter;

/** Class XMLSignatureInput.
 * @author Christian Geuer-Pollmann
 * $todo$ check whether an XMLSignatureInput can be _both_, octet stream _and_ node set? */
final class XMLSignatureInput {

    /*
     * The XMLSignature Input can be either:
     *   A byteArray like with/or without InputStream.
     *   Or a nodeSet like defined either:
     *       * as a collection of nodes
     *       * or as subnode excluding or not comments and excluding or
     *         not other nodes.
     */

    /** Some InputStreams do not support the {@link java.io.InputStream#reset}
     * method, so we read it in completely and work on our Proxy. */
    private InputStream inputOctetStreamProxy = null;

    /** The original NodeSet for this XMLSignatureInput. */
    private Set<Node> inputNodeSet = null;

    /** The original Element. */
    private Node subNode = null;

    /** Exclude Node *for enveloped transformations*. */
    private final Node excludeNode = null;

    private final boolean excludeComments = false;

    private final boolean isNodeSet = false;

    /** A cached bytes. */
    private byte[] bytes = null;

    /** Some Transforms may require explicit MIME type, charset (IANA registered
     * "character set"), or other such information concerning the data they are
     * receiving from an earlier Transform or the source data, although no
     * Transform algorithm specified in this document needs such explicit
     * information. Such data characteristics are provided as parameters to the
     * Transform algorithm and should be described in the specification for the
     * algorithm. */
    private final String mimeType = null;

    /** Field sourceURI. */
    private final String sourceURI = null;

    /** Node Filter list. */
    private final List<NodeFilter> nodeFilters = new ArrayList<>();

    private final boolean needsToBeExpanded = false;

    private DocumentBuilderFactory dfactory;

    /** Construct a XMLSignatureInput from a subtree rooted by rootNode. This
     * method included the node and <I>all</I> his descendants in the output.
     * @param rootNode The root node. */
    public XMLSignatureInput(final Node rootNode) {
        this.subNode = rootNode;
    }

    /** Check if the structure needs to be expanded.
     * @return true if so. */
    public boolean isNeedsToBeExpanded() {
        return this.needsToBeExpanded;
    }

    /** Returns the node set from input which was specified as the parameter of
     * <code>XMLSignatureInput</code> constructor.
     * @return the node set
     * @throws SAXException If error on SAX parsing.
     * @throws IOException If error while reading the data,
     * @throws ParserConfigurationException If error while creating the XML parser. */
    public Set<Node> getNodeSet() throws ParserConfigurationException,
                                         IOException,
                                         SAXException {
        return getNodeSet(false);
    }

    /** Returns the node set from input which was specified as the parameter of
     * <code>XMLSignatureInput</code> constructor.
     * @param circumvent If there's need for fixing Java Bug 2650.
     * @return The node set
     * @throws SAXException If error on SAX parsing.
     * @throws IOException If error while reading the data,
     * @throws ParserConfigurationException If error while creating the XML parser. */
    public Set<Node> getNodeSet(final boolean circumvent) throws ParserConfigurationException,
                                                                 IOException,
                                                                 SAXException {
        if (this.inputNodeSet != null) {
            return this.inputNodeSet;
        }
        if (this.inputOctetStreamProxy == null && this.subNode != null) {
            if (circumvent) {
                XMLUtils.circumventBug2650(XMLUtils.getOwnerDocument(this.subNode));
            }
            this.inputNodeSet = new LinkedHashSet<>();
            XMLUtils.getSet(this.subNode, this.inputNodeSet, this.excludeNode, this.excludeComments);
            return this.inputNodeSet;
        }
        else if (isOctetStream()) {
            convertToNodes();
            final Set<Node> result = new LinkedHashSet<>();
            XMLUtils.getSet(this.subNode, result, null, false);
            return result;
        }

        throw new RuntimeException("getNodeSet() called but no input data present"); //$NON-NLS-1$
    }

    /** Returns the Octet stream(byte Stream) from input which was specified as
     * the parameter of <code>XMLSignatureInput</code> constructor.
     * @return The Octet stream(byte Stream) from input which was specified as
     *         the parameter of <code>XMLSignatureInput</code> constructor.
     * @throws IOException On any data error. */
    public InputStream getOctetStream() throws IOException  {
        if (this.inputOctetStreamProxy != null) {
            return this.inputOctetStreamProxy;
        }
        if (this.bytes != null) {
            this.inputOctetStreamProxy = new ByteArrayInputStream(this.bytes);
            return this.inputOctetStreamProxy;
        }
        return null;
    }

    /** Returns the byte array from input which was specified as the parameter of
     * <code>XMLSignatureInput</code> constructor.
     * @return The byte[] from input which was specified as the parameter of
     *         <code>XMLSignatureInput</code> constructor.
     * @throws IOException On any error getting the bytes. */
    public byte[] getBytes() throws IOException  {
        final byte[] inputBytes = getBytesFromInputStream();
        if (inputBytes != null) {
            return inputBytes;
        }
        if (getSubNode() instanceof Element) {
            return canonicalizeXml(
        		(Element) getSubNode(),
        		nu.xom.canonical.Canonicalizer.CANONICAL_XML
    		);
        }
        throw new IOException(
    		"Cannot get canonicalized bytes from non Element-type Node" //$NON-NLS-1$
		);
    }

    /** Determines if the object has been set up with a Node set.
     * @return true if the object has been set up with a Node set. */
    public boolean isNodeSet() {
        return this.inputOctetStreamProxy == null
            && this.inputNodeSet != null || this.isNodeSet;
    }

    /** Determines if the object has been set up with an octet stream.
     * @return true if the object has been set up with an octet stream. */
    public boolean isOctetStream() {
        return (this.inputOctetStreamProxy != null || this.bytes != null)
          && this.inputNodeSet == null && this.subNode == null;
    }

    /** Returns mimeType.
     * @return mimeType. */
    public String getMIMEType() {
        return this.mimeType;
    }

    /** Return SourceURI.
     * @return SourceURI The source URI. */
    public String getSourceURI() {
        return this.sourceURI;
    }

    /** Gets the node of this XMLSignatureInput.
     * @return The excludeNode set. */
    public Node getSubNode() {
        return this.subNode;
    }

    boolean isExcludeComments() {
        return this.excludeComments;
    }

    private byte[] getBytesFromInputStream() throws IOException {
        if (this.bytes != null) {
            return this.bytes;
        }
        if (this.inputOctetStreamProxy == null) {
            return null;
        }
        try {
            this.bytes = AOUtil.getDataFromInputStream(this.inputOctetStreamProxy);
        }
        finally {
            this.inputOctetStreamProxy.close();
        }
        return this.bytes;
    }

    /** Gets the node filters.
     * @return the node filters. */
    public List<NodeFilter> getNodeFilters() {
        return this.nodeFilters;
    }

    void convertToNodes() throws ParserConfigurationException, IOException, SAXException {
        if (this.dfactory == null) {
            this.dfactory = DocumentBuilderFactory.newInstance();
            this.dfactory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, Boolean.TRUE.booleanValue());
            this.dfactory.setValidating(false);
            this.dfactory.setNamespaceAware(true);
        }
        final DocumentBuilder db = this.dfactory.newDocumentBuilder();
        // select all nodes, also the comments.
        try {
        	// Se ignoran los errores (como en org.apache.xml.security.utils.IgnoreAllErrorHandler)
            db.setErrorHandler(
        		new ErrorHandler() {
					@Override
					public void warning(final SAXParseException exception) throws SAXException {
						// Se ignora
					}

					@Override
					public void error(final SAXParseException exception) throws SAXException {
						// Se ignora
					}

					@Override
					public void fatalError(final SAXParseException exception) throws SAXException {
						// Se ignora
					}
				}
    		);

            final Document doc;
            try (final InputStream is = getOctetStream()) {
            	doc = db.parse(is);
            }
            this.subNode = doc;
        }
        catch (final SAXException ex) {

        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"No se ha encontrado un conjunto de nodos bien formado: " + ex //$NON-NLS-1$
			);

            // if a not-wellformed nodeset exists, put a container around it...
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();

            baos.write("<container>".getBytes(StandardCharsets.UTF_8)); //$NON-NLS-1$
            baos.write(getBytes());
            baos.write("</container>".getBytes(StandardCharsets.UTF_8)); //$NON-NLS-1$

            final byte result[] = baos.toByteArray();
            final Document document = db.parse(new ByteArrayInputStream(result));
            this.subNode = document.getDocumentElement().getFirstChild().getFirstChild();
        }
        finally {
            if (this.inputOctetStreamProxy != null) {
                this.inputOctetStreamProxy.close();
            }
            this.inputOctetStreamProxy = null;
            this.bytes = null;
        }
    }

	private static byte[] canonicalizeXml(final org.w3c.dom.Element element, final String algorithm) throws IOException {
		final nu.xom.Element xomElement = DOMConverter.convert(element);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final nu.xom.canonical.Canonicalizer canonicalizer = new nu.xom.canonical.Canonicalizer(baos, algorithm);
		canonicalizer.write(xomElement);
		return baos.toByteArray();
	}

}