/*
 * eID Applet Project.
 * Copyright (C) 2008-2009 FedICT.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version
 * 3.0 as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, see
 * http://www.gnu.org/licenses/.
 */

/*
 * Copyright (C) 2008-2009 FedICT.
 * This file is part of the eID Applet Project.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Signature;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMCryptoContext;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.Manifest;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignedInfo;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignContext;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.jcp.xml.dsig.internal.dom.DOMReference;
import org.jcp.xml.dsig.internal.dom.DOMSignedInfo;
import org.jcp.xml.dsig.internal.dom.DOMXMLSignature;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.signature.XMLSignature;
import com.sun.org.apache.xml.internal.security.utils.Constants;
import com.sun.org.apache.xpath.internal.XPathAPI;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer.xades.OOXMLXAdESSigner;
import es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.spi.SignatureService;

/** Abstract base class for an XML Signature Service implementation.
 * @author fcorneli */
@SuppressWarnings("restriction")
public abstract class AbstractXmlSignatureService implements SignatureService {

    private SignatureFacet signatureFacet = null;

    /** Adds a signature facet to this XML signature service.
     * @param signatureFacet */
    protected final void addSignatureFacet(final SignatureFacet sigFacet) {
    	this.signatureFacet = sigFacet;
    }

    /** Gives back the enveloping document. Return <code>null</code> in case
     * ds:Signature should be the top-level element. Implementations can
     * override this method to provide a custom enveloping document.
     * @return null */
    private static Document getEnvelopingDocument() {
        return null;
    }

    /** Override this method to change the URI dereferener used by the signing
     * engine. */
	protected abstract URIDereferencer getURIDereferencer(final byte[] ooXmlDocument);

    @Override
	public byte[] preSign(final byte[] ooXmlDocument,
						  final X509Certificate[] signingCertificateChain,
                          final PrivateKey signingKey,
                          final String signatureComments,
                          final String address1,
                          final String address2) throws NoSuchAlgorithmException,
                                                        InvalidAlgorithmParameterException,
                                                        ParserConfigurationException,
                                                        MarshalException,
                                                        XMLSignatureException,
                                                        TransformerException,
                                                        IOException,
                                                        SAXException {

    	try {
	    	final byte[] s1 = OOXMLXAdESSigner.getSignedXML(ooXmlDocument, "SHA512withRSA", signingKey, signingCertificateChain, null); //$NON-NLS-1$
	    	final byte[] s2 = getSignedXML(ooXmlDocument, "SHA-512", signingCertificateChain, signingKey, null, null, null); //$NON-NLS-1$
	    	final File tmpFile1 = File.createTempFile("TEMP1-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
	    	final File tmpFile2 = File.createTempFile("TEMP2-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
	    	final FileOutputStream fos1 = new FileOutputStream(tmpFile1);
	    	final FileOutputStream fos2 = new FileOutputStream(tmpFile2);
	    	fos1.write(s1);
	    	fos2.write(s2);
	    	fos1.flush();
	    	fos2.flush();
	    	fos1.close();
	    	fos2.close();
	    	System.out.println("ESCRITOS XML"); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		e.printStackTrace();
    	}


        return getSignedXML(
    		ooXmlDocument,
    		"SHA-512", //$NON-NLS-1$
    		signingCertificateChain,
    		signingKey,
    		signatureComments,
    		address1,
    		address2
		);
    }

    @Override
	public byte[] postSign(final byte[] signedXML,
                           final X509Certificate[] signingCertificateChain,
                           final String signatureId,
                           final byte[] signatureValue) throws ParserConfigurationException,
                                                               SAXException,
                                                               IOException,
                                                               TransformerException {

        // Load the signature DOM document.
        final Document document = loadDocument(new ByteArrayInputStream(signedXML));

        // Locate the correct ds:Signature node.
        final Element nsElement = document.createElement("ns"); //$NON-NLS-1$
        nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:ds", Constants.SignatureSpecNS); //$NON-NLS-1$

        final Element signatureElement = (Element) XPathAPI.selectSingleNode(document, "//ds:Signature[@Id='" + signatureId + "']", nsElement); //$NON-NLS-1$ //$NON-NLS-2$

        if (null == signatureElement) {
            throw new IllegalArgumentException("ds:Signature not found for @Id: " + signatureId); //$NON-NLS-1$
        }

        // Insert signature value into the ds:SignatureValue element
        final NodeList signatureValueNodeList = signatureElement.getElementsByTagNameNS(javax.xml.crypto.dsig.XMLSignature.XMLNS, "SignatureValue"); //$NON-NLS-1$
        final Element signatureValueElement = (Element) signatureValueNodeList.item(0);
        signatureValueElement.setTextContent(Base64.encode(signatureValue));

        // TODO: Cambiar a OOXMLSignedDocumentOutputStream
        final ByteArrayOutputStream signedDocumentOutputStream = new ByteArrayOutputStream();
        writeDocument(document, signedDocumentOutputStream);
        return signedDocumentOutputStream.toByteArray();
    }

    private byte[] getSignedXML(final byte[] ooXmlDocument,
    		                    final String digestAlgo,
                                final X509Certificate[] signingCertificateChain,
                                final PrivateKey signingKey,
                                final String signatureComments,
                                final String address1,
                                final String address2) throws ParserConfigurationException,
                                                                       NoSuchAlgorithmException,
                                                                       InvalidAlgorithmParameterException,
                                                                       MarshalException,
                                                                       javax.xml.crypto.dsig.XMLSignatureException,
                                                                       TransformerException,
                                                                       IOException,
                                                                       SAXException {
        // DOM Document construction.
        Document document = getEnvelopingDocument();
        if (null == document) {
            final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setNamespaceAware(true);
            document = documentBuilderFactory.newDocumentBuilder().newDocument();
        }

        final XMLSignContext xmlSignContext = new DOMSignContext(signingKey, document);
        final URIDereferencer uriDereferencer = getURIDereferencer(ooXmlDocument);
        if (null != uriDereferencer) {
            xmlSignContext.setURIDereferencer(uriDereferencer);
        }

		final XMLSignatureFactory signatureFactory = XMLSignatureFactory.getInstance("DOM", new org.jcp.xml.dsig.internal.dom.XMLDSigRI()); //$NON-NLS-1$

        // Referencias a firmar
        final List<Reference> references = new LinkedList<Reference>();

        // Invoke the signature facets.
        final String signatureId = "xmldsig-" + UUID.randomUUID().toString(); //$NON-NLS-1$
        final List<XMLObject> objects = new LinkedList<XMLObject>();

    	// Esta prefirma anade los nodos "idPackageObject" (el Manifest) e "idOfficeObject" (SignatureInfo)
        this.signatureFacet.preSign(
        	ooXmlDocument,
    		signatureFactory,
    		document,
    		signatureId,
    		references,
    		objects,
    		signatureComments,
    		address1,
    		address2
		);

        // ds:SignedInfo
        final SignatureMethod signatureMethod = signatureFactory.newSignatureMethod(getSignatureMethod(digestAlgo), null);
        final SignedInfo signedInfo = signatureFactory.newSignedInfo(
    		signatureFactory.newCanonicalizationMethod(
				CanonicalizationMethod.INCLUSIVE,
				(C14NMethodParameterSpec) null
			),
            signatureMethod,
            references
        );

        // Creamos el KeyInfo
        final KeyInfoFactory kif = signatureFactory.getKeyInfoFactory();
        final List<Object> x509Content = new ArrayList<Object>();
        x509Content.add(signingCertificateChain[0]);

        final List<Object> content = new ArrayList<Object>();
        try {
            content.add(kif.newKeyValue(signingCertificateChain[0].getPublicKey()));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error creando el KeyInfo, la informacion puede resultar incompleta: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        content.add(kif.newX509Data(x509Content));

        // JSR105 ds:Signature creation
        final String signatureValueId = signatureId + "-signature-value"; //$NON-NLS-1$
        final javax.xml.crypto.dsig.XMLSignature xmlSignature = signatureFactory.newXMLSignature(
    		signedInfo,
    		kif.newKeyInfo(content), // KeyInfo
            objects,
            signatureId,
            signatureValueId
        );

        // ds:Signature Marshalling.
        final DOMXMLSignature domXmlSignature = (DOMXMLSignature) xmlSignature;
        Node documentNode = document.getDocumentElement();
        if (null == documentNode) {
            documentNode = document; // In case of an empty DOM document.
        }
        final String dsPrefix = null;
        domXmlSignature.marshal(documentNode, dsPrefix, (DOMCryptoContext) xmlSignContext);

        // Completion of undigested ds:References in the ds:Manifests.
        for (final XMLObject object : objects) {

            final List<XMLStructure> objectContentList = object.getContent();
            for (final XMLStructure objectContent : objectContentList) {
                if (!(objectContent instanceof Manifest)) {
                    continue;
                }
                final Manifest manifest = (Manifest) objectContent;
                final List<Reference> manifestReferences = manifest.getReferences();
                for (final Reference manifestReference : manifestReferences) {
                    if (null != manifestReference.getDigestValue()) {
                        continue;
                    }
                    final DOMReference manifestDOMReference = (DOMReference) manifestReference;
                    manifestDOMReference.digest(xmlSignContext);
                }
            }
        }

        // Completion of undigested ds:References.
        final List<Reference> signedInfoReferences = signedInfo.getReferences();
        for (final Reference signedInfoReference : signedInfoReferences) {
            final DOMReference domReference = (DOMReference) signedInfoReference;
            if (null != domReference.getDigestValue()) {
                // ds:Reference with external digest value
                continue;
            }
            domReference.digest(xmlSignContext);
        }

        // Calculation of signature
        final DOMSignedInfo domSignedInfo = (DOMSignedInfo) signedInfo;
        final ByteArrayOutputStream dataStream = new ByteArrayOutputStream();
        domSignedInfo.canonicalize(xmlSignContext, dataStream);
        final byte[] octets = dataStream.toByteArray();
        final Signature sig = Signature.getInstance(digestAlgo.replace("-",  "") + "withRSA"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        final byte[] sigBytes;
        try {
            sig.initSign(signingKey);
            sig.update(octets);
            sigBytes = sig.sign();
        }
        catch (final Exception e) {
            throw new javax.xml.crypto.dsig.XMLSignatureException("Error en la firma PKCS#1 ('" + digestAlgo + "withRSA): " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Sacamos el pre-XML a un OutputStream
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        writeDocument(document, baos);

        // Ya tenemos el XML, con la firma vacia y el SignatureValue, cada uno
        // por su lado...
        return postSign(baos.toByteArray(), signingCertificateChain, signatureId, sigBytes);

    }

    private static String getSignatureMethod(final String digestAlgo) {
        if (null == digestAlgo) {
            throw new IllegalArgumentException("digest algo is null"); //$NON-NLS-1$
        }
        if ("SHA1".equals(AOSignConstants.getDigestAlgorithmName(digestAlgo))) { //$NON-NLS-1$
            return SignatureMethod.RSA_SHA1;
        }
        if ("SHA-256".equals(AOSignConstants.getDigestAlgorithmName(digestAlgo))) { //$NON-NLS-1$
            return XMLSignature.ALGO_ID_SIGNATURE_RSA_SHA256;
        }
        if ("SHA-512".equals(AOSignConstants.getDigestAlgorithmName(digestAlgo))) { //$NON-NLS-1$
            return XMLSignature.ALGO_ID_SIGNATURE_RSA_SHA512;
        }
        if ("SHA-384".equals(AOSignConstants.getDigestAlgorithmName(digestAlgo))) { //$NON-NLS-1$
            return XMLSignature.ALGO_ID_SIGNATURE_RSA_SHA384;
        }
        if ("RIPEMD160".equals(AOSignConstants.getDigestAlgorithmName(digestAlgo))) { //$NON-NLS-1$
            return XMLSignature.ALGO_ID_SIGNATURE_RSA_RIPEMD160;
        }
        throw new IllegalArgumentException("unsupported sign algo: " + digestAlgo); //$NON-NLS-1$
    }

    private static void writeDocument(final Document document,
    		                          final OutputStream documentOutputStream) throws TransformerException,
                                                                                      IOException {
        writeDocumentNoClosing(document, documentOutputStream);
        documentOutputStream.close();
    }

    private static void writeDocumentNoClosing(final Document document, final OutputStream documentOutputStream) throws TransformerException {
        // we need the XML processing initial line for OOXML
        writeDocumentNoClosing(document, documentOutputStream, false);
    }

    protected static void writeDocumentNoClosing(final Document document,
    		                                     final OutputStream documentOutputStream,
    		                                     final boolean omitXmlDeclaration) throws TransformerException {
        final NoCloseOutputStream outputStream = new NoCloseOutputStream(documentOutputStream);
        final Result result = new StreamResult(outputStream);
        final Transformer xformer = TransformerFactory.newInstance().newTransformer();
        if (omitXmlDeclaration) {
            xformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes"); //$NON-NLS-1$
        }
        final Source source = new DOMSource(document);
        xformer.transform(source, result);
    }

    private static Document loadDocument(final InputStream documentInputStream) throws ParserConfigurationException,
                                                                                       SAXException,
                                                                                       IOException {
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        return documentBuilderFactory.newDocumentBuilder().parse(new InputSource(documentInputStream));
    }

    protected static Document loadDocumentNoClose(final InputStream documentInputStream) throws ParserConfigurationException,
                                                                                                SAXException,
                                                                                                IOException {
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        return documentBuilder.parse(new InputSource(new NoCloseInputStream(documentInputStream)));
    }
}
