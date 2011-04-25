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

package es.gob.afirma.be.fedict.eid.applet.service.signer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
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
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Manifest;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignedInfo;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignContext;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.keyinfo.X509Data;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FilenameUtils;
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
import com.sun.org.apache.xml.internal.security.utils.Base64;
import com.sun.org.apache.xml.internal.security.utils.Constants;
import com.sun.org.apache.xpath.internal.XPathAPI;

import es.gob.afirma.be.fedict.eid.applet.service.spi.DigestInfo;
import es.gob.afirma.be.fedict.eid.applet.service.spi.SignatureService;

/**
 * Abstract base class for an XML Signature Service implementation.
 * 
 * @author fcorneli
 * 
 */
public abstract class AbstractXmlSignatureService implements SignatureService {

	private final List<SignatureFacet> signatureFacets;

	/**
	 * Main constructor.
	 */
	public AbstractXmlSignatureService() {
		this.signatureFacets = new LinkedList<SignatureFacet>();
	}

	/**
	 * Adds a signature facet to this XML signature service.
	 * 
	 * @param signatureFacet
	 */
	protected void addSignatureFacet(SignatureFacet signatureFacet) {
		this.signatureFacets.add(signatureFacet);
	}

	/**
	 * Gives back the signature digest algorithm. Allowed values are SHA-1,
	 * SHA-256, SHA-384, SHA-512, RIPEND160. The default algorithm is SHA-1.
	 * Override this method to select another signature digest algorithm.
	 * 
	 * @return
	 */
	protected String getSignatureDigestAlgorithm() {
		return "SHA1";
	}

	/**
	 * Gives back the enveloping document. Return <code>null</code> in case
	 * ds:Signature should be the top-level element. Implementations can
	 * override this method to provide a custom enveloping document.
	 * @return
	 */
	protected Document getEnvelopingDocument() {
		return null;
	}

	/**
	 * Override this method to change the URI dereferener used by the signing
	 * engine.
	 * 
	 * @return
	 */
	protected URIDereferencer getURIDereferencer() {
		return null;
	}

	/**
	 * Gives back the human-readable description of what the citizen will be
	 * signing. The default value is "XML Document". Override this method to
	 * provide the citizen with another description.
	 * 
	 * @return
	 */
	protected String getSignatureDescription() {
		return "XML Document";
	}

	public byte[] preSign(List<DigestInfo> digestInfos, List<X509Certificate> signingCertificateChain, PrivateKey signingKey) throws NoSuchAlgorithmException {
		try {
			return getSignedXML(getSignatureDigestAlgorithm(), digestInfos, signingCertificateChain, signingKey);
		} 
		catch (Exception e) {
			throw new RuntimeException("XML signature error: " + e.getMessage(), e);
		}
	}

	
	public byte[] postSign(byte[] signedXML, List<X509Certificate> signingCertificateChain, String signatureId, byte[] signatureValue) {

		/*
		 * Load the signature DOM document.
		 */
		Document document;
		try {
			document = loadDocument(new ByteArrayInputStream(signedXML));
		} catch (Exception e) {
			throw new RuntimeException("DOM error: " + e.getMessage(), e);
		}

		/*
		 * Locate the correct ds:Signature node.
		 */
		Element nsElement = document.createElement("ns");
		nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:ds",
				Constants.SignatureSpecNS);
		Element signatureElement;
		try {
			signatureElement = (Element) XPathAPI.selectSingleNode(
				document,
				"//ds:Signature[@Id='" + signatureId + "']", 
				nsElement
			);
		} 
		catch (TransformerException e) {
			throw new RuntimeException("XPATH error: " + e.getMessage(), e);
		}
		if (null == signatureElement) {
			throw new RuntimeException("ds:Signature not found for @Id: " + signatureId);
		}
		
		/*
		 * Insert signature value into the ds:SignatureValue element
		 */
		NodeList signatureValueNodeList = signatureElement.getElementsByTagNameNS(
			javax.xml.crypto.dsig.XMLSignature.XMLNS,
			"SignatureValue"
		);
		Element signatureValueElement = (Element) signatureValueNodeList.item(0);
		signatureValueElement.setTextContent(Base64.encode(signatureValue));


		//TODO: Cambiar a OOXMLSignedDocumentOutputStream
		ByteArrayOutputStream signedDocumentOutputStream = new ByteArrayOutputStream();
		try {
			writeDocument(document, signedDocumentOutputStream);
		} 
		catch (Throwable e) {
			throw new RuntimeException("error writing the signed XML document: " + e.getMessage(), e);
		}
		return signedDocumentOutputStream.toByteArray();
	}

	protected String getCanonicalizationMethod() {
		return CanonicalizationMethod.EXCLUSIVE;
	}

	private byte[] getSignedXML(String digestAlgo,
			                    List<DigestInfo> digestInfos,
			                    List<X509Certificate> signingCertificateChain,
			                    PrivateKey signingKey) throws ParserConfigurationException, 
			                                                  NoSuchAlgorithmException, 
			                                                  InvalidAlgorithmParameterException, 
			                                                  MarshalException, 
			                                                  javax.xml.crypto.dsig.XMLSignatureException, 
			                                                  TransformerFactoryConfigurationError, 
			                                                  TransformerException, 
			                                                  IOException {

		/*
		 * DOM Document construction.
		 */
		Document document = getEnvelopingDocument();
		if (null == document) {
			DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
			documentBuilderFactory.setNamespaceAware(true);
			document = documentBuilderFactory.newDocumentBuilder().newDocument();
		}

		XMLSignContext xmlSignContext = new DOMSignContext(signingKey, document);
		URIDereferencer uriDereferencer = getURIDereferencer();
		if (null != uriDereferencer) xmlSignContext.setURIDereferencer(uriDereferencer);

		XMLSignatureFactory signatureFactory = XMLSignatureFactory.getInstance(
			"DOM", new org.jcp.xml.dsig.internal.dom.XMLDSigRI()
		);

		/*
		 * Add ds:References that come from signing client local files.
		 */
		List<Reference> references = new LinkedList<Reference>();
		addDigestInfosAsReferences(digestInfos, signatureFactory, references);

		/*
		 * Invoke the signature facets.
		 */
		String signatureId = "xmldsig-" + UUID.randomUUID().toString();
		List<XMLObject> objects = new LinkedList<XMLObject>();
		for (SignatureFacet signatureFacet : this.signatureFacets) {
			signatureFacet.preSign(
				signatureFactory, 
				document, 
				signatureId, 
				signingCertificateChain, 
				references, 
				objects
			);
		}

		/*
		 * ds:SignedInfo
		 */
		SignatureMethod signatureMethod = signatureFactory.newSignatureMethod(
			getSignatureMethod(digestAlgo), null
		);
		CanonicalizationMethod canonicalizationMethod = signatureFactory.newCanonicalizationMethod(
			getCanonicalizationMethod(),
			(C14NMethodParameterSpec) null
		);
		SignedInfo signedInfo = signatureFactory.newSignedInfo(
			canonicalizationMethod, signatureMethod, references
		);

		// Creamos el KeyInfo
        X509Data cerData;
        KeyInfoFactory kif = signatureFactory.getKeyInfoFactory();
        List<Object> x509Content = new ArrayList<Object>();	        
        x509Content.add(signingCertificateChain.get(0));
        cerData = kif.newX509Data(x509Content);   
        
        List<Object> content = new ArrayList<Object>();
        try {
        	content.add(kif.newKeyValue(signingCertificateChain.get(0).getPublicKey()));
        }
        catch(Throwable e) {
        	Logger.getLogger("es.gob.afirma").severe("Error creando el KeyInfo, la informacion puede resultar incompleta: " + e);
        }
        content.add(cerData);
        
        KeyInfo ki = kif.newKeyInfo(content);
		
		
		/*
		 * JSR105 ds:Signature creation
		 */
		String signatureValueId = signatureId + "-signature-value";
		javax.xml.crypto.dsig.XMLSignature xmlSignature = signatureFactory.newXMLSignature(
			signedInfo, 
			ki, // KeyInfo
			objects, 
			signatureId, 
			signatureValueId
		);
		
		/*
		 * ds:Signature Marshalling.
		 */
		DOMXMLSignature domXmlSignature = (DOMXMLSignature) xmlSignature;
		Node documentNode = document.getDocumentElement();
		if (null == documentNode) {
			/*
			 * In case of an empty DOM document.
			 */
			documentNode = document;
		}
		String dsPrefix = null;
		domXmlSignature.marshal(documentNode, dsPrefix, (DOMCryptoContext) xmlSignContext);

		/*
		 * Completion of undigested ds:References in the ds:Manifests.
		 */
		for (XMLObject object : objects) {

			List<XMLStructure> objectContentList = object.getContent();
			for (XMLStructure objectContent : objectContentList) {

				if (false == objectContent instanceof Manifest) {
					continue;
				}
				Manifest manifest = (Manifest) objectContent;
				List<Reference> manifestReferences = manifest.getReferences();
				for (Reference manifestReference : manifestReferences) {
					if (null != manifestReference.getDigestValue()) {
						continue;
					}
					DOMReference manifestDOMReference = (DOMReference) manifestReference;
					manifestDOMReference.digest(xmlSignContext);
				}
			}
		}

		/*
		 * Completion of undigested ds:References.
		 */
		List<Reference> signedInfoReferences = signedInfo.getReferences();
		for (Reference signedInfoReference : signedInfoReferences) {
			DOMReference domReference = (DOMReference) signedInfoReference;
			if (null != domReference.getDigestValue()) {
				// ds:Reference with external digest value
				continue;
			}
			domReference.digest(xmlSignContext);
		}

		
		/*
		 * Calculation of signature
		 */
		DOMSignedInfo domSignedInfo = (DOMSignedInfo) signedInfo;
		ByteArrayOutputStream dataStream = new ByteArrayOutputStream();
		domSignedInfo.canonicalize(xmlSignContext, dataStream);
		byte[] octets = dataStream.toByteArray();
		
		Signature sig = Signature.getInstance(digestAlgo + "withRSA");
		byte[] sigBytes;
		try {
			sig.initSign(signingKey);
			sig.update(octets);
			sigBytes = sig.sign();
		}
		catch(Throwable e) {
			throw new javax.xml.crypto.dsig.XMLSignatureException("Error en la firma PKCS#1 ('" + digestAlgo + "withRSA): " + e);
		}

		
		// Sacamos el pre-XML a un OutputStream
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		writeDocument(document, baos);
		
		// Ya tenemos el XML, con la firma vacia y el SignatureValue, cada uno por su lado...
		return postSign(baos.toByteArray(), signingCertificateChain, signatureId, sigBytes);
		
	}

	private void addDigestInfosAsReferences(List<DigestInfo> digestInfos,
			XMLSignatureFactory signatureFactory, List<Reference> references)
			throws NoSuchAlgorithmException,
			InvalidAlgorithmParameterException, MalformedURLException {
		if (null == digestInfos) {
			return;
		}
		for (DigestInfo digestInfo : digestInfos) {
			byte[] documentDigestValue = digestInfo.digestValue;

			DigestMethod digestMethod = signatureFactory.newDigestMethod(
					getXmlDigestAlgo(digestInfo.digestAlgo), null);

			String uri = FilenameUtils.getName(new File(digestInfo.description)
					.toURI().toURL().getFile());

			Reference reference = signatureFactory.newReference(uri,
					digestMethod, null, null, null, documentDigestValue);
			references.add(reference);
		}
	}

	private String getXmlDigestAlgo(String digestAlgo) {
		if ("SHA1".equals(digestAlgo) || "SHA-1".equals(digestAlgo) || "SHA".equals(digestAlgo)) {
			return DigestMethod.SHA1;
		}
		if ("SHA-256".equals(digestAlgo) || "SHA256".equals(digestAlgo)) {
			return DigestMethod.SHA256;
		}
		if ("SHA-512".equals(digestAlgo) || "SHA512".equals(digestAlgo)) {
			return DigestMethod.SHA512;
		}
		throw new RuntimeException("unsupported digest algo: " + digestAlgo);
	}

	private String getSignatureMethod(String digestAlgo) {
		if (null == digestAlgo) {
			throw new RuntimeException("digest algo is null");
		}
		if ("SHA-1".equals(digestAlgo) || "SHA1".equals(digestAlgo) || "SHA".equals(digestAlgo)) {
			return SignatureMethod.RSA_SHA1;
		}
		if ("SHA-256".equals(digestAlgo) || "SHA256".equals(digestAlgo)) {
			return XMLSignature.ALGO_ID_SIGNATURE_RSA_SHA256;
		}
		if ("SHA-512".equals(digestAlgo) || "SHA512".equals(digestAlgo)) {
			return XMLSignature.ALGO_ID_SIGNATURE_RSA_SHA512;
		}
		if ("SHA-384".equals(digestAlgo) || "SHA384".equals(digestAlgo)) {
			return XMLSignature.ALGO_ID_SIGNATURE_RSA_SHA384;
		}
		if ("RIPEMD160".equals(digestAlgo) || "RIPEMD-160".equals(digestAlgo)) {
			return XMLSignature.ALGO_ID_SIGNATURE_RSA_RIPEMD160;
		}
		throw new RuntimeException("unsupported sign algo: " + digestAlgo);
	}

	protected void writeDocument(Document document, OutputStream documentOutputStream) throws TransformerConfigurationException, TransformerFactoryConfigurationError, TransformerException, IOException {
		writeDocumentNoClosing(document, documentOutputStream);
		documentOutputStream.close();
	}

	protected void writeDocumentNoClosing(Document document, OutputStream documentOutputStream) throws TransformerConfigurationException, TransformerFactoryConfigurationError, TransformerException {
		// we need the XML processing initial line for OOXML
		writeDocumentNoClosing(document, documentOutputStream, false);
	}

	protected void writeDocumentNoClosing(Document document, OutputStream documentOutputStream, boolean omitXmlDeclaration) throws TransformerConfigurationException, TransformerFactoryConfigurationError, TransformerException {
		NoCloseOutputStream outputStream = new NoCloseOutputStream(documentOutputStream);
		Result result = new StreamResult(outputStream);
		Transformer xformer = TransformerFactory.newInstance().newTransformer();
		if (omitXmlDeclaration) {
			xformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		}
		Source source = new DOMSource(document);
		xformer.transform(source, result);
	}

	protected Document loadDocument(InputStream documentInputStream) throws ParserConfigurationException, SAXException, IOException {
		InputSource inputSource = new InputSource(documentInputStream);
		DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
		documentBuilderFactory.setNamespaceAware(true);
		DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
		Document document = documentBuilder.parse(inputSource);
		return document;
	}

	protected Document loadDocumentNoClose(InputStream documentInputStream)
			throws ParserConfigurationException, SAXException, IOException {
		NoCloseInputStream noCloseInputStream = new NoCloseInputStream(
				documentInputStream);
		InputSource inputSource = new InputSource(noCloseInputStream);
		DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory
				.newInstance();
		documentBuilderFactory.setNamespaceAware(true);
		DocumentBuilder documentBuilder = documentBuilderFactory
				.newDocumentBuilder();
		Document document = documentBuilder.parse(inputSource);
		return document;
	}
}
