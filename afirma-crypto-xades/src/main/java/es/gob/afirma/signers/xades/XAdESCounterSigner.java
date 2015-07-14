/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;

import net.java.xades.security.xml.XAdES.DataObjectFormat;
import net.java.xades.security.xml.XAdES.DataObjectFormatImpl;
import net.java.xades.security.xml.XAdES.ObjectIdentifier;
import net.java.xades.security.xml.XAdES.ObjectIdentifierImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;

/** Contrafirmador XAdES. */
public final class XAdESCounterSigner {

	private static final String CSURI = "http://uri.etsi.org/01903#CountersignedSignature";	//$NON-NLS-1$

	private static final Logger	LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Contrafirma firmas en formato XAdES.
	 * <p>
	 * Este m&eacute;todo contrafirma los nodos de firma indicados de un
	 * documento de firma.
	 * </p>
	 * @param sign
	 *            Documento con las firmas iniciales.
	 * @param algorithm
	 *            Algoritmo a usar para la firma.
	 *            <p>
	 *            Se aceptan los siguientes algoritmos en el par&aacute;metro
	 *            <code>algorithm</code>:
	 *            </p>
	 *            <ul>
	 *            <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
	 *            <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
	 *            <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
	 *            <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
	 *            </ul>
	 * @param targetType
	 *            Mecanismo de selecci&oacute;n de los nodos de firma que se
	 *            deben contrafirmar.
	 *            <p>
	 *            Las distintas opciones son:
	 *            </p>
	 *            <ul>
	 *            <li>Todos los nodos del &aacute;rbol de firma</li>
	 *            <li>Los nodos hoja del &aacute;rbol de firma</li>
	 *            <li>Los nodos de firma cuyas posiciones se especifican en
	 *            <code>target</code></li>
	 *            <li>Los nodos de firma realizados por los firmantes cuyo
	 *            <i>Common Name</i> se indica en <code>target</code></li>
	 *            </ul>
	 *            <p>
	 *            Cada uno de estos tipos se define en
	 *            {@link es.gob.afirma.core.signers.CounterSignTarget}.
	 * @param targets Listado de nodos o firmantes que se deben contrafirmar
	 *                seg&uacute;n el {@code targetType} seleccionado.
	 * @param key Clave privada a usar para firmar.
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @return Contrafirma en formato XAdES.
	 * @throws AOException
	 *             Cuando ocurre cualquier problema durante el proceso
	 */
	public static byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties xParams) throws AOException {

		final Properties extraParams = xParams != null ? xParams
				: new Properties();

		String encoding = extraParams.getProperty("encoding"); //$NON-NLS-1$
		if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
			encoding = XMLConstants.BASE64_ENCODING;
		}

		if (sign == null) {
			throw new IllegalArgumentException(
					"El objeto de firma no puede ser nulo"); //$NON-NLS-1$
		}

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException(
					"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// nueva instancia de DocumentBuilderFactory que permita espacio de
		// nombres (necesario para XML)
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		// flag que indica si el documento tiene una firma simple o esta
		// cofirmado
		// por defecto se considera que es un documento cofirmado
		boolean esFirmaSimple = false;

		// se carga el documento XML y su raiz
		final Map<String, String> originalXMLProperties = new Hashtable<String, String>();
		Element root;
		Document doc;
		try {
			doc = dbf.newDocumentBuilder()
					.parse(new ByteArrayInputStream(sign));

			if (encoding == null) {
				encoding = doc.getXmlEncoding();
			}

			// Ademas del encoding, sacamos otros datos del doc XML original.
			// Hacemos la comprobacion del base64 por si se establecido desde
			// fuera
			if (encoding != null
					&& !XMLConstants.BASE64_ENCODING.equals(encoding)) {
				originalXMLProperties.put(OutputKeys.ENCODING, encoding);
			}
			String tmpXmlProp = doc.getXmlVersion();
			if (tmpXmlProp != null) {
				originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
			}
			final DocumentType dt = doc.getDoctype();
			if (dt != null) {
				tmpXmlProp = dt.getSystemId();
				if (tmpXmlProp != null) {
					originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM,
							tmpXmlProp);
				}
			}

			root = doc.getDocumentElement();

			// si no es un documento cofirma se anade temporalmente el nodo raiz
			// AFIRMA
			// para que las operaciones de contrafirma funcionen correctamente
			if (root.getNodeName().equals(AOXAdESSigner.SIGNATURE_NODE_NAME)) {
				esFirmaSimple = true;
				doc = AOXAdESSigner.insertarNodoAfirma(doc);
				root = doc.getDocumentElement();
			}
		} catch (final Exception e) {
			throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
		}

		try {
			if (targetType == CounterSignTarget.TREE) {
				XAdESCounterSigner.countersignTree(
					root,
					key,
					certChain,
					extraParams,
				    algorithm,
				    doc
			    );
			}
			else if (targetType == CounterSignTarget.LEAFS) {
				XAdESCounterSigner.countersignLeafs(
					root,
					key,
					certChain,
					extraParams,
					algorithm,
					doc
				);
			}
			else if (targetType == CounterSignTarget.NODES) {
				XAdESCounterSigner.countersignNodes(
					root,
					targets,
					key,
					certChain,
					extraParams,
					algorithm,
					doc
				);
			}
			else if (targetType == CounterSignTarget.SIGNERS) {
				XAdESCounterSigner.countersignSigners(
					root,
					targets,
					key,
					certChain,
					extraParams,
					algorithm,
					doc
				);
			}
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la contrafirma", e); //$NON-NLS-1$
		}

		// si el documento recibido no estaba cofirmado se elimina el nodo raiz
		// temporal AFIRMA
		// y se vuelve a dejar como raiz el nodo Signature original
		if (esFirmaSimple) {
			try {
				final Document newdoc = dbf.newDocumentBuilder().newDocument();
				newdoc.appendChild(
					newdoc.adoptNode(
						doc.getElementsByTagNameNS(
							XMLConstants.DSIGNNS,
							AOXAdESSigner.SIGNATURE_TAG
						).item(0)
					)
				);
				doc = newdoc;
			}
			catch (final Exception e) {
				XAdESCounterSigner.LOGGER
						.info("No se ha eliminado el nodo padre '<AFIRMA>': " + e); //$NON-NLS-1$
			}
		}

		return Utils.writeXML(doc.getDocumentElement(), originalXMLProperties,
				null, null);
	}

	/** Realiza la contrafirma de todos los nodos hoja del &aacute;rbol.
	 * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignLeafs(final Element root,
			                             final PrivateKey key,
			                             final Certificate[] certChain,
			                             final Properties extraParams,
			                             final String algorithm,
			                             final Document doc) throws AOException {

		// obtiene todas las firmas
		final NodeList signatures = root.getElementsByTagNameNS(
				XMLConstants.DSIGNNS, AOXAdESSigner.SIGNATURE_TAG);
		int numSignatures = signatures.getLength();

		// comprueba cuales son hojas
		try {
			for (int i = 0; i < numSignatures; i++) {
				final Element signature = (Element) signatures.item(i);
				final int numCounterSigns = signature.getElementsByTagNameNS(
						XMLConstants.DSIGNNS, AOXAdESSigner.SIGNATURE_TAG)
						.getLength();

				// y crea sus contrafirmas
				if (numCounterSigns == 0) {
					XAdESCounterSigner.cs(signature, key, certChain, extraParams, algorithm, doc);
					numSignatures++;
					i++;
				}
			}
		} catch (final Exception e) {
			throw new AOException(
					"No se ha podido realizar la contrafirma de hojas", e); //$NON-NLS-1$
		}
	}

	/** Realiza la contrafirma de los nodos indicados en el par&aacute;metro
	 * <code>targets</code>.
	 * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
	 * @param tgts Array con las posiciones de los nodos a contrafirmar
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignNodes(final Element root,
			                             final Object[] tgts,
			                             final PrivateKey key,
			                             final Certificate[] certChain,
			                             final Properties extraParams,
			                             final String algorithm,
			                             final Document doc) throws AOException {

		// descarta las posiciones que esten repetidas
		final List<Integer> targetsList = new ArrayList<Integer>();
		for (int i = 0; i < tgts.length; i++) {
			if (!targetsList.contains(tgts[i])) {
				targetsList.add((Integer) tgts[i]);
			}
		}
		final Object[] targets = targetsList.toArray();

		// obtiene todas las firmas
		final NodeList signatures = root.getElementsByTagNameNS(
				XMLConstants.DSIGNNS, AOXAdESSigner.SIGNATURE_TAG);

		// obtiene los nodos indicados en targets
		final Element[] nodes = new Element[targets.length];
		try {
			for (int i = 0; i < targets.length; i++) {
				nodes[i] = (Element) signatures.item(((Integer) targets[i]).intValue());
				if (nodes[i] == null) {
					throw new AOException("Posicion de nodo no valida."); //$NON-NLS-1$
				}
			}
		}
		catch (final ClassCastException e) {
			throw new AOException("Valor de nodo no valido", e); //$NON-NLS-1$
		}

		// y crea sus contrafirmas
		try {
			for (final Element node : nodes) {
				XAdESCounterSigner.cs(node, key, certChain, extraParams, algorithm, doc);
			}
		} catch (final Exception e) {
			throw new AOException(
					"No se ha podido realizar la contrafirma de nodos", e); //$NON-NLS-1$
		}
	}

	/** Realiza la contrafirma de los firmantes indicados en el par&aacute;metro
	 * targets.
	 * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
	 * @param targets Array con el nombre de los firmantes de los nodos a contrafirmar
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignSigners(final Element root,
			                               final Object[] targets,
			                               final PrivateKey key,
			                               final Certificate[] certChain,
			                               final Properties extraParams,
			                               final String algorithm,
			                               final Document doc) throws AOException {

		// obtiene todas las firmas
		final NodeList signatures = root.getElementsByTagNameNS(
				XMLConstants.DSIGNNS, AOXAdESSigner.SIGNATURE_TAG);

		final List<Object> signers = Arrays.asList(targets);
		final List<Element> nodes = new ArrayList<Element>();

		// obtiene los nodos de los firmantes indicados en targets
		for (int i = 0; i < signatures.getLength(); i++) {
			final Element node = (Element) signatures.item(i);
			if (signers.contains(AOUtil.getCN(Utils.getCertificate(node
					.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Certificate") //$NON-NLS-1$
					.item(0))))) {
				nodes.add(node);
			}
		}

		// y crea sus contrafirmas
		final Iterator<Element> i = nodes.iterator();
		while (i.hasNext()) {
			XAdESCounterSigner.cs(i.next(), key, certChain, extraParams, algorithm, doc);
		}
	}

	/** Realiza la contrafirma de todos los nodos del &aacute;rbol.
	 * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignTree(final Element root,
			                            final PrivateKey key,
			                            final Certificate[] certChain,
			                            final Properties extraParams,
			                            final String algorithm,
			                            final Document doc) throws AOException {

		// obtiene todas las firmas
		final NodeList signatures = root.getElementsByTagNameNS(
				XMLConstants.DSIGNNS, AOXAdESSigner.SIGNATURE_TAG);
		final int numSignatures = signatures.getLength();

		final Element[] nodes = new Element[numSignatures];
		for (int i = 0; i < numSignatures; i++) {
			nodes[i] = (Element) signatures.item(i);
		}

		// y crea sus contrafirmas
		try {
			for (int i = 0; i < numSignatures; i++) {
				XAdESCounterSigner.cs(
					nodes[i],
					key,
					certChain,
					extraParams,
					algorithm,
					doc
				);
			}
		} catch (final Exception e) {
			throw new AOException(
					"No se ha podido realizar la contrafirma del arbol", e); //$NON-NLS-1$
		}
	}

	/** Realiza la contrafirma de la firma pasada por par&aacute;metro
	 * @param signature Elemento con el nodo de la firma a contrafirmar
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void cs(final Element signature,
			               final PrivateKey key,
			               final Certificate[] certChain,
			               final Properties xParams,
			               final String algorithm,
			               final Document doc) throws AOException {

	    final String sigPrefix = Utils.guessXAdESNamespacePrefix(signature);

		if (doc == null) {
			throw new IllegalArgumentException(
				"El documento DOM no puede ser nulo" //$NON-NLS-1$
			);
		}

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final String digestMethodAlgorithm = extraParams.getProperty(
				"referencesDigestMethod", AOXAdESSigner.DIGEST_METHOD); //$NON-NLS-1$
		final String canonicalizationAlgorithm = extraParams.getProperty(
				"canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$
		final String xadesNamespace = extraParams.getProperty(
				"xadesNamespace", AOXAdESSigner.XADESNS); //$NON-NLS-1$
		final String signedPropertiesTypeUrl = extraParams.getProperty(
				"signedPropertiesTypeUrl", AOXAdESSigner.XADES_SIGNED_PROPERTIES_TYPE); //$NON-NLS-1$

		// crea un nodo CounterSignature
		final Element counterSignature = doc.createElement(
				sigPrefix + ":CounterSignature"); //$NON-NLS-1$

		// recupera o crea un nodo UnsignedSignatureProperties
		final NodeList usp = signature.getElementsByTagNameNS(
			"*", "UnsignedSignatureProperties" //$NON-NLS-1$ //$NON-NLS-2$
		);

		Element unsignedSignatureProperties;

		if (usp.getLength() == 0) {
			unsignedSignatureProperties = doc.createElement(
				sigPrefix + ":UnsignedSignatureProperties" //$NON-NLS-1$
			);
		}
		else {
			unsignedSignatureProperties = (Element) usp.item(0);
		}

		unsignedSignatureProperties.appendChild(counterSignature);

		// recupera o crea un nodo UnsignedProperties
		final NodeList up = signature.getElementsByTagNameNS(
				"*", "UnsignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$
		final Element unsignedProperties;
		if (up.getLength() == 0) {
			unsignedProperties = doc.createElement(
				sigPrefix + ":UnsignedProperties" //$NON-NLS-1$
			);
		}
		else {
			unsignedProperties = (Element) up.item(0);
		}

		unsignedProperties.appendChild(unsignedSignatureProperties);

		// inserta el nuevo nodo en QualifyingProperties
		final Node qualifyingProperties = signature.getElementsByTagNameNS(
				"*", "QualifyingProperties").item(0); //$NON-NLS-1$ //$NON-NLS-2$
		qualifyingProperties.appendChild(unsignedProperties);

		// obtiene el nodo SignatureValue
		final Element signatureValue = (Element) signature
				.getElementsByTagNameNS(XMLConstants.DSIGNNS, "SignatureValue").item(0); //$NON-NLS-1$

		// crea la referencia a la firma que se contrafirma
		final List<Reference> referenceList = new ArrayList<Reference>();
		final XMLSignatureFactory fac = Utils.getDOMFactory();

		final DigestMethod digestMethod;
		try {
			digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		final String referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$

		try {
			// Transformada para la canonicalizacion inclusiva con comentarios
			final List<Transform> transformList = new ArrayList<Transform>();
			transformList.add(fac.newTransform(canonicalizationAlgorithm,
					(TransformParameterSpec) null));

			// Aunque el metodo utilizado para generar las contrafirmas hacen
			// que no sea necesario indicar el tipo de la referencia, lo agregamos por si resultase
			// de utilidad
			referenceList.add(
				fac.newReference(
					"#" + signatureValue.getAttribute("Id"), //$NON-NLS-1$ //$NON-NLS-2$
					digestMethod,
					transformList,
					XAdESCounterSigner.CSURI,
					referenceId
				)
			);
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
		}

		// nueva instancia XADES_EPES del nodo a contrafirmar
		final XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(
                XAdES.EPES,
				xadesNamespace,
				sigPrefix,
				AOXAdESSigner.XML_SIGNATURE_PREFIX,
				digestMethodAlgorithm,
				counterSignature.getOwnerDocument(),
				counterSignature
		);

		// establece el certificado
		final X509Certificate cert = (X509Certificate) certChain[0];
		xades.setSigningCertificate(cert);

		XAdESCommonMetadataUtil.addCommonMetadata(xades, extraParams);

		// DataObjectFormats
		final ObjectIdentifier objectIdentifier = new ObjectIdentifierImpl("OIDAsURN", "urn:oid:1.2.840.10003.5.109.10", null, new ArrayList<String>(0)); //$NON-NLS-1$ //$NON-NLS-2$
		final DataObjectFormat dataObjectFormat = new DataObjectFormatImpl(null, objectIdentifier, "text/xml", doc.getInputEncoding(), "#" + referenceId); //$NON-NLS-1$ //$NON-NLS-2$
		final List<DataObjectFormat> dataObjectFormats = new ArrayList<DataObjectFormat>();
		dataObjectFormats.add(dataObjectFormat);

		xades.setDataObjectFormats(dataObjectFormats);

		// crea la firma
		final AOXMLAdvancedSignature xmlSignature = XAdESUtil.getXmlAdvancedSignature(
			xades,
			signedPropertiesTypeUrl,
			digestMethodAlgorithm,
			canonicalizationAlgorithm
		);

		try {
			final boolean onlySignningCert = Boolean.parseBoolean(
				extraParams.getProperty("includeOnlySignningCertificate", Boolean.FALSE.toString()) //$NON-NLS-1$
			);
			if (onlySignningCert) {
				xmlSignature.sign(
					(X509Certificate) certChain[0],
					key,
					XMLConstants.SIGN_ALGOS_URI.get(algorithm),
					referenceList,
					"Signature-" + UUID.randomUUID().toString() //$NON-NLS-1$
				);
			}
			else {
				xmlSignature.sign(
					Arrays.asList(certChain),
					key,
					XMLConstants.SIGN_ALGOS_URI.get(algorithm),
					referenceList,
					"Signature-" + UUID.randomUUID().toString(), //$NON-NLS-1$
					Boolean.parseBoolean(extraParams.getProperty("addKeyInfoKeyValue", Boolean.TRUE.toString())), //$NON-NLS-1$
					Boolean.parseBoolean(extraParams.getProperty("addKeyInfoKeyName", Boolean.FALSE.toString())) //$NON-NLS-1$
				);
			}
		}
		catch (final NoSuchAlgorithmException e) {
			throw new UnsupportedOperationException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'", e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
		}
	}

	private XAdESCounterSigner() {
		// No permitimos la instanciacion
	}

}
