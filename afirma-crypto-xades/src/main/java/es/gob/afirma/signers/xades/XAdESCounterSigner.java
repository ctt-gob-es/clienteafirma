/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.SigningLTSException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormat;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormatImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifier;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifierImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdESBase;

/** Contrafirmador XAdES. */
public final class XAdESCounterSigner {

	private static final String CSURI = "http://uri.etsi.org/01903#CountersignedSignature";	//$NON-NLS-1$

	private static final Logger	LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Contrafirma firmas en formato XAdES.
	 * <p>
	 * Contrafirma los nodos de firma indicados de un
	 * documento de firma.
	 * </p>
	 * @param sign Documento con las firmas iniciales.
	 * @param algorithm
	 *            Algoritmo a usar para la firma.
	 * @param targetType
	 *            Mecanismo de selecci&oacute;n de los nodos de firma que se
	 *            deben contrafirmar.
	 *            <p>
	 *            Las distintas opciones son:
	 *            </p>
	 *            <ul>
	 *             <li>Todos los nodos del &aacute;rbol de firma</li>
	 *             <li>Los nodos hoja del &aacute;rbol de firma</li>
	 *             <li>Los nodos de firma cuyas posiciones se especifican en
	 *              <code>target</code></li>
	 *             <li>Los nodos de firma realizados por los firmantes cuyo
	 *              <i>Common Name</i> se indica en <code>target</code></li>
	 *            </ul>
	 *            <p>
	 *            Cada uno de estos tipos se define en
	 *            {@link es.gob.afirma.core.signers.CounterSignTarget}.
	 * @param targets Listado de nodos o firmantes que se deben contrafirmar
	 *                seg&uacute;n el {@code targetType} seleccionado.
	 * @param key Clave privada a usar para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @return Contrafirma en formato XAdES.
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso.
	 */
	public static byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties xParams) throws AOException {
		return countersign(sign, algorithm, targetType, targets, key, certChain, xParams, null);
	}

	/**
	 * Contrafirma firmas en formato XAdES.
	 * <p>
	 * Contrafirma los nodos de firma indicados de un
	 * documento de firma.
	 * </p>
	 * @param sign Documento con las firmas iniciales.
	 * @param algorithm
	 *            Algoritmo a usar para la firma.
	 * @param targetType
	 *            Mecanismo de selecci&oacute;n de los nodos de firma que se
	 *            deben contrafirmar.
	 *            <p>
	 *            Las distintas opciones son:
	 *            </p>
	 *            <ul>
	 *             <li>Todos los nodos del &aacute;rbol de firma</li>
	 *             <li>Los nodos hoja del &aacute;rbol de firma</li>
	 *             <li>Los nodos de firma cuyas posiciones se especifican en
	 *              <code>target</code></li>
	 *             <li>Los nodos de firma realizados por los firmantes cuyo
	 *              <i>Common Name</i> se indica en <code>target</code></li>
	 *            </ul>
	 *            <p>
	 *            Cada uno de estos tipos se define en
	 *            {@link es.gob.afirma.core.signers.CounterSignTarget}.
	 * @param targets Listado de nodos o firmantes que se deben contrafirmar
	 *                seg&uacute;n el {@code targetType} seleccionado.
	 * @param key Clave privada a usar para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @param uriDereferencer Derreferenciador a medida.
	 * @return Contrafirma en formato XAdES.
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso.
	 */
	public static byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties xParams,
			                  final URIDereferencer uriDereferencer) throws AOException {

		Document signDocument;
		try {
			signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido cargar el documento de firmas", e); //$NON-NLS-1$
		}

		return countersign(signDocument, algorithm, targetType, targets, key, certChain, xParams, uriDereferencer);
	}

	/**
	 * Contrafirma firmas en formato XAdES.
	 * <p>
	 * Contrafirma los nodos de firma indicados de un
	 * documento de firma.
	 * </p>
	 * @param signDocument Documento XML con las firmas iniciales.
	 * @param signAlgorithm
	 *            Algoritmo a usar para la firma.
	 * @param targetType
	 *            Mecanismo de selecci&oacute;n de los nodos de firma que se
	 *            deben contrafirmar.
	 *            <p>
	 *            Las distintas opciones son:
	 *            </p>
	 *            <ul>
	 *             <li>Todos los nodos del &aacute;rbol de firma</li>
	 *             <li>Los nodos hoja del &aacute;rbol de firma</li>
	 *             <li>Los nodos de firma cuyas posiciones se especifican en
	 *              <code>target</code></li>
	 *             <li>Los nodos de firma realizados por los firmantes cuyo
	 *              <i>Common Name</i> se indica en <code>target</code></li>
	 *            </ul>
	 *            <p>
	 *            Cada uno de estos tipos se define en
	 *            {@link es.gob.afirma.core.signers.CounterSignTarget}.
	 * @param targets Listado de nodos o firmantes que se deben contrafirmar
	 *                seg&uacute;n el {@code targetType} seleccionado.
	 * @param key Clave privada a usar para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @return Contrafirma en formato XAdES.
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso.
	 */
	public static byte[] countersign(final Document signDocument,
			final String signAlgorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties xParams) throws AOException {
		return countersign(signDocument, signAlgorithm, targetType, targets, key, certChain, xParams, null);
	}

	/**
	 * Contrafirma firma
	 *
	 * s en formato XAdES.
	 * <p>
	 * Contrafirma los nodos de firma indicados de un
	 * documento de firma.
	 * </p>
	 * @param signDocument Documento XML con las firmas iniciales.
	 * @param signAlgorithm
	 *            Algoritmo a usar para la firma.
	 * @param targetType
	 *            Mecanismo de selecci&oacute;n de los nodos de firma que se
	 *            deben contrafirmar.
	 *            <p>
	 *            Las distintas opciones son:
	 *            </p>
	 *            <ul>
	 *             <li>Todos los nodos del &aacute;rbol de firma</li>
	 *             <li>Los nodos hoja del &aacute;rbol de firma</li>
	 *             <li>Los nodos de firma cuyas posiciones se especifican en
	 *              <code>target</code></li>
	 *             <li>Los nodos de firma realizados por los firmantes cuyo
	 *              <i>Common Name</i> se indica en <code>target</code></li>
	 *            </ul>
	 *            <p>
	 *            Cada uno de estos tipos se define en
	 *            {@link es.gob.afirma.core.signers.CounterSignTarget}.
	 * @param targets Listado de nodos o firmantes que se deben contrafirmar
	 *                seg&uacute;n el {@code targetType} seleccionado.
	 * @param key Clave privada a usar para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @param uriDereferencer Derreferenciador a medida.
	 * @return Contrafirma en formato XAdES.
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso.
	 */
	public static byte[] countersign(final Document signDocument,
			final String signAlgorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties xParams,
			final URIDereferencer uriDereferencer) throws AOException {

		if (signDocument == null) {
			throw new IllegalArgumentException(
					"El objeto de firma no puede ser nulo"); //$NON-NLS-1$
		}

		final String algorithm = signAlgorithm != null ? signAlgorithm : AOSignConstants.DEFAULT_SIGN_ALGO;
		final Properties extraParams = xParams != null ? xParams : new Properties();

		checkParams(algorithm, extraParams);

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new IllegalArgumentException(
					"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'" //$NON-NLS-1$ //$NON-NLS-2$
					);
		}

		final String outputXmlEncoding = extraParams.getProperty(XAdESExtraParams.OUTPUT_XML_ENCODING);

		// Flag que indica si el documento tiene una firma simple o esta cofirmado
		// por defecto se considera que es un documento cofirmado
		boolean esFirmaSimple = false;

		final Map<String, String> originalXMLProperties;

		// Se carga el documento XML y su raiz

		Document doc = signDocument;
		Element root = signDocument.getDocumentElement();
		try {
			originalXMLProperties = XAdESUtil.getOriginalXMLProperties(signDocument, outputXmlEncoding);

			// Si el nodo principal es la firma, se anade temporalmente el nodo
			// raiz AFIRMA para que las operaciones de contrafirma funcionen
			// correctamente
			if (root.getLocalName().equals(XMLConstants.TAG_SIGNATURE)) {
				esFirmaSimple = true;
				doc = AOXAdESSigner.insertarNodoAfirma(doc);
				root = doc.getDocumentElement();
			}
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido realizar la contrafirma: " + e, e); //$NON-NLS-1$
		}

		// Obtenemos todas las firmas del documento
		final NodeList signaturesList = root.getElementsByTagNameNS(
				XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);

		// Comprobamos que no haya firmas de archivo, salvo que nos indiquen que debe firmarse
		// incluso en ese caso
		final String allowSignLts = extraParams.getProperty(XAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES);
		if (allowSignLts == null || !Boolean.parseBoolean(allowSignLts)) {
			try {
				XAdESUtil.checkArchiveSignatures(signaturesList);
			}
			catch (final SigningLTSException e) {
				// Si se indico expresamente que no se debia permitir la contrafirma de
				// firmas de archivo, se lanza una excepcion bloqueando la ejecucion.
				// Si no, se informa debidamente para que se consulte al usuario
				if (allowSignLts != null) {
					throw new AOException(e.getMessage());
				}
				throw new SigningLTSException("La contrafirma de firmas de archivo podria invalidar el sello de archivo si se aplicase a contrafirmas internas", e, true); //$NON-NLS-1$
			}
		}

		// Contrafirmamos
		try {
			if (targetType == CounterSignTarget.TREE) {
				countersignTree(
						signaturesList,
						key,
						certChain,
						extraParams,
						algorithm,
						doc,
						uriDereferencer
						);
			}
			else if (targetType == CounterSignTarget.LEAFS) {
				countersignLeafs(
						signaturesList,
						key,
						certChain,
						extraParams,
						algorithm,
						doc,
						uriDereferencer
						);
			}
			else if (targetType == CounterSignTarget.NODES) {
				countersignNodes(
						signaturesList,
						targets,
						key,
						certChain,
						extraParams,
						algorithm,
						doc,
						uriDereferencer
						);
			}
			else if (targetType == CounterSignTarget.SIGNERS) {
				countersignSigners(
						signaturesList,
						targets,
						key,
						certChain,
						extraParams,
						algorithm,
						doc,
						uriDereferencer
						);
			}
		}
		catch (final AOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la contrafirma", e); //$NON-NLS-1$
		}

		// si el documento recibido no estaba cofirmado se elimina el nodo raiz
		// temporal AFIRMA
		// y se vuelve a dejar como raiz el nodo Signature original
		if (esFirmaSimple) {
			try {
				final Document newdoc = Utils.getNewDocumentBuilder().newDocument();
				newdoc.appendChild(
						newdoc.adoptNode(
								doc.getElementsByTagNameNS(
										XMLConstants.DSIGNNS,
										XMLConstants.TAG_SIGNATURE
										).item(0)
								)
						);
				doc = newdoc;
			}
			catch (final Exception e) {
				LOGGER.info(
						"No se ha eliminado el nodo padre '<AFIRMA>': " + e //$NON-NLS-1$
						);
			}
		}

		return Utils.writeXML(
				doc.getDocumentElement(),
				originalXMLProperties,
				null,
				null
				);
	}

	/** Realiza la contrafirma de todos los nodos hoja del &aacute;rbol.
	 * @param signaturesList Listado completos de firmas del documento.
	 * @param key Clave privada para la firma.
	 * @param certChain Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma.
	 * @param algorithm Algoritmo de firma XML.
	 * @param doc Documento DOM XML a contrafirmar.
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignLeafs(final NodeList signaturesList,
			                             final PrivateKey key,
			                             final Certificate[] certChain,
			                             final Properties extraParams,
			                             final String algorithm,
			                             final Document doc,
			                             final URIDereferencer uriDereferencer) throws AOException {

		int numSignatures = signaturesList.getLength();

		// comprueba cuales son hojas
		try {
			for (int i = 0; i < numSignatures; i++) {
				final Element signature = (Element) signaturesList.item(i);
				final int numCounterSigns = signature.getElementsByTagNameNS(
						XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE)
						.getLength();

				// y crea sus contrafirmas
				if (numCounterSigns == 0) {
					cs(signature, key, certChain, extraParams, algorithm, doc, uriDereferencer);
					numSignatures++;
					i++;
				}
			}
		} catch (final AOException e) {
			throw e;
		} catch (final Exception e) {
			throw new AOException(
					"No se ha podido realizar la contrafirma de hojas", e); //$NON-NLS-1$
		}
	}

	/** Realiza la contrafirma de los nodos indicados en el par&aacute;metro
	 * <code>targets</code>.
	 * @param signaturesList Listado completos de firmas del documento.
	 * @param tgts Array con las posiciones de los nodos a contrafirmar
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignNodes(final NodeList signaturesList,
			                             final Object[] tgts,
			                             final PrivateKey key,
			                             final Certificate[] certChain,
			                             final Properties extraParams,
			                             final String algorithm,
			                             final Document doc,
			                             final URIDereferencer uriDereferencer) throws AOException {

		// descarta las posiciones que esten repetidas
		final List<Integer> targetsList = new ArrayList<>();
		for (int i = 0; i < tgts.length; i++) {
			if (!targetsList.contains(tgts[i])) {
				targetsList.add((Integer) tgts[i]);
			}
		}
		final Object[] targets = targetsList.toArray();

		// obtiene los nodos indicados en targets
		final Element[] nodes = new Element[targets.length];
		try {
			for (int i = 0; i < targets.length; i++) {
				nodes[i] = (Element) signaturesList.item(((Integer) targets[i]).intValue());
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
				cs(node, key, certChain, extraParams, algorithm, doc, uriDereferencer);
			}
		} catch (final AOException e) {
			throw e;
		} catch (final Exception e) {
			throw new AOException(
					"No se ha podido realizar la contrafirma de nodos", e); //$NON-NLS-1$
		}
	}

	/** Realiza la contrafirma de los firmantes indicados en el par&aacute;metro
	 * targets.
	 * @param signaturesList Listado completos de firmas del documento.
	 * @param targets Array con el nombre de los firmantes de los nodos a contrafirmar
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignSigners(final NodeList signaturesList,
			                               final Object[] targets,
			                               final PrivateKey key,
			                               final Certificate[] certChain,
			                               final Properties extraParams,
			                               final String algorithm,
			                               final Document doc,
			                               final URIDereferencer uriDereferencer) throws AOException {

		final List<Object> signers = Arrays.asList(targets);
		final List<Element> nodes = new ArrayList<>();

		// obtiene los nodos de los firmantes indicados en targets
		for (int i = 0; i < signaturesList.getLength(); i++) {
			final Element node = (Element) signaturesList.item(i);
			if (signers.contains(AOUtil.getCN(Utils.getCertificate(node
					.getElementsByTagNameNS(XMLConstants.DSIGNNS, XAdESConstants.TAG_X509_CERTIFICATE)
					.item(0))))) {
				nodes.add(node);
			}
		}

		// y crea sus contrafirmas
		final Iterator<Element> i = nodes.iterator();
		while (i.hasNext()) {
			cs(i.next(), key, certChain, extraParams, algorithm, doc, uriDereferencer);
		}
	}

	/** Realiza la contrafirma de todos los nodos del &aacute;rbol.
	 * @param signaturesList Listado completos de firmas del documento.
	 * @param key Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param extraParams Par&aacute;metros adicionales de configuraci&oacute;n de la firma
	 * @param algorithm Algoritmo de firma XML
	 * @param doc Documento DOM XML a contrafirmar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static void countersignTree(final NodeList signaturesList,
			                            final PrivateKey key,
			                            final Certificate[] certChain,
			                            final Properties extraParams,
			                            final String algorithm,
			                            final Document doc,
			                            final URIDereferencer uriDereferencer) throws AOException {

		final int numSignatures = signaturesList.getLength();

		final Element[] nodes = new Element[numSignatures];
		for (int i = 0; i < numSignatures; i++) {
			nodes[i] = (Element) signaturesList.item(i);
		}

		// y crea sus contrafirmas
		try {
			for (int i = 0; i < numSignatures; i++) {
				cs(
					nodes[i],
					key,
					certChain,
					extraParams,
					algorithm,
					doc,
					uriDereferencer
				);
			}
		} catch (final AOException e) {
			throw e;
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
			               final Document doc,
			               final URIDereferencer uriDereferencer) throws AOException {

		if (doc == null) {
			throw new IllegalArgumentException(
				"El documento DOM no puede ser nulo" //$NON-NLS-1$
			);
		}

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final String digestMethodAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.REFERENCES_DIGEST_METHOD, XAdESConstants.DEFAULT_DIGEST_METHOD);
		final String canonicalizationAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.CANONICALIZATION_ALGORITHM, CanonicalizationMethod.INCLUSIVE);

		// Comprobamos si se ha indicado validar el PKCS#1 generado (por defecto, si)
		final boolean validatePkcs1 = Boolean.parseBoolean(extraParams.getProperty(
				XAdESExtraParams.INTERNAL_VALIDATE_PKCS1, Boolean.TRUE.toString()));

		// Identificamos el prefijo y el espacio de nombre que debemos usar para incluir la contrafirma
		final Element signedPropertiesReference = XAdESUtil.getSignedPropertiesReference(signature);
		final Element signedPropertiesElement = XAdESUtil.getSignedPropertiesElement(signature, signedPropertiesReference);
		if (signedPropertiesElement == null) {
			throw new AOException("No se han encontrado los atributos firmados de la firma original"); //$NON-NLS-1$
		}

		String xadesPrefix = signedPropertiesElement.getPrefix();
		final String xadesNamespace = signedPropertiesElement.getNamespaceURI();
		final String signedPropertiesTypeUrl = signedPropertiesReference.getAttribute("Type"); //$NON-NLS-1$

		// Buscamos un nodo UnsignedProperties
		final NodeList up = signature.getElementsByTagNameNS(
				"*", "UnsignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$

		// Usamos el nodo encontrado y su prefijo o creamos uno si no existia
		final Element unsignedProperties;
		if (up.getLength() == 0) {
			unsignedProperties = doc.createElement(
				addNSPrefix(xadesPrefix, "UnsignedProperties") //$NON-NLS-1$
			);
		}
		else {
			unsignedProperties = (Element) up.item(0);
			xadesPrefix = unsignedProperties.getPrefix();
		}

		// Buscamos un nodo UnsignedSignatureProperties
		final NodeList usp = unsignedProperties.getElementsByTagNameNS(
			"*", "UnsignedSignatureProperties" //$NON-NLS-1$ //$NON-NLS-2$
		);

		// Usamos el nodo encontrado y su prefijo o creamos uno si no existia
		Element unsignedSignatureProperties;
		if (usp.getLength() == 0) {
			unsignedSignatureProperties = doc.createElement(
					addNSPrefix(xadesPrefix, "UnsignedSignatureProperties") //$NON-NLS-1$
			);
		}
		else {
			unsignedSignatureProperties = (Element) usp.item(0);
			xadesPrefix = unsignedSignatureProperties.getPrefix();
		}

		// Crea un nodo CounterSignature
		final Element counterSignature = doc.createElement(
				addNSPrefix(xadesPrefix, "CounterSignature")); //$NON-NLS-1$

		// Apilamos los nodos para crear bajo ellos la contrafirma
		unsignedSignatureProperties.appendChild(counterSignature);
		unsignedProperties.appendChild(unsignedSignatureProperties);

		// inserta el nuevo nodo en QualifyingProperties
		final Node qualifyingProperties = signature.getElementsByTagNameNS(
				"*", "QualifyingProperties").item(0); //$NON-NLS-1$ //$NON-NLS-2$
		qualifyingProperties.appendChild(unsignedProperties);

		// obtiene el nodo SignatureValue
		final Element signatureValue = (Element) signature.getElementsByTagNameNS(
			XMLConstants.DSIGNNS,
			"SignatureValue" //$NON-NLS-1$
		).item(0);

		// crea la referencia a la firma que se contrafirma
		final List<Reference> referenceList = new ArrayList<>();
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
			final List<Transform> transformList = new ArrayList<>();
			transformList.add(fac.newTransform(canonicalizationAlgorithm,
					(TransformParameterSpec) null));

			// Segun la version de XAdES que se utilice se debera indicar o no el tipo de la referencia al
			// SignatureValue de la firma
			final String referenceType = needCounterSignatureReferenceType(xadesNamespace) ? CSURI : null;

			referenceList.add(
				fac.newReference(
					"#" + signatureValue.getAttribute(XAdESConstants.ID_IDENTIFIER), //$NON-NLS-1$
					digestMethod,
					transformList,
					referenceType,
					referenceId
				)
			);
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido realizar la contrafirma", e); //$NON-NLS-1$
		}

		// Comprobamos el perfil de firma que hay que aplicar
		String profile = extraParams.getProperty(
				XAdESExtraParams.PROFILE, AOSignConstants.SIGN_PROFILE_ADVANCED);

		// Si se solicito realizar una contrafirma XAdES baseline, pero el
		// espacio de nombres de firma original no lo soporta, se ignora el
		// perfil
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equals(profile) &&
				!XAdESUtil.isBaselineCompatible(xadesNamespace)) {
			LOGGER.warning("La firma original utiliza un espacio de nombres no compatible con baseline (" //$NON-NLS-1$
					+ xadesNamespace + "). No se generara una firma baseline"); //$NON-NLS-1$
			profile = AOSignConstants.SIGN_PROFILE_ADVANCED;
		}

		// Instancia XAdES
		final XAdESBase xades = XAdESUtil.newInstance(
                profile,
				xadesNamespace,
				xadesPrefix,
				XAdESConstants.DEFAULT_XML_SIGNATURE_PREFIX,
				digestMethodAlgorithm,
				counterSignature.getOwnerDocument(),
				counterSignature,
				(X509Certificate) certChain[0]
		);

		// Metadatos de firma
		XAdESCommonMetadataUtil.addCommonMetadata(xades, extraParams);

		// Agregamos el DataObjectFormats, salvo en firma baseline, en las que no
		// se debe agregar a las contrafirmas: ETSI EN 319 132-1 V1.1.1 (2016-04)
		// Apartado 6.3, aclaracion k)
		if (!profile.equalsIgnoreCase(AOSignConstants.SIGN_PROFILE_BASELINE)) {
			final ObjectIdentifier objectIdentifier = new ObjectIdentifierImpl("OIDAsURN", "urn:oid:1.2.840.10003.5.109.10", null, new ArrayList<String>(0)); //$NON-NLS-1$ //$NON-NLS-2$
			final DataObjectFormat dataObjectFormat = new DataObjectFormatImpl(null, objectIdentifier, "text/xml", doc.getInputEncoding(), "#" + referenceId); //$NON-NLS-1$ //$NON-NLS-2$
			final List<DataObjectFormat> dataObjectFormats = new ArrayList<>();
			dataObjectFormats.add(dataObjectFormat);
			xades.setDataObjectFormats(dataObjectFormats);
		}

		// crea la firma
		final AOXMLAdvancedSignature xmlSignature = XAdESUtil.getXmlAdvancedSignature(
			xades,
			signedPropertiesTypeUrl,
			digestMethodAlgorithm,
			canonicalizationAlgorithm,
			uriDereferencer
		);

		try {
			final boolean onlySignningCert = Boolean.parseBoolean(
				extraParams.getProperty(XAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE, Boolean.FALSE.toString())
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
					Boolean.parseBoolean(extraParams.getProperty(XAdESExtraParams.ADD_KEY_INFO_KEY_VALUE, Boolean.TRUE.toString())),
					Boolean.parseBoolean(extraParams.getProperty(XAdESExtraParams.ADD_KEY_INFO_KEY_NAME, Boolean.FALSE.toString())),
					Boolean.parseBoolean(extraParams.getProperty(XAdESExtraParams.ADD_KEY_INFO_X509_ISSUER_SERIAL, Boolean.FALSE.toString())),
					Boolean.parseBoolean(extraParams.getProperty(XAdESExtraParams.KEEP_KEYINFO_UNSIGNED, Boolean.FALSE.toString())),
					validatePkcs1
				);
			}
		}
		catch (final NoSuchAlgorithmException e) {
			throw new IllegalArgumentException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'", e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch (final AOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la contrafirma XAdES", e); //$NON-NLS-1$
		}
	}

	/**
	 * Agrega al nombre de un nodo el prefijo indicado.
	 * @param prefix Prefijo que se debe agregar.
	 * @param nodeName Nombre del nodo.
	 * @return Nombre del nodo con el prefijo agregado o s&oacute;lo el nombre del nodo
	 * si el prefijo era {@code null} o cadena vac&iacute;a.
	 */
	private static String addNSPrefix(final String prefix, final String nodeName) {
		if (prefix == null || prefix.isEmpty()) {
			return nodeName;
		}
		return prefix + ":" + nodeName; //$NON-NLS-1$
	}

	private XAdESCounterSigner() {
		// No permitimos la instanciacion
	}

	/**
	 * Comprueba que no existan incompatibilidades entre los par&aacute;metros proporcionados
	 * y elimina aquellos que se vayan a ignorar. Tambi&eacute;n muestra advertencias sobre
	 * opciones de configuraci&oacute;n no recomendadas.
	 * @param algorithm Algoritmo de firma.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n.
	 */
	private static void checkParams(final String algorithm, final Properties extraParams) {

    	if (algorithm.toUpperCase(Locale.US).startsWith("MD")) { //$NON-NLS-1$
    		throw new IllegalArgumentException("XAdES no permite huellas digitales MD2 o MD5 (Decision 130/2011 CE)"); //$NON-NLS-1$
    	}

		// Comprobacion del perfil de firma y el algoritmo de firma seleccionado
		final String profile = extraParams.getProperty(XAdESExtraParams.PROFILE);
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)) {
			if (AOSignConstants.isSHA1SignatureAlgorithm(algorithm)) {
				LOGGER.warning("El algoritmo '" + algorithm + "' no esta recomendado para su uso en las firmas baseline"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			final String digestMethodAlgorithm = extraParams.getProperty(
					XAdESExtraParams.REFERENCES_DIGEST_METHOD);
			if (XMLConstants.URL_SHA1.equals(digestMethodAlgorithm)) {
				LOGGER.warning("El algoritmo SHA1 no esta recomendado para generar referencias en las firmas baseline"); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Indica si se debe agregar el tipo espec&iacute;fico usado en las referencias a
	 * las contrafirmas seg&uacute;n el espacio de nombres con el que se genere la firma.
	 * @param xadesNamespace Espacio de nombres de la firma.
	 * @return {@code true} si las contradirmas deben declarar el tipo en la referencia
	 * al SignatureValue de la firma, {@code false} en caso contrario.
	 */
	public static boolean needCounterSignatureReferenceType(final String xadesNamespace) {
		return !XAdESConstants.NAMESPACE_XADES_1_1_1.equals(xadesNamespace) &&
				!XAdESConstants.NAMESPACE_XADES_1_2_2.equals(xadesNamespace);
	}
}
