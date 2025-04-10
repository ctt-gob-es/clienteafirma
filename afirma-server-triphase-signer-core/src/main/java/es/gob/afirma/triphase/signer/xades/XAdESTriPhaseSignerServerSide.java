/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.xades;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.AlgorithmMethod;
import javax.xml.crypto.KeySelector;
import javax.xml.crypto.KeySelectorException;
import javax.xml.crypto.KeySelectorResult;
import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLValidateContext;
import javax.xml.crypto.dsig.dom.DOMValidateContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.xades.XAdESCoSigner;
import es.gob.afirma.signers.xades.XAdESCounterSigner;
import es.gob.afirma.signers.xades.XAdESExtraParams;
import es.gob.afirma.signers.xades.XAdESSigner;
import es.gob.afirma.signers.xades.XAdESUtil;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;

/** Parte servidora del firmador trif&aacute;sico XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XAdESTriPhaseSignerServerSide {

	private XAdESTriPhaseSignerServerSide() {
		// No permitimos la instanciacion
	}

	private static final String COUNTERSIGN_TARGET_KEY = "target"; //$NON-NLS-1$

	/** Operaciones de firma soportadas. */
	public enum Op {
		/** Firma. */
		SIGN,
		/** Co-firma. */
		COSIGN,
		/** Contrafirma. */
		COUNTERSIGN
	}

	private static final String XML_DEFAULT_ENCODING = "UTF-8"; //$NON-NLS-1$

	/** C&oacute;digo de indice en la cadena de reemplazo. */
	public static final String REPLACEMENT_CODE = "%i"; //$NON-NLS-1$

	/** Cadena por la que reemplazaremos el PKCS#1 impostado de la firma. */
	public static final String REPLACEMENT_STRING = "====REPLACEME_" + REPLACEMENT_CODE + "===="; //$NON-NLS-1$ //$NON-NLS-2$

	private static final String XML_NODE_ID = "Id"; //$NON-NLS-1$

	/** Cantidad de caracteres del PKCS#1 para los que buscaremos coincidencia.
	 * Cogemos un valor menor que el maximo definido por MIME como longitud de linea para los Base64 (76). */
	private static final int NUM_CHARACTERS_TO_COMPARE = 60;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Prefirma (firma simple) en formato XAdES.
	 * @param data Datos a prefirmar
	 * @param algorithm Algoritmo de firma
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales de la firma
	 * @param op Operaci&oacute;n espec&iacute;fica de firma a realizar
	 * @return Listado de prefirma XML
	 * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario.
	 * @throws AOException Si ocurre cualquier otro error.
	 * @throws SAXException Si hay problemas en el an&aacute;lisis XML.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws ParserConfigurationException Si hay problemas con el analizador por defecto de XML.
	 * @throws MarshalException Si hay problemas con el empaquetado XML.
	 * @throws XMLSignatureException Si hay problemas en la propia firma XMLDSig.
	 * @throws InvalidKeyException Si la clave proporcinoada no es v&aacute;lida.
	 * @throws SignatureException Si hay problemas con la firma PKCS#1.
	 * @throws XmlPreSignException Si hay un error en la pre-firma XAdES. */
	public static XmlPreSignResult preSign(final byte[] data,
			                               final String algorithm,
			                               final Certificate[] certChain,
			                               final Properties xParams,
			                               final Op op) throws NoSuchAlgorithmException,
			                                                   AOException,
			                                                   SAXException,
			                                                   IOException,
			                                                   ParserConfigurationException,
			                                                   MarshalException,
			                                                   XMLSignatureException,
			                                                   InvalidKeyException,
			                                                   SignatureException,
			                                                   XmlPreSignException {


		if ((data == null || data.length < 1) && XAdESUtil.isDataMandatory(op.name(), xParams)) {
			throw new IllegalArgumentException("Los datos a prefirmar no pueden ser nulos ni vacios"); //$NON-NLS-1$
		}
		if (algorithm == null || "".equals(algorithm)) { //$NON-NLS-1$
			throw new IllegalArgumentException("El algoritmo de firma no puede ser nulo ni vacio"); //$NON-NLS-1$
		}
		if (certChain == null || certChain.length < 1) {
			throw new IllegalArgumentException(
				"La cadena de certificados no puede ser nula y debe contener al menos un certificado" //$NON-NLS-1$
			);
		}

		// Miramos si la entrada es un XML que ya contenga firmas (almacenando los ID para luego localizar cual es la firma
		// nueva y no confundirla con las antiguas), y si lo es cual es su codificacion con un Document.getXmlEncoding()
		final List<String> previousSignaturesIds = new ArrayList<>();
		Document xml = null;
		String xmlEncoding = XML_DEFAULT_ENCODING;

		if (AOFileUtils.isXML(data)) {
			try {

				// Si los datos eran XML, comprobamos y almacenamos las firmas previas
				final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
				dbf.setNamespaceAware(true);
				xml = dbf.newDocumentBuilder().parse(
						new ByteArrayInputStream(data)
						);
				if (xml.getXmlEncoding() != null) {
					xmlEncoding = xml.getXmlEncoding();
				}
			}
			catch(final Exception e) {
				LOGGER.log(Level.WARNING, "Error al cargar el documento XML", e);  //$NON-NLS-1$
			}
		}
		else {
			LOGGER.info("El documento a firmar no es XML, por lo que no contiene firmas previas");  //$NON-NLS-1$
		}

		if (xml == null && (op == Op.COSIGN || op == Op.COUNTERSIGN)) {
			LOGGER.severe("Solo se pueden cofirmar y contrafirmar firmas XML");  //$NON-NLS-1$
			throw new AOException("Los datos introducidos no se corresponden con una firma XML"); //$NON-NLS-1$
		}

		if (xml != null) {

			final NodeList signatureNodeList = xml.getElementsByTagNameNS(
					XMLSignature.XMLNS,
					XMLConstants.TAG_SIGNATURE
					);

			// Si se ha solicitado una firma y detectamos que el XML ya esta
			// firmado, devolvemos un error ya que este se puede cofirmar o
			// contrafirmar, pero no firmar
			if (op == Op.SIGN) {
				final boolean rootIsSignature =
						XMLSignature.XMLNS.equals(xml.getNamespaceURI())
						&& XMLConstants.TAG_SIGNATURE.equals(xml.getNodeName());
				if (rootIsSignature || signatureNodeList.getLength() > 0) {
					LOGGER.severe("El XML ya esta firmado, por lo que no se permite volverlo a firmar (debe cofirmarse o contrafirmarse)");  //$NON-NLS-1$
					throw new AOException("El XML ya esta firmado, por lo que no se permite volverlo a firmar (debe cofirmarse o contrafirmarse)"); //$NON-NLS-1$
				}
			}

			for (int i = 0; i < signatureNodeList.getLength(); i++) {

				final Node currentNode = signatureNodeList.item(i);
				if (currentNode.getNodeType() == Node.ELEMENT_NODE) {
					final NamedNodeMap nnm = currentNode.getAttributes();
					if (nnm != null) {
						final Node node = nnm.getNamedItem(XML_NODE_ID);
						if (node != null) {
							final String id = node.getNodeValue();
							if (id != null) {
								previousSignaturesIds.add(id);
							}
						}
					}
				}
			}

			// Si se trara de una cofirma o contrafirma de manifest, pero no se declaro como tal,
			// lo declaramos para que despues no se eliminen partes comunes que son necesarias
			if (signatureNodeList.getLength() > 0
					&& (op == Op.COSIGN || op == Op.COUNTERSIGN)
					&& !Boolean.parseBoolean(xParams.getProperty(XAdESExtraParams.USE_MANIFEST))) {
				// Comprobamos los datos firmados de la primera firma
				final Element signature = (Element) signatureNodeList.item(0);
				if (XAdESUtil.hasHashManifestReference(signature)) {
					xParams.setProperty(XAdESExtraParams.USE_MANIFEST,  Boolean.TRUE.toString());
				}
			}
		}

		// Desactivamos la validacion del PKCS#1 para evitar errores por el uso
		// de una clave de firma mock durante la prefirma
		final Properties extraParams = getExtraParams(xParams);
		extraParams.setProperty(XAdESExtraParams.INTERNAL_VALIDATE_PKCS1, Boolean.FALSE.toString());

		// Generamos un par de claves para hacer la firma temporal, que despues sustituiremos por la real
		final PublicKey publicKey = certChain[0].getPublicKey();
		final PrivateKey privateKey = KeyHelperFactory.getKeyHelper(publicKey).getPrivateKey(publicKey);

		final byte[] result;
		switch (op) {
			case SIGN:
				result = XAdESSigner.sign(
					data,
					algorithm,
					privateKey,
					certChain,
					extraParams
				);
				break;
			case COSIGN:
				result = XAdESCoSigner.cosign(
					data,
					algorithm,
					privateKey,
					certChain,
					extraParams
				);
				break;
			case COUNTERSIGN:
				final CounterSignTarget targets =
					CounterSignTarget.LEAFS.name().equalsIgnoreCase(extraParams.getProperty(COUNTERSIGN_TARGET_KEY)) ?
						CounterSignTarget.LEAFS : CounterSignTarget.TREE;

				result = XAdESCounterSigner.countersign(
					data,
					algorithm,
					targets,
					null,
					privateKey,
					certChain,
					extraParams
				);
				break;
			default:
				throw new IllegalStateException(
					"No se puede dar una operacion no contemplada en el enumerado de operaciones: " + op //$NON-NLS-1$
				);
		}

		// Cargamos el XML firmado en un String
		String xmlResult = new String(result, xmlEncoding);

		// Recuperamos los signed info que se han firmado
		final List<SignatureData> signatureDatas = XAdESTriPhaseSignerServerSide.getSignatureData(
			result,
			certChain[0].getPublicKey(),
			previousSignaturesIds // Identificadores de firmas previas, para poder omitirlos
		);

		// Ponemos un reemplazo en el XML en lugar de los PKCS#1 de las firmas generadas
		final List<byte[]> signedInfos = new ArrayList<>(signatureDatas.size());
		for (int i = 0; i < signatureDatas.size(); i++) {

			final SignatureData signatureData = signatureDatas.get(i);

			final String signatureValueB64 = Base64.encode(signatureData.getSignatureValue());

			// Por seguridad identificamos la posicion del elemento, teniendo cuidado de que si
			// la cadena es pequena se compruebe que el elemento sea completo
			String signValuePrefix = ">"; //$NON-NLS-1$
			if (signatureValueB64.length() < NUM_CHARACTERS_TO_COMPARE) {
				signValuePrefix += signatureValueB64 + "<"; //$NON-NLS-1$
			} else {
				signValuePrefix += signatureValueB64.substring(0, NUM_CHARACTERS_TO_COMPARE);
			}
			final int signValueStart = xmlResult.indexOf(signValuePrefix) + 1;

			if (signValueStart > 0) {
				final int signValueEnd = xmlResult.indexOf('<', signValueStart);
				final String signatureValue = xmlResult.substring(signValueStart, signValueEnd);
				xmlResult = xmlResult.replace(signatureValue, REPLACEMENT_STRING.replace(REPLACEMENT_CODE, Integer.toString(i)));
				signedInfos.add(signatureData.getSignedInfo());
			}
			else {
				LOGGER.warning("No se encontro uno de los PKCS#1 internos para sustitucion"); //$NON-NLS-1$
			}
		}

		return new XmlPreSignResult(xmlResult.getBytes(xmlEncoding), signedInfos, xmlEncoding);
	}

	/** Recupera los <code>SignedInfo</code> de una firma XML, excluyendo los de las firmas indicadas a trav&eacute;s
	 * de su Id.
	 * @param xmlSign XML del que se desea recuperar los <code>SignedInfo</code>.
	 * @param pk Clave publica usada en las firmas de las que se desea obtener los signedInfo.
	 * @param excludedIds Identificadores de las firmas excluidas.
	 * @return Listado de signedInfos.
	 * @throws SAXException Si hay problemas durante el an&aacute;lisis XML.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws ParserConfigurationException Si hay problemas con el analizador por defecto para XML.
	 * @throws MarshalException Si hay problemas con el empaquetado XML.
	 * @throws XMLSignatureException Si hay problemas en la propia firma XMLDSig.
	 * @throws XmlPreSignException Cuando no se ha encontrado ning&uacute;n signedInfo que devolver. */
	private static List<SignatureData> getSignatureData(final byte[] xmlSign,
			                                   final PublicKey pk,
			                                   final List<String> excludedIds) throws SAXException,
			                                                                          IOException,
			                                                                          ParserConfigurationException,
			                                                                          MarshalException,
			                                                                          XMLSignatureException,
			                                                                          XmlPreSignException {
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		final NodeList signatureNodeList = dbf.newDocumentBuilder().parse(
			new ByteArrayInputStream(xmlSign)
		).getElementsByTagNameNS(
			XMLSignature.XMLNS,
			XMLConstants.TAG_SIGNATURE
		);
		if (signatureNodeList.getLength() == 0) {
			throw new IllegalArgumentException("Se ha proporcionado un XML sin firmas"); //$NON-NLS-1$
		}

		// Recogemos el signedInfo de cada firma cuyo identificador no este en la lista de excluidos (excludedIds)
		final List<SignatureData> signedInfos = new ArrayList<>();
		for (int i = 0; i < signatureNodeList.getLength(); i++) {

			final Node currentNode = signatureNodeList.item(i);

			// Saltamos las firmas sin identificador
			if (currentNode.getAttributes() == null || currentNode.getAttributes().getNamedItem(XML_NODE_ID) == null) {
				LOGGER.warning("El documento contiene firmas sin identificador reconocido");  //$NON-NLS-1$
				continue;
			}

			// Saltamos las firmas excluidas (las que estaban antes)
			final String id = currentNode.getAttributes().getNamedItem(XML_NODE_ID).getNodeValue();
			if (excludedIds != null && excludedIds.contains(id)) {
				continue;
			}

			// Agregamos el signed info de la firma al listado
			final XMLValidateContext valContext = new DOMValidateContext(new SimpleKeySelector(pk), currentNode);
			valContext.setProperty("javax.xml.crypto.dsig.cacheReference", Boolean.TRUE); //$NON-NLS-1$
			final XMLSignature signature = Utils.getDOMFactory().unmarshalXMLSignature(valContext);
			signature.validate(valContext);

			byte[] signedInfo;
			try (InputStream is = signature.getSignedInfo().getCanonicalizedData()) {
				signedInfo = AOUtil.getDataFromInputStream(is);
			}
			final byte[] signatureValue = signature.getSignatureValue().getValue();

			signedInfos.add(new SignatureData(signedInfo, signatureValue));
		}

		if (signedInfos.isEmpty()) {
			throw new XmlPreSignException("Se ha creado un nodo firma, pero no se ha encontrado en el postproceso"); //$NON-NLS-1$
		}

		return signedInfos;
	}

	private static class SignatureData {

		private final byte[] signedInfo;

		private final byte[] signatureValue;

		public SignatureData(final byte[] signedInfo, final byte[] signatureValue) {
			this.signedInfo = signedInfo;
			this.signatureValue = signatureValue;
		}

		public byte[] getSignedInfo() {
			return this.signedInfo;
		}

		public byte[] getSignatureValue() {
			return this.signatureValue;
		}
	}

	private static class SimpleKeySelector extends KeySelector {

		private final PublicKey pk;

		SimpleKeySelector(final PublicKey key) {
			this.pk = key;
		}

		@Override
		public KeySelectorResult select(final KeyInfo keyInfo,
				final KeySelector.Purpose purpose,
				final AlgorithmMethod method,
				final XMLCryptoContext context) throws KeySelectorException {
			return new SimpleKeySelectorResult(this.pk);
		}

	}

	private static class SimpleKeySelectorResult implements KeySelectorResult {
		private final PublicKey pk;
		SimpleKeySelectorResult(final PublicKey pk) {
			this.pk = pk;
		}

		@Override
		public Key getKey() { return this.pk; }
	}

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = extraParams != null ?
    			(Properties) extraParams.clone() : new Properties();

    	return newExtraParams;
    }
}
