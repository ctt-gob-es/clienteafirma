package es.gob.afirma.signers.xadestri.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.crypto.AlgorithmMethod;
import javax.xml.crypto.KeySelector;
import javax.xml.crypto.KeySelectorException;
import javax.xml.crypto.KeySelectorResult;
import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
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
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xades.XAdESCoSigner;
import es.gob.afirma.signers.xades.XAdESSigner;


/** Parte servidora del firmador trif&aacute;sico XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XAdESTriPhaseSignerServerSide {

	public static enum Op {
		SIGN,
		COSIGN
	}

	private static final String XML_DEFAULT_ENCODING = "UTF-8"; //$NON-NLS-1$

	/** Cadena por la que reemplazaremos el PKCS#1 impostado de la firma. */
	public static final String REPLACEMENT_STRING = "%%REPLACEME%%"; //$NON-NLS-1$

	private static final String XML_NODE_ID = "Id"; //$NON-NLS-1$

	/** Cantidad de caracteres del PKCS#1 para los que buscaremos coincidencia.
	 * Cogemos un valor menor que el maximo definido por MIME como longitud de linea para los base 64 (76). */
	private static final int NUM_CHARACTERS_TO_COMPARE = 60;

	/** Prefirma (firma simple) en formato XAdES.
	 * @param data Datos a prefirmar
	 * @param algorithm Algoritmo de firma
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales de la firma
	 * @return Prefirma XML
	 * @throws NoSuchAlgorithmException
	 * @throws AOException
	 * @throws SAXException
	 * @throws IOException
	 * @throws ParserConfigurationException
	 * @throws MarshalException
	 * @throws XMLSignatureException
	 * @throws InvalidKeyException
	 * @throws SignatureException
	 * @throws XmlPreSignException */
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
		if (data == null || data.length < 1) {
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
		final List<String> previousSignaturesIds = new ArrayList<String>();
		Document xml = null;
		String xmlEncoding = XML_DEFAULT_ENCODING;
		try {
			xml = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(data));
			if (xml.getXmlEncoding() != null) {
				xmlEncoding = xml.getXmlEncoding();
			}
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").info("El documento a firmar no es XML, por lo que no contiene firmas previas");  //$NON-NLS-1$//$NON-NLS-2$
		}
		if (xml != null) {
			final Element rootElement = xml.getDocumentElement();
			if (rootElement.getNodeName().endsWith(":" + AOXAdESSigner.SIGNATURE_TAG)) { //$NON-NLS-1$
				final NamedNodeMap nnm = rootElement.getAttributes();
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
			else {
				final NodeList mainChildNodes = xml.getDocumentElement().getChildNodes();
				for (int i = 0; i < mainChildNodes.getLength(); i++) {
					final Node currentNode = mainChildNodes.item(i);
					if (currentNode.getNodeType() == Node.ELEMENT_NODE && currentNode.getNodeName().endsWith(":" + AOXAdESSigner.SIGNATURE_TAG)) { //$NON-NLS-1$
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
			}
		}


		final RSAPrivateKey prk = (RSAPrivateKey) generateKeyPair(
				((RSAPublicKey)((X509Certificate)certChain[0]).getPublicKey()).getModulus().bitLength()
				).getPrivate();

		final byte[] result;
		switch (op) {
		case SIGN:
			result = XAdESSigner.sign(
					data,
					algorithm,
					certChain,
					prk,
					xParams
					);
			break;
		case COSIGN:
			result = XAdESCoSigner.cosign(
					data,
					algorithm,
					certChain,
					prk,
					xParams
					);
			break;
		default:
			throw new IllegalStateException(
					"No se puede dar una operacion no contemplada en el enumerado de operaciones" //$NON-NLS-1$
					);
		}

		final byte[] signedInfo = XAdESTriPhaseSignerServerSide.getSignedInfo(
				result,
				certChain[0].getPublicKey(),
				previousSignaturesIds // Identificadores de firmas previas, para poder omitirlos
				);

		// Calculamos el valor PKCS#1 con la clave privada impostada
		final Signature signature = Signature.getInstance(algorithm);
		signature.initSign(prk);
		signature.update(signedInfo);

		final String cleanSignatureValue = cleanBase64(Base64.encode(signature.sign()));

		// Cargamos el XML original en un String
		final String xmlResult = new String(result, xmlEncoding);

		// Buscamos el PKCS#1 en Base64 en el XML original y lo sustituimos por la cadena de reemplazo
		final String signValuePrefix = ">" + cleanSignatureValue.substring(0, NUM_CHARACTERS_TO_COMPARE); //$NON-NLS-1$
		final int signValuePos = xmlResult.indexOf(signValuePrefix) + 1;
		final int signValueFinalPos = xmlResult.indexOf('<', signValuePos);
		final String pkcs1String = xmlResult.substring(signValuePos, signValueFinalPos);

		final String cleanPkcs1String = cleanBase64(pkcs1String);
		if (cleanSignatureValue.equals(cleanPkcs1String)) {
			return new XmlPreSignResult(
					Base64.encode(
							xmlResult.replace(pkcs1String, REPLACEMENT_STRING).getBytes()
							),
							Base64.encode(signedInfo)
					);
		}
		throw new XmlPreSignException();

	}

	private static String cleanBase64(final String base64) {
		return base64 == null ? null : base64.replace("\n", "").replace("\r", "").replace("\t", "").replace(" ", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
	}

	private static byte[] getSignedInfo(final byte[] xmlSign,
			final PublicKey pk,
			final List<String> excludedIds) throws SAXException,
			IOException,
			ParserConfigurationException,
			MarshalException,
			XMLSignatureException, XmlPreSignException {
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		final NodeList nl = dbf.newDocumentBuilder().parse(
				new ByteArrayInputStream(xmlSign)
				).getElementsByTagNameNS(
						XMLSignature.XMLNS,
						AOXAdESSigner.SIGNATURE_TAG
						);
		if (nl.getLength() == 0) {
			throw new IllegalArgumentException("Se ha proporcionado un XML sin firmas"); //$NON-NLS-1$
		}

		// De los nodos Signature obtenidos, descartar aquellos cuyos identificadores esten en la lista
		// de excluidos (excludedIds).
		Node newSignature = null;
		for (int i=0;i<nl.getLength();i++) {
			final Node currentNode = nl.item(i);
			final NamedNodeMap nnm = currentNode.getAttributes();
			if (nnm != null) {
				final Node node = nnm.getNamedItem(XML_NODE_ID);
				if (node != null) {
					final String id = node.getNodeValue();
					if (excludedIds == null || !excludedIds.contains(id)) {
						newSignature = currentNode;
						break;
					}
				}
			}
		}

		if (newSignature == null) {
			throw new XmlPreSignException("Se ha creado un nodo firma, pero no se ha encontrado en el postproceso"); //$NON-NLS-1$
		}

		final XMLValidateContext valContext = new DOMValidateContext(new SimpleKeySelector(pk), newSignature);
		valContext.setProperty("javax.xml.crypto.dsig.cacheReference", Boolean.TRUE); //$NON-NLS-1$

		final XMLSignature signature = XMLSignatureFactory.getInstance("DOM").unmarshalXMLSignature(valContext); //$NON-NLS-1$

		signature.validate(valContext);

		return 	AOUtil.getDataFromInputStream(
				signature.getSignedInfo().getCanonicalizedData()
				);

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

	/** Genera un par de claves RSA.
	 * @param keySize Tama&ntilde;o de las claves a generar
	 * @return Par de claves RSA
	 * @throws NoSuchAlgorithmException Si no se soporta la generaci&oacute;n de claves RSA */
	private static KeyPair generateKeyPair(final int keySize) throws NoSuchAlgorithmException {
		final KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA"); //$NON-NLS-1$
		keyGen.initialize(keySize);
		return keyGen.generateKeyPair();
	}

}
