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
import java.io.IOException;
import java.net.URI;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignatureFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AGEPolicyIncompatibilityException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.SigningLTSException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicyPropertiesManager;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormat;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormatImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifier;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifierImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdESBase;

/** Co-firmador XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XAdESCoSigner {

	private static final Logger	LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private XAdESCoSigner() {
		// No permitimos la instanciacion
	}

	/**
	 * Cofirma datos en formato XAdES.
	 * <p>
	 *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
	 *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
	 *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
	 * </p>
	 * <p>
	 *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta
	 *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto
	 *  de XMLDSig, el resultado de una cofirma XAdES sobre un documento firmado previamente con
	 *  XMLDSig (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
	 *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES,
	 *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
	 * </p>
	 * @param sign Firma inicial.
	 * @param algorithm Algoritmo a usar para la firma.
	 * @param pk Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @return Cofirma en formato XAdES
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	public static byte[] cosign(final byte[] sign,
			final String algorithm,
			final PrivateKey pk,
			final Certificate[] certChain,
			final Properties xParams) throws AOException {

		Document signDocument;
		try {
			signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido leer el documento XML de firmas", e); //$NON-NLS-1$
		}

		return cosign(signDocument, algorithm, pk, certChain, xParams);
	}


	/**
	 * Cofirma datos en formato XAdES.
	 * <p>
	 *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
	 *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
	 *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
	 * </p>
	 * <p>
	 *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta
	 *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto
	 *  de XMLDSig, el resultado de una cofirma XAdES sobre un documento firmado previamente con
	 *  XMLDSig (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
	 *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES,
	 *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
	 * </p>
	 * @param signDocument Documento XML con las firmas iniciales.
	 * @param algorithm Algoritmo a usar para la firma.
	 * @param pk Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @return Cofirma en formato XAdES
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	public static byte[] cosign(final Document signDocument,
			                    final String algorithm,
			                    final PrivateKey pk,
			                    final Certificate[] certChain,
			                    final Properties xParams) throws AOException {
		return cosign(signDocument, algorithm, pk, certChain, xParams, null);
	}

	/**
	 * Cofirma datos en formato XAdES.
	 * <p>
	 *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
	 *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
	 *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
	 * </p>
	 * <p>
	 *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta
	 *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto
	 *  de XMLDSig, el resultado de una cofirma XAdES sobre un documento firmado previamente con
	 *  XMLDSig (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
	 *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES,
	 *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
	 * </p>
	 * @param signDocument Documento XML con las firmas iniciales.
	 * @param signAlgorithm Algoritmo a usar para la firma.
	 * @param pk Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @param uriDereferencer Derreferenciador a medida.
	 * @return Cofirma en formato XAdES
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	public static byte[] cosign(final Document signDocument,
			                    final String signAlgorithm,
			                    final PrivateKey pk,
			                    final Certificate[] certChain,
			                    final Properties xParams,
			                    final URIDereferencer uriDereferencer) throws AOException {


		final String algorithm = signAlgorithm != null ? signAlgorithm : AOSignConstants.DEFAULT_SIGN_ALGO;
		final Properties extraParams = xParams != null ? xParams: new Properties();

		checkParams(algorithm, extraParams);

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new IllegalArgumentException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final String digestMethodAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.REFERENCES_DIGEST_METHOD, XAdESConstants.DEFAULT_DIGEST_METHOD);

		final String canonicalizationAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.CANONICALIZATION_ALGORITHM, CanonicalizationMethod.INCLUSIVE);

		final boolean addKeyInfoKeyValue = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_KEY_VALUE, Boolean.TRUE.toString()));

		final boolean addKeyInfoKeyName = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_KEY_NAME, Boolean.FALSE.toString()));

		final boolean addKeyInfoX509IssuerSerial = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_X509_ISSUER_SERIAL, Boolean.FALSE.toString()));

		final boolean keepKeyInfoUnsigned = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.KEEP_KEYINFO_UNSIGNED, Boolean.FALSE.toString()));

		final String outputXmlEncoding = extraParams.getProperty(
		        XAdESExtraParams.OUTPUT_XML_ENCODING);

		final String oid = extraParams.getProperty(XAdESExtraParams.CONTENT_TYPE_OID);

		String mimeType = extraParams.getProperty(XAdESExtraParams.CONTENT_MIME_TYPE);

		String encoding = extraParams.getProperty(XAdESExtraParams.CONTENT_ENCODING);

		// Dejamos que indiquen "base64" en vez de la URI, hacemos el cambio manualmente
		if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
			encoding = XMLConstants.BASE64_ENCODING;
		}

		// Comprobamos que sea una URI
		if (encoding != null && !encoding.isEmpty()) {
			try {
				// No se usa el objeto, solo se crea para ver si saltan excepciones
				new URI(encoding);
			}
			catch(final Exception e) {
				throw new AOException(
					"La codificacion indicada en 'encoding' debe ser una URI: " + e, e //$NON-NLS-1$
				);
			}
		}

		// Comprobamos si se ha indicado validar el PKCS#1 generado (por defecto, si)
		final boolean validatePkcs1 = Boolean.parseBoolean(extraParams.getProperty(
				XAdESExtraParams.INTERNAL_VALIDATE_PKCS1, Boolean.TRUE.toString()));

		// Perfil de firma XAdES que se desea aplicar
		String profile = extraParams.getProperty(
		        XAdESExtraParams.PROFILE, AOSignConstants.DEFAULT_SIGN_PROFILE);

		// Si el documento contiene una firma simple se inserta como raiz el
		// nodo AFIRMA
		Document docSig = signDocument;
		Element root = signDocument.getDocumentElement();
		if (root.getLocalName().equals(XMLConstants.TAG_SIGNATURE)) {
			try {
				docSig = AOXAdESSigner.insertarNodoAfirma(docSig);
				root = docSig.getDocumentElement();
			}
			catch (final Exception e) {
				throw new AOException("No se ha estructurar el documento XML de firmas", e); //$NON-NLS-1$
			}
		}

		// Comprobamos que cualquier firma del documento no sea de archivo
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
				// Si se indico expresamente que no se debia permitir la cofirma de
				// firmas de archivo, se lanza una excepcion bloqueando la ejecucion.
				// Si no, se informa debidamente para que se consulte al usuario
				if (allowSignLts != null) {
					throw new AOException(e.getMessage());
				}
				throw new SigningLTSException("La cofirma de firmas de archivo invalidara el sello de archivo", e, false); //$NON-NLS-1$
			}
		}

		// Propiedades del documento XML original
		final Map<String, String> originalXMLProperties = XAdESUtil.getOriginalXMLProperties(
			docSig,
			outputXmlEncoding
		);

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

		// Localizamos la primera firma (primer nodo "Signature") en profundidad en el arbol de firma.
		// Se considera que todos los objetos "Signature" del documento firman (referencian) los mismos
		// objetos, por lo que podemos extraerlos de cualquiera de ellas.
		// Buscamos dentro de la firma la refencia a los atributos firmados y, mediante esta referencia
		// y el propio nodo de atributos, obtenemos la URL del tipo que declara y el espacio de nombres
		// de XAdES que se debe utilizar
		final Element signatureElement = XAdESUtil.getFirstSignatureElement(docSig.getDocumentElement());
		final Element signedPropertiesReference = XAdESUtil.getSignedPropertiesReference(signatureElement);

		// Identificamos el Almacenaremos ademas, el tipo con el que se declara la referencia de los SignedPropeties y el
		// Id del propio nodo SignedPropeties para poder generar la nueva firma con los mismos datos.
		String signedPropertiesType = signedPropertiesReference.getAttribute("Type"); //$NON-NLS-1$
		if (signedPropertiesType == null || signedPropertiesType.isEmpty()) {
			signedPropertiesType = XAdESConstants.REFERENCE_TYPE_SIGNED_PROPERTIES;
		}
		final Element signedPropertiesElement = XAdESUtil.getSignedPropertiesElement(signatureElement, signedPropertiesReference);
		String xadesNamespace = signedPropertiesElement.getNamespaceURI();
		if (xadesNamespace == null) {
			xadesNamespace = XAdESConstants.DEFAULT_NAMESPACE_XADES;
		}

		// Si se solicito realizar una cofirma XAdES baseline, pero el espacio de nombres
		// de firma original no lo soporta, se ignora el perfil
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equals(profile) &&
				!XAdESUtil.isBaselineCompatible(xadesNamespace)) {
			LOGGER.warning("La firma original utiliza un espacio de nombres no compatible con baseline (" //$NON-NLS-1$
					+ xadesNamespace + "). No se generara una firma baseline"); //$NON-NLS-1$
			profile = AOSignConstants.SIGN_PROFILE_ADVANCED;
		}

		// Creamos el listado de referencias que deberan aparecer en la firma
		final List<Reference> referenceList = new ArrayList<>();

		// Creamos el listado con la informacion de los datos firmados que se agregaran a la firma
		final ArrayList<DataObjectFormat> objectFormats = new ArrayList<>();

		// Objeto en donde se almacenaran los datos firmados en caso de tratarse de
		// una firma enveloping
		XMLObject newInternalObject = null;
		boolean isEnveloping = false;
		String referenceId = null;
		final List<Element> dataReferencesList = XAdESUtil.getSignatureDataReferenceList(signatureElement);
		for (final Element currentReference : dataReferencesList) {

			// Buscamos las transformaciones declaradas en la Referencia,
			// para anadirlas tambien en la nueva
			final List<Transform> currentTransformList;
			try {
				currentTransformList = Utils.getObjectReferenceTransforms(currentReference, XAdESConstants.DEFAULT_XML_SIGNATURE_PREFIX);
			}
			catch (final NoSuchAlgorithmException e) {
				throw new AOException("Se ha declarado una transformacion personalizada de un tipo no soportado", e); //$NON-NLS-1$
			}
			catch (final InvalidAlgorithmParameterException e) {
				throw new AOException("Se han especificado parametros erroneos para una transformacion personalizada", e); //$NON-NLS-1$
			}

			// Creamos un identificador de referencia para el objeto a firmar y lo almacenamos
			// para mantener un listado con todas. En el caso de las hojas de estilo lo creamos con un
			// identificador descriptivo
			if (currentReference.getAttribute(XAdESConstants.ID_IDENTIFIER) != null &&
					currentReference.getAttribute(XAdESConstants.ID_IDENTIFIER).startsWith("StyleReference-")) { //$NON-NLS-1$
				referenceId = "StyleReference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
			}
			else {
				referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
			}

			// Buscamos y analizamos el nodo de datos para obtener su tipo
			final String referenceUri = currentReference.getAttribute("URI"); //$NON-NLS-1$
			String referenceType = currentReference.getAttribute("Type"); //$NON-NLS-1$
			if (referenceType != null && referenceType.isEmpty()) {
				referenceType = null;
			}


			// Agregamos a la firma las referencia al dato y los tipos que esta tenga asociados

			// Firmas manifest
			if (XAdESConstants.REFERENCE_TYPE_MANIFEST.equals(referenceType)) {

				// Hacemos una copia del manifest de la firma original
				final Element newManifestElement = copyManifest(referenceUri, signatureElement);

				// Hacemos copia de los DataObjectFormat de la firma original
				final ArrayList<DataObjectFormat> dataObjectFormats = copyDataObjectFormats(signedPropertiesElement);

				// Actualizamos los identificadores del manifest y de los datos referenciados
				final String newManifestId = renewManifestIds(newManifestElement, dataObjectFormats);

				// Creamos un nuevo objeto con el manifest para agregarlo a la firma
				newInternalObject = createSignatureObject(newManifestElement, fac, mimeType, encoding);

				// Agregamos a la firma la referencia al nuevo manifest
				referenceList.add(
						fac.newReference(
								"#" + newManifestId, //$NON-NLS-1$
								digestMethod,
								currentTransformList,
								referenceType,
								referenceId
								)
						);

				// Agregamos al listado de formatos de datos los del nuevo manifest
				objectFormats.addAll(dataObjectFormats);
			}

			// Firmas enveloped
			else if ("".equals(referenceUri)) { //$NON-NLS-1$

				// Si no se declaro un mimetype, se usara el de XML
				if (mimeType == null) {
					mimeType = "text/xml"; //$NON-NLS-1$
				}

				// Creamos la referencia a los datos con las transformaciones de la original
				referenceList.add(
						fac.newReference(
								referenceUri, // Aqui siempre vale ""
								digestMethod,
								currentTransformList,
								XMLConstants.OBJURI,
								referenceId
								)
						);

				// Agregamos a la firma el tipo de los datos de esta referencia (DataObjectFormat)
				addReferenceDataObjectFormat(objectFormats, referenceId, mimeType, oid, encoding);
			}

			// Firmas enveloping y detached
			else {

				final String dataNodeId = referenceUri.substring(referenceUri.startsWith("#") ? 1 : 0); //$NON-NLS-1$
				Element dataObjectElement = null;
				final Element docElement = docSig.getDocumentElement();

				// Comprobamos si el nodo raiz o sus hijos inmediatos son el nodo de datos
				Node nodeAttributeId = docElement.getAttributes() != null ?
						docElement.getAttributes().getNamedItem(XAdESConstants.ID_IDENTIFIER) : null;
				if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
					dataObjectElement = docElement;
				}
				else {
					// Recorremos los hijos al reves para acceder antes a los datos y las firmas
					final NodeList rootChildNodes = docElement.getChildNodes();
					for (int j = rootChildNodes.getLength() - 1; j >= 0; j--) {

						nodeAttributeId = rootChildNodes.item(j).getAttributes() != null ?
								rootChildNodes.item(j).getAttributes().getNamedItem(XAdESConstants.ID_IDENTIFIER) :
									null;
								if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
									dataObjectElement = (Element) rootChildNodes.item(j);
									break;
								}

								// Si es un nodo de firma tambien miramos en sus nodos hijos
								if (XMLConstants.TAG_SIGNATURE.equals(rootChildNodes.item(j).getLocalName())) {
									final NodeList subChildsNodes = rootChildNodes.item(j).getChildNodes();
									for (int k = subChildsNodes.getLength() - 1; k >= 0; k--) {
										nodeAttributeId = subChildsNodes.item(k).getAttributes() != null ?
												subChildsNodes.item(k).getAttributes().getNamedItem(XAdESConstants.ID_IDENTIFIER) :
													null;
												if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
													dataObjectElement = (Element) subChildsNodes.item(k);
													break;
												}
									}
									if (dataObjectElement != null) {
										break;
									}
								}
					}
				}
				if (dataObjectElement != null) {
					if (mimeType == null) {
						mimeType = dataObjectElement.getAttribute("MimeType"); //$NON-NLS-1$
					}
					if (encoding == null) {
						encoding = dataObjectElement.getAttribute("Encoding"); //$NON-NLS-1$
					}
				}

				final NodeList signatureChildNodes = docSig.getElementsByTagNameNS(
						XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE
						).item(0).getChildNodes();
				for (int j = 0; j < signatureChildNodes.getLength(); j++) {
					final Node subNode = signatureChildNodes.item(j);
					final NamedNodeMap nnm = subNode.getAttributes();
					if (nnm != null) {
						final Node idAttrNode = nnm.getNamedItem(XAdESConstants.ID_IDENTIFIER);
						if (idAttrNode != null && dataNodeId.equals(idAttrNode.getNodeValue())) {
							isEnveloping = true;
						}
					}
				}

				// Firma enveloping
				if (isEnveloping && dataObjectElement != null) {

					// Si se declara la politica de firma de la AGE, debemos tener en cuenta
					// que esta no es compatible con las firmas enveloping, asi que debemos
					// establecer un comportamiento alternativo:
					//  - Si no se indica que hacer, lanzaremos una excepcion indicando la incompatiblidad. Esto
					//    puede conllevar que las aplicaciones adapten el comportamiento.
					//  - Si se indico que se evitasen las incompatibilidades, se adapta la configuraci&oacute;n
					//    segun lo establecido por la excepcion para generar una firma valida.
					//  - Se se indico que no se evitasen la incompatibilidades, indicaremos que la operacion
					//    fallo.
					final String policyId = extraParams.getProperty(XAdESExtraParams.POLICY_IDENTIFIER);
					if (AdESPolicyPropertiesManager.isAgePolicyConfigurated(policyId)) {
						final String avoidAgePolicyIncompatibilities = extraParams.getProperty(XAdESExtraParams.AVOID_AGE_POLICY_INCOMPATIBILITIES);
						if (avoidAgePolicyIncompatibilities == null) {
							throw new AGEPolicyIncompatibilityException("La politica de la AGE no soporta la cofirma XAdES Enveloping", AGEPolicyIncompatibilityException.OP_COSIGN); //$NON-NLS-1$
						}
						else if (Boolean.parseBoolean(avoidAgePolicyIncompatibilities)) {
							new AGEPolicyIncompatibilityException("La politica de la AGE no soporta la cofirma XAdES Enveloping") //$NON-NLS-1$
								.prepareOperationWithConfirmation(extraParams);
						}
						else {
							throw new AOException("La politica de la AGE no soporta la cofirma XAdES Enveloping"); //$NON-NLS-1$
						}
					}

					// crea el nuevo elemento Object que con el documento afirmar
					final List<XMLStructure> structures = new ArrayList<>(1);
					structures.add(new DOMStructure(dataObjectElement.getFirstChild().cloneNode(true)));

					final String objectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
					newInternalObject = fac.newXMLObject(
							structures,
							objectId,
							mimeType,
							encoding
							);

					// Agregamos la referencia al nuevo objeto de datos
					referenceList.add(
							fac.newReference(
									"#" + objectId, //$NON-NLS-1$
									digestMethod,
									currentTransformList,
									referenceType,
									referenceId
									)
							);
				}
				// Firma detached
				else {
					// Agregamos la referencia a los datos ya existentes
					referenceList.add(
							fac.newReference(
									referenceUri,
									digestMethod,
									currentTransformList,
									referenceType,
									referenceId
									)
							);
				}

				// Agregamos a la firma el tipo de los datos de esta referencia (DataObjectFormat)
				addReferenceDataObjectFormat(objectFormats, referenceId, mimeType, oid, encoding);
			}

		}

		// Instancia XAdES
		final XAdESBase xades = XAdESUtil.newInstance(
				profile,
				xadesNamespace,
				XAdESConstants.DEFAULT_XADES_SIGNATURE_PREFIX,
				XAdESConstants.DEFAULT_XML_SIGNATURE_PREFIX,
				digestMethodAlgorithm,
				root.getOwnerDocument(),
				root,
				(X509Certificate) certChain[0]
				);

		// Metadatos de firma
		XAdESCommonMetadataUtil.addCommonMetadata(xades, extraParams);

		// Agregamos los elementos creados con los tipos de los datos firmados
		if (!objectFormats.isEmpty()) {
			xades.setDataObjectFormats(objectFormats);
		}

		// crea la firma
		final AOXMLAdvancedSignature xmlSignature = XAdESUtil.getXmlAdvancedSignature(
			xades,
			signedPropertiesType,
			digestMethodAlgorithm,
			canonicalizationAlgorithm,
			uriDereferencer
		);

		// Si se creo un nuevo objeto para incluir en la firma (caso de las
		// cofirmas enveloping y de manifest, en las que se copia el objeto
		// que se firma), lo agregamos
		if (newInternalObject != null) {
			xmlSignature.addXMLObject(newInternalObject);
		}

		try {
			final boolean onlySignningCert = Boolean.parseBoolean(
				extraParams.getProperty(
					XAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE,
					Boolean.FALSE.toString()
				)
			);
			if (onlySignningCert) {
				xmlSignature.sign(
						(X509Certificate) certChain[0],
						pk,
						algoUri,
						referenceList,
						"Signature-" + UUID.randomUUID().toString() //$NON-NLS-1$
						);
			}
			else {
				xmlSignature.sign(
					Arrays.asList(certChain),
					pk,
					algoUri,
					referenceList,
					"Signature-" + UUID.randomUUID().toString(), //$NON-NLS-1$
					addKeyInfoKeyValue,
					addKeyInfoKeyName,
					addKeyInfoX509IssuerSerial,
					keepKeyInfoUnsigned,
					validatePkcs1
				);
			}
		}
		catch (final NoSuchAlgorithmException e) {
			throw new IllegalArgumentException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "':" + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch (final AOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la cofirma XAdES", e); //$NON-NLS-1$
		}

		return Utils.writeXML(
			root,
			originalXMLProperties,
			null,
			null
		);
	}

	/**
	 * Hace una copia del manifest de una firma.
	 * @param referenceUri URI del manifest.
	 * @param signatureElement Firma en la que se encuentra el manifest.
	 * @return Copia del elemento manifest.
	 * @throws AOException Cuando no se puede localizar el manifest en la firma.
	 */
	private static Element copyManifest(final String referenceUri, final Element signatureElement) throws AOException {
		// Obtenemos el manifest de la firma original
		if (!referenceUri.startsWith("#")) { //$NON-NLS-1$
			throw new AOException("La URI de la firma original que referencia al manifest debe ser local"); //$NON-NLS-1$
		}
		final String manifestId = referenceUri.substring(1);
		final Element manifestElement = XAdESUtil.findElementById(manifestId, signatureElement, false);
		if (manifestElement == null) {
			throw new AOException("No se encontro el manifest dentro de la firma"); //$NON-NLS-1$
		}

		// Clonamos el nodo del manifest
		return (Element) manifestElement.cloneNode(true);
	}

	/**
	 * Obtiene una copia de los elementos DataObjectFormat declarados en una
	 * firma.
	 * @param signedPropertiesElement Propiedades firmadas de la firma de la
	 * que copiar los elementos.
	 * return Listado de DataObjectFormat.
	 */
	private static ArrayList<DataObjectFormat> copyDataObjectFormats(final Element signedPropertiesElement) {

		// Identificamos el nodo de los atributos firmados de los datos
		Element signedDataObjectPropertiesElement = null;
		final NodeList signedPropertiesNodeList = signedPropertiesElement.getChildNodes();
		for (int i = 0; signedDataObjectPropertiesElement == null && i < signedPropertiesNodeList.getLength(); i++) {
			if (signedPropertiesNodeList.item(i).getNodeType() == Node.ELEMENT_NODE
					&& XAdESConstants.TAG_SIGNED_DATA_OBJECT_PROPERTIES.equals(signedPropertiesNodeList.item(i).getLocalName())) {
				signedDataObjectPropertiesElement = (Element) signedPropertiesNodeList.item(i);
			}
		}

		final ArrayList<DataObjectFormat> dataObjectFormats = new ArrayList<>();

		// Si la firma tenia declaramos los tipos de los datos, replicamos esta
		// informacion en la nueva firma
		if (signedDataObjectPropertiesElement != null) {
			final NodeList dataObjectFormatNodeList = signedDataObjectPropertiesElement.getElementsByTagNameNS(
					signedDataObjectPropertiesElement.getNamespaceURI(),
					XAdESConstants.TAG_DATA_OBJECT_FORMAT);

			// Recorremos el listado de elementos con los tipos de datos y
			// agregamos los mismos parametros a la nueva firma
			for (int i = 0; i < dataObjectFormatNodeList.getLength(); i++) {
				final DataObjectFormat dataObjectFormat = DataObjectFormatParser.parseDataObjectFormat(
						(Element) dataObjectFormatNodeList.item(i));
				dataObjectFormats.add(dataObjectFormat);
			}
		}

		return dataObjectFormats;
	}


	private static String renewManifestIds(final Element newManifestElement, final ArrayList<DataObjectFormat> dataObjectFormats) {

		// Actualizamos el ID del nuevo manifest
		final String newManifestId = "Manifest-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		newManifestElement.setAttribute(XAdESConstants.ID_IDENTIFIER, newManifestId);

		// Recorremos las referencias del manifest y actualizamos sus Ids a la
		// vez que actualizamos las de los formatos datos asociados
		final NodeList references = newManifestElement.getElementsByTagNameNS(newManifestElement.getNamespaceURI(), XAdESConstants.TAG_REFERENCE);
		for (int i = 0; i < references.getLength(); i++) {
			final Element reference = (Element) references.item(i);
			final String referenceId = reference.getAttribute(XAdESConstants.ID_IDENTIFIER);
			if (referenceId != null) {
				final String newReferenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
				reference.setAttribute(XAdESConstants.ID_IDENTIFIER, newReferenceId);
				for (int j = 0; j < dataObjectFormats.size(); j++) {
					final DataObjectFormat objectFormat = dataObjectFormats.get(j);
					final String objectReference = objectFormat.getObjectReference();
					if (objectReference != null && objectReference.equals("#" + referenceId)) { //$NON-NLS-1$
						final DataObjectFormat newObjectFormat = new DataObjectFormatImpl(
								objectFormat.getDescription(),
								objectFormat.getObjectIdentifier(),
								objectFormat.getMimeType(),
								objectFormat.getEncoding(),
								"#" + newReferenceId); //$NON-NLS-1$
						dataObjectFormats.set(j, newObjectFormat);
					}
				}
			}
		}

		return newManifestId;
	}

	/**
	 * Crea un nuevo objeto de firma.
	 * @param contentElement Elemento que se incorpora al objeto de firma.
	 * @param fac Factoria de la firma a la que se agregar&aacute; el objeto.
	 * @param mimeType Mimetype de los datos o {@code null} si no hay que declararlo.
	 * @param encoding Codificaci&oacute;n de los datos o {@code null} si no hay que declararla.
	 * @return Objeto de firma.
	 */
	private static XMLObject createSignatureObject(final Element contentElement, final XMLSignatureFactory fac, final String mimeType, final String encoding) {

		final List<XMLStructure> structures = new ArrayList<>(1);
		structures.add(new DOMStructure(contentElement));
		final String objectId = "ManifestObject-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		return fac.newXMLObject(
				structures,
				objectId,
				mimeType,
				encoding
				);
	}


	private static void addReferenceDataObjectFormat(final List<DataObjectFormat> objectFormats, final String referenceId,
			final String mimeType, final String fixedOid, final String encoding) {

		String oid = fixedOid;
		if (oid == null && mimeType != null) {
			try {
				oid = MimeHelper.transformMimeTypeToOid(mimeType);
			} catch (final IOException e) {
				LOGGER.warning("Error en la obtencion del OID del tipo de datos a partir del MimeType: " + e); //$NON-NLS-1$
			}
		}

		ObjectIdentifier objectIdentifier = null;
		if (oid != null) {
			objectIdentifier = new ObjectIdentifierImpl(
					"OIDAsURN", (oid.startsWith("urn:oid:") ? "" : "urn:oid:") + oid, null, new ArrayList<String>(0)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}

		// Cremos los elementos descriptivos de los datos
		final DataObjectFormat objectFormat = new DataObjectFormatImpl(
				null,
				objectIdentifier,
				mimeType,
				encoding,
				"#" + referenceId //$NON-NLS-1$
				);
		objectFormats.add(objectFormat);
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

		// En las cofirmas siempre se usara el espacio de nombres y la URL del
		// tipo de datos firmados de la firma original
		if (extraParams.containsKey(XAdESExtraParams.XADES_NAMESPACE)) {
			LOGGER.warning("Se ignorara el espacio de nombres indicado. En las cofirmas siempre se usara el mismo espacio de nombres que la firma original"); //$NON-NLS-1$
			extraParams.remove(XAdESExtraParams.XADES_NAMESPACE);
		}
		if (extraParams.containsKey(XAdESExtraParams.SIGNED_PROPERTIES_TYPE_URL)) {
			LOGGER.warning("Se ignorara la URL indicada para el tipo SignedProperties. En las cofirmas siempre se usara la misma URL que la firma original"); //$NON-NLS-1$
			extraParams.remove(XAdESExtraParams.SIGNED_PROPERTIES_TYPE_URL);
		}
	}
}
