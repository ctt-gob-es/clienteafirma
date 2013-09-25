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
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import net.java.xades.security.xml.XAdES.DataObjectFormat;
import net.java.xades.security.xml.XAdES.DataObjectFormatImpl;
import net.java.xades.security.xml.XAdES.ObjectIdentifierImpl;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifier;
import net.java.xades.security.xml.XAdES.SignatureProductionPlace;
import net.java.xades.security.xml.XAdES.SignerRole;
import net.java.xades.security.xml.XAdES.SignerRoleImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.xml.InvalidXMLException;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.Utils.CannotDereferenceException;
import es.gob.afirma.signers.xml.Utils.IsInnerlException;
import es.gob.afirma.signers.xml.Utils.ReferenceIsNotXMLException;
import es.gob.afirma.signers.xml.XMLConstants;

/** Firmador simple XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XAdESSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	/** Firma datos en formato XAdES.
	 * <p>
	 * Este m&eacute;todo, al firmar un XML, firmas tambi&eacute;n sus hojas de
	 * estilo XSL asociadas, siguiendo el siguiente criterio:
	 * <ul>
	 * <li>Firmas XML <i>Enveloped</i></li>
	 * <ul>
	 * <li>Hoja de estilo con ruta relativa</li>
	 * <ul>
	 * <li>No se firma.</li>
	 * </ul>
	 * <li>Hola de estilo remota con ruta absoluta</li>
	 * <ul>
	 * <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba
	 * en el XML original</li>
	 * <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
	 * </ul>
	 * <li>Hoja de estilo empotrada</li>
	 * <ul>
	 * <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba
	 * en el XML original</li>
	 * </ul>
	 * </ul>
	 * <li>Firmas XML <i>Externally Detached</i></li>
	 * <ul>
	 * <li>Hoja de estilo con ruta relativa</li>
	 * <ul>
	 * <li>No se firma.</li>
	 * </ul>
	 * <li>Hola de estilo remota con ruta absoluta</li>
	 * <ul>
	 * <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
	 * </ul>
	 * <li>Hoja de estilo empotrada</li>
	 * <ul>
	 * <li>No es necesaria ninguna acci&oacute;n</li>
	 * </ul>
	 * </ul>
	 * <li>Firmas XML <i>Enveloping</i></li>
	 * <ul>
	 * <li>Hoja de estilo con ruta relativa</li>
	 * <ul>
	 * <li>No se firma.</li>
	 * </ul>
	 * <li>Hola de estilo remota con ruta absoluta</li>
	 * <ul>
	 * <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
	 * </ul>
	 * <li>Hoja de estilo empotrada</li>
	 * <ul>
	 * <li>No es necesaria ninguna acci&oacute;n</li>
	 * </ul>
	 * </ul>
	 * <li>Firmas XML <i>Internally Detached</i></li>
	 * <ul>
	 * <li>Hoja de estilo con ruta relativa</li>
	 * <ul>
	 * <li>No se firma.</li>
	 * </ul>
	 * <li>Hola de estilo remota con ruta absoluta</li>
	 * <ul>
	 * <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
	 * </ul>
	 * <li>Hoja de estilo empotrada</li>
	 * <ul>
	 * <li>No es necesaria ninguna acci&oacute;n</li>
	 * </ul>
	 * </ul> </ul>
	 * </p>
	 *
	 * @param data
	 *            Datos que deseamos firmar.
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
	 * @param certChain Cadena de certificados del firmante
	 * @param pk Clave privada del firmante
	 * @param xParams
	 *            Par&aacute;metros adicionales para la firma.
	 *            <p>
	 *            Se aceptan los siguientes valores en el par&aacute;metro
	 *            <code>xParams</code>:
	 *            </p>
	 *            <dl>
	 *            <dt><b><i>avoidXpathExtraTransformsOnEnveloped</i></b></dt>
	 *            <dd>
	 *             Indica si se debe evitar la inclusi&oacute;n de la transformaci&oacute;n
	 *             XPATH2 que normalmente se a&ntilde;ade para posibilitar las cofirmas y
	 *             que elimina todas las firmas del documento para dejar &uacute;nicamente
	 *             el contenido. Por defecto, se encuentra a {@code false}.
	 *            </dd>
	 *            <dt><b><i>nodeToSign</i></b></dt>
	 *            <dd>
	 *             Indica un nombre de nodo a firmar, para el caso en el que quiera firmarse
	 *             un nodo concreto en vez de todo el documento. Solo aplica a la firma de
	 *             documentos XML.
	 *             <br>
	 *             El nombre nodo debe estar indicado con el valor de un atributo llamado <i>Id</i>,
	 *             y se ignorar&aacute;n los atributos con nombre distinto, aunque se hayan declarado
	 *             en el esquema como identificadores (con una l&iacute;nea del tipo
	 *             <code>&lt;xs:attribute name="otronombre" type="xs:ID"/&gt;</code>), para evitar
	 *             conflictos con el esquema de XAdES y XMLDSig.
	 *            </dd>
	 *            <dt><b><i>uri</i></b></dt>
	 *            <dd>URL en la que se encuentra el documento a firmar,
	 *            necesario en el caso del formato <i>XAdES Externally
	 *            Detached</i>.</dd>
	 *            <dt><b><i>format</i></b></dt>
	 *            <dd>
	 *            Formato de firma. Se aceptan los siguientes valores:<br>
	 *            <ul>
	 *            <li>
	 *            <i>XAdES Detached</i> (
	 *            <code>AOSignConstants.SIGN_FORMAT_XADES_DETACHED</code>)</li>
	 *            <li>
	 *            <i>XAdES Externally Detached</i> (
	 *            <code>AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED</code>)
	 *            <p>
	 *            Para el uso del formato <i>XAdES Externally Detached</i> es
	 *            necesario establecer tambi&eacute;n el par&aacute;metro
	 *            <code>uri</code> con una direcci&oacute;n accesible
	 *            universalmente.
	 *            </p>
	 *            </li>
	 *            <li>
	 *            <i>XAdES Enveloped</i> (
	 *            <code>AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED</code>)</li>
	 *            <li>
	 *            <i>XAdES Enveloping</i> (
	 *            <code>AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING</code>)</li>
	 *            </ul>
	 *            </dd>
	 *            <dt><b><i>policyIdentifier</i></b></dt>
	 *            <dd>Identificador de la pol&iacute;tica de firma (normalmente
	 *            una URL hacia la pol&iacute;tica en formato XML procesable).</dd>
	 *            <dt><b><i>policyIdentifierHash</i></b></dt>
	 *            <dd>
	 *            Huella digital del documento de pol&iacute;tica de firma
	 *            (normlamente del mismo fichero en formato XML procesable). Si
	 *            no se indica, es obligatorio que el par&aacute;metro
	 *            <code>policyIdentifier</code> sea una URL accesible
	 *            universalmente.</dd>
	 *            <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
	 *            <dd>Algoritmo usado para el c&aacute;lculo de la huella
	 *            digital indicada en el par&aacute;metro
	 *            <code>policyIdentifierHash</code>
	 *            <dt><b><i>policyDescription</i></b></dt>
	 *            <dd>Descripci&oacute;n textual de la pol&iacute;tica.</dd>
	 *            <dt><b><i>policyQualifier</i></b></dt>
	 *            <dd>URL hacia el documento (legible por personas, normalmente
	 *            en formato PDF) descriptivo de la pol&iacute;tica de firma.</dd>
	 *            <dt><b><i>includeOnlySignningCertificate</i></b></dt>
	 *            <dd>Indica, mediante un {@code true} o {@code false}, que debe
	 *            indicarse en la firma &uacute;nicamente el certificado utilizado
	 *            para firmar y no su cadena de certificaci&oacute;n completa.
	 *            Por defecto, se incluir&aacute; toda la cadena de certificaci&oacute;n.</dd>
	 *            <dt><b><i>facturaeSign</i></b></dt>
	 *            <dd>Indica, mediante un {@code true} o {@code false}, si se
	 *            deben realizar las restricciones de comportamiento necesarias
	 *            para la firma de facturas electr&oacute;nicas (FACTURAe).
	 *            Estas restricciones son, no introducir la
	 *            transformaci&oacute;n de canonicalizaci&oacute;n de la firma,
	 *            ni la transformaci&oacute;n XPATH en las firmas Enveloped.</dd>
	 *            <dt><b><i>signerClaimedRole</i></b></dt>
	 *            <dd>Cargo atribuido para el firmante.</dd>
	 *            <dt><b><i>signerCertifiedRole</i></b></dt>
	 *            <dd>Cargo confirmado para el firmante.</dd>
	 *            <dt><b><i>precalculatedHashAlgorithm</i></b></dt>
	 *            <dd>Algoritmo de huella digital cuando esta se proporciona
	 *            precalculada.</dd>
	 *            <dt><b><i>signatureProductionCity</i></b></dt>
	 *            <dd>Ciudad en la que se realiza la firma.</dd>
	 *            <dt><b><i>signatureProductionProvince</i></b></dt>
	 *            <dd>Provincia en la que se realiza la firma.</dd>
	 *            <dt><b><i>signatureProductionPostalCode</i></b></dt>
	 *            <dd>C&oacute;digo postal en el que se realiza la firma.</dd>
	 *            <dt><b><i>signatureProductionCountry</i></b></dt>
	 *            <dd>Pa&iacute;s en el que se realiza la firma.</dd>
	 *            <dt><b><i>xmlTransforms</i></b></dt>
	 *            <dd>N&uacute;mero de transformaciones a aplicar al XML antes
	 *            de firmarlo.</dd>
	 *            <dt><b><i>xmlTransform</i>n<i>Type</i></b></dt>
	 *            <dd>Tipo de la transformaci&oacute;n <i>n</i> (debe ser la URL
	 *            del algoritmo segun define W3C).</dd>
	 *            <dt><b><i>xmlTransform</i>n<i>Subtype</i></b></dt>
	 *            <dd>Subtipo de la transformaci&oacute;n <i>n</i> (por ejemplo,
	 *            "intersect", "subtract" o "union" para XPATH2).</dd>
	 *            <dt><b><i>xmlTransform</i>n<i>Body</i></b></dt>
	 *            <dd>Cuerpo de la transformaci&oacute;n <i>n</i>.</dd>
	 *            <dt><b><i>referencesDigestMethod</i></b></dt>
	 *            <dd>
	 *            Algoritmo de huella digital a usar en las referencias XML
	 *            (referencesDigestMethod). Debe indicarse como una URL,
	 *            acept&aacute;ndose los siguientes valores:
	 *            <ul>
	 *            <li><i>http://www.w3.org/2000/09/xmldsig#sha1</i> (SHA-1)</li>
	 *            <li><i>http://www.w3.org/2001/04/xmlenc#sha256</i> (SHA-256,
	 *            valor recomendado)</li>
	 *            <li><i>http://www.w3.org/2001/04/xmlenc#sha512</i> (SHA-512)</li>
	 *            <li><i>http://www.w3.org/2001/04/xmlenc#ripemd160
	 *            (RIPEMD-160)</i></li>
	 *            </ul>
	 *            </dd>
	 *            <dt><b><i>mimeType</i></b></dt>
	 *            <dd>
	 *            MIME-Type de los datos a firmar. Si no se indica se realiza
	 *            una auto-detecci&oacute;n cuyo resultado puede ser inexacto.</dd>
	 *            <dt><b><i>encoding</i></b></dt>
	 *            <dd>
	 *            Codificaci&oacute;n de los datos a firmar.</dd>
	 *            <dt><b><i>contentTypeOid</i></b>
	 *            <dt>
	 *            <dd>OID que identifica el tipo de datos a firmar.</dd>
	 *            <dt><b><i>canonicalizationAlgorithm</i></b></dt>
	 *            <dd>Algoritmo de canonicalizaci&oacute;n.</dd>
	 *            <dt><b><i>xadesNamespace</i></b></dt>
	 *            <dd>
	 *            URL de definici&oacute;n del espacio de nombres de XAdES (y
	 *            por extensi&oacute;n, versi&oacute;n de XAdES). Si se
	 *            establece este par&aacute;metro es posible que se necesite
	 *            establecer tambi&eacute;n el par&aacute;metro
	 *            <code>signedPropertiesTypeUrl</code> para evitar incoherencias
	 *            en la versi&oacute;n de XAdES.</dd>
	 *            <dt><b><i>signedPropertiesTypeUrl</i></b></dt>
	 *            <dd>
	 *            URL de definici&oacute;n del tipo de las propiedades firmadas
	 *            (<i>Signed Properties</i>) de XAdES. Si se establece este
	 *            par&aacute;metro es posible que se necesite establecer
	 *            tambi&eacute;n el par&aacute;metro <code>xadesNamespace</code>
	 *            para evitar incoherencias en la versi&oacute;n de XAdES.<br>
	 *            Si no se establece se usa el valor por defecto: <a
	 *            href="http://uri.etsi.org/01903#SignedProperties"
	 *            >http://uri.etsi.org/01903#SignedProperties</a>.</dd>
	 *            <dt><b><i>ignoreStyleSheets</i></b></dt>
	 *            <dd>
	 *            Ignora las hojas de estilo externas de los XML (no las firma)
	 *            si se establece a <code>true</code>, si se establece a
	 *            <code>false</code> act&uacute;a normalmente (s&iacute; las
	 *            firma).</dd>
	 *            <dt><b><i>avoidBase64Transforms</i></b></dt>
	 *            <dd>
	 *            No declara transformaciones Base64 incluso si son necesarias
	 *            si se establece a <code>true</code>, si se establece a
	 *            <code>false</code> act&uacute;a normalmente (s&iacute; las
	 *            declara).</dd>
	 *            <dt><b><i>headLess</i></b></dt>
	 *            <dd>
	 *            Evita cualquier interacci&oacute;n con el usuario si se
	 *            establece a <code>true</code>, si se establece a
	 *            <code>false</code> act&uacute;a normalmente (puede mostrar
	 *            di&aacute;logos, por ejemplo, para la dereferenciaci&oacute;n
	 *            de hojas de estilo enlazadas con rutas relativas). &Uacute;til
	 *            para los procesos desatendidos y por lotes.</dd>
	 *            <dt><b><i>applySystemDate</i></b></dt>
	 *            <dd>
	 *            Indica si se debe introducir en la firma el atributo
	 *            <i>signingTime</i> con la fecha actual del sistema. Por
	 *            defecto, se encuentra a {@code true}.</dd>
	 *            </dl>
	 *            <p>
	 *            Respecto al uso de los par&aacute;metros
	 *            <code>xmlTransform</code>n<code>Type</code>,
	 *            <code>xmlTransform</code>n<code>Subtype</code> y
	 *            <code>xmlTransform</code>n<code>Body</code>, sus valores van
	 *            ligados, acept&aacute;ndose las siguientes combinaciones:
	 *            </p>
	 *            <p>
	 *            Transformaci&oacute;n <b>XPATH</b><br>
	 *            &nbsp;&nbsp;-<b>Tipo</b>:
	 *            <code>http://www.w3.org/TR/1999/REC-xpath-19991116</code><br>
	 *            &nbsp;&nbsp;-<b>Subtipos</b>: No tiene subtipos.<br>
	 *            &nbsp;&nbsp;-<b>Cuerpo</b>: Especificado mediante sentencias
	 *            de tipo XPATH.<br>
	 *            <br>
	 *            Transformaci&oacute;n <b>XPATH2</b><br>
	 *            &nbsp;&nbsp;-<b>Tipo</b>:
	 *            <code>http://www.w3.org/2002/06/xmldsig-filter2</code><br>
	 *            &nbsp;&nbsp;-<b>Subtipos</b>:<br>
	 *            &nbsp;&nbsp;&nbsp;&nbsp;<b><i>subtract</i></b>: Resta.<br>
	 *            &nbsp;&nbsp;&nbsp;&nbsp;<b><i>intersect</i></b>:
	 *            Intersecci&oacute;n<br>
	 *            &nbsp;&nbsp;&nbsp;&nbsp;<b><i>union</i></b>: Uni&oacute;n<br>
	 *            &nbsp;&nbsp;-<b>Cuerpo</b>: Especificado mediante sentencias
	 *            de tipo XPATH2.<br>
	 *            <br>
	 *            Transformaci&oacute;n <b>XSLT</b><br>
	 *            &nbsp;&nbsp;-<b>Tipo</b>:
	 *            <code>http://www.w3.org/TR/1999/REC-xslt-19991116</code><br>
	 *            &nbsp;&nbsp;-<b>Subtipos</b>: No tiene subtipos.<br>
	 *            &nbsp;&nbsp;-<b>Cuerpo</b>: Especificado mediante sentencias
	 *            de tipo XSLT.<br>
	 *            <br>
	 *            Transformaci&oacute;n <b>BASE64</b><br>
	 *            &nbsp;&nbsp;-<b>Tipo</b>:
	 *            <code>http://www.w3.org/2000/09/xmldsig#base64</code><br>
	 *            &nbsp;&nbsp;-<b>Subtipos</b>: No tiene subtipos.<br>
	 *            &nbsp;&nbsp;-<b>Cuerpo</b>: No tiene cuerpo.
	 *            </p>
	 *            <p>
	 *            No es posible especificar transformaciones complejas que
	 *            incluyan varias sentencias. En su lugar, puede declararse una
	 *            sucesi&oacute;n de transformaciones simples que produzcan el
	 *            mismo resultado. Cada una de las transformaciones se
	 *            aplicar&aacute; de forma ordenada sobre el resultado de la
	 *            anterior.
	 *            </p>
	 * @return Firma en formato XAdES
	 * @throws AOException
	 *             Cuando ocurre cualquier problema durante el proceso
	 */
	public static byte[] sign(final byte[] data,
			                  final String algorithm,
			                  final PrivateKey pk,
			                  final Certificate[] certChain,
			                  final Properties xParams) throws AOException {

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final boolean avoidXpathExtraTransformsOnEnveloped = Boolean.parseBoolean(extraParams.getProperty(
				"avoidXpathExtraTransformsOnEnveloped", Boolean.FALSE.toString())); //$NON-NLS-1$
		final String nodeToSign = extraParams.getProperty(
				"nodeToSign"); //$NON-NLS-1$
		final String format = extraParams.getProperty(
				"format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
		final String mode = extraParams.getProperty(
				"mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		final String digestMethodAlgorithm = extraParams.getProperty(
				"referencesDigestMethod", AOXAdESSigner.DIGEST_METHOD); //$NON-NLS-1$
		final String canonicalizationAlgorithm = extraParams.getProperty(
				"canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$
		final String xadesNamespace = extraParams.getProperty(
				"xadesNamespace", AOXAdESSigner.XADESNS); //$NON-NLS-1$
		final String signedPropertiesTypeUrl = extraParams.getProperty(
				"signedPropertiesTypeUrl", AOXAdESSigner.XADES_SIGNED_PROPERTIES_TYPE); //$NON-NLS-1$
		final boolean ignoreStyleSheets = Boolean.parseBoolean(extraParams.getProperty(
				"ignoreStyleSheets", Boolean.TRUE.toString())); //$NON-NLS-1$
		final boolean avoidBase64Transforms = Boolean.parseBoolean(extraParams.getProperty(
				"avoidBase64Transforms", Boolean.FALSE.toString())); //$NON-NLS-1$
		final boolean headLess = Boolean.parseBoolean(extraParams.getProperty(
				"headLess", Boolean.TRUE.toString())); //$NON-NLS-1$
		final String precalculatedHashAlgorithm = extraParams.getProperty(
				"precalculatedHashAlgorithm"); //$NON-NLS-1$
		final boolean facturaeSign = Boolean.parseBoolean(extraParams.getProperty(
				"facturaeSign", Boolean.FALSE.toString())); //$NON-NLS-1$
		String mimeType = extraParams.getProperty(
				"mimeType", XMLConstants.DEFAULT_MIMETYPE); //$NON-NLS-1$
		String encoding = extraParams.getProperty(
				"encoding"); //$NON-NLS-1$
		if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
			encoding = XMLConstants.BASE64_ENCODING;
		}

		URI uri = null;
		try {
			uri = AOUtil.createURI(extraParams.getProperty("uri")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			// Ignoramos errores, el parametro es opcional
		}

		Utils.checkIllegalParams(format, mode, uri, precalculatedHashAlgorithm, true);

		// Un externally detached con URL permite los datos nulos o vacios
		if ((data == null || data.length == 0)
				&& !(format.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED) && uri != null)) {
			throw new AOException("No se han podido leer los datos a firmar"); //$NON-NLS-1$
		}

		// Propiedades del documento XML original
		final Map<String, String> originalXMLProperties = new Hashtable<String, String>();

		// carga el documento xml
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		// Elemento de datos
		Element dataElement;

		final String contentId = AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME
				+ "-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		final String styleId = AOXAdESSigner.DETACHED_STYLE_ELEMENT_NAME
				+ "-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		boolean isBase64 = false;
		boolean wasEncodedToBase64 = false;

		// Elemento de estilo
		Element styleElement = null;
		String styleType = null;
		String styleHref = null;
		String styleEncoding = null;

		if (mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
			try {

				// Obtenemos el objeto XML
				final Document docum = dbf.newDocumentBuilder().parse(
					new ByteArrayInputStream(data)
				);

				if (nodeToSign != null && XAdESUtil.getElementById(docum, nodeToSign) == null) {
					throw new InvalidXMLException(
						"El nodo XML indicado para su firma (" + nodeToSign + ") no existe" //$NON-NLS-1$ //$NON-NLS-2$
					);
				}

				// ************************************************
				// **** Obtencion de la hoja de estilo del XML ****
				// ************************************************
				try {
					final Properties p;
					if (!ignoreStyleSheets) {
						p = Utils.getStyleSheetHeader(new String(data));
					}
					else {
						p = new Properties();
					}
					styleType = p.getProperty("type"); //$NON-NLS-1$
					styleHref = p.getProperty("href"); //$NON-NLS-1$

					if (styleType != null && styleHref != null) {

						XAdESSigner.LOGGER.info(
							"Se ha encontrado una hoja de estilo asociada al XML a firmar: tipo=" + styleType //$NON-NLS-1$
								+ ", referencia=" //$NON-NLS-1$
								+ styleHref
						);

						try {
							final Document tmpDoc = Utils.dereferenceStyleSheet(
								TransformerFactory.newInstance().getAssociatedStylesheet(
									new StreamSource(
										new ByteArrayInputStream(data)
									),
									null,
									null,
									null
								).getSystemId(),
								headLess
							);

							// Cuidado!! Solo rellenamos el Elemento DOM si no es HTTP o HTTPS,
							// porque si es accesible remotamente no necesito el elemento, ya que se
							// firma via referencia Externally Detached
							if (!styleHref.startsWith(AOXAdESSigner.HTTP_PROTOCOL_PREFIX) &&
								!styleHref.startsWith(AOXAdESSigner.HTTPS_PROTOCOL_PREFIX)) {
								styleElement = tmpDoc.getDocumentElement();
							}

							styleEncoding = tmpDoc.getXmlEncoding();
						}
						catch (final IsInnerlException ex) {
							XAdESSigner.LOGGER.info(
								"La hoja de estilo esta referenciada internamente, por lo que no se necesita dereferenciar" //$NON-NLS-1$
							);
						}
						catch (final ReferenceIsNotXMLException ex) {
							XAdESSigner.LOGGER.warning(
								"La hoja de estilo referenciada no es XML o no se ha dereferenciado apropiadamente" //$NON-NLS-1$
							);
						}
						catch (final CannotDereferenceException ex) {
							XAdESSigner.LOGGER.warning(
								"La hoja de estilo no ha podido dereferenciar, probablemente sea un enlace relativo local" //$NON-NLS-1$
							);
						}
						catch (final Exception ex) {
							XAdESSigner.LOGGER.severe(
								"Error intentando dereferenciar la hoja de estilo: " + ex //$NON-NLS-1$
							);
						}
					}
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.info(
						"No se ha encontrado ninguna hoja de estilo asociada al XML a firmar" //$NON-NLS-1$
					);
				}
				// ************************************************
				// ** Fin obtencion de la hoja de estilo del XML **
				// ************************************************

				// Si no hay asignado un MimeType o es el por defecto
				// establecemos el de XML
				if (mimeType == null || XMLConstants.DEFAULT_MIMETYPE.equals(mimeType)) {
					mimeType = "text/xml"; //$NON-NLS-1$
				}

				// Obtenemos el encoding del documento original
				if (encoding == null) {
					encoding = docum.getXmlEncoding();
				}

				// Hacemos la comprobacion del Base64 por si se establecido desde fuera
				if (encoding != null && !XMLConstants.BASE64_ENCODING.equals(encoding)) {
					originalXMLProperties.put(OutputKeys.ENCODING, encoding);
				}

				String tmpXmlProp = docum.getXmlVersion();
				if (tmpXmlProp != null) {
					originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
				}
				final DocumentType dt = docum.getDoctype();
				if (dt != null) {
					tmpXmlProp = dt.getSystemId();
					if (tmpXmlProp != null) {
						originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
					}
				}

				// En Detached se crea un elemento nuevo que contiene los datos
				if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)) {
					dataElement = docum.createElement(AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME);
					dataElement.setAttributeNS(null, "Id", contentId); //$NON-NLS-1$
					dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);
					if (encoding != null && !"".equals(encoding)) { //$NON-NLS-1$
						dataElement.setAttributeNS(null, AOXAdESSigner.ENCODING_STR, encoding);
					}

					dataElement.appendChild(docum.getDocumentElement());

					// Tambien el estilo
					if (styleElement != null) {
						try {
							final Element tmpStyleElement = docum.createElement(AOXAdESSigner.DETACHED_STYLE_ELEMENT_NAME);
							tmpStyleElement.setAttributeNS(null, "Id", styleId); //$NON-NLS-1$
							if (styleType != null) {
								tmpStyleElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, styleType);
							}
							tmpStyleElement.setAttributeNS(null, AOXAdESSigner.ENCODING_STR, styleEncoding);
							tmpStyleElement.appendChild(docum.adoptNode(styleElement.cloneNode(true)));
							styleElement = tmpStyleElement;
						}
						catch (final Exception e) {
							XAdESSigner.LOGGER.warning(
								"No ha sido posible crear el elemento DOM para incluir la hoja de estilo del XML como Internally Detached: " + e //$NON-NLS-1$
							);
							styleElement = null;
						}
					}
				}
				// En cualquier otro caso los datos a firmar son o el XML inicial o el
				// nodo especifico indicado
				else {
					dataElement = docum.getDocumentElement();
				}

			}
			// Captura de error en caso de no ser un documento XML
			// **********************************************************
			// ********* Contenido no XML *******************************
			// **********************************************************
			catch (final Exception e) {
				// Error cuando los datos no son XML y se pide firma enveloped o si se pide firmar
				// un nodo XML especifico
				if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) || nodeToSign != null) {
					throw new InvalidXMLException(e);
				}
				// Para los formatos de firma internally detached y enveloping
				// se trata de convertir el documento a base64
				XAdESSigner.LOGGER.info(
					"El documento no es un XML valido. Se convertira a Base64: " + e //$NON-NLS-1$
				);

				try {
					// Crea un nuevo nodo XML para contener los datos en base 64
					final Document docFile = dbf.newDocumentBuilder().newDocument();
					dataElement = docFile.createElement(AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME);
					uri = null;
					if (mimeType == null) {
						mimeType = XMLConstants.DEFAULT_MIMETYPE;
					}

					dataElement.setAttributeNS(null, "Id", contentId); //$NON-NLS-1$

					// Si es base 64, lo firmamos indicando como contenido el dato pero, ya que puede
					// poseer un formato particular o caracteres valido pero extranos para el XML,
					// realizamos una decodificacion y recodificacion para asi homogenizar el formato.
					if (AOUtil.isBase64(data)
							&& (XMLConstants.BASE64_ENCODING.equals(encoding) || "base64".equalsIgnoreCase(encoding))) { //$NON-NLS-1$
						XAdESSigner.LOGGER.info(
							"El documento se ha indicado como Base64, se insertara como tal en el XML" //$NON-NLS-1$
						);

						// Adicionalmente, si es un base 64 intentamos obtener el tipo del contenido
						// decodificado para asi reestablecer el MimeType.
						final byte[] decodedData = Base64.decode(new String(data));
						final MimeHelper mimeTypeHelper = new MimeHelper(decodedData);
						final String tempMimeType = mimeTypeHelper.getMimeType();
						mimeType = tempMimeType != null ? tempMimeType : XMLConstants.DEFAULT_MIMETYPE;
						dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);
						dataElement.setTextContent(Base64.encode(decodedData));
					}
					else {
						if (XMLConstants.BASE64_ENCODING.equals(encoding)) {
							XAdESSigner.LOGGER.info(
								"El documento se ha indicado como Base64, pero no es un Base64 valido. Se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion" //$NON-NLS-1$
							);
						}
						else {
							XAdESSigner.LOGGER.info(
								"El documento se considera binario, se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion" //$NON-NLS-1$
							);
						}

						if (mimeType == XMLConstants.DEFAULT_MIMETYPE) {
							final MimeHelper mimeTypeHelper = new MimeHelper(data);
							final String tempMimeType = mimeTypeHelper.getMimeType();
							mimeType = tempMimeType != null ? tempMimeType : XMLConstants.DEFAULT_MIMETYPE;
							dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);
						}

						dataElement.setTextContent(Base64.encode(data));
						wasEncodedToBase64 = true;
					}
					isBase64 = true;
					encoding = XMLConstants.BASE64_ENCODING;
					dataElement.setAttributeNS(null, AOXAdESSigner.ENCODING_STR, encoding);
				}
				catch (final Exception ex) {
					throw new AOException("Error al convertir los datos a base64", ex); //$NON-NLS-1$
				}
			}
		}
		// **********************************************************
		// ********* Fin contenido no XML ***************************
		// **********************************************************

		// Firma Explicita
		else {

			// No se firman nodos sueltos en firmas explicitas
			if (nodeToSign != null) {
				throw new AOException(
					"No se soporta la firma de nodos independientes en firmas XAdES explicitas" //$NON-NLS-1$
				);
			}

			// ESTE BLOQUE CONTIENE EL PROCESO A SEGUIR EN EL MODO EXPLICITO,
			// ESTO ES, NO FIRMAMOS LOS DATOS SINO SU HASH
			byte[] digestValue = null;
			// Si la URI no es nula recogemos los datos de fuera
			if (uri != null) {

				final byte[] tmpData;
				try {
					tmpData = AOUtil.getDataFromInputStream(AOUtil.loadFile(uri));
				}
				catch (final Exception e) {
					throw new AOException(
						"No se han podido obtener los datos de la URI externa '" + uri + "'", e //$NON-NLS-1$ //$NON-NLS-2$
					);
				}
				// Vemos si hemos obtenido bien los datos de la URI
				if (tmpData != null && tmpData.length > 0) {
					try {
						digestValue = MessageDigest.getInstance("SHA1").digest(tmpData); //$NON-NLS-1$
					}
					catch (final Exception e) {
						throw new AOException("No se ha podido obtener el SHA1 de los datos de la URI externa", e); //$NON-NLS-1$
					}
				}
			}
			// Si se nos ha introducido el messageDigest, firmamos este como si
			// fuesen los datos
			else if (precalculatedHashAlgorithm != null) {
				digestValue = data;
			}
			// El hash de los datos, ni una URI a traves de la que calcularlos,entonces lo calculamos
			// a traves de los datos introducidos (Siempre se calcula el SHA-1 de los datos)
			else {
				try {
					digestValue = MessageDigest.getInstance("SHA1").digest(data); //$NON-NLS-1$
				}
				catch (final Exception e) {
					throw new AOException(
						"No se ha podido obtener el SHA1 de los datos proporcionados", e //$NON-NLS-1$
					);
				}
			}

			if (digestValue == null || digestValue.length < 1) {
				throw new AOException(
					"Error al obtener la huella SHA1 de los datos" //$NON-NLS-1$
				);
			}

			final Document docFile;
			try {
				docFile = dbf.newDocumentBuilder().newDocument();
			}
			catch (final Exception e) {
				throw new AOException(
					"No se ha podido crear el documento XML contenedor", e //$NON-NLS-1$
				);
			}
			dataElement = docFile.createElement(AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME);

			encoding = XMLConstants.BASE64_ENCODING;
			// En el caso de la firma explicita, se firma el Hash de los datos en lugar de los propios datos.
			// En este caso, los indicaremos a traves del MimeType en donde establecemos un tipo especial
			// que designa al hash. Independientemente del algoritmo de firma utilizado, el Hash de las firmas
			// explicitas de datos siempre sera SHA1, salvo que el hash se haya establecido desde fuera.
			String hashAlgoUri;
			if (precalculatedHashAlgorithm != null) {
				mimeType = "hash/" + precalculatedHashAlgorithm.toLowerCase(); //$NON-NLS-1$
				hashAlgoUri = XMLConstants.MESSAGEDIGEST_ALGOS_URI.get(
					precalculatedHashAlgorithm.toLowerCase()
				);
			}
			else {
				mimeType = "hash/sha1"; //$NON-NLS-1$
				hashAlgoUri = XMLConstants.MESSAGEDIGEST_ALGOS_URI.get("sha1"); //$NON-NLS-1$
			}

			dataElement.setAttributeNS(null, "Id", contentId); //$NON-NLS-1$
			dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);
			dataElement.setAttributeNS(null, AOXAdESSigner.ENCODING_STR, encoding);
			if (hashAlgoUri != null) {
				dataElement.setAttributeNS(null, "hashAlgorithm", hashAlgoUri); //$NON-NLS-1$
			}
			dataElement.setTextContent(Base64.encode(digestValue));
			isBase64 = true;

			// FIN BLOQUE EXPLICITO
		}

		// ***************************************************
		// ***************************************************

		// La URI de contenido a firmat puede ser el nodo especifico si asi se indico o el
		// nodo de contenido completo
		final String tmpUri = "#" + (nodeToSign != null ? nodeToSign : contentId); //$NON-NLS-1$
		final String tmpStyleUri = "#" + styleId; //$NON-NLS-1$

		// Crea el nuevo documento de firma
		Document docSignature = null;
		try {
			docSignature = dbf.newDocumentBuilder().newDocument();
			if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)) {
				docSignature.appendChild(docSignature.adoptNode(dataElement));
			}
			else {
				docSignature.appendChild(docSignature.createElement(AOXAdESSigner.AFIRMA));
			}
		}
		catch (final Exception e) {
			throw new AOException(
				"Error al crear la firma en formato " + format + ", modo " + mode, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

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
		final String referenceStyleId = AOXAdESSigner.STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString();

		final List<Transform> transformList = new ArrayList<Transform>();

		// Primero anadimos las transformaciones a medida
		Utils.addCustomTransforms(transformList, extraParams, AOXAdESSigner.XML_SIGNATURE_PREFIX);

		final Transform canonicalizationTransform;
		try {
			canonicalizationTransform = fac.newTransform(
				canonicalizationAlgorithm,
				(TransformParameterSpec) null
			);
		}
		catch (final Exception e1) {
			throw new AOException(
				"No se ha posido crear el canonizador para el algoritmo indicado (" + canonicalizationAlgorithm + "): " + e1, e1 //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		// Solo canonicalizo si es XML
		if (!isBase64) {
			// Las facturas electronicas no se canonicalizan
			if (!facturaeSign) {
				try {
					// Transformada para la canonicalizacion inclusiva
					transformList.add(canonicalizationTransform);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
						"No se puede encontrar el algoritmo de canonicalizacion, la referencia no se canonicalizara: " + e //$NON-NLS-1$
					);
				}
			}
		}
		// Si no era XML y tuve que convertir a Base64 yo mismo declaro la
		// transformacion
		else if (wasEncodedToBase64 && !avoidBase64Transforms) {
			try {
				transformList.add(
					fac.newTransform(
						Transform.BASE64,
						(TransformParameterSpec) null
					)
				);
			}
			catch (final Exception e) {
				XAdESSigner.LOGGER.severe(
					"No se puede encontrar el algoritmo transformacion Base64, esta no se declarara: " + e //$NON-NLS-1$
				);
			}
		}

		// crea una referencia al documento insertado en un nodo Object para la
		// firma enveloping y a el estilo
		XMLObject envelopingObject = null;
		XMLObject envelopingStyleObject = null;

		if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
			try {
				// crea el nuevo elemento Object que contiene el documento a firmar
				final List<XMLStructure> structures = new ArrayList<XMLStructure>(1);

				// Si los datos se han convertido a base64, bien por ser
				// binarios o explicitos
				structures.add(
					new DOMStructure(
						isBase64 ? dataElement.getFirstChild() : dataElement
					)
				);

				final String objectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
				envelopingObject = fac.newXMLObject(structures, objectId, mimeType, encoding);

				// Crea la referencia al nuevo elemento Object o al nodo especifico a firmar
				// si asi se hubiese indicado
				referenceList.add(
					fac.newReference(
						"#" + (nodeToSign != null ? nodeToSign : objectId), //$NON-NLS-1$
						digestMethod,
						transformList,
						AOXAdESSigner.OBJURI,
						referenceId
					)
				);

				// ******************************************************************
				// ************** Hojas de estilo ***********************************
				// ******************************************************************
				if (styleElement != null) {
					final String objectStyleId = "StyleObject-" + UUID.randomUUID().toString(); //$NON-NLS-1$
					envelopingStyleObject = fac.newXMLObject(
						Collections.singletonList(
							new DOMStructure(styleElement)
						),
						objectStyleId,
						styleType,
						styleEncoding
					);
					referenceList.add(
						fac.newReference(
							"#" + objectStyleId, //$NON-NLS-1$
							digestMethod,
							Collections.singletonList(canonicalizationTransform),
							AOXAdESSigner.OBJURI,
							referenceStyleId
						)
					);

				}
				// ******************************************************************
				// ************** Fin hojas de estilo ***********************************
				// ******************************************************************
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al generar la firma en formato enveloping", e //$NON-NLS-1$
				);
			}

			// ******************************************************************
			// ***** Hojas de estilo para enveloping en Externally Detached *****
			// ******************************************************************
			if (styleHref != null
					&& styleElement == null
					&& (styleHref.startsWith(AOXAdESSigner.HTTP_PROTOCOL_PREFIX) ||
						styleHref.startsWith(AOXAdESSigner.HTTPS_PROTOCOL_PREFIX))) {
				// Comprobamos Si la referencia al estilo es externa
				try {
					referenceList.add(
						fac.newReference(
							styleHref,
							digestMethod,
							Collections.singletonList(canonicalizationTransform),
							null,
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// ******************************************************************
			// *** Fin hojas de estilo para enveloping en Externally Detached ***
			// ******************************************************************

		}

		// crea una referencia al documento mediante la URI hacia el
		// identificador del nodo CONTENT
		else if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)) {
			try {
				if (dataElement != null) {
					// Inserta en el nuevo documento de firma el documento a firmar
					docSignature.getDocumentElement().appendChild(
						docSignature.adoptNode(dataElement)
					);
					// Crea la referencia a los datos firmados que se encontraran en el mismo
					// documento
					referenceList.add(
						fac.newReference(
							tmpUri,
							digestMethod,
							transformList,
							null,
							referenceId
						)
					);
				}

				// ******************************************************************
				// ************** Hojas de estilo ***********************************
				// ******************************************************************
				if (styleElement != null) {
					// Inserta en el nuevo documento de firma la hoja de estilo
					docSignature.getDocumentElement().appendChild(
						docSignature.adoptNode(styleElement)
					);
					// Crea la referencia a los datos firmados que se encontraran en el mismo documento
					referenceList.add(
						fac.newReference(
							tmpStyleUri,
							digestMethod,
							Collections.singletonList(canonicalizationTransform),
							null,
							referenceStyleId
						)
					);
				}
				// ******************************************************************
				// ************** Fin hojas de estilo *******************************
				// ******************************************************************
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al generar la firma en formato detached implicito", e //$NON-NLS-1$
				);
			}

			// ******************************************************************
			// ************* Hojas de estilo remotas para Detached **************
			// ******************************************************************
			if (styleHref != null
					&& styleElement == null
					&& (styleHref.startsWith(AOXAdESSigner.HTTP_PROTOCOL_PREFIX) ||
						styleHref.startsWith(AOXAdESSigner.HTTPS_PROTOCOL_PREFIX))) {
				// Comprobamos si la referencia al estilo es externa
				try {
					referenceList.add(
						fac.newReference(
							styleHref,
							digestMethod,
							Collections.singletonList(canonicalizationTransform),
							null,
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// ******************************************************************
			// *********** Fin hojas de estilo remotas para Detached ************
			// ******************************************************************

		}

		// Crea una referencia al documento mediante la URI externa si la
		// tenemos o usando un Message Digest
		// precalculado si no tenemos otro remedio
		else if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
			Reference ref = null;
			// No tenemos uri, suponemos que los datos son el message digest
			if (precalculatedHashAlgorithm != null &&
					(uri == null ||
			         uri.getScheme().equals("") || //$NON-NLS-1$
			         uri.getScheme().equals("file"))) { //$NON-NLS-1$
				DigestMethod dm = null;
				try {
					// Convertimos el algo del Message Digest externo a la nomenclatura XML
					if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA1")) { //$NON-NLS-1$
						dm = fac.newDigestMethod(DigestMethod.SHA1, null);
					}
					else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-256")) { //$NON-NLS-1$
						dm = fac.newDigestMethod(DigestMethod.SHA256, null);
					}
					else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-512")) { //$NON-NLS-1$
						dm = fac.newDigestMethod(DigestMethod.SHA512, null);
					}
					else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("RIPEMD160")) { //$NON-NLS-1$
						dm = fac.newDigestMethod(DigestMethod.RIPEMD160, null);
					}
				}
				catch (final Exception e) {
					throw new AOException(
						"No se ha podido crear el metodo de huella digital para la referencia Externally Detached", e //$NON-NLS-1$
					);
				}
				if (dm == null) {
					throw new AOException(
						"Metodo de Message Digest para la referencia Externally Detached no soportado: " + precalculatedHashAlgorithm //$NON-NLS-1$
					);
				}
				ref = fac.newReference("", dm, null, null, referenceId, data); //$NON-NLS-1$
			}
			// Tenemos URI y no nos han establecido algoritmo de message digest,
			// por lo que es una referencia externa accesible
			else {
				// Si es una referencia de tipo file:// obtenemos el fichero y
				// creamos una referencia solo con el message digest
				if (uri != null && uri.getScheme().equals("file")) { //$NON-NLS-1$
					try {
						ref = fac.newReference(
							"", //$NON-NLS-1$
							digestMethod,
							null,
							null,
							referenceId,
							MessageDigest.getInstance(
								AOSignConstants.getDigestAlgorithmName(digestMethodAlgorithm)
							).digest(
								AOUtil.getDataFromInputStream(
									AOUtil.loadFile(uri)
								)
							)
						);
					}
					catch (final Exception e) {
						throw new AOException(
							"No se ha podido crear la referencia XML a partir de la URI local (" + uri.toASCIIString() + ")", e //$NON-NLS-1$ //$NON-NLS-2$
						);
					}
				}
				// Si es una referencia distinta de file:// suponemos que es dereferenciable de forma universal
				// por lo que dejamos que Java lo haga todo
				else if (uri != null) {
					try {
						ref = fac.newReference(uri.toASCIIString(), digestMethod);
					}
					catch (final Exception e) {
						throw new AOException(
							"No se ha podido crear la referencia Externally Detached, probablemente por no obtenerse el metodo de digest", e //$NON-NLS-1$
						);
					}
				}
			}
			if (ref == null) {
				throw new AOException(
					"Error al generar la firma Externally Detached, no se ha podido crear la referencia externa" //$NON-NLS-1$
				);
			}
			referenceList.add(ref);

			// *******************************************************
			// **** Hojas de estilo remotas en Externally Detached ***
			// *******************************************************
			if (styleHref != null && styleElement == null) {
				// Comprobamos que la URL es valida
				if (styleHref.startsWith(AOXAdESSigner.HTTP_PROTOCOL_PREFIX) ||
					styleHref.startsWith(AOXAdESSigner.HTTPS_PROTOCOL_PREFIX)) {
					try {
						referenceList.add(
							fac.newReference(
								styleHref,
								digestMethod,
								Collections.singletonList(canonicalizationTransform),
								null,
								referenceStyleId
							)
						);
					}
					catch (final Exception e) {
						XAdESSigner.LOGGER.severe(
							"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e //$NON-NLS-1$
						);
					}
				}
				else {
					XAdESSigner.LOGGER.warning(
						"Se necesita una referencia externa HTTP o HTTPS a la hoja de estilo para referenciarla en firmas XML Externally Detached" //$NON-NLS-1$
					);
				}
			}
			// *******************************************************
			// ** Fin hojas de estilo remotas en Externally Detached *
			// *******************************************************

		}

		// Crea una referencia indicando que se trata de una firma enveloped
		else if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)) {
			try {

				// Transformacion enveloped.
				// La enveloped siempre la primera, para que no se quede sin
				// nodos Signature por haber ejecutado antes otra transformacion
				transformList.add(
					fac.newTransform(
						Transform.ENVELOPED,
						(TransformParameterSpec) null
					)
				);

				// Salvo que sea una factura electronica o se haya indicado lo contrario,
				// se agrega una transformacion XPATH para eliminar el resto de firmas del
				// documento en las firmas Enveloped
				if (!facturaeSign && !avoidXpathExtraTransformsOnEnveloped) {
					transformList.add(
						fac.newTransform(
							Transform.XPATH,
							new XPathFilterParameterSpec(
								"not(ancestor-or-self::" + AOXAdESSigner.XML_SIGNATURE_PREFIX + ":Signature)", //$NON-NLS-1$ //$NON-NLS-2$
								Collections.singletonMap(
									AOXAdESSigner.XML_SIGNATURE_PREFIX,
									XMLSignature.XMLNS
								)
							)
						)
					);
				}

				// Crea la referencia
				referenceList.add(
					fac.newReference(
						nodeToSign != null ? "#" + nodeToSign : "", //$NON-NLS-1$ //$NON-NLS-2$
						digestMethod,
						transformList,
						null,
						referenceId
					)
				);
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al generar la firma en formato enveloped", e //$NON-NLS-1$
				);
			}

			// *******************************************************
			// ******** Hojas de estilo remotas en Enveloped *********
			// *******************************************************
			if (styleHref != null
					&& styleElement == null
					&& (styleHref
							.startsWith(AOXAdESSigner.HTTP_PROTOCOL_PREFIX) || styleHref
							.startsWith(AOXAdESSigner.HTTPS_PROTOCOL_PREFIX))) {
				// Comprobamos si la referencia al estilo es externa
				try {
					referenceList.add(
						fac.newReference(
							styleHref,
							digestMethod,
							Collections.singletonList(canonicalizationTransform),
							null,
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja de estilo del XML, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// *******************************************************
			// ****** Fin hojas de estilo remotas en Enveloped *******
			// *******************************************************

		}

		// Instancia XADES_EPES
		final XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(
			XAdES.EPES, // XAdES
			xadesNamespace, // XAdES NameSpace
			AOXAdESSigner.XADES_SIGNATURE_PREFIX, // XAdES Prefix
			AOXAdESSigner.XML_SIGNATURE_PREFIX, // XMLDSig Prefix
			digestMethodAlgorithm, // DigestMethod
			docSignature.getDocumentElement() // Element
		);

		// SigningCertificate
		xades.setSigningCertificate((X509Certificate) certChain[0]);

		// SignaturePolicyIdentifier
		final SignaturePolicyIdentifier spi = AOXAdESSigner.getPolicy(
			extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
			extraParams.getProperty("policyIdentifierHash"), //$NON-NLS-1$
			extraParams.getProperty("policyIdentifierHashAlgorithm"), //$NON-NLS-1$
			extraParams.getProperty("policyDescription"), //$NON-NLS-1$
			extraParams.getProperty("policyQualifier") //$NON-NLS-1$
		);
		if (spi != null) {
			xades.setSignaturePolicyIdentifier(spi);
		}

		// SignatureProductionPlace
		final SignatureProductionPlace spp = AOXAdESSigner.getSignatureProductionPlace(
			extraParams.getProperty("signatureProductionCity"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionProvince"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionPostalCode"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionCountry") //$NON-NLS-1$
		);
		if (spp != null) {
			xades.setSignatureProductionPlace(spp);
		}

		// SignerRole
		SignerRole signerRole = null;
		try {
			final String claimedRole = extraParams.getProperty("signerClaimedRole"); //$NON-NLS-1$
			final String certifiedRole = extraParams.getProperty("signerCertifiedRole"); //$NON-NLS-1$
			signerRole = new SignerRoleImpl();
			if (claimedRole != null) {
				signerRole.addClaimedRole(claimedRole);
			}
			if (certifiedRole != null) {
				signerRole.addCertifiedRole(certifiedRole);
			}
		}
		catch (final Exception e) {
			// Se ignoran los errores, el parametro es opcional
		}
		if (signerRole != null) {
			xades.setSignerRole(signerRole);
		}

		// SigningTime
		if (Boolean.parseBoolean(
			extraParams.getProperty(
				"applySystemDate", //$NON-NLS-1$
				Boolean.TRUE.toString()
			)
		)) {
			xades.setSigningTime(new Date());
		}

		// DataObjectFormat
		String oid = extraParams.getProperty("contentTypeOid"); //$NON-NLS-1$
		if (oid == null && mimeType != null) {
			try {
				oid = MimeHelper.transformMimeTypeToOid(mimeType);
			}
        	catch (final Exception e) {
				LOGGER.warning("Error en la obtencion del OID del tipo de datos a partir del MimeType: " + e); //$NON-NLS-1$
			}
			// Si no se reconoce el MimeType se habra establecido el por defecto. Evitamos este comportamiento
			if (!MimeHelper.DEFAULT_MIMETYPE.equals(mimeType) && MimeHelper.DEFAULT_CONTENT_OID_DATA.equals(oid)) {
				oid = null;
			}
		}
		final ObjectIdentifierImpl objectIdentifier = oid != null ? new ObjectIdentifierImpl(
			"OIDAsURN", (oid.startsWith("urn:oid:") ? "" : "urn:oid:") + oid, null, new ArrayList<String>(0)) : null; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

				final ArrayList<DataObjectFormat> objectFormats = new ArrayList<DataObjectFormat>();
				final DataObjectFormat objectFormat = new DataObjectFormatImpl(
					null,
					objectIdentifier,
					mimeType,
					encoding,
					"#" + referenceId //$NON-NLS-1$
				);
				objectFormats.add(objectFormat);
				xades.setDataObjectFormats(objectFormats);

				final AOXMLAdvancedSignature xmlSignature;
				try {
					xmlSignature = AOXMLAdvancedSignature.newInstance(xades);
				}
				catch (final Exception e) {
					throw new AOException(
						"No se ha podido instanciar la firma XML Avanzada de JXAdES", e //$NON-NLS-1$
					);
				}

				// Establecemos el tipo de propiedades firmadas
				xmlSignature.setSignedPropertiesTypeUrl(signedPropertiesTypeUrl);

				try {
					xmlSignature.setDigestMethod(digestMethodAlgorithm);
					xmlSignature.setCanonicalizationMethod(canonicalizationAlgorithm);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
						"No se ha podido establecer el algoritmo de huella digital (" + algoUri //$NON-NLS-1$
							+ "), es posible que el usado en la firma difiera del indicado: " //$NON-NLS-1$
							+ e
					);
				}

				// en el caso de formato enveloping se inserta el elemento Object con el
				// documento a firmar
				if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
					xmlSignature.addXMLObject(envelopingObject);
					if (envelopingStyleObject != null) {
						xmlSignature.addXMLObject(envelopingStyleObject);
					}
				}

				// *******************************************************
				// *********** Hojas de estilo en Enveloped **************
				// *******************************************************
				if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) && styleElement != null) {

					// Si es enveloped hay que anadir la hoja de estilo dentro de la firma y
					// referenciarla

					xmlSignature.addStyleSheetEnvelopingOntoSignature(
						styleElement,
						styleType,
						styleEncoding,
						styleId
					);

					try {
						referenceList.add(
							fac.newReference(
								tmpStyleUri,
								digestMethod,
								Collections.singletonList(canonicalizationTransform),
								null,
								referenceStyleId
							)
						);
					}
					catch (final Exception e) {
						XAdESSigner.LOGGER.severe(
							"No se ha podido anadir una referencia a la hoja de estilo, esta se incluira dentro de la firma, pero no estara firmada: " + e //$NON-NLS-1$
						);
					}
				}
				// *******************************************************
				// ********* Fin hojas de estilo en Enveloped ************
				// *******************************************************

				// Genera la firma
				try {
					final boolean onlySignningCert = Boolean.parseBoolean(
						extraParams.getProperty(
							"includeOnlySignningCertificate", //$NON-NLS-1$
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
							Arrays.asList((X509Certificate[]) certChain),
							pk,
							algoUri,
							referenceList,
							"Signature-" + UUID.randomUUID().toString() //$NON-NLS-1$
						);
					}
				}
				catch (final NoSuchAlgorithmException e) {
					throw new UnsupportedOperationException(
						"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'", e //$NON-NLS-1$ //$NON-NLS-2$
					);
				}
				catch (final Exception e) {
					throw new AOException("Error al generar la firma XAdES: " + e, e); //$NON-NLS-1$
				}

				// Si se esta realizando una firma enveloping simple no tiene sentido el nodo raiz,
				// asi que sacamos el nodo de firma a un documento aparte
				if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
					try {
						if (docSignature.getElementsByTagNameNS(XMLConstants.DSIGNNS,
								AOXAdESSigner.SIGNATURE_TAG).getLength() == 1) {
							final Document newdoc = dbf.newDocumentBuilder().newDocument();
							newdoc.appendChild(
								newdoc.adoptNode(
									docSignature.getElementsByTagNameNS(
										XMLConstants.DSIGNNS,
										AOXAdESSigner.SIGNATURE_TAG
									).item(0)
								)
							);
							docSignature = newdoc;
						}
					}
					catch (final Exception e) {
						XAdESSigner.LOGGER.info(
							"No se ha eliminado el nodo padre '<AFIRMA>': " + e //$NON-NLS-1$
						);
					}
				}

				// Si no es enveloped quito los valores para que no se inserte la
				// cabecera de hoja de estilo
				if (!format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)) {
					styleHref = null;
					styleType = null;
				}

				return Utils.writeXML(
					docSignature.getDocumentElement(),
					originalXMLProperties,
					styleHref,
					styleType
				);

	}

	private XAdESSigner() {
		// No permitimos la instanciacion
	}

}
