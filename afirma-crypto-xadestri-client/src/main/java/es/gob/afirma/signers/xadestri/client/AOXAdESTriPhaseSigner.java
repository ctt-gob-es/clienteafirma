/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xadestri.client;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseDataSigner;
import es.gob.afirma.core.signers.TriphaseUtil;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Manejador de firmas XAdES trif&aacute;sicas. Mediante este manejador un usuario puede firmar un documento remoto
 * indicando el identificador del documento. Este manejador requiere de un servicio remoto que genere la estructura
 * de firma en servidor. La operaci&oacute;n criptogr&aacute;fica de firma se realiza en el PC o dispositivo del usuario,
 * por lo que la clave privada de su certificado nunca sale de este.<br>
 * El resultado de las operaciones criptogr&aacute;ficas no es el resultado generado sino el identificador con el que
 * el resultado se ha guardado en el servidor remoto (gestor documental, sistema de ficheros,...).
 * La l&oacute;gica de resoluci&oacute;n del identificador de entrada, recuperaci&oacute;n de los datos y el guardado
 * del resultado recae el un manejador conectado al servicio de firma.
 * Como alternativa, a indicar los datos mediante un identificador, un usuario puede introducir directamente los datos
 * (prevaleciendo estos sobre el identificador) de tal forma que estos viajan en cada una de las operaciones con el
 * servidor. El resultado ser&aacute; an&aacute;logo al anterior, recuperandose &uacute;nicamente el identificador
 * remoto asignado al resultado. */
public class AOXAdESTriPhaseSigner implements AOSigner {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** URI que define el espacio de nombres de firma XMLdSig (Compatible XAdES). */
    public static final String DSIGNNS = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$

    static final String XML_SIGNATURE_PREFIX = "ds"; //$NON-NLS-1$

    /** Etiqueta de los nodos firma de los XML firmados. */
    public static final String SIGNATURE_TAG = "Signature"; //$NON-NLS-1$

    static final String SIGNATURE_NODE_NAME = XML_SIGNATURE_PREFIX + ":Signature"; //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de cofirma. */
	private static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de contrafirma. */
	private static final String CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n trif&aacute;sica
	 * en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica
	 * en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
		private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$

	// Nombres de las propiedades intercambiadas con el servidor como Properties

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	/** Clave de la propiedad de configuraci&oacute;n del tipo de contrafirma. */
	private static final String COUNTERSIGN_TARGET_KEY = "target"; //$NON-NLS-1$

	/** Valor para la configuraci&oacute;n de la contrafirma de nodos hoja. */
	public static final String COUNTERSIGN_TARGET_LEAFS = "leafs"; //$NON-NLS-1$

	/** Valor para la configuraci&oacute;n de la contrafirma de todos los nodos del &aacute;rbol. */
	public static final String COUNTERSIGN_TARGET_TREE = "tree"; //$NON-NLS-1$

	private final String signFormat;

	protected AOXAdESTriPhaseSigner(final String format) {
		this.signFormat = format;
	}

	/** Construye un manejador de firmas XAdES trif&aacute;sicas. */
	public AOXAdESTriPhaseSigner() {
		this(AOSignConstants.SIGN_FORMAT_XADES);
	}

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties xParams) throws AOException {
		return triPhaseOperation(
			this.signFormat,
			CRYPTO_OPERATION_SIGN,
			data,
			algorithm,
			key,
			certChain,
			xParams
		);
	}

	/** {@inheritDoc} */
	@Override
	public final byte[] getData(final byte[] sign) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException {
		return triPhaseOperation(
			this.signFormat,
			CRYPTO_OPERATION_COSIGN,
			sign,
			algorithm,
			key,
			certChain,
			xParams
		);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException {
		return cosign(null, sign, algorithm, key, certChain, xParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			final String algorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties xParams) throws AOException {

		// Si no se ha definido nodos objeto de la contrafirma se definen los nodos hijo
		if (targetType == null) {
			throw new IllegalArgumentException("No se han indicado los nodos objetivo de la contrafirma"); //$NON-NLS-1$
		}

		// Comprobamos si es un tipo de contrafirma soportado
		if (targetType != CounterSignTarget.TREE && targetType != CounterSignTarget.LEAFS) {
			throw new UnsupportedOperationException("El objetivo indicado para la contrafirma no esta soportado: " + targetType); //$NON-NLS-1$
		}

		final Properties params = xParams != null ? xParams : new Properties();

		params.setProperty(COUNTERSIGN_TARGET_KEY, targetType.toString());

		return triPhaseOperation(
			this.signFormat,
			CRYPTO_OPERATION_COUNTERSIGN,
			sign,
			algorithm,
			key,
			certChain,
			params
		);
	}

	/** {@inheritDoc} */
	@Override
	public final AOTreeModel getSignersStructure(final byte[] sign,
			                                     final boolean asSimpleSignInfo) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] sign) {
        if (sign == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        try {
            // Carga el documento a validar
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);

            // JXades no captura un nodo de firma si se pasa este como raiz del arbol de firmas, asi
            // que nos vemos obligados a crear un nodo padre, del que colgara todo el arbol de firmas,
            // para que lo detecte correctamente
            final Element rootNode = dbf.newDocumentBuilder().parse(
        		new ByteArrayInputStream(sign)
    		).getDocumentElement();

            final List<Node> signNodes = new ArrayList<Node>();
            if (rootNode.getNodeName().equals(SIGNATURE_NODE_NAME)) {
                signNodes.add(rootNode);
            }

            final NodeList signatures = rootNode.getElementsByTagNameNS(DSIGNNS, SIGNATURE_TAG);
            for (int i = 0; i < signatures.getLength(); i++) {
                signNodes.add(signatures.item(i));
            }

            // Si no se encuentran firmas, no es un documento de firma (obviamos si son XAdES o no)
            if (signNodes.size() == 0) {
                return false;
            }
        }
        catch (final Exception e) {
            return false;
        }
        return true;
	}

	/** {@inheritDoc} */
	@Override
	public boolean isValidDataFile(final byte[] data) {
		if (data == null) {
			LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".xsig"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] sign) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	/** Ejecuta una operaci&oacute;n de firma/multifirma en 3 fases.<br>
	 * <b>IMPORTANTE:</b> Hay que optimizar la transferencia de datos entre servidor
	 * y cliente, detectando despu&eacute;s de la prefirma las propiedades {@code nd} y {@code np}
	 * enviadas por el servidor trif&aacute;sico. Si se env&iacute;a {@code nd} y es {@code true} se
	 * tienen que volver a remitir los datos en la postfirma y si se env&iacute;a {@code np} y es
	 * {@code true} se tiene que reenviar la prefirma en la petici&oacute;n de postfirma. Si no, se
	 * puede omitir esta informaci&oacute;n reduciendo la cantidad de datos que se env&iacute;a.
	 * @param format Formato de firma ("XAdES" o "XAdES-ASiC-S" en este caso).
	 * @param cryptoOperation Tipo de operaci&oacute;n.
	 * @param data Datos o firma sobre la que operar
	 * @param algorithm Algoritmo de firma
	 * @param key Clave privada del certificado de firma.
	 * @param certChain Cadena de certificaci&oacute;n.
	 * @param extraParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
	 * @return Resultado de la operaci&oacute;n de firma.
	 * @throws AOException Cuando se produce un error durante la operaci&oacute;n. */
	protected static byte[] triPhaseOperation(final String format,
			                                final String cryptoOperation,
			                                final byte[] data,
			                                final String algorithm,
			                                final PrivateKey key,
			                                final Certificate[] certChain,
			                                final Properties extraParams) throws AOException {
		if (extraParams == null) {
			throw new IllegalArgumentException("Se necesitan parametros adicionales"); //$NON-NLS-1$
		}
		if (key == null) {
			throw new IllegalArgumentException("Es necesario proporcionar la clave privada de firma"); //$NON-NLS-1$
		}
		if (certChain == null || certChain.length == 0) {
			throw new IllegalArgumentException("Es necesario proporcionar el certificado de firma"); //$NON-NLS-1$
		}
		if (data == null) {
			throw new IllegalArgumentException("No se ha proporcionado el identificador de documento a firmar"); //$NON-NLS-1$
		}

		// Comprobamos la direccion del servidor
		final URL signServerUrl;
		try {
			signServerUrl = new URL(extraParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL));
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"No se ha proporcionado una URL valida para el servidor de firma: " + extraParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL), e //$NON-NLS-1$
			);
		}

		// Retiramos del extraParams la URL del servidor de firma sin mutar los parametros de entrada
		final Properties xParams = (Properties) extraParams.clone();
		xParams.remove(PROPERTY_NAME_SIGN_SERVER_URL);

		// Decodificamos el identificador del documento
		final String documentId = Base64.encode(data, true);

		final UrlHttpManager urlManager = UrlHttpManagerFactory.getInstalledManager();

		// Preparamos el parametro de cadena de certificados
		final String cerChainParamContent;
		try {
			cerChainParamContent = TriphaseUtil.prepareCertChainParam(certChain, xParams);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando la cadena de certificados: " + e, e); //$NON-NLS-1$
		}

		// ---------
		// PREFIRMA
		// ---------

		// Empezamos la prefirma
		final byte[] preSignResult;
		try {
			// Llamamos a una URL pasando como parametros los datos necesarios para
			// configurar la operacion:
			//  - Operacion trifasica (prefirma o postfirma)
			//  - Operacion criptografica (firma, cofirma o contrafirma)
			//  - Formato de firma
			//  - Algoritmo de firma a utilizar
			//  - Certificado de firma
			//  - Parametros extra de configuracion
			//  - Datos o identificador del documento a firmar
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_PRESIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(format).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(cerChainParamContent).append(HTTP_AND).
			append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId);

			if (xParams.size() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).
				append(AOUtil.properties2Base64(xParams));
			}

			final String postUrl = urlBuffer.toString();

			LOGGER.info("Se llamara por POST a la siguiente URL:\n" + postUrl);  //$NON-NLS-1$

			preSignResult = urlManager.readUrl(postUrl, UrlHttpMethod.POST);
			urlBuffer.setLength(0);
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}

		LOGGER.info("Tamano de la prefirma XAdES: " + preSignResult.length + " octetos."); //$NON-NLS-1$ //$NON-NLS-2$

		// ----------
		// FIRMA
		// ----------

		// Convertimos la respuesta del servidor en un Objeto de sesion
		final TriphaseData triphaseData;
		try {
			triphaseData = TriphaseData.parser(
				Base64.decode(preSignResult, 0, preSignResult.length, true)
			);
		}
		catch (final Exception e) {
			LOGGER.severe("Error al analizar la prefirma enviada por el servidor: " + e); //$NON-NLS-1$
			throw new AOException("Error al analizar la prefirma enviada por el servidor: " + e, e); //$NON-NLS-1$
		}

		final String preResultAsBase64 = Base64.encode(
			TriphaseDataSigner.doSign(
				new AOPkcs1Signer(),
				algorithm,
				key,
				certChain,
				triphaseData
			).toString().getBytes(),
			true
		);

		// ---------
		// POSTFIRMA
		// ---------

		final byte[] triSignFinalResult;
		try {
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN)    .append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(format)                   .append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm)             .append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(cerChainParamContent)       .append(HTTP_AND).
			append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId).append(HTTP_AND).
			append(PARAMETER_NAME_SESSION_DATA).append(HTTP_EQUALS).append(preResultAsBase64);

			if (xParams.size() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).
				append(AOUtil.properties2Base64(xParams));
			}

			triSignFinalResult = urlManager.readUrl(urlBuffer.toString(), UrlHttpMethod.POST);

		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// Analizamos la respuesta del servidor
		final String stringTrimmedResult = new String(triSignFinalResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + new String(triSignFinalResult)); //$NON-NLS-1$
		}

		// Los datos no se devuelven, se quedan en el servidor
		try {
			return Base64.decode(stringTrimmedResult.substring((SUCCESS + " NEWID=").length()), true); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$
			throw new AOException("El resultado devuelto por el servidor no es correcto: " + e, e); //$NON-NLS-1$
		}
	}

}
