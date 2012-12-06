/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.padestri.client;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.UrlHttpManagerImpl;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Firmador PAdES en tres fases.
 * Las firmas que genera no se etiquetan como ETSI, sino como "Adobe PKCS#7 Detached".
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class AOPDFTriPhaseSigner implements AOSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	/** Identificador del documento a firmar, por el cual se obtiene desde el servidor documental. */
	private static final String PROPERTY_NAME_DOCUMENT_ID = "documentId"; //$NON-NLS-1$

	/** Identificador de la operacion de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "0"; //$NON-NLS-1$

	/** Identificador de la operacion de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "1"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro de c&oacute;digo de operaci&oacute;n en la URL de llamada al servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$

	private static final String PADES_FORMAT = "pades"; //$NON-NLS-1$

	// Nombres de las propiedades intercambiadas con el servidor como Properties

	/** Prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Identificador interno del PDF. */
	private static final String PROPERTY_NAME_PDF_UNIQUE_ID = "PID"; //$NON-NLS-1$

	/** Momento de la firma, establecido en el servidor. */
	private static final String PROPERTY_NAME_SIGN_TIME = "TIME"; //$NON-NLS-1$

    private static final String PDF_FILE_HEADER = "%PDF-"; //$NON-NLS-1$
    private static final String PDF_FILE_SUFFIX = ".pdf"; //$NON-NLS-1$

    /** Indicador de finalizaci&oacute;n correcta de proceso. */
    private static final String SUCCESS = "OK"; //$NON-NLS-1$

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKeyEntry keyEntry,
			           final Properties extraParams) throws AOException {

		if (extraParams == null) {
			throw new IllegalArgumentException("Se necesitan parametros adicionales"); //$NON-NLS-1$
		}
		if (keyEntry == null) {
			throw new IllegalArgumentException("Es necesario proporcionar una entrada a la clave privada de firma"); //$NON-NLS-1$
		}
		if (data != null) {
			LOGGER.warning("Se han recibido datos, pero se ignoraran, ya que estos se obtienen directamente del servidor documental"); //$NON-NLS-1$
		}

		final Properties configParams = new Properties();
		for (final String key : extraParams.keySet().toArray(new String[extraParams.size()])) {
			configParams.setProperty(key, extraParams.getProperty(key));
		}

		final URL signServerUrl;
		try {
			signServerUrl = new URL(configParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL));
		}
		catch (final MalformedURLException e) {
			throw new IllegalArgumentException("No se ha proporcionado la URL del servidor de firma: " + configParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL), e); //$NON-NLS-1$
		}
		configParams.remove(PROPERTY_NAME_SIGN_SERVER_URL);

		final String documentId = configParams.getProperty(PROPERTY_NAME_DOCUMENT_ID);
		if (documentId == null || "".equals(documentId)) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se ha proporcionado un identificador de documento"); //$NON-NLS-1$
		}
		configParams.remove(PROPERTY_NAME_DOCUMENT_ID);

		// ---------
		// PREFIRMA
		// ---------

		// Empezamos la prefirma
		final byte[] preSignResult;
		try {
			// Llamamos a una URL pasando como parametros los datos necesarios para
			// configurar la operacion:
			//  - Operacion trifasica (Prefirma o postfirma)
			//  - Identificador del documento a firmar
			//  - Algoritmo de firma a utilizar
			//  - Certificado de firma
			//  - Parametros extra de configuracion
			preSignResult = UrlHttpManagerImpl.readUrlByPost(
				signServerUrl + HTTP_CGI +
				PARAMETER_NAME_OPERATION +
				HTTP_EQUALS +
				OPERATION_PRESIGN +
				HTTP_AND +
				PARAMETER_NAME_DOCID +
				HTTP_EQUALS +
				documentId +
				HTTP_AND +
				PARAMETER_NAME_FORMAT +
				HTTP_EQUALS +
				PADES_FORMAT +
				HTTP_AND +
				PARAMETER_NAME_ALGORITHM +
				HTTP_EQUALS +
				algorithm +
				HTTP_AND +
				PARAMETER_NAME_CERT +
				HTTP_EQUALS +
				Base64.encodeBytes(keyEntry.getCertificate().getEncoded(), Base64.URL_SAFE) +
				HTTP_AND +
				PARAMETER_NAME_EXTRA_PARAM +
				HTTP_EQUALS +
				properties2Base64(configParams)
			);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}

		final Properties preSign;
		try {
			preSign = base642Properties(new String(preSignResult));
		}
		catch (final IOException e) {
			throw new AOException("La respuesta del servidor no es valida: " + new String(preSignResult), e); //$NON-NLS-1$
		}

		final byte[] cadesSignedAttributes;
		try {
			cadesSignedAttributes = Base64.decode(preSign.getProperty(PROPERTY_NAME_PRESIGN));
		}
		catch (final IOException e) {
			throw new AOException("Error decodificando los atributos CAdES a firmar: " + e, e); //$NON-NLS-1$
		}

		// ---------
		// FIRMA
		// ---------

		final byte[] pkcs1sign = new AOPkcs1Signer().sign(
			cadesSignedAttributes,
			algorithm,
			keyEntry,
			null // No hay parametros en PKCS#1
		);

		// ---------
		// POSTFIRMA
		// ---------
		// Creamos la peticion de postfirma
		configParams.put(PROPERTY_NAME_PKCS1_SIGN, Base64.encode(pkcs1sign));
		configParams.put(PROPERTY_NAME_PRESIGN, preSign.getProperty(PROPERTY_NAME_PRESIGN));
		configParams.put(PROPERTY_NAME_PDF_UNIQUE_ID, preSign.getProperty(PROPERTY_NAME_PDF_UNIQUE_ID));
		configParams.put(PROPERTY_NAME_SIGN_TIME, preSign.getProperty(PROPERTY_NAME_SIGN_TIME));

		final byte[] triSignFinalResult;
		try {
			triSignFinalResult = UrlHttpManagerImpl.readUrlByPost(
				signServerUrl + HTTP_CGI +
				PARAMETER_NAME_OPERATION +
				HTTP_EQUALS +
				OPERATION_POSTSIGN +
				HTTP_AND +
				PARAMETER_NAME_DOCID +
				HTTP_EQUALS +
				documentId +
				HTTP_AND +
				PARAMETER_NAME_FORMAT +
				HTTP_EQUALS +
				PADES_FORMAT +
				HTTP_AND +
				PARAMETER_NAME_ALGORITHM +
				HTTP_EQUALS +
				algorithm +
				HTTP_AND +
				PARAMETER_NAME_CERT +
				HTTP_EQUALS +
				Base64.encodeBytes(keyEntry.getCertificate().getEncoded(), Base64.URL_SAFE) +
				HTTP_AND +
				PARAMETER_NAME_EXTRA_PARAM +
				HTTP_EQUALS +
				properties2Base64(configParams)
			);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
		}

		final String stringTrimmedResult = new String(triSignFinalResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + new String(triSignFinalResult)); //$NON-NLS-1$
		}

		// Los datos no se devuelven, se quedan en el servidor
		try {
			return Base64.decode(stringTrimmedResult.replace("OK NEWID=", "")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IOException e) {
			LOGGER.warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$
			return stringTrimmedResult.getBytes();
		}
	}

	private static String properties2Base64(final Properties p) throws IOException {
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		p.store(baos, ""); //$NON-NLS-1$
		return Base64.encodeBytes(baos.toByteArray(), Base64.URL_SAFE);
	}

	private static Properties base642Properties(final String base64) throws IOException {
		final Properties p = new Properties();
		p.load(new ByteArrayInputStream(Base64.decode(base64, Base64.URL_SAFE)));
		return p;
	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKeyEntry keyEntry,
			             final Properties extraParams) throws AOException {
		return sign(null, null, keyEntry, extraParams);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKeyEntry keyEntry,
			             final Properties extraParams) throws AOException {
		return sign(null, null, keyEntry, extraParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKeyEntry keyEntry,
			                  final Properties extraParams) throws AOException {
		throw new UnsupportedOperationException("No se soportan contrafirmas en PAdES"); //$NON-NLS-1$
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign,
										   final boolean asSimpleSignInfo) {
		throw new UnsupportedOperationException("No soportado para firmas trifasicas"); //$NON-NLS-1$
	}

	@Override
	public boolean isSign(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        if (!isPdfFile(data)) {
        	return false;
        }
        return getSignersStructure(data, false).getCount().intValue() > 0;
	}

	@Override
	public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isPdfFile(data);
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
        final String inTextInt = inText != null ? inText : ""; //$NON-NLS-1$
        if (originalName == null) {
            return "signed.pdf"; //$NON-NLS-1$
        }
        if (originalName.toLowerCase().endsWith(PDF_FILE_SUFFIX)) {
            return originalName.substring(0, originalName.length() - PDF_FILE_SUFFIX.length()) + inTextInt + PDF_FILE_SUFFIX;
        }
        return originalName + inTextInt + PDF_FILE_SUFFIX;
	}

	@Override
	public byte[] getData(final byte[] sign) throws AOException {
        // Si no es una firma PDF valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }
        // TODO: Devolver el PDF sin firmar
        return sign;
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(data)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_PDF);
        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia que se almacenan en el objeto AOSignInfo
	}

    private static boolean isPdfFile(final byte[] data) {
        byte[] buffer = new byte[PDF_FILE_HEADER.length()];
        try {
            new ByteArrayInputStream(data).read(buffer);
        }
        catch (final Exception e) {
            buffer = null;
        }
        // Comprobamos que cuente con una cabecera PDF
        if (buffer != null && !PDF_FILE_HEADER.equals(new String(buffer))) {
            return false;
        }
        return true;
    }

}
