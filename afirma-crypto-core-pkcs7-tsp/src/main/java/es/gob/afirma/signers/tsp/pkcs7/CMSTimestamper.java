/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.tsp.pkcs7;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.Socket;
import java.net.URI;
import java.net.URLConnection;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cmp.PKIFailureInfo;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.x509.X509ObjectIdentifiers;
import org.bouncycastle.cms.CMSSignedData;
import org.bouncycastle.cms.SignerInformation;
import org.bouncycastle.cms.SignerInformationStore;
import org.bouncycastle.tsp.TimeStampRequest;
import org.bouncycastle.tsp.TimeStampRequestGenerator;
import org.bouncycastle.tsp.TimeStampResponse;
import org.bouncycastle.tsp.TimeStampToken;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;

/** Generador local de sellos de tiempo para PKCS#7.
 * Puede probarse de forma sencilla con la TSA de CatCert:
 * <ul>
 *  <li>URL TSP: "http://psis.catcert.net/psis/catcert/tsp"</li>
 *  <li>OID Pol&iacute;tica de sello de tiempo: "0.4.0.2023.1.1"</li>
 *  <li>Requiere certificado: S&iacute;</li>
 * </ul> */
public final class CMSTimestamper {

    private static final String SIGNATURE_TIMESTAMP_TOKEN_OID = "1.2.840.113549.1.9.16.2.14"; //$NON-NLS-1$

	private static final HostnameVerifier DEFAULT_HOSTNAME_VERIFIER = HttpsURLConnection.getDefaultHostnameVerifier();
	private static final SSLSocketFactory DEFAULT_SSL_SOCKET_FACTORY = HttpsURLConnection.getDefaultSSLSocketFactory();

	private static final int SOCKET_TIMEOUT = 500000;

    private final TimeStampRequestGenerator tsqGenerator;
    private final URI tsaURL;
    private final String tsaUsername;
    private final String tsaPassword;

    private byte[] sslP12KeyStoreFile = null;
    private String sslP12KeyStorePassword = null;

    /** Construye un estampador de sellos de tiempo para estructuras CMS y CAdES.
     * @param requireCert <code>true</code> si la TSA requiere certificado, <code>false</code> en caso contrario
     * @param policy OID de la pol&iacute;tica de sellado de tiempo
     * @param tsa URL de la autoridad de sellado de tiempo
     * @param tsaUsr Nombre de usuario si la TSA requiere autenticaci&oacute;n (puede ser <code>null</code> si no se necesita autenticaci&oacute;n)
     * @param tsaPwd Contrase&ntilde;a del usuario de la TSA (puede ser <code>null</code> si no se necesita autenticaci&oacute;n)
     * @param extensions Extensiones a a&ntilde;adir a la petici&oacute;n de sello de tiempo
     * @param p12KeyStoreFile Fichero PKCS#12 / PFX de almac&eacute;n (formato PKCS#12) del certificado cliente a usar en conexiones SSL
     * @param p12KeyStoreFilePassword Contrase&ntilde;a del ichero PKCS#12 / PFX de almac&eacute;n (formato PKCS#12) del certificado
     *                                cliente a usar en conexiones SSL */
    public CMSTimestamper(final boolean requireCert,
                     final String policy,
                     final URI tsa,
                     final String tsaUsr,
                     final String tsaPwd,
                     final TsaRequestExtension[] extensions,
                     final byte[] p12KeyStoreFile,
                     final String p12KeyStoreFilePassword) {
    	this(requireCert, policy, tsa, tsaUsr, tsaPwd, extensions);
    	if (p12KeyStoreFile == null || p12KeyStoreFilePassword == null) {
    		throw new IllegalArgumentException("El almacen PKCS#12 y su contrasena no pueden ser nulos"); //$NON-NLS-1$
    	}
    	this.sslP12KeyStoreFile = p12KeyStoreFile.clone();
    	this.sslP12KeyStorePassword = p12KeyStoreFilePassword;
    }

    /** Construye un estampador de sellos de tiempo para estructuras CMS y CAdES.
     * @param requireCert <code>true</code> si la TSA requiere certificado, <code>false</code> en caso contrario
     * @param policy OID de la pol&iacute;tica de sellado de tiempo
     * @param tsa URL de la autoridad de sellado de tiempo
     * @param tsaUsr Nombre de usuario si la TSA requiere autenticaci&oacute;n (puede ser <code>null</code> si no se necesita autenticaci&oacute;n)
     * @param tsaPwd Contrase&ntilde;a del usuario de la TSA (puede ser <code>null</code> si no se necesita autenticaci&oacute;n)
     * @param extensions Extensiones a a&ntilde;adir a la petici&oacute;n de sello de tiempo */
    public CMSTimestamper(final boolean requireCert,
                     final String policy,
                     final URI tsa,
                     final String tsaUsr,
                     final String tsaPwd,
                     final TsaRequestExtension[] extensions) {
        this.tsqGenerator = new TimeStampRequestGenerator();
        if (extensions != null) {
        	for (final TsaRequestExtension ext : extensions) {
        		this.tsqGenerator.addExtension(
    				new ASN1ObjectIdentifier(ext.getOid()),
    				ext.isCritical(),
    				ext.getValue()
				);
        		Logger.getLogger("es.gob.afirma").info("Anadida extension a la solicitud de sello de tiempo: " + ext); //$NON-NLS-1$ //$NON-NLS-2$
        	}
        }
        this.tsqGenerator.setCertReq(requireCert);
        this.tsqGenerator.setReqPolicy(new ASN1ObjectIdentifier(policy));
        this.tsaURL = tsa;
        this.tsaPassword = tsaPwd;
        this.tsaUsername = tsaUsr;
    }

    /** A&ntilde;ade un sello de tiempo a las firmas encontradas dentro de una estructura PKCS#7.
     * @param pkcs7 Estructura que contiene las firmas a estampar un sello de tiempo
     * @param hashAlgorithm Algoritmo de huella digital a usar en los sellos de tiempo (si se indica <code>null</code> se usa SHA-1)
     * @return Nueva estructura PKCS#7 con los sellos de tiempo a&ntilde;adidos
     * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella digital del sello de tiempo
     * @throws AOException Cuando ocurren errores gen&eacute;ricos
     * @throws IOException Si hay errores de entrada / salida */
    public byte[] addTimestamp(final byte[] pkcs7, final String hashAlgorithm) throws NoSuchAlgorithmException, AOException, IOException {

        final CMSSignedData signedData;
        try {
            signedData = new CMSSignedData(pkcs7);
        }
        catch (final Exception e) {
            throw new IllegalArgumentException("Los datos de entrada no son un SignedData de CMS: " + e); //$NON-NLS-1$
        }

        final SignerInformationStore origSignerInfoStore =  signedData.getSignerInfos();

        // Insertamos un sello de tiempo en cada una de las firmas encontradas en el PKCS#7
        final List<SignerInformation> vNewSigners = new ArrayList<SignerInformation>();

        final Collection<?> ovSigners = origSignerInfoStore.getSigners();
        for (final Object name : ovSigners) {

             final SignerInformation si = (SignerInformation) name;
             final byte[] tsToken = getTimeStampToken(si.getSignature(), hashAlgorithm);

             final ASN1InputStream is = new ASN1InputStream(new ByteArrayInputStream(tsToken));
             final ASN1Primitive derObj = is.readObject();
             is.close();
             final DERSet derSet = new DERSet(derObj);

             final Attribute unsignAtt = new Attribute(new ASN1ObjectIdentifier(SIGNATURE_TIMESTAMP_TOKEN_OID), derSet);

             final Hashtable<DERObjectIdentifier, Attribute> ht = new Hashtable<DERObjectIdentifier, Attribute>();
             ht.put(new ASN1ObjectIdentifier(SIGNATURE_TIMESTAMP_TOKEN_OID), unsignAtt);

             final AttributeTable unsignedAtts = new AttributeTable(ht);


             vNewSigners.add(SignerInformation.replaceUnsignedAttributes(si, unsignedAtts));
        }

        return CMSSignedData.replaceSigners(signedData, new SignerInformationStore(vNewSigners)).getEncoded();


    }

    private byte[] getTSAResponse(final byte[] request) throws IOException {
    	if (this.tsaURL.getScheme().equals("socket")) { //$NON-NLS-1$
			return getTSAResponseSocket(request);
    	}
    	else if (this.tsaURL.getScheme().equals("http")) { //$NON-NLS-1$
    		return getTSAResponseHttp(request);
    	}
    	else if (this.tsaURL.getScheme().equals("https")) { //$NON-NLS-1$
    		return getTSAResponseHttps(request);
    	}
    	else {
			throw new UnsupportedOperationException("Protocolo de conexion con TSA no soportado: " + this.tsaURL.getScheme()); //$NON-NLS-1$
		}
    }

    private byte[] getTSAResponseSocket(final byte[] request) throws IOException {
    	final Socket socket = new Socket(this.tsaURL.getHost(), this.tsaURL.getPort());
    	socket.setSoTimeout(SOCKET_TIMEOUT);
    	final byte[] ret = getTSAResponseExternalSocket(request, socket);
    	socket.close();
    	return ret;
    }

    private static byte[] getTSAResponseExternalSocket(final byte[] request, final Socket socket) throws IOException {

    	// Envio de datos...
    	final DataOutputStream dataoutputstream = new DataOutputStream(socket.getOutputStream());
    	dataoutputstream.writeInt(request.length + 1);
    	dataoutputstream.writeByte(0);
    	dataoutputstream.write(request);
    	dataoutputstream.flush();
    	socket.getOutputStream().flush();

    	// Y recogida de la respuesta
    	final DataInputStream datainputstream = new DataInputStream(socket.getInputStream());
    	final int size = datainputstream.readInt();
    	final byte byte0 = datainputstream.readByte();
    	final byte[] resp = new byte[size - 1];
    	datainputstream.readFully(resp);
    	if (byte0 != 5 && byte0 != 6) {
			throw new IOException("Obtenida resuesta incorrecta del servidor TSA: " + new String(resp)); //$NON-NLS-1$
    	}
    	socket.close();

    	return resp;
    }

    /** Obtiene el <i>token</i> de sello de tiempo por HTTPS (SSL).
     * @param requestBytes Petici&oacute;n a TSA en ASN.1 binario
     * @return Respuesta de la TSA (array de bytes seg&uacute;n RFC 3161)
     * @throws IOException Si hay problemas de dentrada / salida */
    private byte[] getTSAResponseHttps(final byte[] requestBytes) throws IOException {
    	final SSLContext sc;
    	try {
    		sc = SSLContext.getInstance("SSL"); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		throw new IOException("No se ha podido obtener el contexto de seguridad SSL: " + e, e); //$NON-NLS-1$
    	}
    	final KeyManagerFactory keyManagerFactory;
    	if (this.sslP12KeyStoreFile != null && this.sslP12KeyStorePassword != null) {
    		try {
    			final KeyStore keystore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
				keystore.load(new ByteArrayInputStream(this.sslP12KeyStoreFile), this.sslP12KeyStorePassword.toCharArray());
	    		keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
	    		keyManagerFactory.init(keystore, this.sslP12KeyStorePassword.toCharArray());
			}
    		catch (final Exception e) {
    			throw new IOException("Error obteniendo el almacen de certificados cliente para el SSL: " + e, e); //$NON-NLS-1$
			}
    	}
    	else {
    		keyManagerFactory = null;
    	}
    	try {
    		sc.init(
				keyManagerFactory != null ?
					keyManagerFactory.getKeyManagers() :
						null,
				DUMMY_TRUST_MANAGER,
				new java.security.SecureRandom()
			);
    	}
    	catch(final Exception e) {
    		throw new IOException("Error creando el gestor laxo de seguridad SSL: " + e, e); //$NON-NLS-1$
    	}
		HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
		HttpsURLConnection.setDefaultHostnameVerifier(new HostnameVerifier() {
			@Override
			public boolean verify(final String hostname, final SSLSession session) {
				return true;
			}
		});
		try {
			return getTSAResponseHttp(requestBytes);
		}
		finally {
			HttpsURLConnection.setDefaultSSLSocketFactory(DEFAULT_SSL_SOCKET_FACTORY);
			HttpsURLConnection.setDefaultHostnameVerifier(DEFAULT_HOSTNAME_VERIFIER);
		}
    }

    /** Obtiene el <i>token</i> de sello de tiempo por HTTP.
     * @param requestBytes Petici&oacute;n a TSA en ASN.1 binario
     * @return Respuesta de la TSA (array de bytes seg&uacute;n RFC 3161)
     * @throws IOException Si hay problemas de dentrada / salida */
    private byte[] getTSAResponseHttp(final byte[] requestBytes) throws IOException {

         final URLConnection tsaConnection = this.tsaURL.toURL().openConnection();
         tsaConnection.setDoInput(true);
         tsaConnection.setDoOutput(true);
         tsaConnection.setUseCaches(false);
         tsaConnection.setRequestProperty("Content-Type", "application/timestamp-query"); //$NON-NLS-1$ //$NON-NLS-2$
         tsaConnection.setRequestProperty("Content-Transfer-Encoding", "binary"); //$NON-NLS-1$ //$NON-NLS-2$

         if (this.tsaUsername != null && !"".equals(this.tsaUsername) ) { //$NON-NLS-1$
             final String userPassword = this.tsaUsername + ":" + this.tsaPassword; //$NON-NLS-1$
             tsaConnection.setRequestProperty("Authorization", "Basic " + new String(Base64.encode(userPassword.getBytes()))); //$NON-NLS-1$ //$NON-NLS-2$
         }

         final OutputStream out = tsaConnection.getOutputStream();
         out.write(requestBytes);
         out.flush();
         out.close();

         final byte[] respBytes = AOUtil.getDataFromInputStream(tsaConnection.getInputStream());

         final String encoding = tsaConnection.getContentEncoding();
         if (encoding != null && encoding.equalsIgnoreCase("base64")) { //$NON-NLS-1$
             return Base64.decode(new String(respBytes));
         }

         return respBytes;

     }

    /** Obtiene directamente el <i>token</i> de sello de tiempo seg&uacute;n RFC3161.
     * @param imprint Huella digital de los datos sobre los que se quiere obtener el sello de tiempo
     * @param hashAlgorithm Algoritmo de huella digital usado
     * @return <i>Token</i> de sello de tiempo seg&uacute;n RFC3161
     * @throws AOException Si se produce un error en el protocolo TSA o en ASN.1
     * @throws IOException Si hay errores en la comunicaci&oacute;n o en la lectura de datos con la TSA */
    public byte[] getTimeStampToken(final byte[] imprint, final String hashAlgorithm) throws AOException, IOException {

         final TimeStampRequest request = this.tsqGenerator.generate(
               new ASN1ObjectIdentifier(hashAlgorithm != null ? AOAlgorithmID.getOID(hashAlgorithm) : X509ObjectIdentifiers.id_SHA1.getId()),
               imprint,
               BigInteger.valueOf(System.currentTimeMillis())
          );

         final byte[] requestBytes = request.getEncoded();

         final byte[] rawResponse = getTSAResponse(requestBytes);
         final TimeStampResponse response;
         try {
            response = new TimeStampResponse(rawResponse);
         }
         catch (final Exception e) {
            throw new AOException("Error obteniendo la respuesta de la TSA", e); //$NON-NLS-1$
         }

         // Validamos los atributos de la respuesta (RFC 3161 PKIStatus)
         try {
            response.validate(request);
        }
        catch (final Exception e) {
            throw new AOException("Error validando la respuesta de la TSA", e); //$NON-NLS-1$
        }
         final PKIFailureInfo failure = response.getFailInfo();
         final int value = failure == null ? 0 : failure.intValue();
         if (value != 0) {
             throw new AOException("Respuesta invalida de la TSA ('" + this.tsaURL + "') con el codigo " + value); //$NON-NLS-1$ //$NON-NLS-2$
         }

         // Extraemos el token de sello de tiempo (quitando la informacion de estado de las comunicaciones)
         final TimeStampToken  tsToken = response.getTimeStampToken();
         if (tsToken == null) {
             throw new AOException("La respuesta de la TSA ('" + this.tsaURL + "') no es un sello de tiempo valido: " + new String(rawResponse)); //$NON-NLS-1$ //$NON-NLS-2$
         }

         return tsToken.getEncoded();
     }

	 private static final TrustManager[] DUMMY_TRUST_MANAGER = new TrustManager[] {
		new X509TrustManager() {
			@Override
			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
				return null;
			}
			@Override
			public void checkClientTrusted(final X509Certificate[] certs, final String authType) { /* No hacemos nada */ }
			@Override
			public void checkServerTrusted(final X509Certificate[] certs, final String authType) {  /* No hacemos nada */  }
		}
	 };
}
