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
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.net.Socket;
import java.net.URI;
import java.net.URLConnection;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.logging.Logger;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1Primitive;
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
import es.gob.afirma.core.signers.AOSignConstants;
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

	private static final int SOCKET_TIMEOUT = 500000;

	private static final String STORE_TYPE_PKCS12 = "PKCS12"; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final TimeStampRequestGenerator tsqGenerator;
    private final URI tsaURL;
    private final String tsaUsername;
    private final String tsaPassword;

    private byte[] sslKeyStoreFile = null;
    private String sslKeyStorePassword = null;
    private String sslKeyStoreType = null;

    private byte[] sslTrustStoreFile = null;
    private String sslTrustStorePassword = null;
    private String sslTrustStoreType = null;

    private boolean verifyHostname = true;

    /** Construye un estampador de sellos de tiempo para estructuras CMS y CAdES.
     * @param requireCert <code>true</code> si la TSA requiere certificado, <code>false</code> en caso contrario.
     * @param policy OID de la pol&iacute;tica de sellado de tiempo.
     * @param tsa URL de la autoridad de sellado de tiempo.
     * @param tsaUsr Nombre de usuario si la TSA requiere autenticaci&oacute;n (puede ser <code>null</code> si no se necesita autenticaci&oacute;n).
     * @param tsaPwd Contrase&ntilde;a del usuario de la TSA (puede ser <code>null</code> si no se necesita autenticaci&oacute;n).
     * @param extensions Extensiones a a&ntilde;adir a la petici&oacute;n de sello de tiempo.
     * @param p12KeyStoreFile Fichero PKCS#12 / PFX de almac&eacute;n (formato PKCS#12) del certificado cliente a usar en conexiones SSL.
     * @param p12KeyStoreFilePassword Contrase&ntilde;a del ichero PKCS#12 / PFX de almac&eacute;n (formato PKCS#12) del certificado.
     *                                cliente a usar en conexiones SSL */
    public CMSTimestamper(final boolean requireCert,
                     final String policy,
                     final URI tsa,
                     final String tsaUsr,
                     final String tsaPwd,
                     final TsaRequestExtension[] extensions,
                     final byte[] p12KeyStoreFile,
                     final String p12KeyStoreFilePassword) {
        this(requireCert, policy, tsa, tsaUsr, tsaPwd, extensions, p12KeyStoreFile, p12KeyStoreFilePassword, STORE_TYPE_PKCS12, null, null, null, false);
    }

    /** Construye un estampador de sellos de tiempo para estructuras CMS y CAdES.
     * @param requireCert <code>true</code> si la TSA requiere certificado, <code>false</code> en caso contrario.
     * @param policy OID de la pol&iacute;tica de sellado de tiempo.
     * @param tsa URL de la autoridad de sellado de tiempo.
     * @param tsaUsr Nombre de usuario si la TSA requiere autenticaci&oacute;n (puede ser <code>null</code> si no se necesita autenticaci&oacute;n).
     * @param tsaPwd Contrase&ntilde;a del usuario de la TSA (puede ser <code>null</code> si no se necesita autenticaci&oacute;n).
     * @param extensions Extensiones a a&ntilde;adir a la petici&oacute;n de sello de tiempo.
     * @param keyStoreFile Fichero de almac&eacute;n del certificado cliente a usar en conexiones SSL.
     * @param keyStorePassword Contrase&ntilde;a del almac&eacute;n del certificado cliente a usar en conexiones SSL.
     * @param keyStoreType Tipo de almac&eacute;n del certificado cliente a usar en conexiones SSL (tal y como se especifica en <code>KeyStore.getInstance()</code>).
     * @param trustStoreFile Fichero de almac&eacute;n para certificados de confianza en conexiones SSL.
     * @param trustStorePassword Contrase&ntilde;a del almac&eacute;n para certificados de confianza en conexiones SSL.
     * @param trustStoreType Tipo de almac&eacute;n para certificados de confianza en conexiones SSL (tal y como se especifica en <code>KeyStore.getInstance()</code>).
     * @param verifyHostname Si se especifica <code>true</code>, se verifica el nombre de <i>host</i> en las comprobaciones SSL, si por el contrario
     *                       se especifica <code>false</code> se omite esta comprobaci&oacute;n. */
    public CMSTimestamper(final boolean requireCert,
                     final String policy,
                     final URI tsa,
                     final String tsaUsr,
                     final String tsaPwd,
                     final TsaRequestExtension[] extensions,
                     final byte[] keyStoreFile,
                     final String keyStorePassword,
                     final String keyStoreType,
                     final byte[] trustStoreFile,
                     final String trustStorePassword,
                     final String trustStoreType,
                     final boolean verifyHostname) {
        this.tsqGenerator = new TimeStampRequestGenerator();
        if (extensions != null) {
        	for (final TsaRequestExtension ext : extensions) {
        		this.tsqGenerator.addExtension(
    				new ASN1ObjectIdentifier(ext.getOid()),
    				ext.isCritical(),
    				ext.getValue()
				);
        		LOGGER.info("Anadida extension a la solicitud de sello de tiempo: " + ext); //$NON-NLS-1$
        	}
        }
        this.tsqGenerator.setCertReq(requireCert);
        this.tsqGenerator.setReqPolicy(new ASN1ObjectIdentifier(policy));
        this.tsaURL = tsa;
        this.tsaPassword = tsaPwd;
        this.tsaUsername = tsaUsr;
		this.sslKeyStoreFile = keyStoreFile != null ? keyStoreFile.clone() : null;
		this.sslKeyStorePassword = keyStorePassword;
		this.sslKeyStoreType = keyStoreType;
		this.sslTrustStoreFile = trustStoreFile != null ? trustStoreFile.clone() : null;
		this.sslTrustStorePassword = trustStorePassword;
		this.sslTrustStoreType = trustStoreType;
		this.verifyHostname = verifyHostname;
    }

    /** Construye un estampador de sellos de tiempo para estructuras CMS y CAdES.
     * @param params Par&aacute;metros de configuraci&oacute;n de una Autoridad de Sellado de Tiempo. */
    public CMSTimestamper(final TsaParams params) {
    	this(
			params.doTsaRequireCert(),
			params.getTsaPolicy(),
			params.getTsaUrl(),
			params.getTsaUsr(),
			params.getTsaPwd(),
			params.getExtensions(),
			params.getSslKeyStore(),
			params.getSslKeyStorePassword(),
			params.getSslKeyStoreType(),
			params.getSslTrustStore(),
			params.getSslTrustStorePassword(),
			params.getSslTrustStoreType(),
			params.isVerifyHostname()
		);
    }

    /** A&ntilde;ade un sello de tiempo a las firmas encontradas dentro de una estructura PKCS#7.
     * @param pkcs7 Estructura que contiene las firmas a estampar un sello de tiempo
     * @param hashAlgorithm Algoritmo de huella digital a usar en los sellos de tiempo (si se indica <code>null</code> se usa SHA-1)
     * @param time Tiempo del sello
     * @return Nueva estructura PKCS#7 con los sellos de tiempo a&ntilde;adidos
     * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella digital del sello de tiempo
     * @throws AOException Cuando ocurren errores gen&eacute;ricos
     * @throws IOException Si hay errores de entrada / salida */
    public byte[] addTimestamp(final byte[] pkcs7, final String hashAlgorithm, final Calendar time) throws NoSuchAlgorithmException, AOException, IOException {

    	final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(hashAlgorithm);

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

             final byte[] tsToken = getTimeStampToken(
        		 MessageDigest.getInstance(digestAlgorithm).digest(si.getSignature()),
        		 digestAlgorithm,
        		 time
    		 );

             final ASN1InputStream is = new ASN1InputStream(new ByteArrayInputStream(tsToken));
             final ASN1Primitive derObj = is.readObject();
             is.close();
             final DERSet derSet = new DERSet(derObj);

             final Attribute unsignAtt = new Attribute(new ASN1ObjectIdentifier(SIGNATURE_TIMESTAMP_TOKEN_OID), derSet);

             final Hashtable<ASN1ObjectIdentifier, Attribute> ht = new Hashtable<ASN1ObjectIdentifier, Attribute>();
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

    /** Obtiene el <i>token</i> de sello de tiempo por HTTP.
     * @param requestBytes Petici&oacute;n a TSA en ASN.1 binario
     * @return Respuesta de la TSA (array de bytes seg&uacute;n RFC 3161)
     * @throws IOException Si hay problemas de dentrada / salida */
    private byte[] getTSAResponseHttp(final byte[] requestBytes) throws IOException {
    	return getTSAResponseHttp(requestBytes, prepareConnection(false));
     }


    /** Obtiene el <i>token</i> de sello de tiempo por HTTPS (SSL).
     * @param requestBytes Petici&oacute;n a TSA en ASN.1 binario
     * @return Respuesta de la TSA (array de bytes seg&uacute;n RFC 3161)
     * @throws IOException Si hay problemas de dentrada / salida */
    private byte[] getTSAResponseHttps(final byte[] requestBytes) throws IOException {
		return getTSAResponseHttp(requestBytes, prepareConnection(true));
    }

    /** Obtiene el <i>token</i> de sello de tiempo por HTTP.
     * @param requestBytes Petici&oacute;n a TSA en ASN.1 binario
     * @param conn Conexi&oacute;n a trav&eacute;s de la cual enviar la petici&oacute;n.
     * @return Respuesta de la TSA (array de bytes seg&uacute;n RFC 3161)
     * @throws IOException Si hay problemas de dentrada / salida */
    private static byte[] getTSAResponseHttp(final byte[] requestBytes, final URLConnection conn) throws IOException {

         final OutputStream out = conn.getOutputStream();
         out.write(requestBytes);
         out.flush();
         out.close();

         final byte[] respBytes = AOUtil.getDataFromInputStream(conn.getInputStream());

         final String encoding = conn.getContentEncoding();
         if (encoding != null && encoding.equalsIgnoreCase("base64")) { //$NON-NLS-1$
             return Base64.decode(new String(respBytes));
         }

         return respBytes;
     }

    /**
     * Crea una conexi&oacute;n contra la TSA. Esta se realizar&aacute; sobre HTTPS o HTTP
     * seg&uacute;n se indique en el par&aacute;metro {@code secureConnection}.
     * @param secureConnection {@code true} para que la conexi&oacute;n se realice sobre HTTPS,
     * {@code false} para HTTP.
     * @return Conex&iacute;n contra la TSA.
     * @throws IOException Cuando ocurre un error al abrir la conexi&oacute;n o si la URL no es v&aacute;lida.
     */
    private URLConnection prepareConnection(final boolean secureConnection) throws IOException {

    	final URLConnection tsaConnection = this.tsaURL.toURL().openConnection();
        tsaConnection.setDoInput(true);
        tsaConnection.setDoOutput(true);
        tsaConnection.setUseCaches(false);
        tsaConnection.setRequestProperty("Content-Type", "application/timestamp-query"); //$NON-NLS-1$ //$NON-NLS-2$
        tsaConnection.setRequestProperty("Content-Transfer-Encoding", "binary"); //$NON-NLS-1$ //$NON-NLS-2$

        if (secureConnection) {
        	configureHttpsConnection(tsaConnection);
        }

        if (this.tsaUsername != null && !"".equals(this.tsaUsername) ) { //$NON-NLS-1$
            final String userPassword = this.tsaUsername + ":" + this.tsaPassword; //$NON-NLS-1$
            tsaConnection.setRequestProperty("Authorization", "Basic " + new String(Base64.encode(userPassword.getBytes()))); //$NON-NLS-1$ //$NON-NLS-2$
        }

        return tsaConnection;
    }

    /** Configura la conexi&oacute;n con los datos necesarios para realizarse sobre HTTPS.
     * @param conn Conexi&oacute;n SSL.
     * @throws IOException Cuando el entorno no permite la configuraci&oacute;n. */
    @SuppressWarnings("deprecation")
	private void configureHttpsConnection(final URLConnection conn) throws IOException {

    	if (conn == null) {
    		throw new IllegalArgumentException("La conexion no puede ser nula"); //$NON-NLS-1$
    	}

    	if (!this.verifyHostname) {

    		// Podrian encontrarse varios tipos de conexion HTTPS

	    	if (conn instanceof javax.net.ssl.HttpsURLConnection) {
	    		((javax.net.ssl.HttpsURLConnection)conn).setHostnameVerifier(
					new javax.net.ssl.HostnameVerifier() {
		    			@Override
		    			public boolean verify(final String hostname, final SSLSession session) {
		    				return true;
		    			}
		    		}
				);
	    	}
	    	else {
	    		Class<?> sunHttpsURLConnectionClass;
	    		try {
	    			sunHttpsURLConnectionClass = Class.forName("com.sun.net.ssl.HttpsURLConnection"); //$NON-NLS-1$
	    		}
	    		catch (final Exception e) {
	    			sunHttpsURLConnectionClass = null;
	    		}

	    		if (sunHttpsURLConnectionClass != null && sunHttpsURLConnectionClass.isInstance(conn)) {

	    			try {
	    				// Este caso es problematico porque se deshabilita globalmente (metodo estatico) la comprobacion de
	    				// nombre de host, y no solo para la conexion en curso.
	    				// No obstante, la JVM no deberia darnos nunca este tipo de conexiones, porque estan ya deprecadas
	    				// y obsoletas.
	    				final Method setDefaultHostnameVerifierMethod = sunHttpsURLConnectionClass.getDeclaredMethod("setDefaultHostnameVerifier"); //$NON-NLS-1$
	    				setDefaultHostnameVerifierMethod.invoke(
	    						null,
	    						new com.sun.net.ssl.HostnameVerifier() {
	    							@Override
	    							public boolean verify(final String arg0, final String arg1) {
	    								return true;
	    							}
	    						}
						);
	    			}
	    			catch (final Exception e) {
	    				LOGGER.warning(
	    						"Ocurrio un error al intentar instanciar una conexion de tipo 'com.sun.net.ssl.HttpsURLConnection' para sobreescribir la conexion el host, se continuara la operacion" //$NON-NLS-1$
	    						);
	    			}
	    		}
	    		else {
	    			LOGGER.warning(
	    				"No se ha podido deshabilitar la comprobacion de nombre de host, tipo desconocido de conexion: " + conn.getClass().getName() //$NON-NLS-1$
	    			);
	    		}
	    	}
    	}

    	final SSLContext sc;
    	try {
    		sc = SSLContext.getInstance("TLS"); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		throw new IOException("No se ha podido obtener el contexto de seguridad SSL: " + e, e); //$NON-NLS-1$
    	}

    	// Configuramos el almacen con el certificado cliente
    	KeyManager[] keyManagers = null;
    	if (this.sslKeyStoreFile != null && this.sslKeyStorePassword != null) {
    		try {
    			final KeyStore keystore = KeyStore.getInstance(this.sslKeyStoreType == null ? "JKS" : this.sslKeyStoreType); //$NON-NLS-1$
				keystore.load(new ByteArrayInputStream(this.sslKeyStoreFile), this.sslKeyStorePassword.toCharArray());
				final KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
	    		keyManagerFactory.init(keystore, this.sslKeyStorePassword.toCharArray());

	    		keyManagers = keyManagerFactory.getKeyManagers();
			}
    		catch (final Exception e) {
    			throw new IOException("Error obteniendo el almacen de certificados cliente para el SSL: " + e, e); //$NON-NLS-1$
			}
    	}

    	// Configuramos el almacen con las autoridades de confianza
    	TrustManager[] trustManagers = null;
    	if (this.sslTrustStoreFile != null && this.sslTrustStorePassword != null) {
    		try {
    			final KeyStore keystore = KeyStore.getInstance(this.sslTrustStoreType == null ? "JKS" : this.sslTrustStoreType); //$NON-NLS-1$
				keystore.load(new ByteArrayInputStream(this.sslTrustStoreFile), this.sslTrustStorePassword.toCharArray());
				final TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
				trustManagerFactory.init(keystore);

				trustManagers = trustManagerFactory.getTrustManagers();
			}
    		catch (final Exception e) {
    			throw new IOException("Error obteniendo el almacen de confianza con los certificados de CA con los que se configuro la SSL: " + e, e); //$NON-NLS-1$
			}
    	}
    	else {
    		trustManagers = new TrustManager[] {
    			new X509TrustManager() {
					@Override
					public void checkClientTrusted(final X509Certificate[] arg0, final String arg1) throws CertificateException { /* No hacemos nada. */ }

					@Override
					public void checkServerTrusted(final X509Certificate[] arg0, final String arg1) throws CertificateException { /* No hacemos nada. */ }

					@Override
					public X509Certificate[] getAcceptedIssuers() {
						return new X509Certificate[0];
					}
				}
    		};
    	}

    	// Se configura el contexto SSL
    	try {
    		sc.init(
				keyManagers,
				trustManagers,
				null // Dejamos que use el generador de aleatorios por defecto
			);
    	}
    	catch(final Exception e) {
    		throw new IOException("Error creando el gestor de seguridad SSL: " + e, e); //$NON-NLS-1$
    	}

    	try {
    		final Method setSSLSocketFactoryMethod = conn.getClass().getMethod("setSSLSocketFactory", SSLSocketFactory.class); //$NON-NLS-1$
    		setSSLSocketFactoryMethod.invoke(conn, sc.getSocketFactory());
    	}
    	catch (final Exception e) {
    		LOGGER.severe("Error en la configuracion del acceso a la URL sobre SSL"); //$NON-NLS-1$
    		throw new IOException("Error en la configuracion del acceso a la URL sobre SSL", e); //$NON-NLS-1$
    	}
    }

    /** Obtiene directamente el <i>token</i> de sello de tiempo seg&uacute;n RFC3161.
     * @param imprint Huella digital de los datos sobre los que se quiere obtener el sello de tiempo
     * @param hashAlgorithm Algoritmo de huella digital usado para calcular la huella indicada en <code>imprint</code>.
     * @param time Tiempo de solicitud del sello.
     * @return <i>Token</i> de sello de tiempo seg&uacute;n RFC3161.
     * @throws AOException Si se produce un error en el protocolo TSA o en ASN.1.
     * @throws IOException Si hay errores en la comunicaci&oacute;n o en la lectura de datos con la TSA. */
    public byte[] getTimeStampToken(final byte[] imprint, final String hashAlgorithm, final Calendar time) throws AOException, IOException {

    	final TimeStampRequest request = this.tsqGenerator.generate(
               new ASN1ObjectIdentifier(hashAlgorithm != null ? AOAlgorithmID.getOID(hashAlgorithm) : X509ObjectIdentifiers.id_SHA1.getId()),
               imprint,
               BigInteger.valueOf(time != null ? time.getTimeInMillis() : System.currentTimeMillis())
          );

         final byte[] requestBytes = request.getEncoded();

         final byte[] rawResponse = getTSAResponse(requestBytes);
         final TimeStampResponse response;
         try {
            response = new TimeStampResponse(rawResponse);
         }
         catch (final Exception e) {
            throw new AOException("Error obteniendo la respuesta de la TSA: " + e, e); //$NON-NLS-1$
         }

         // Validamos los atributos de la respuesta (RFC 3161 PKIStatus)
         try {
            response.validate(request);
         }
         catch (final Exception e) {
            throw new AOException("Error validando la respuesta de la TSA: " + e, e); //$NON-NLS-1$
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
}
