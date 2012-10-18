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
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.logging.Logger;

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

/** Generador local de sellos de tiempo para PKCS#7. */
public final class CMSTimestamper {

	/** Extensi&oacute;n para una solicitud de TSA seg&uacute;n RFC 2161. */
	public static class TsaRequestExtension {

		private final String oid;
		private final boolean critical;
		private final byte[] value;

		@Override
		public String toString() {
			return "Extension [OID: " + this.oid + ", citical: " + this.critical + ", value: " + new String(this.value) + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}

		/** Crea una extensi&oacute;n para una solicitud de TSA seg&uacute;n RFC 2161.
		 * @param oid OID de la extensi&oacute;n
		 * @param isCritical <code>true</code> si la extensi&oacute;n es cr&iacute;tica, <code>false</code> en caso contrario
		 * @param value Valor de la extensi&oacute;n */
		public TsaRequestExtension(final String oid, final boolean isCritical, final byte[] value) {
			if (oid == null || "".equals(oid)) { //$NON-NLS-1$
				throw new IllegalArgumentException("Las extensiones TSA necesitan obligatoriamente un OID"); //$NON-NLS-1$
			}
			if (value == null || value.length < 1) {
				throw new IllegalArgumentException("Las extensiones TSA necesitan obligatoriamente un valor"); //$NON-NLS-1$
			}
			this.oid = oid;
			this.critical = isCritical;
			this.value = value.clone();
		}

		boolean isCritical() {
			return this.critical;
		}

		String getOid() {
			return this.oid;
		}

		byte[] getValue() {
			return this.value.clone();
		}
	}

    /** URL de la TSA de CatCert. */
    public static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$

    /** Pol&iacute;tica de sello de tiempo de la TSA de CatCert. */
    public static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$

    /** CatCert si requiere el Certificado. */
    public static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

    private static final String SIGNATURE_TIMESTAMP_TOKEN_OID = "1.2.840.113549.1.9.16.2.14"; //$NON-NLS-1$

    private final TimeStampRequestGenerator tsqGenerator;
    private final URI tsaURL;
    private final String tsaUsername;
    private final String tsaPassword;

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
     * @throws NoSuchAlgorithmException
     * @throws AOException
     * @throws IOException
     */
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

             final ASN1Primitive derObj = new ASN1InputStream(new ByteArrayInputStream(tsToken)).readObject();
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
    	else {
			throw new UnsupportedOperationException("Protocolo de conexion con TSA no soportado"); //$NON-NLS-1$
		}
    }

    private byte[] getTSAResponseSocket(final byte[] request) throws IOException {
    	final Socket socket = new Socket(this.tsaURL.getHost(), this.tsaURL.getPort());
    	socket.setSoTimeout(500000);

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

    	System.out.println(new String(resp));
    	return resp;
    }

    /** Obtiene el <i>token</i> de sello de tiempo por HTTP.
     * @return Respuesta de la TSA (array de bytes seg&uacute;n RFC 3161)
     * @throws IOException */
    private byte[] getTSAResponseHttp(final byte[] requestBytes) throws IOException {

         final URLConnection tsaConnection = this.tsaURL.toURL().openConnection();
         tsaConnection.setDoInput(true);
         tsaConnection.setDoOutput(true);
         tsaConnection.setUseCaches(false);
         tsaConnection.setRequestProperty("Content-Type", "application/timestamp-query"); //$NON-NLS-1$ //$NON-NLS-2$
         tsaConnection.setRequestProperty("Content-Transfer-Encoding", "binary"); //$NON-NLS-1$ //$NON-NLS-2$

         if ((this.tsaUsername != null) && !"".equals(this.tsaUsername) ) { //$NON-NLS-1$
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

     private byte[] getTimeStampToken(final byte[] imprint, final String hashAlgorithm) throws AOException, IOException {

         final TimeStampRequest request = this.tsqGenerator.generate(
               new ASN1ObjectIdentifier((hashAlgorithm != null) ? AOAlgorithmID.getOID(hashAlgorithm) : X509ObjectIdentifiers.id_SHA1.getId()),
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
         final int value = (failure == null) ? 0 : failure.intValue();
         if (value != 0) {
             throw new AOException("Respuesta invalida de la TSA ('" + this.tsaURL + "') con el codigo " + value); //$NON-NLS-1$ //$NON-NLS-2$
         }

         // Extraemos el token de sello de tiempo (quitando la informacion de estado de las comunicaciones)
         final TimeStampToken  tsToken = response.getTimeStampToken();
         if (tsToken == null) {
             throw new AOException("La respuesta de la TSA ('" + this.tsaURL + "') no es un sello de tiempo valido: " + new String(rawResponse)); //$NON-NLS-1$ //$NON-NLS-2$
         }

         return tsToken.getEncoded();

         // El tamano aproximado del token es tsToken.getEncoded().length + 32;
     }


}
