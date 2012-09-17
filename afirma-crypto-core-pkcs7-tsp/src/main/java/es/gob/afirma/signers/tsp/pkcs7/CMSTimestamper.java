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
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.URL;
import java.net.URLConnection;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;

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

    /** URL de la TSA de CatCert. */
    public static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$

    /** Pol&iacute;tica de sello de tiempo de la TSA de CatCert. */
    public static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$

    /** CatCert si requiere el Certificado. */
    public static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

    private static final String SIGNATURE_TIMESTAMP_TOKEN_OID = "1.2.840.113549.1.9.16.2.14"; //$NON-NLS-1$

    private final TimeStampRequestGenerator tsqGenerator;
    private final URL tsaURL;
    private final String tsaUsername;
    private final String tsaPassword;

    /** Construye un estampador de sellos de tiempo para estructuras CMS y CAdES.
     * @param requireCert <code>true</code> si la TSA requiere certificado, <code>false</code> en caso contrario
     * @param policy OID de la pol&iacute;tica de sellado de tiempo
     * @param tsa URL de la autoridad de sellado de tiempo
     * @param tsaUsr Nombre de usuario si la TSA requiere autenticaci&oacute;n (puede ser <code>null</code> si no se necesita autenticaci&oacute;n)
     * @param tsaPwd Contrase&ntilde;a del usuario de la TSA (puede ser <code>null</code> si no se necesita autenticaci&oacute;n)
     */
    public CMSTimestamper(final boolean requireCert,
                     final String policy,
                     final URL tsa,
                     final String tsaUsr,
                     final String tsaPwd) {
        this.tsqGenerator = new TimeStampRequestGenerator();
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

    /** Obtiene el <i>token</i> de sello de tiempo.
     * @return Respuesta de la TSA (array de bytes seg&uacute;n RFC 3161)
     * @throws IOException */
    private byte[] getTSAResponse(final byte[] requestBytes) throws IOException {

         final URLConnection tsaConnection = this.tsaURL.openConnection();
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

         final TimeStampResponse response;
         try {
            response = new TimeStampResponse(getTSAResponse(requestBytes));
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
             throw new AOException("La respuesta de la TSA ('" + this.tsaURL + "') no es un sello de tiempo valido"); //$NON-NLS-1$ //$NON-NLS-2$
         }

         return tsToken.getEncoded();

         // El tamano aproximado del token es tsToken.getEncoded().length + 32;
     }


}
