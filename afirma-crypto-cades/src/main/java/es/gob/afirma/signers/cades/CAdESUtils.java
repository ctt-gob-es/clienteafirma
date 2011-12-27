/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Date;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.DERIA5String;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.ess.ESSCertID;
import org.bouncycastle.asn1.ess.ESSCertIDv2;
import org.bouncycastle.asn1.ess.SigningCertificate;
import org.bouncycastle.asn1.ess.SigningCertificateV2;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.DigestInfo;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.IssuerSerial;
import org.bouncycastle.asn1.x509.PolicyInformation;
import org.bouncycastle.asn1.x509.PolicyQualifierId;
import org.bouncycastle.asn1.x509.PolicyQualifierInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.util.encoders.Base64Encoder;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Utilidades varias relacionadas con firmas electr&oacute;nicas CAdES. 
 * Se declara como clase p&uacute;blica para permitir su uso en el m&oacute;dulo de multifirmas CAdES */
public final class CAdESUtils {
    
    private CAdESUtils() {
        // No permitimos la instanciacion
    }
    
    /** Genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param cert Certificado del firmante
     * @param digestAlgorithmName Nombre del algoritmo de huella digital a usar
     * @param datos Datos firmados
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     * @param messageDigest Huella digital de los datos firmados
     * @param signDate Fecha de la firma (debe establecerse externamente para evitar desincronismos en la firma trif&aacute;sica)
     * @param padesMode <code>true</code> para generar una firma CAdES compatible PAdES, <code>false</code> para generar una firma CAdES normal
     * @return Los datos necesarios para generar la firma referente a los datos del usuario.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws CertificateEncodingException */
    public static ASN1EncodableVector generateSignerInfo(final X509Certificate cert,
                                                  final String digestAlgorithmName,
                                                  final byte[] datos,
                                                  final AdESPolicy policy,
                                                  final boolean signingCertificateV2,
                                                  final byte[] messageDigest,
                                                  final Date signDate,
                                                  final boolean padesMode) throws NoSuchAlgorithmException,
                                                                                     IOException,
                                                                                     CertificateEncodingException { 
        
        // ALGORITMO DE HUELLA DIGITAL
        final AlgorithmIdentifier digestAlgorithmOID = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));
        
        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = initContexExpecific(
                digestAlgorithmName, 
                datos, 
                PKCSObjectIdentifiers.data.getId(), 
                messageDigest, 
                signDate,
                padesMode
        );

        // Serial Number
        // comentar lo de abajo para version del rfc 3852
        // contexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        if (signingCertificateV2) {

            // INICIO SINGING CERTIFICATE-V2

            /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
             * CertificateSerialNumber */

            final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            final GeneralNames gns = new GeneralNames(new GeneralName(tbs.getIssuer()));

            final IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.getSerialNumber());

            /** ESSCertIDv2 ::= SEQUENCE { hashAlgorithm AlgorithmIdentifier
             * DEFAULT {algorithm id-sha256}, certHash Hash, issuerSerial
             * IssuerSerial OPTIONAL }
             * Hash ::= OCTET STRING */

            final byte[] certHash = MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded());
            final ESSCertIDv2[] essCertIDv2 = {
                new ESSCertIDv2(digestAlgorithmOID, certHash, isuerSerial)
            };

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            final SigningCertificateV2 scv2;
            if(policy.getPolicyIdentifier() != null) {                                                

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                scv2 = new SigningCertificateV2(essCertIDv2, getPolicyInformation(policy)); // con
                                                                  // politica
            }
            else {
                scv2 = new SigningCertificateV2(essCertIDv2); // Sin politica
            }

            // Secuencia con singningCertificate
            contexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_signingCertificateV2, new DERSet(scv2)));

            // FIN SINGING CERTIFICATE-V2

        }
        else {

            // INICIO SINGNING CERTIFICATE

            /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
             * CertificateSerialNumber } */

            final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            final GeneralName gn = new GeneralName(tbs.getIssuer());
            final GeneralNames gns = new GeneralNames(gn);

            final IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.getSerialNumber());

            /** ESSCertID ::= SEQUENCE { certHash Hash, issuerSerial IssuerSerial
             * OPTIONAL }
             * Hash ::= OCTET STRING -- SHA1 hash of entire certificate */
            final byte[] certHash = MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded());
            final ESSCertID essCertID = new ESSCertID(certHash, isuerSerial);

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            final SigningCertificate scv;
            if (policy.getPolicyIdentifier() != null) {

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                /*
                 * HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY
                 * CASTLE NO TIENE DICHO CONSTRUCTOR.
                 */
                final ASN1EncodableVector v = new ASN1EncodableVector();
                v.add(new DERSequence(essCertID));
                v.add(new DERSequence(getPolicyInformation(policy)));
                scv = new SigningCertificate(new DERSequence(v)); // con
                                                                  // politica
            }
            else {
                scv = new SigningCertificate(essCertID); // Sin politica
            }

            /** id-aa-signingCertificate OBJECT IDENTIFIER ::= { iso(1)
             * member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9) smime(16)
             * id-aa(2) 12 } */
            // Secuencia con singningCertificate
            contexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_signingCertificate, new DERSet(scv)));
        }

        // INICIO SIGPOLICYID ATTRIBUTE

        if (policy.getPolicyIdentifier() != null) {
            /**
             * SigPolicyId ::= OBJECT IDENTIFIER Politica de firma.
             */
            final DERObjectIdentifier doiSigPolicyId = new DERObjectIdentifier(policy.getPolicyIdentifier().toLowerCase().replace("urn:oid:", "")); //$NON-NLS-1$ //$NON-NLS-2$

            /**
             *   OtherHashAlgAndValue ::= SEQUENCE {
             *     hashAlgorithm    AlgorithmIdentifier,
             *     hashValue        OCTET STRING }
             *
             */
            

            // Algoritmo para el hash
            final AlgorithmIdentifier hashid;
            // si tenemos algoritmo de calculo de hash, lo ponemos
            if(policy.getPolicyIdentifierHashAlgorithm()!=null){
                hashid = SigUtils.makeAlgId(
                                    AOAlgorithmID.getOID(
                                    AOSignConstants.getDigestAlgorithmName(
                                       policy.getPolicyIdentifierHashAlgorithm())));
            }
            // si no tenemos, ponemos el algoritmo de firma.
            else{
                hashid= digestAlgorithmOID;
            }
            // hash del documento, descifrado en b64
            final byte[] hashed;
            if(policy.getPolicyIdentifierHash()!=null) {
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                new Base64Encoder().decode(policy.getPolicyIdentifierHash(), baos);
                hashed = baos.toByteArray();
            }
            else{
                hashed = new byte[]{0};
            }
            
            final DigestInfo otherHashAlgAndValue = new DigestInfo(hashid, hashed);
            
            /**
             *   AOSigPolicyQualifierInfo ::= SEQUENCE {
             *       SigPolicyQualifierId  SigPolicyQualifierId,
             *       SigQualifier          ANY DEFINED BY policyQualifierId }
             */
            AOSigPolicyQualifierInfo spqInfo = null;
            if(policy.getPolicyQualifier()!=null){
                spqInfo = new AOSigPolicyQualifierInfo(policy.getPolicyQualifier().toString());
            }
            
            /**
             * SignaturePolicyId ::= SEQUENCE {
             *  sigPolicyId           SigPolicyId,
             *  sigPolicyHash         SigPolicyHash,
             *  sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
             *                          AOSigPolicyQualifierInfo OPTIONAL}
             *
             */
            final ASN1EncodableVector v = new ASN1EncodableVector();
            // sigPolicyId
            v.add(doiSigPolicyId);
            // sigPolicyHash
            v.add(otherHashAlgAndValue.toASN1Object()); // como sequence
            // sigPolicyQualifiers
            if(spqInfo!=null) {
                v.add(spqInfo.toASN1Object());
            }

            final DERSequence ds = new DERSequence(v);

            // Secuencia con singningCertificate
            contexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId, new DERSet(ds.toASN1Object())));
            // FIN SIGPOLICYID ATTRIBUTE
        }

        return contexExpecific;
    }
    
    /**
     * Obtiene un PolicyInformation a partir de los datos de la pol&iacute;tica.
     * Sirve para los datos de SigningCertificate y SigningCertificateV2. Tiene que llevar algunos
     * datos de la pol&iacute;tica.
     * <pre>
     * PolicyInformation ::= SEQUENCE {
     * policyIdentifier   CertPolicyId,
     * policyQualifiers   SEQUENCE SIZE (1..MAX) OF
     *                          PolicyQualifierInfo OPTIONAL }                          
     *                          
     *                          
     * CertPolicyId ::= OBJECT IDENTIFIER
     *
     * PolicyQualifierInfo ::= SEQUENCE {
     *      policyQualifierId  PolicyQualifierId,
     *      qualifier          ANY DEFINED BY policyQualifierId }
     *
     * -- policyQualifierIds for Internet policy qualifiers
     *
     * id-qt          OBJECT IDENTIFIER ::=  { id-pkix 2 }
     * id-qt-cps      OBJECT IDENTIFIER ::=  { id-qt 1 }
     * id-qt-unotice  OBJECT IDENTIFIER ::=  { id-qt 2 }
     *
     * PolicyQualifierId ::=
     *      OBJECT IDENTIFIER ( id-qt-cps | id-qt-unotice )
     *
     * Qualifier ::= CHOICE {
     *      cPSuri           CPSuri,
     *      userNotice       UserNotice }
     *
     * CPSuri ::= IA5String
     *
     * UserNotice ::= SEQUENCE {
     *      noticeRef        NoticeReference OPTIONAL,
     *      explicitText     DisplayText OPTIONAL}
     *
     * NoticeReference ::= SEQUENCE {
     *      organization     DisplayText,
     *      noticeNumbers    SEQUENCE OF INTEGER }
     *
     * DisplayText ::= CHOICE {
     *      ia5String        IA5String      (SIZE (1..200)),
     *      visibleString    VisibleString  (SIZE (1..200)),
     *      bmpString        BMPString      (SIZE (1..200)),
     *      utf8String       UTF8String     (SIZE (1..200)) }
     * </pre>
     * 
     * @param policy    Pol&iacute;tica de la firma.
     * @return          Estructura con la pol&iacute;tica preparada para insertarla en la firma.
     */
    private static PolicyInformation[] getPolicyInformation(final AdESPolicy policy){
        
        if (policy == null) {
            throw new IllegalArgumentException("La politica de firma no puede ser nula en este punto"); //$NON-NLS-1$
        }
        
        /**
         * PolicyQualifierInfo ::= SEQUENCE {
         *          policyQualifierId  PolicyQualifierId,
         *          qualifier          ANY DEFINED BY policyQualifierId } 
         */
        
        final PolicyQualifierId pqid = PolicyQualifierId.id_qt_cps;
        DERIA5String uri = null;

        if (policy.getPolicyQualifier()!=null && !policy.getPolicyQualifier().equals("")){ //$NON-NLS-1$
            uri = new DERIA5String(policy.getPolicyQualifier().toString());
        }

        final ASN1EncodableVector v = new ASN1EncodableVector();
        PolicyQualifierInfo pqi = null;
        if(uri != null){
            v.add(pqid);
            v.add(uri);
            pqi = new PolicyQualifierInfo(new DERSequence(v));
        }
        
        /**
         * PolicyInformation ::= SEQUENCE {
         *     policyIdentifier   CertPolicyId,
         *     policyQualifiers   SEQUENCE SIZE (1..MAX) OF
         *                          PolicyQualifierInfo OPTIONAL }
         */
        
        if (policy.getPolicyQualifier()==null || pqi == null) {
            return new PolicyInformation[] {
                new PolicyInformation(new DERObjectIdentifier(policy.getPolicyIdentifier().toLowerCase().replace("urn:oid:", ""))) //$NON-NLS-1$ //$NON-NLS-2$
            };
        }

        return new PolicyInformation[] {
            new PolicyInformation(new DERObjectIdentifier(policy.getPolicyIdentifier().toLowerCase().replace("urn:oid:", "")), new DERSequence(pqi)) //$NON-NLS-1$ //$NON-NLS-2$
        };
        
    }
    
    /** Inicializa el contexto. */
    static ASN1EncodableVector initContexExpecific(final String digestAlgorithm, 
                                                   final byte[] datos, 
                                                   final String dataType, 
                                                   final byte[] messageDigest, 
                                                   final Date signDate,
                                                   final boolean padesMode) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        if (dataType != null) {
            contexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType))));
        }

        // fecha de firma, no se anade en modo PAdES
        if (!padesMode) {
            contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(signDate))));
        }

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString((messageDigest != null) ? messageDigest : MessageDigest.getInstance(digestAlgorithm).digest(datos)))));

        return contexExpecific;
    }
    
}
