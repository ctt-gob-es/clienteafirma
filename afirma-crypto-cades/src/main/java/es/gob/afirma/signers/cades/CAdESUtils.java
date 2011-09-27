package es.gob.afirma.signers.cades;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Date;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
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
import org.bouncycastle.asn1.x500.style.RFC4519Style;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.DigestInfo;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.IssuerSerial;
import org.bouncycastle.asn1.x509.PolicyInformation;
import org.bouncycastle.asn1.x509.PolicyQualifierInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.signers.beans.AdESPolicy;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Utilidades varias para las firmas CAdES. */
public class CAdESUtils {
    
    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param cert Certificado de firma.
     * @param digestAlgorithmName Nombre del algoritmo de huella digital a usar
     * @param datos Datos firmados.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     * @param dataType Tipo del contenido a firmar.
     * @param messageDigest Huella digital de los datos firmados
     * @param signDate Fecha de la firma (debe establecerse externamente para evitar desincronismos en la firma trif&aacute;sica)
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
                                                  final Date signDate) throws NoSuchAlgorithmException,
                                                                                     IOException,
                                                                                     CertificateEncodingException {
        
        //TODO: Incorporar la politica segun la nueva interpretacion 
        
        // ALGORITMO DE HUELLA DIGITAL
        final AlgorithmIdentifier digestAlgorithmOID = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));
        
        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = initContexExpecific(digestAlgorithmName, datos, PKCSObjectIdentifiers.data.getId(), messageDigest, signDate);

        // Serial Number
        // comentar lo de abajo para version del rfc 3852
        contexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        if (signingCertificateV2) {

            //********************************************/
            //***** La Nueva operatividad esta comentada */
            //********************************************/
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

            final MessageDigest md = MessageDigest.getInstance(digestAlgorithmName);
            final byte[] certHash = md.digest(cert.getEncoded());
            final ESSCertIDv2[] essCertIDv2 = {
                new ESSCertIDv2(digestAlgorithmOID, certHash, isuerSerial)
            };

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            PolicyInformation[] pI;
            SigningCertificateV2 scv2 = null;
            if (policyQualifier != null) {

                final DERObjectIdentifier oidQualifier = new DERObjectIdentifier(policyQualifier.toString());
                if (policyIdentifier.equals("")) { //$NON-NLS-1$
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier)
                    };
                }
                else {
                    final PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(policyIdentifier);
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier, new DERSequence(pqInfo))
                    };
                }

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                scv2 = new SigningCertificateV2(essCertIDv2, pI); // con
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

            PolicyInformation[] pI;
            SigningCertificate scv = null;
            if (policyQualifier != null) {

                final DERObjectIdentifier oidQualifier = new DERObjectIdentifier(policyQualifier.toString());
                if (policyIdentifier.equals("")) { //$NON-NLS-1$
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier)
                    };
                }
                else {
                    final PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(policyIdentifier);
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier, new DERSequence(pqInfo))
                    };
                }

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                /*
                 * HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY
                 * CASTLE NO TIENE DICHO CONSTRUCTOR.
                 */
                final ASN1EncodableVector v = new ASN1EncodableVector();
                v.add(new DERSequence(essCertID));
                v.add(new DERSequence(pI));
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

        if (policyQualifier != null) {
            /*
             * SigPolicyId ::= OBJECT IDENTIFIER Politica de firma.
             */
            final DERObjectIdentifier DOISigPolicyId = new DERObjectIdentifier(policyQualifier.toString());

            /*
             * OtherHashAlgAndValue ::= SEQUENCE { hashAlgorithm
             * AlgorithmIdentifier, hashValue OCTET STRING }
             */
            final MessageDigest mdgest = MessageDigest.getInstance(digestAlgorithmName);
            final byte[] hashed = mdgest.digest(policyIdentifier.getBytes());
            final DigestInfo OtherHashAlgAndValue = new DigestInfo(digestAlgorithmOID, hashed);

            /*
             * SigPolicyQualifierInfo ::= SEQUENCE { SigPolicyQualifierId
             * SigPolicyQualifierId, SigQualifier ANY DEFINED BY
             * policyQualifierId }
             */

            final SigPolicyQualifierInfo spqInfo = new SigPolicyQualifierInfo(policyIdentifier);

            /*
             * SignaturePolicyId ::= SEQUENCE { sigPolicyId SigPolicyId,
             * sigPolicyHash SigPolicyHash, sigPolicyQualifiers SEQUENCE SIZE
             * (1..MAX) OF SigPolicyQualifierInfo OPTIONAL}
             */
            final ASN1EncodableVector v = new ASN1EncodableVector();
            // sigPolicyId
            v.add(DOISigPolicyId);
            // sigPolicyHash
            v.add(OtherHashAlgAndValue.toASN1Object()); // como sequence
            // sigPolicyQualifiers
            v.add(spqInfo.toASN1Object());

            final DERSequence ds = new DERSequence(v);

            // Secuencia con singningCertificate
            contexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId, new DERSet(ds.toASN1Object())));
            // FIN SIGPOLICYID ATTRIBUTE
        }

        return contexExpecific;
    }
    
    /** Inicializa el context
     * @param digestAlgorithm
     * @param datos
     * @param dataType
     * @param messageDigest
     * @return ASN1EncodableVector
     * @throws NoSuchAlgorithmException */
    static ASN1EncodableVector initContexExpecific(final String digestAlgorithm, final byte[] datos, final String dataType, final byte[] messageDigest, final Date signDate) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        if (dataType != null) {
            ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));
        }

        // fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(signDate))));

        // MessageDigest
        ContexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString((messageDigest != null) ? messageDigest : MessageDigest.getInstance(digestAlgorithm).digest(datos)))));

        return ContexExpecific;
    }    
}
