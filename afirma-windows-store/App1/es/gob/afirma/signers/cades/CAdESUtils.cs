using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using es.gob.afirma.core.csutils;
using es.gob.afirma.core.signers;
using es.gob.afirma.signers.pkcs7;
using Org.BouncyCastle.Asn1;
using Org.BouncyCastle.Asn1.Cms;
using Org.BouncyCastle.Asn1.Ess;
using Org.BouncyCastle.Asn1.X509;

namespace es.gob.afirma.signers.cades
{
    class CAdESUtils
    {
        public static Asn1EncodableVector GenerateSignerInfo(X509Certificate2 cert,
                                                   String digestAlgorithmName,
                                                   byte[] datos,
                                                   AdESPolicy policy,
                                                   bool signingCertificateV2,
                                                   byte[] messageDigest,
                                                   DateTime signDate,
                                                   bool padesMode,
                                                   String contentType,
                                                   String contentDescription)
        {

            // ALGORITMO DE HUELLA DIGITAL
            AlgorithmIdentifier digestAlgorithmOID = SigUtils.MakeAlgId(AOAlgorithmID.GetOID(digestAlgorithmName));

            // // ATRIBUTOS

            // authenticatedAttributes
            Asn1EncodableVector contexExpecific = InitContexExpecific(
                   digestAlgorithmName,
                   datos,
                   Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.Data.Id,
                   messageDigest,
                   signDate,
                   padesMode
           );

            // Serial Number
            // comentar lo de abajo para version del rfc 3852

            if (signingCertificateV2)
            {
                // INICIO SINGING CERTIFICATE-V2

                /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
                 * CertificateSerialNumber */



                TbsCertificateStructure tbs = TbsCertificateStructure.GetInstance(
                    Asn1Object.FromByteArray(
                    new Org.BouncyCastle.X509.X509Certificate(
                        X509CertificateStructure.GetInstance(
                        Asn1Object.FromByteArray(
                        cert.GetRawCertData()))).GetTbsCertificate()));

                GeneralNames gns = new GeneralNames(new GeneralName(tbs.Issuer));

                IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.SerialNumber);

                /** ESSCertIDv2 ::= SEQUENCE { hashAlgorithm AlgorithmIdentifier
                 * DEFAULT {algorithm id-sha256}, certHash Hash, issuerSerial
                 * IssuerSerial OPTIONAL }
                 * Hash ::= OCTET STRING */

                byte[] certHash = Digester.Digest(cert.GetRawCertData(), digestAlgorithmName);
                EssCertIDv2[] essCertIDv2 = { new EssCertIDv2(digestAlgorithmOID, certHash, isuerSerial) };

                /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
                 * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
                 * OPTIONAL }
                 * CertPolicyId ::= OBJECT IDENTIFIER
                 * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
                 * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

                SigningCertificateV2 scv2;
                if (policy.GetPolicyIdentifier() != null)
                {

                    /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                     * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                     * } */
                    scv2 = new SigningCertificateV2(essCertIDv2, GetPolicyInformation(policy)); // con politica
                }
                else
                {
                    scv2 = new SigningCertificateV2(essCertIDv2); // Sin politica
                }

                // Secuencia con singningCertificate
                contexExpecific.Add(new Org.BouncyCastle.Asn1.Cms.Attribute(Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.IdAASigningCertificateV2, new DerSet(scv2)));

                // FIN SINGING CERTIFICATE-V2

            }
            else
            {
                // INICIO SINGNING CERTIFICATE

                /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
                 * CertificateSerialNumber } */

                TbsCertificateStructure tbs = TbsCertificateStructure.GetInstance(
                    Asn1Object.FromByteArray(
                    new Org.BouncyCastle.X509.X509Certificate(
                        X509CertificateStructure.GetInstance(
                        Asn1Object.FromByteArray(
                        cert.GetRawCertData()))).GetTbsCertificate()));

                GeneralName gn = new GeneralName(tbs.Issuer);
                GeneralNames gns = new GeneralNames(gn);

                IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.SerialNumber);

                /** ESSCertID ::= SEQUENCE { certHash Hash, issuerSerial IssuerSerial
                 * OPTIONAL }
                 * Hash ::= OCTET STRING -- SHA1 hash of entire certificate */
                byte[] certHash = Digester.Digest(cert.GetRawCertData(), digestAlgorithmName);

                EssCertID essCertID = new EssCertID(certHash, isuerSerial);

                /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
                 * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
                 * OPTIONAL }
                 * CertPolicyId ::= OBJECT IDENTIFIER
                 * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
                 * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

                SigningCertificate scv;
                if (policy.GetPolicyIdentifier() != null)
                {

                    /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                     * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                     * } */
                    /*
                     * HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY
                     * CASTLE NO TIENE DICHO CONSTRUCTOR.
                     */
                    Asn1EncodableVector v = new Asn1EncodableVector();
                    v.Add(new DerSequence(essCertID));
                    v.Add(new DerSequence(GetPolicyInformation(policy)));
                    scv = SigningCertificate.GetInstance(new DerSequence(v)); // con politica
                }
                else
                {
                    scv = new SigningCertificate(essCertID); // Sin politica
                }

                /** id-aa-signingCertificate OBJECT IDENTIFIER ::= { iso(1)
                 * member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9) smime(16)
                 * id-aa(2) 12 } */
                // Secuencia con singningCertificate
                contexExpecific.Add(new Org.BouncyCastle.Asn1.Cms.Attribute(Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.IdAASigningCertificate, new DerSet(scv)));
            }

            // INICIO SIGPOLICYID ATTRIBUTE

            if (policy.GetPolicyIdentifier() != null)
            {
                /**
                 * SigPolicyId ::= OBJECT IDENTIFIER Politica de firma.
                 */
                DerObjectIdentifier doiSigPolicyId = new DerObjectIdentifier(policy.GetPolicyIdentifier().ToLower().Replace("urn:oid:", ""));

                /**
                 *   OtherHashAlgAndValue ::= SEQUENCE {
                 *     hashAlgorithm    AlgorithmIdentifier,
                 *     hashValue        OCTET STRING }
                 *
                 */


                // Algoritmo para el hash
                AlgorithmIdentifier hashid;
                // si tenemos algoritmo de calculo de hash, lo ponemos
                if (policy.GetPolicyIdentifierHashAlgorithm() != null)
                {
                    hashid = SigUtils.MakeAlgId(
                                        AOAlgorithmID.GetOID(
                                        AOSignConstants.GetDigestAlgorithmName(
                                           policy.GetPolicyIdentifierHashAlgorithm())));
                }
                // si no tenemos, ponemos el algoritmo de firma.
                else
                {
                    hashid = digestAlgorithmOID;
                }
                // hash del documento, descifrado en b64
                byte[] hashed;
                if (policy.GetPolicyIdentifierHash() != null)
                {
                    hashed = System.Convert.FromBase64String(policy.GetPolicyIdentifierHash());
                }
                else
                {
                    hashed = new byte[] { 0 };
                }

                DigestInfo otherHashAlgAndValue = new DigestInfo(hashid, hashed);

                /**
                 *   SigPolicyQualifierInfo ::= SEQUENCE {
                 *       SigPolicyQualifierId  SigPolicyQualifierId,
                 *       SigQualifier          ANY DEFINED BY policyQualifierId }
                 */

                AOSigPolicyQualifierInfo spqInfo = null;
                if (policy.GetPolicyQualifier() != null)
                {
                    spqInfo = new AOSigPolicyQualifierInfo(policy.GetPolicyQualifier().ToString());
                }

                /**
                 * SignaturePolicyId ::= SEQUENCE {
                 *  sigPolicyId           SigPolicyId,
                 *  sigPolicyHash         SigPolicyHash,
                 *  sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
                 *                          AOSigPolicyQualifierInfo OPTIONAL}
                 *
                 */
                Asn1EncodableVector v = new Asn1EncodableVector();
                // sigPolicyId
                v.Add(doiSigPolicyId);
                // sigPolicyHash
                v.Add(otherHashAlgAndValue.ToAsn1Object()); // como sequence
                // sigPolicyQualifiers
                if (spqInfo != null)
                {
                    v.Add(spqInfo.toASN1Primitive());
                }

                DerSequence ds = new DerSequence(v);

                // Secuencia con singningCertificate
                contexExpecific.Add(new Org.BouncyCastle.Asn1.Cms.Attribute(Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.IdAAEtsSigPolicyID, new DerSet(ds.ToAsn1Object())));
                // FIN SIGPOLICYID ATTRIBUTE
            }

            /**
             * Secuencia con el tipo de contenido firmado. No se agrega en firmas PAdES.
             *
             * ContentHints ::= SEQUENCE {
             *	  contentDescription UTF8String (SIZE (1..MAX)) OPTIONAL,
             *	  contentType ContentType }
             */
            if (contentType != null && !padesMode)
            {
                ContentHints contentHints;
                if (contentDescription != null)
                {
                    contentHints = new ContentHints(new DerObjectIdentifier(contentType),
                                                    new DerUtf8String(contentDescription));
                }
                else
                {
                    contentHints = new ContentHints(new DerObjectIdentifier(contentType));
                }
                contexExpecific.Add(new Org.BouncyCastle.Asn1.Cms.Attribute(
                        Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.IdAAContentHint,
                        new DerSet(contentHints.ToAsn1Object())));
            }

            return contexExpecific;
        }

        private static PolicyInformation[] GetPolicyInformation(AdESPolicy policy)
        {

            if (policy == null)
            {
                throw new ArgumentException("La politica de firma no puede ser nula en este punto");
            }

            /**
             * PolicyQualifierInfo ::= SEQUENCE {
             *          policyQualifierId  PolicyQualifierId,
             *          qualifier          ANY DEFINED BY policyQualifierId }
             */

            PolicyQualifierID pqid = PolicyQualifierID.IdQtCps;
            DerIA5String uri = null;

            if (policy.GetPolicyQualifier() != null && !policy.GetPolicyQualifier().Equals(""))
            {
                uri = new DerIA5String(policy.GetPolicyQualifier().ToString());
            }

            Asn1EncodableVector v = new Asn1EncodableVector();
            PolicyQualifierInfo pqi = null;
            if (uri != null)
            {
                v.Add(pqid);
                v.Add(uri);

                /**
                 * 
                 * ESTO TIENE ALTAS PROBABILIDADES DE FALLAR 
                 *
                 */
                pqi = PolicyQualifierInfo.GetInstance(new DerSequence(v));
            }

            /**
             * PolicyInformation ::= SEQUENCE {
             *     policyIdentifier   CertPolicyId,
             *     policyQualifiers   SEQUENCE SIZE (1..MAX) OF
             *                          PolicyQualifierInfo OPTIONAL }
             */

            if (policy.GetPolicyQualifier() == null || pqi == null)
            {
                return new PolicyInformation[] {
                new PolicyInformation(new DerObjectIdentifier(policy.GetPolicyIdentifier().ToLower().Replace("urn:oid:", "")))  
            };
            }

            return new PolicyInformation[] {
            new PolicyInformation(new DerObjectIdentifier(policy.GetPolicyIdentifier().ToLower().Replace("urn:oid:", "")), new DerSequence(pqi))  
        };
        }

        static Asn1EncodableVector InitContexExpecific(String digestAlgorithm,
                                                    byte[] datos,
                                                    String dataType,
                                                    byte[] messageDigest,
                                                    DateTime signDate,
                                                    bool padesMode)
        {
            // authenticatedAttributes
            Asn1EncodableVector contexExpecific = new Asn1EncodableVector();

            // tipo de contenido
            if (dataType != null)
            {
                contexExpecific.Add(new Org.BouncyCastle.Asn1.Cms.Attribute(CmsAttributes.ContentType, new DerSet(new DerObjectIdentifier(dataType))));
            }

            // fecha de firma, no se anade en modo PAdES
            if (!padesMode)
            {
                contexExpecific.Add(new Org.BouncyCastle.Asn1.Cms.Attribute(CmsAttributes.SigningTime, new DerSet(new DerUtcTime(signDate))));
            }

            // MessageDigest
            contexExpecific.Add(
                new Org.BouncyCastle.Asn1.Cms.Attribute(
                    CmsAttributes.MessageDigest,
                    new DerSet(new DerOctetString((messageDigest != null) ? messageDigest : Digester.Digest(datos, digestAlgorithm)))
                    )
                );

            return contexExpecific;
        }
    }
}