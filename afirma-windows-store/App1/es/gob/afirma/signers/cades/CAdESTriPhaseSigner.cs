using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using es.gob.afirma.core.signers;
using es.gob.afirma.signers.pkcs7;
using Org.BouncyCastle.Asn1;
using Org.BouncyCastle.Asn1.Cms;
using Org.BouncyCastle.Asn1.X509;
using Org.BouncyCastle.Cms;

namespace es.gob.afirma.signers.cades
{
    class CAdESTriPhaseSigner
    {
        public static byte[] PreSign(String digestAlgorithmName,
                               byte[] content,
                               X509Certificate2[] signerCertificateChain,
                               AdESPolicy policy,
                               bool signingCertificateV2,
                               byte[] messageDigest,
                               DateTime signDate,
                               bool padesMode,
                               String contentType,
                               String contentDescription)
        {

            if (signerCertificateChain == null || signerCertificateChain.Length == 0)
            {
                throw new ArgumentException("La cadena de certificados debe contener al menos una entrada");
            }

            // Atributos firmados
            Asn1Set signedAttributes;

            signedAttributes = SigUtils.GetAttributeSet(
               new Org.BouncyCastle.Asn1.Cms.AttributeTable(
                  CAdESUtils.GenerateSignerInfo(
                     signerCertificateChain[0],
                     digestAlgorithmName,
                     content,
                     policy,
                     signingCertificateV2,
                     messageDigest,
                     signDate,
                     padesMode,
                     contentType,
                     contentDescription
                  )
               )
            );

            return signedAttributes.GetEncoded("DER");
        }

        public static byte[] PostSign(String digestAlgorithmName,
                            byte[] content,
                            X509Certificate2[] signerCertificateChain,
                            byte[] signature,
                            byte[] signedAttributes)
        {

            if (signerCertificateChain == null || signerCertificateChain.Length == 0)
            {
                throw new ArgumentException("La cadena de certificados debe contener al menos una entrada");
            }

            TbsCertificateStructure tbsCertificateStructure;

            //TODO Revisar esta parte del código
            /**
             * 
             *  Revisar esta parte del código
             *  
             */
            tbsCertificateStructure = TbsCertificateStructure.GetInstance(
                Asn1Object.FromByteArray(
                new Org.BouncyCastle.X509.X509Certificate(
                    X509CertificateStructure.GetInstance(Asn1Object.FromByteArray(signerCertificateChain[0].GetRawCertData()))).GetTbsCertificate()
                    )
                    );

            SignerIdentifier signerIdentifier = new SignerIdentifier(
              new IssuerAndSerialNumber(X509Name.GetInstance(tbsCertificateStructure.Issuer), tbsCertificateStructure.SerialNumber)
           );

            // Algoritmo de huella digital
            AlgorithmIdentifier digestAlgorithmOID;
            digestAlgorithmOID = SigUtils.MakeAlgId(AOAlgorithmID.GetOID(digestAlgorithmName));

            // EncryptionAlgorithm
            AlgorithmIdentifier keyAlgorithmIdentifier;
            keyAlgorithmIdentifier = SigUtils.MakeAlgId(AOAlgorithmID.GetOID("RSA"));

            // Firma PKCS#1 codificada
            Asn1OctetString encodedPKCS1Signature = new DerOctetString(signature);

            // Atributos firmados
            Asn1Set asn1SignedAttributes;
            asn1SignedAttributes = (Asn1Set) Asn1Object.FromByteArray(signedAttributes);

            // SignerInfo
            Asn1EncodableVector signerInfo = new Asn1EncodableVector();
            signerInfo.Add(new SignerInfo(signerIdentifier, digestAlgorithmOID, asn1SignedAttributes, keyAlgorithmIdentifier, encodedPKCS1Signature, null));

            // ContentInfo
            ContentInfo contentInfo;
            if (content != null)
            {
                MemoryStream baos = new MemoryStream();
                CmsProcessable msg = new CmsProcessableByteArray(content);
                msg.Write(baos);

                contentInfo = new ContentInfo(new DerObjectIdentifier(Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.Data.Id), new BerOctetString(baos.ToArray()));
            }
            else
            {
                contentInfo = new ContentInfo(new DerObjectIdentifier(Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.Data.Id), null);
            }

            // Certificados
            List<Asn1Encodable> ce = new List<Asn1Encodable>();
            foreach (X509Certificate2 cert in signerCertificateChain)
            {
                /**
                 * 
                 *  Revisar el uso que hacemos de X509CertificateStructure
                 *  ya que puede ser un posible punto de errores
                 *  
                 */
                ce.Add(X509CertificateStructure.GetInstance(Asn1Object.FromByteArray(cert.GetRawCertData())));

            }
            Asn1Set certificates = SigUtils.CreateBerSetFromList(ce);

            // Algoritmos de huella digital
            Asn1EncodableVector digestAlgorithms = new Asn1EncodableVector();
            digestAlgorithms.Add(digestAlgorithmOID);

            return new ContentInfo(
               Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.SignedData,
               new SignedData(
                  new DerSet(digestAlgorithms),
                  contentInfo,
                  certificates,
                  null,
                  new DerSet(signerInfo)
               )
            ).GetEncoded("DER");
        }
    }
}