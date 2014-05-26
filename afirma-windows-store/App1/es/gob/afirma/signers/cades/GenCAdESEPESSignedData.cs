using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using es.gob.afirma.core.csutils;
using es.gob.afirma.core.signers;
using es.gob.afirma.signers.pkcs7;

namespace es.gob.afirma.signers.cades
{
    class GenCAdESEPESSignedData
    {
        public static byte[] generateSignedData(
            P7ContentSignerParameters parameters,
            bool omitContent,
            AdESPolicy policy,
            bool signingCertificateV2,
            X509Certificate2 keyEntry,
            byte[] messageDigest,
            bool padesMode,
            String contentType,
            String contentDescription)
        {

            if (parameters == null)
            {
                throw new ArgumentNullException("Los parametros no pueden ser nulos");
            }
            String signatureAlgorithm = parameters.GetSignatureAlgorithm();

            X509Certificate2[] signerCertificateChain = parameters.GetSignerCertificateChain();

            DateTime signDate = DateTime.Now;

            // Ya que el contenido de la firma puede ser grande, lo obtenemos solo al principio
            byte[] content = parameters.GetContent();

            byte[] preSignature = CAdESTriPhaseSigner.PreSign(
               AOSignConstants.GetDigestAlgorithmName(signatureAlgorithm),
               (omitContent) ? null : content,
               signerCertificateChain,
               policy,
               signingCertificateV2,
               (messageDigest == null && content != null) ?
                  Digester.Digest(content, AOSignConstants.GetDigestAlgorithmName(signatureAlgorithm)) : messageDigest,
               signDate,
               padesMode,
               contentType,
               contentDescription
           );

            byte[] signature = new AOPkcs1Signer().sign(preSignature, signatureAlgorithm, keyEntry);

            return CAdESTriPhaseSigner.PostSign(
                AOSignConstants.GetDigestAlgorithmName(signatureAlgorithm),
                (omitContent) ? null : content,
                signerCertificateChain,
                signature,
                // Volvemos a crear la prefirma simulando una firma trifasica en la que la postfirma no cuenta con el
                // resultado de la prefirma
                CAdESTriPhaseSigner.PreSign(
                    AOSignConstants.GetDigestAlgorithmName(signatureAlgorithm),
                    (omitContent) ? null : content,
                    signerCertificateChain,
                    policy,
                    signingCertificateV2,
                    (messageDigest == null && content != null) ?
                       Digester.Digest(content, AOSignConstants.GetDigestAlgorithmName(signatureAlgorithm)) : messageDigest,
                    signDate,
                    padesMode,
                    contentType,
                    contentDescription
                    )
                );
        }
    }
}