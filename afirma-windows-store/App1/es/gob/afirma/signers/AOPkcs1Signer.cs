using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using es.gob.afirma.core.signers;
using Org.BouncyCastle.Crypto;
using Org.BouncyCastle.Security;
using Org.BouncyCastle.Crypto.Parameters;

namespace es.gob.afirma.signers
{
    class AOPkcs1Signer
    {
        public byte[] sign(byte[] data, String algorithm, X509Certificate2 keyEntry)
        {

            if (algorithm == null)
            {
                throw new ArgumentNullException("El algoritmo de huella digital no puede ser nulo");
            }
            switch (algorithm)
            {
                /**
                 * ALGORITMOS COMPUESTOS
                 */
                /** Algoritmo de firma SHA1withRSA. */
                case AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("SHA-1withRSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }


                /** Algoritmo de firma MD5withRSA. */
                case AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("MD5withRSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }

                /** Algoritmo de firma MD2withRSA. */
                case AOSignConstants.SIGN_ALGORITHM_MD2WITHRSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("MD2withRSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }

                /** Algoritmo de firma SHA256withRSA. */
                case AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("SHA-256withRSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }

                /** Algoritmo de firma SHA384withRSA. */
                case AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("SHA-384withRSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }

                /** Algoritmo de firma SHA512withRSA. */
                case AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("SHA-512withRSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }

                /** Algoritmo de firma RSA que no incluye la generaci&oacute;n de la huella
                 * digital (NONEwithRSA). */
                case AOSignConstants.SIGN_ALGORITHM_NONEWITHRSA:
                    {
                        throw new ArgumentNullException("El algoritmo SIGN_ALGORITHM_NONEWITHRSA no esta soportado");
                    }

                /** Algoritmo de firma SHA1withDSA. */
                case AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("SHA-1withDSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }

                /** Algoritmo de firma SHA1withECDSA. */
                case AOSignConstants.SIGN_ALGORITHM_SHA1WITHECDSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("SHA-1withECDSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }

                /** Algoritmo de firma ECDSA que no incluye la generaci&oacute;n de la huella
                 * digital (NONEwithEDSSA). */
                case AOSignConstants.SIGN_ALGORITHM_NONEWITHECDSA:
                    {
                        ISigner signer = SignerUtilities.GetSigner("NONEwithECDSA");
                        signer.Init(true, keyEntry.GetKey());
                        signer.BlockUpdate(data, 0, data.Length);
                        return signer.GenerateSignature();
                    }
                

                default:
                    // You can use the default case.
                    throw new ArgumentNullException("El algoritmo no es reconocido");
            }

        }
      
    }
}