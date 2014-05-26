using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Security.Cryptography;
using es.gob.afirma.core.signers;
using Org.BouncyCastle.Crypto.Digests;

namespace es.gob.afirma.core.csutils
{
    class Digester
    {
        public static byte[] Digest(byte[] data, String algo)
        {
            if (algo == null)
            {
                throw new ArgumentNullException("El algoritmo de huella digital no puede ser nulo");
            }
            if (data == null)
            {
                throw new ArgumentNullException("Los datos no pueden ser nulos");
            }

            switch (algo)
            {
                /**
                 * ALGORITMOS DE HASING
                 */
                case AOSignConstants.SIGN_ALGORITHM_SHA1:
                    {
                        Sha1Digest dig = new Sha1Digest();
                        dig.BlockUpdate(data, 0, data.Length);
                        byte[] result = new byte[dig.GetDigestSize()];
                        dig.DoFinal(result, 0);
                        return result;
                    }

                case AOSignConstants.SIGN_ALGORITHM_SHA256:
                    {
                        Sha256Digest dig = new Sha256Digest();
                        dig.BlockUpdate(data, 0, data.Length);
                        byte[] result = new byte[dig.GetDigestSize()];
                        dig.DoFinal(result, 0);
                        return result;
                    }

                case AOSignConstants.SIGN_ALGORITHM_SHA384:
                    {
                        Sha384Digest dig = new Sha384Digest();
                        dig.BlockUpdate(data, 0, data.Length);
                        byte[] result = new byte[dig.GetDigestSize()];
                        dig.DoFinal(result, 0);
                        return result;
                    }

                case AOSignConstants.SIGN_ALGORITHM_SHA512:
                    {
                        Sha512Digest dig = new Sha512Digest();
                        dig.BlockUpdate(data, 0, data.Length);
                        byte[] result = new byte[dig.GetDigestSize()];
                        dig.DoFinal(result, 0);
                        return result;
                    }

                case AOSignConstants.SIGN_ALGORITHM_RIPEMD160:
                    {
                        RipeMD160Digest dig = new RipeMD160Digest();
                        dig.BlockUpdate(data, 0, data.Length);
                        byte[] result = new byte[dig.GetDigestSize()];
                        dig.DoFinal(result, 0);
                        return result;
                    }
                case AOSignConstants.SIGN_ALGORITHM_MD5:
                    {
                        MD5Digest dig = new MD5Digest();
                        dig.BlockUpdate(data, 0, data.Length);
                        byte[] result = new byte[dig.GetDigestSize()];
                        dig.DoFinal(result, 0);
                        return result;
                    }
                
                case AOSignConstants.SIGN_ALGORITHM_MD2:
                    {
                        MD2Digest dig = new MD2Digest();
                        dig.BlockUpdate(data, 0, data.Length);
                        byte[] result = new byte[dig.GetDigestSize()];
                        dig.DoFinal(result, 0);
                        return result;
                    }

                default:
                    // You can use the default case.
                    throw new ArgumentNullException("El algoritmo no es reconocido");
            }


            throw new ArgumentNullException("Algoritmo de hash no soportado: " + algo);
        }
    }
}