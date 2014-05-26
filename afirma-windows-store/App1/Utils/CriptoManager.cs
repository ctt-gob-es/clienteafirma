using es.gob.afirma.core.signers;
using es.gob.afirma.signers.cades;
using Org.BouncyCastle.Crypto.Parameters;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using Windows.Security.Cryptography;
using Windows.Security.Cryptography.Core;
using Windows.Storage.Streams;

namespace AfirmaWMetro.Utils
{
    class CriptoManager
    {

        /// <summary>
        /// Genera la firma CADES
        /// </summary>
        /// <param name="data">Datos a firmar</param>
        /// <param name="privateKey">Clave privada usada para la firma</param>
        /// <param name="certificado">Certificado del firmante</param>
        /// <param name="algo">Algoritmo de firma</param>
        /// <param name="format">Formato de firma</param>
        /// <returns>Firma en formato binario</returns>
        public static byte[] getCades(byte[] data,
                                        RsaKeyParameters privateKey,
                                        byte[] certificado,
                                        string algo,
                                        string format)
        {

            GenCAdESEPESSignedData prueba = new GenCAdESEPESSignedData();
            Dictionary<string, string> p1 = new Dictionary<string, string>();
            p1.Add("format", format);
            p1.Add("mode", "implicit");
            p1.Add("policyIdentifier", null);
            p1.Add("policyIdentifierHash", null);
            p1.Add("policyIdentifierHashAlgorithm", null);
            p1.Add("policyQualifier", null);
            p1.Add("signingCertificateV2", "false");

            AdESPolicy politica = new AdESPolicy(p1);

            X509Certificate2 cert = new X509Certificate2();
            cert.SetRawCertData(certificado);
            cert.SetKey(privateKey);

            X509Certificate2[] certs = new X509Certificate2[1];
            certs[0] = cert;

            AOCAdESSigner cadesSigner = new AOCAdESSigner();
            byte[] datos = cadesSigner.sign(data, algo, certs, p1);

            return datos;
        }

        /// <summary>
        /// Obtiene los datos cifrados con DES
        /// </summary>
        /// <param name="data">Datos a cifrar</param>
        /// <param name="keyString">clave con la que cifrar</param>
        /// <returns></returns>
        public static byte[] getDES(byte[] data, string keyString)
        {
            //creamos el proveedor de cifrado
            SymmetricKeyAlgorithmProvider symmetricKeyAlgorithmProvider =
                SymmetricKeyAlgorithmProvider.OpenAlgorithm(SymmetricAlgorithmNames.DesEcb);
            
            // Declaramos el encoding
            BinaryStringEncoding encoding = BinaryStringEncoding.Utf8;
            // Creamos un buffer con el contenido de la clave
            IBuffer buffkey = CryptographicBuffer.ConvertStringToBinary(keyString, encoding);

            //creamos la clave
            CryptographicKey cryptographicKey =
               symmetricKeyAlgorithmProvider.CreateSymmetricKey(buffkey);

            // Creamos un buffer con el contenido a cifrar
            IBuffer dataBuffer = CryptographicBuffer.CreateFromByteArray(data);
            IBuffer dataToSign = dataBuffer;
            if ((data.Length % 8) > 0)
            {
                int diff = (int)(data.Length % 8);
                byte[] buff = new byte[dataBuffer.Length+(8-diff)];
                for (int i = 0; i< buff.Length; i++)
                {
                    buff[i] = 0;
                }
                Array.Copy(data, buff, data.Length);
                dataToSign = CryptographicBuffer.CreateFromByteArray(buff);
            }
            //ciframos
            IBuffer cipherTextBuffered = CryptographicEngine.Encrypt(cryptographicKey, dataToSign, null);

            //Se prepara el contenedor del mensaje cifrado
            byte[] cipherText = new byte[cipherTextBuffered.Length];

            //Se copia el contenido
            CryptographicBuffer.CopyToByteArray(cipherTextBuffered, out cipherText);

            //se devuelve el contenido cifrado.
            return cipherText;
        }

        /// <summary>
        /// Este método decodifica un string en base64 con las características de padding especiales.
        /// </summary>
        /// <param name="b64String">cadena con los datos cifrados y con el padding definido</param>
        /// <param name="keyString">clave de descifrado</param>
        /// <returns></returns>
        public static byte[] decriptSpecialDES(string b64String, string keyString)
        {
            byte[] returnData = null;
            string sPadding = b64String.Substring(0,2);
            byte[] encoded = Convert.FromBase64String(b64String.Substring(2));
            int padding = 0;
            bool  intConv = int.TryParse(sPadding.Substring(0,1), out padding);
            if(!intConv)
            {
                return returnData;
            }

            //creamos el proveedor de cifrado
            SymmetricKeyAlgorithmProvider symmetricKeyAlgorithmProvider =
                SymmetricKeyAlgorithmProvider.OpenAlgorithm(SymmetricAlgorithmNames.DesEcb);

            // Declaramos el encoding
            BinaryStringEncoding encoding = BinaryStringEncoding.Utf8;
            // Creamos un buffer con el contenido de la clave
            IBuffer buffkey = CryptographicBuffer.ConvertStringToBinary(keyString, encoding);

            //creamos la clave
            CryptographicKey cryptographicKey =
               symmetricKeyAlgorithmProvider.CreateSymmetricKey(buffkey);

            // Creamos un buffer con el contenido a cifrar
            IBuffer dataBuffer = CryptographicBuffer.CreateFromByteArray(encoded);

            //ciframos
            IBuffer cipherTextBuffered = CryptographicEngine.Decrypt(cryptographicKey, dataBuffer, null);

            //Se prepara el contenedor del mensaje cifrado
            byte[] cipherText = new byte[cipherTextBuffered.Length];

            //Se copia el contenido
            CryptographicBuffer.CopyToByteArray(cipherTextBuffered, out cipherText);

            returnData = new byte[cipherText.Length - 8 - padding];
            for (int i = 0; i < cipherText.Length - 8 - padding; i++)
            {
                returnData[i] = cipherText[i];
            }

            return returnData;
            
        }

    }
}
