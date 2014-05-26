using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace es.gob.afirma.signers.pkcs7
{
    class AOAlgorithmID
    {
        private static String OID_SHA1 = "1.3.14.3.2.26";
        private static String OID_SHA512 = "2.16.840.1.101.3.4.2.3";
        private static String OID_MD2 = "1.2.840.113549.2.2";
        private static String OID_MD5 = "1.2.840.113549.2.5";
        private static String OID_SHA256 = "2.16.840.1.101.3.4.2.1";
        private static String OID_SHA384 = "2.16.840.1.101.3.4.2.2";
        private static String OID_RSA = "1.2.840.113549.1.1.1";

        private static Dictionary<String, String> OIDS = new Dictionary<String, String>()
        {
            { "SHA1", OID_SHA1 },
            { "SHA-1", OID_SHA1 },
            { "SHA", OID_SHA1 },
            { OID_SHA1, OID_SHA1 },
            { "SHA-512", OID_SHA512 },
            { "SHA512", OID_SHA512 },
            { OID_SHA512, OID_SHA512 },
            { "MD2", OID_MD2 },
            { OID_MD2, OID_MD2 },
            { "MD5", OID_MD5 },
            { OID_MD5, OID_MD5 },
            { "SHA-256", OID_SHA256 },
            { "SHA256", OID_SHA256 },
            { OID_SHA256, OID_SHA256 },
            { "SHA-384", OID_SHA384 },
            { "SHA384", OID_SHA384 },
            { OID_SHA384, OID_SHA384 },
            { "RSA", OID_RSA },
            { OID_RSA, OID_RSA }
        };

        public static String GetOID(String name)
        {
            if (name == null)
            {
                return null;
            }
            if (!OIDS.ContainsKey(name.ToUpper()))
            {
                throw new ArgumentException("Se deconoce el algoritmo '" + name + "'");
            }
            return OIDS[name.ToUpper()];
        }
    }
}