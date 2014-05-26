using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace es.gob.afirma.core.signers
{
    class AOSignConstants
    {
        /** Algoritmo de firma SHA1withRSA. */
        public const String SIGN_ALGORITHM_SHA1WITHRSA = "SHA1withRSA"; 

        /** Algoritmo de firma MD5withRSA. */
        public const String SIGN_ALGORITHM_MD5WITHRSA = "MD5withRSA"; 

        /** Algoritmo de firma MD2withRSA. */
        public const String SIGN_ALGORITHM_MD2WITHRSA = "MD2withRSA"; 

        /** Algoritmo de firma SHA256withRSA. */
        public const String SIGN_ALGORITHM_SHA256WITHRSA = "SHA256withRSA"; 

        /** Algoritmo de firma SHA384withRSA. */
        public const String SIGN_ALGORITHM_SHA384WITHRSA = "SHA384withRSA"; 

        /** Algoritmo de firma SHA512withRSA. */
        public const String SIGN_ALGORITHM_SHA512WITHRSA = "SHA512withRSA"; 

        /** Algoritmo de firma RSA que no incluye la generaci&oacute;n de la huella
         * digital (NONEwithRSA). */
        public const String SIGN_ALGORITHM_NONEWITHRSA = "NONEwithRSA"; 

        /** Algoritmo de firma SHA1withDSA. */
        public const String SIGN_ALGORITHM_SHA1WITHDSA = "SHA1withDSA"; 

        /** Algoritmo de firma SHA1withECDSA. */
        public const String SIGN_ALGORITHM_SHA1WITHECDSA = "SHA1withECDSA"; 

        /** Algoritmo de firma ECDSA que no incluye la generaci&oacute;n de la huella
         * digital (NONEwithEDSSA). */
        public const String SIGN_ALGORITHM_NONEWITHECDSA = "NONEwithECDSA"; 

        //Algoritmos de hashing
        public const String SIGN_ALGORITHM_SHA1 = "SHA1";
        public const String SIGN_ALGORITHM_SHA256 = "SHA-256";
        public const String SIGN_ALGORITHM_SHA384 = "SHA-384";
        public const String SIGN_ALGORITHM_SHA512 = "SHA-512";
        public const String SIGN_ALGORITHM_RIPEMD160 = "RIPEMD160";
        public const String SIGN_ALGORITHM_MD5 = "MD5";
        public const String SIGN_ALGORITHM_MD2 = "MD2";
        

        /** Algoritmo de firma por defecto. */
        public const String DEFAULT_SIGN_ALGO = SIGN_ALGORITHM_SHA1WITHRSA;

        /** Identificador del modo de firma Explicita (Los datos NO se incluyen en la
         * firma). */
        public const String SIGN_MODE_EXPLICIT = "explicit"; //$NON-NLS-1$

        /** Identificador del modo de firma Implicita (Los datos SI se incluyen en la
         * firma). */
        public const String SIGN_MODE_IMPLICIT = "implicit"; //$NON-NLS-1$

        /** Modo de firma por defecto. */
        public const String DEFAULT_SIGN_MODE = SIGN_MODE_EXPLICIT;

        /** Descriptor por defecto del tipo de contenido. */
        public const String DEFAULT_CONTENT_DESCRIPTION = "binary"; //$NON-NLS-1$

        /** OID del tipo de datos gen&eacute;rico. */
        public const String DEFAULT_CONTENT_OID_DATA = "1.2.840.113549.1.7.1"; //$NON-NLS-1$

        public static String GetDigestAlgorithmName(String pseudoName)
        {
            String upperPseudoName = pseudoName.ToUpper();
            if (upperPseudoName.Equals("SHA")
             || upperPseudoName.Equals("http://www.w3.org/2000/09/xmldsig#sha1".ToUpper())
             || upperPseudoName.Equals("1.3.14.3.2.26")
             || upperPseudoName.StartsWith("SHA1")
             || upperPseudoName.StartsWith("SHA-1"))
            {
                return SIGN_ALGORITHM_SHA1;
            }

            if (upperPseudoName.Equals("http://www.w3.org/2001/04/xmlenc#sha256".ToUpper())
             || upperPseudoName.Equals("2.16.840.1.101.3.4.2.1")
             || upperPseudoName.StartsWith("SHA256")
             || upperPseudoName.StartsWith("SHA-256"))
            {
                return SIGN_ALGORITHM_SHA256;
            }

            if (upperPseudoName.StartsWith("SHA384")
             || upperPseudoName.Equals("2.16.840.1.101.3.4.2.2")
             || upperPseudoName.StartsWith("SHA-384"))
            {
                return SIGN_ALGORITHM_SHA384;
            }

            if (upperPseudoName.Equals("http://www.w3.org/2001/04/xmlenc#sha512".ToUpper())
             || upperPseudoName.Equals("2.16.840.1.101.3.4.2.3")
             || upperPseudoName.StartsWith("SHA512")
             || upperPseudoName.StartsWith("SHA-512"))
            {
                return SIGN_ALGORITHM_SHA512;
            }

            if (upperPseudoName.Equals("http://www.w3.org/2001/04/xmlenc#ripemd160".ToUpper())
             || upperPseudoName.StartsWith("RIPEMD160")
             || upperPseudoName.StartsWith("RIPEMD-160"))
            {
                return SIGN_ALGORITHM_RIPEMD160;
            }

            if (upperPseudoName.Equals("MD5")
             || upperPseudoName.Equals("1.2.840.113549.2.5")
             || upperPseudoName.StartsWith("MD5"))
            {
                return SIGN_ALGORITHM_MD5;
            }

            if (upperPseudoName.Equals("MD2")
             || upperPseudoName.Equals("1.2.840.113549.2.2")
             || upperPseudoName.StartsWith("MD2"))
            {
                return SIGN_ALGORITHM_MD2;
            }

            throw new ArgumentException("Algoritmo de huella digital no soportado: " + pseudoName);
        }

        /**
         * Comprueba si un algoritmo de firma utiliza un algoritmo de huella digital
         * perteneciente a la familia de algoritmos SHA2.
         * @param algorithm Algoritmo de firma.
         * @return {@code true} cuando el algoritmo es un SHA2, {@code false} en caso contrario.
         */
        public static bool isSHA2SignatureAlgorithm(string algorithm) {
    	    return SIGN_ALGORITHM_SHA256WITHRSA.Equals(algorithm) ||
    		    SIGN_ALGORITHM_SHA384WITHRSA.Equals(algorithm) ||
    		    SIGN_ALGORITHM_SHA512WITHRSA.Equals(algorithm);
        }
    }
}