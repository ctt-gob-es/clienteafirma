using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace es.gob.afirma.core.misc
{
    class AOUtil
    {
        private static String BASE_64_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz=_-\n+/0123456789\r~";

        public static bool IsBase64(byte[] data)
        {
            int count = 0;

            // Comprobamos que todos los caracteres de la cadena pertenezcan al
            // alfabeto base 64
            foreach (byte b in data)
            {
                if (BASE_64_ALPHABET.IndexOf((char)b) == -1)
                {
                    return false;
                }

                if (b != '\n' && b != '\r')
                {
                    count++;
                }
            }
            // Comprobamos que la cadena tenga una longitud multiplo de 4 caracteres
            return count % 4 == 0;
        }

        private static char[] HEX_CHARS = {
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
    };


        public static String hexify(byte[] abyte0, bool separator)
        {
            if (abyte0 == null)
            {
                return "null"; //$NON-NLS-1$
            }

            String stringbuffer = "";
            int i = 0;
            for (int j = 0; j < abyte0.Length; j++)
            {
                if (separator && i > 0)
                {
                    stringbuffer += '-';
                }
                stringbuffer += HEX_CHARS[abyte0[j] >> 4 & 0xf];
                stringbuffer += HEX_CHARS[abyte0[j] & 0xf];
                ++i;
                if (i == 16)
                {
                    if (separator && j < abyte0.Length - 1)
                    {
                        stringbuffer += '\n';
                    }
                    i = 0;
                }
            }
            return stringbuffer;
        }
    }
}