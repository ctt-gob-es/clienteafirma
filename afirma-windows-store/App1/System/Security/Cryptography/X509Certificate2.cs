using Org.BouncyCastle.Crypto.Parameters;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace System.Security.Cryptography.X509Certificates
{
    class X509Certificate2
    {
        byte[] certificado = null;
        RsaKeyParameters key = null;

        public byte[] GetRawCertData(){
            return certificado;
        }

        public void SetRawCertData(byte[] data){
            certificado = data;
        }

        public void SetKey(RsaKeyParameters rsaKeyParameter)
        {
            this.key = rsaKeyParameter;
        }

        public RsaKeyParameters GetKey()
        {
            return this.key;
        }

    }
}
