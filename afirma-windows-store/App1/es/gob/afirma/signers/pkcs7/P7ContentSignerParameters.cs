using es.gob.afirma.core.signers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;

namespace es.gob.afirma.signers.pkcs7
{
    class P7ContentSignerParameters
    {
        private byte[] Data;
        public String SigatureAlgorithm;
        private X509Certificate2[] CertChain;
        private byte[] Signature;

        public P7ContentSignerParameters(byte[] dat, String signAlgo, X509Certificate2[] cChain)
        {
            this.Data = dat;
            if (signAlgo == null || signAlgo.Length < 1)
            {
                this.SigatureAlgorithm = AOSignConstants.DEFAULT_SIGN_ALGO;
            }
            else
            {
                this.SigatureAlgorithm = signAlgo;
            }
            if (cChain == null)
            {
                this.CertChain = new X509Certificate2[0];
            }
            else
            {
                this.CertChain = cChain;
            }
            this.Signature = new byte[0]; // la firma se realizara despues

        }

        public string GetSignatureAlgorithm()
        {
            return this.SigatureAlgorithm;
        }

        internal byte[] GetContent()
        {
            return this.Data;
        }

        internal X509Certificate2[] GetSignerCertificateChain()
        {
            return this.CertChain;
        }
    }
}