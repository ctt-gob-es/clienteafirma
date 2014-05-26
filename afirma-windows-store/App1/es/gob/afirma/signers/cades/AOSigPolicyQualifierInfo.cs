using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Org.BouncyCastle.Asn1;

namespace es.gob.afirma.signers.cades
{
    class AOSigPolicyQualifierInfo
    {
        private DerObjectIdentifier sigPolicyQualifierId;
        private Asn1Encodable sigQualifier;

        public AOSigPolicyQualifierInfo(String cps)
        {
            this.sigPolicyQualifierId = Org.BouncyCastle.Asn1.Pkcs.PkcsObjectIdentifiers.IdSpqEtsUri;
            this.sigQualifier = new DerIA5String(cps);
        }

        public Asn1Object toASN1Primitive()
        {
            Asn1EncodableVector dev = new Asn1EncodableVector();
            dev.Add(this.sigPolicyQualifierId);
            dev.Add(this.sigQualifier);
            return new DerSequence(dev);
        }
    }
}