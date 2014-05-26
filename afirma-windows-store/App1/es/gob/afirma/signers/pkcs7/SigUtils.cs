using System;
using System.Collections.Generic;
using Org.BouncyCastle.Asn1;
using Org.BouncyCastle.Asn1.X509;

namespace es.gob.afirma.signers.pkcs7
{
    class SigUtils
    {
        public static AlgorithmIdentifier MakeAlgId(String oid)
        {
            return new AlgorithmIdentifier(new DerObjectIdentifier(oid), DerNull.Instance);
        }

        public static Asn1Set CreateBerSetFromList(List<Asn1Encodable> derObjects)
        {
            Asn1EncodableVector v = new Asn1EncodableVector();
            foreach (Asn1Encodable d in derObjects)
            {
                v.Add(d);
            }
            return new BerSet(v);
        }

        public static Asn1Set GetAttributeSet(Org.BouncyCastle.Asn1.Cms.AttributeTable attr)
        {
            if (attr != null)
            {
                return new DerSet(attr.ToAsn1EncodableVector());
            }
            return null;
        }
    }
}