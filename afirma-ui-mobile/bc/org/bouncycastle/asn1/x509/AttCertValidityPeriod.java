package org.bouncycastle.asn1.x509;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.DERGeneralizedTime;
import org.bouncycastle.asn1.DERSequence;

public class AttCertValidityPeriod
    extends ASN1Object
{
    DERGeneralizedTime  notBeforeTime;
    DERGeneralizedTime  notAfterTime;

    public static AttCertValidityPeriod getInstance(
            Object  obj)
    {
        if (obj instanceof AttCertValidityPeriod)
        {
            return (AttCertValidityPeriod)obj;
        }
        else if (obj != null)
        {
            return new AttCertValidityPeriod(ASN1Sequence.getInstance(obj));
        }
        
        return null;
    }
    
    private AttCertValidityPeriod(
        ASN1Sequence    seq)
    {
        if (seq.size() != 2)
        {
            throw new IllegalArgumentException("Bad sequence size: "
                    + seq.size());
        }

        notBeforeTime = DERGeneralizedTime.getInstance(seq.getObjectAt(0));
        notAfterTime = DERGeneralizedTime.getInstance(seq.getObjectAt(1));
    }

    /**
     * @param notBeforeTime
     * @param notAfterTime
     */
    public AttCertValidityPeriod(
        DERGeneralizedTime notBeforeTime,
        DERGeneralizedTime notAfterTime)
    {
        this.notBeforeTime = notBeforeTime;
        this.notAfterTime = notAfterTime;
    }

    public DERGeneralizedTime getNotBeforeTime()
    {
        return notBeforeTime;
    }

    public DERGeneralizedTime getNotAfterTime()
    {
        return notAfterTime;
    }

    /**
     * Produce an object suitable for an ASN1OutputStream.
     * <pre>
     *  AttCertValidityPeriod  ::= SEQUENCE {
     *       notBeforeTime  GeneralizedTime,
     *       notAfterTime   GeneralizedTime
     *  } 
     * </pre>
     */
    public ASN1Primitive toASN1Primitive()
    {
        ASN1EncodableVector  v = new ASN1EncodableVector();

        v.add(notBeforeTime);
        v.add(notAfterTime);

        return new DERSequence(v);
    }
}
