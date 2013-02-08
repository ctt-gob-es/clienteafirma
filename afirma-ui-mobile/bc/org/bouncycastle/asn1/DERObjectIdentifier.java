package org.bouncycastle.asn1;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;

import org.bouncycastle.util.Arrays;

public class DERObjectIdentifier
    extends ASN1Primitive
{
    String      identifier;

    private     byte[] body;

    /**
     * return an OID from the passed in object
     *
     * @exception IllegalArgumentException if the object cannot be converted.
     */
    public static ASN1ObjectIdentifier getInstance(
        Object  obj)
    {
        if (obj == null || obj instanceof ASN1ObjectIdentifier)
        {
            return (ASN1ObjectIdentifier)obj;
        }

        if (obj instanceof DERObjectIdentifier)
        {
            return new ASN1ObjectIdentifier(((DERObjectIdentifier)obj).getId());
        }

        throw new IllegalArgumentException("illegal object in getInstance: " + obj.getClass().getName());
    }

    /**
     * return an Object Identifier from a tagged object.
     *
     * @param obj the tagged object holding the object we want
     * @param explicit true if the object is meant to be explicitly
     *              tagged false otherwise.
     * @exception IllegalArgumentException if the tagged object cannot
     *               be converted.
     */
    public static ASN1ObjectIdentifier getInstance(
        ASN1TaggedObject obj,
        boolean          explicit)
    {
        ASN1Primitive o = obj.getObject();

        if (explicit || o instanceof DERObjectIdentifier)
        {
            return getInstance(o);
        }
        else
        {
            return ASN1ObjectIdentifier.fromOctetString(ASN1OctetString.getInstance(obj.getObject()).getOctets());
        }
    }

    DERObjectIdentifier(
        byte[]  bytes)
    {
        StringBuffer    objId = new StringBuffer();
        long            value = 0;
        BigInteger      bigValue = null;
        boolean         first = true;

        for (int i = 0; i != bytes.length; i++)
        {
            int b = bytes[i] & 0xff;

            if (value < 0x80000000000000L) 
            {
                value = value * 128 + (b & 0x7f);
                if ((b & 0x80) == 0)             // end of number reached
                {
                    if (first)
                    {
                        switch ((int)value / 40)
                        {
                        case 0:
                            objId.append('0');
                            break;
                        case 1:
                            objId.append('1');
                            value -= 40;
                            break;
                        default:
                            objId.append('2');
                            value -= 80;
                        }
                        first = false;
                    }

                    objId.append('.');
                    objId.append(value);
                    value = 0;
                }
            } 
            else 
            {
                if (bigValue == null)
                {
                    bigValue = BigInteger.valueOf(value);
                }
                bigValue = bigValue.shiftLeft(7);
                bigValue = bigValue.or(BigInteger.valueOf(b & 0x7f));
                if ((b & 0x80) == 0) 
                {
                    objId.append('.');
                    objId.append(bigValue);
                    bigValue = null;
                    value = 0;
                }
            }
        }

        this.identifier = objId.toString();
    }

    public DERObjectIdentifier(
        String  identifier)
    {
        if (!isValidIdentifier(identifier))
        {
            throw new IllegalArgumentException("string " + identifier + " not an OID");
        }

        this.identifier = identifier;
    }

    public String getId()
    {
        return identifier;
    }

    private void writeField(
        ByteArrayOutputStream    out,
        long                     fieldValue)
    {
        byte[] result = new byte[9];
        int pos = 8;
        result[pos] = (byte)((int)fieldValue & 0x7f);
        while (fieldValue >= (1L << 7))
        {
            fieldValue >>= 7;
            result[--pos] = (byte)((int)fieldValue & 0x7f | 0x80);
        }
        out.write(result, pos, 9 - pos);
    }

    private void writeField(
        ByteArrayOutputStream   out,
        BigInteger              fieldValue)
    {
        int byteCount = (fieldValue.bitLength()+6)/7;
        if (byteCount == 0) 
        {
            out.write(0);
        }  
        else 
        {
            BigInteger tmpValue = fieldValue;
            byte[] tmp = new byte[byteCount];
            for (int i = byteCount-1; i >= 0; i--) 
            {
                tmp[i] = (byte) ((tmpValue.intValue() & 0x7f) | 0x80);
                tmpValue = tmpValue.shiftRight(7); 
            }
            tmp[byteCount-1] &= 0x7f;
            out.write(tmp, 0, tmp.length);
        }
    }

    private void doOutput(ByteArrayOutputStream aOut)
    {
        OIDTokenizer            tok = new OIDTokenizer(identifier);

        writeField(aOut,
                    Integer.parseInt(tok.nextToken()) * 40
                    + Integer.parseInt(tok.nextToken()));

        while (tok.hasMoreTokens())
        {
            String token = tok.nextToken();
            if (token.length() < 18)
            {
                writeField(aOut, Long.parseLong(token));
            }
            else
            {
                writeField(aOut, new BigInteger(token));
            }
        }
    }

    protected byte[] getBody()
    {
        if (body == null)
        {
            ByteArrayOutputStream bOut = new ByteArrayOutputStream();

            doOutput(bOut);

            body = bOut.toByteArray();
        }

        return body;
    }

    boolean isConstructed()
    {
        return false;
    }

    int encodedLength()
        throws IOException
    {
        int length = getBody().length;

        return 1 + StreamUtil.calculateBodyLength(length) + length;
    }

    void encode(
        ASN1OutputStream out)
        throws IOException
    {
        byte[]                     enc = getBody();

        out.write(BERTags.OBJECT_IDENTIFIER);
        out.writeLength(enc.length);
        out.write(enc);
    }

    public int hashCode()
    {
        return identifier.hashCode();
    }

    boolean asn1Equals(
        ASN1Primitive  o)
    {
        if (!(o instanceof DERObjectIdentifier))
        {
            return false;
        }

        return identifier.equals(((DERObjectIdentifier)o).identifier);
    }

    public String toString()
    {
        return getId();
    }

    private static boolean isValidIdentifier(
        String identifier)
    {
        if (identifier.length() < 3
            || identifier.charAt(1) != '.')
        {
            return false;
        }

        char first = identifier.charAt(0);
        if (first < '0' || first > '2')
        {
            return false;
        }

        boolean periodAllowed = false;
        for (int i = identifier.length() - 1; i >= 2; i--)
        {
            char ch = identifier.charAt(i);

            if ('0' <= ch && ch <= '9')
            {
                periodAllowed = true;
                continue;
            }

            if (ch == '.')
            {
                if (!periodAllowed)
                {
                    return false;
                }

                periodAllowed = false;
                continue;
            }

            return false;
        }

        return periodAllowed;
    }

    private static ASN1ObjectIdentifier[][] cache = new ASN1ObjectIdentifier[255][];

    static ASN1ObjectIdentifier fromOctetString(byte[] enc)
    {
        if (enc.length < 3)
        {
            return new ASN1ObjectIdentifier(enc);
        }

        int idx1 = enc[enc.length - 2] & 0xff;
        ASN1ObjectIdentifier[] first = cache[idx1];

        if (first == null)
        {
            first = cache[idx1] = new ASN1ObjectIdentifier[255];
        }

        int idx2 = enc[enc.length - 1] & 0xff;

        ASN1ObjectIdentifier possibleMatch = first[idx2];

        if (possibleMatch == null)
        {
            possibleMatch = first[idx2] = new ASN1ObjectIdentifier(enc);
            return possibleMatch;
        }

        if (Arrays.areEqual(enc, possibleMatch.getBody()))
        {
            return possibleMatch;
        }
        else
        {
            idx1 = (idx1 + 1) % 256;
            first = cache[idx1];
            if (first == null)
            {
                first = cache[idx1] = new ASN1ObjectIdentifier[255];
            }

            possibleMatch = first[idx2];

            if (possibleMatch == null)
            {
                possibleMatch = first[idx2] = new ASN1ObjectIdentifier(enc);
                return possibleMatch;
            }

            if (Arrays.areEqual(enc, possibleMatch.getBody()))
            {
                return possibleMatch;
            }

            idx2 = (idx2 + 1) % 256;
            possibleMatch = first[idx2];

            if (possibleMatch == null)
            {
                possibleMatch = first[idx2] = new ASN1ObjectIdentifier(enc);
                return possibleMatch;
            }

            if (Arrays.areEqual(enc, possibleMatch.getBody()))
            {
                return possibleMatch;
            }
        }

        return new ASN1ObjectIdentifier(enc);
    }
}
