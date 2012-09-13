/*
 * Copyright (c) 2000 - 2011 The Legion Of The Bouncy Castle (http://www.bouncycastle.org)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package es.gob.jmulticard.jse.provider.digest;

/**
 * FIPS 180-2 implementation of SHA-384.
 *
 * <pre>
 *         block  word  digest
 * SHA-1   512    32    160
 * SHA-256 512    32    256
 * SHA-384 1024   64    384
 * SHA-512 1024   64    512
 * </pre>
 */
public class SHA384Digest
    extends LongDigest
{

    private static final int    DIGEST_LENGTH = 48;

    /**
     * Standard constructor
     */
    public SHA384Digest()
    {
    }

    /**
     * Copy constructor.  This will copy the state of the provided
     * message digest.
     */
    public SHA384Digest(SHA384Digest t)
    {
        super(t);
    }

    public String getAlgorithmName()
    {
        return "SHA-384";
    }

    public int getDigestSize()
    {
        return DIGEST_LENGTH;
    }

    public int doFinal(
        byte[]  out,
        int     outOff)
    {
        finish();

        Pack.longToBigEndian(H1, out, outOff);
        Pack.longToBigEndian(H2, out, outOff + 8);
        Pack.longToBigEndian(H3, out, outOff + 16);
        Pack.longToBigEndian(H4, out, outOff + 24);
        Pack.longToBigEndian(H5, out, outOff + 32);
        Pack.longToBigEndian(H6, out, outOff + 40);

        reset();

        return DIGEST_LENGTH;
    }

    /**
     * reset the chaining variables
     */
    public void reset()
    {
        super.reset();

        /* SHA-384 initial hash value
         * The first 64 bits of the fractional parts of the square roots
         * of the 9th through 16th prime numbers
         */
        H1 = 0xcbbb9d5dc1059ed8l;
        H2 = 0x629a292a367cd507l;
        H3 = 0x9159015a3070dd17l;
        H4 = 0x152fecd8f70e5939l;
        H5 = 0x67332667ffc00b31l;
        H6 = 0x8eb44a8768581511l;
        H7 = 0xdb0c2e0d64f98fa7l;
        H8 = 0x47b5481dbefa4fa4l;
    }
}
