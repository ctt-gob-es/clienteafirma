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

/** Generador de huellas digitales SHA1.
 * C&oacute;digo fuente proveniente de
 * <i>The Legion Of The Bouncy Castle (<a href="http://www.bouncycastle.org">http://www.bouncycastle.org</a>)</i> */
public final class SHA1Digest extends GeneralDigest {
    private static final int DIGEST_LENGTH = 20;

    private int h1Value, h2Value, h3Value, h4Value, h5Value;

    private final int[] xValue = new int[80];
    private int xOff;

    /** Construye un generador de huellas digitales SHA1. */
    public SHA1Digest() {
        super();
        reset();
    }

    /** {@inheritDoc} */
    @Override
    public int getDigestSize() {
        return DIGEST_LENGTH;
    }

    @Override
    protected void processWord(final byte[] in, final int inOffset) {
        int inOff = inOffset;
        // Note: Inlined for performance
        // X[xOff] = Pack.bigEndianToInt(in, inOff);
        int n = in[inOff] << 24;
        n |= (in[++inOff] & 0xff) << 16;
        n |= (in[++inOff] & 0xff) << 8;
        n |= (in[++inOff] & 0xff);
        this.xValue[this.xOff] = n;

        if (++this.xOff == 16) {
            processBlock();
        }
    }

    @Override
    protected void processLength(final long bitLength) {
        if (this.xOff > 14) {
            processBlock();
        }

        this.xValue[14] = (int) (bitLength >>> 32);
        this.xValue[15] = (int) (bitLength & 0xffffffff);
    }

    /** {@inheritDoc} */
    @Override
    public int doFinal(final byte[] out, final int outOff) {
        finish();

        Pack.intToBigEndian(this.h1Value, out, outOff);
        Pack.intToBigEndian(this.h2Value, out, outOff + 4);
        Pack.intToBigEndian(this.h3Value, out, outOff + 8);
        Pack.intToBigEndian(this.h4Value, out, outOff + 12);
        Pack.intToBigEndian(this.h5Value, out, outOff + 16);

        reset();

        return DIGEST_LENGTH;
    }

    /** {@inheritDoc} */
    @Override
    public void reset() {
        super.reset();

        this.h1Value = 0x67452301;
        this.h2Value = 0xefcdab89;
        this.h3Value = 0x98badcfe;
        this.h4Value = 0x10325476;
        this.h5Value = 0xc3d2e1f0;

        this.xOff = 0;
        for (int i = 0; i != this.xValue.length; i++) {
            this.xValue[i] = 0;
        }
    }

    //
    // Additive constants
    //
    private static final int Y1_VALUE = 0x5a827999;
    private static final int Y2_VALUE = 0x6ed9eba1;
    private static final int Y3_VALUE = 0x8f1bbcdc;
    private static final int Y4_VALUE = 0xca62c1d6;

    private static int fFunc(final int u, final int v, final int w) {
        return ((u & v) | ((~u) & w));
    }

    private static int hFunc(final int u, final int v, final int w) {
        return (u ^ v ^ w);
    }

    private static int gFunc(final int u, final int v, final int w) {
        return ((u & v) | (u & w) | (v & w));
    }

    @Override
    protected void processBlock() {
        //
        // expand 16 word block into 80 word block.
        //
        for (int i = 16; i < 80; i++) {
            final int t = this.xValue[i - 3] ^ this.xValue[i - 8] ^ this.xValue[i - 14] ^ this.xValue[i - 16];
            this.xValue[i] = t << 1 | t >>> 31;
        }

        //
        // set up working variables.
        //
        int a = this.h1Value;
        int b = this.h2Value;
        int c = this.h3Value;
        int d = this.h4Value;
        int e = this.h5Value;

        //
        // round 1
        //
        int idx = 0;

        for (int j = 0; j < 4; j++) {
            // E = rotateLeft(A, 5) + f(B, C, D) + E + X[idx++] + Y1_VALUE
            // B = rotateLeft(B, 30)
            e += (a << 5 | a >>> 27) + fFunc(b, c, d) + this.xValue[idx++] + Y1_VALUE;
            b = b << 30 | b >>> 2;

            d += (e << 5 | e >>> 27) + fFunc(a, b, c) + this.xValue[idx++] + Y1_VALUE;
            a = a << 30 | a >>> 2;

            c += (d << 5 | d >>> 27) + fFunc(e, a, b) + this.xValue[idx++] + Y1_VALUE;
            e = e << 30 | e >>> 2;

            b += (c << 5 | c >>> 27) + fFunc(d, e, a) + this.xValue[idx++] + Y1_VALUE;
            d = d << 30 | d >>> 2;

            a += (b << 5 | b >>> 27) + fFunc(c, d, e) + this.xValue[idx++] + Y1_VALUE;
            c = c << 30 | c >>> 2;
        }

        //
        // round 2
        //
        for (int j = 0; j < 4; j++) {
            // E = rotateLeft(A, 5) + h(B, C, D) + E + X[idx++] + Y2_VALUE
            // B = rotateLeft(B, 30)
            e += (a << 5 | a >>> 27) + hFunc(b, c, d) + this.xValue[idx++] + Y2_VALUE;
            b = b << 30 | b >>> 2;

            d += (e << 5 | e >>> 27) + hFunc(a, b, c) + this.xValue[idx++] + Y2_VALUE;
            a = a << 30 | a >>> 2;

            c += (d << 5 | d >>> 27) + hFunc(e, a, b) + this.xValue[idx++] + Y2_VALUE;
            e = e << 30 | e >>> 2;

            b += (c << 5 | c >>> 27) + hFunc(d, e, a) + this.xValue[idx++] + Y2_VALUE;
            d = d << 30 | d >>> 2;

            a += (b << 5 | b >>> 27) + hFunc(c, d, e) + this.xValue[idx++] + Y2_VALUE;
            c = c << 30 | c >>> 2;
        }

        //
        // round 3
        //
        for (int j = 0; j < 4; j++) {
            // E = rotateLeft(A, 5) + g(B, C, D) + E + X[idx++] + Y3_VALUE
            // B = rotateLeft(B, 30)
            e += (a << 5 | a >>> 27) + gFunc(b, c, d) + this.xValue[idx++] + Y3_VALUE;
            b = b << 30 | b >>> 2;

            d += (e << 5 | e >>> 27) + gFunc(a, b, c) + this.xValue[idx++] + Y3_VALUE;
            a = a << 30 | a >>> 2;

            c += (d << 5 | d >>> 27) + gFunc(e, a, b) + this.xValue[idx++] + Y3_VALUE;
            e = e << 30 | e >>> 2;

            b += (c << 5 | c >>> 27) + gFunc(d, e, a) + this.xValue[idx++] + Y3_VALUE;
            d = d << 30 | d >>> 2;

            a += (b << 5 | b >>> 27) + gFunc(c, d, e) + this.xValue[idx++] + Y3_VALUE;
            c = c << 30 | c >>> 2;
        }

        //
        // round 4
        //
        for (int j = 0; j <= 3; j++) {
            // E = rotateLeft(A, 5) + h(B, C, D) + E + X[idx++] + Y4_VALUE
            // B = rotateLeft(B, 30)
            e += (a << 5 | a >>> 27) + hFunc(b, c, d) + this.xValue[idx++] + Y4_VALUE;
            b = b << 30 | b >>> 2;

            d += (e << 5 | e >>> 27) + hFunc(a, b, c) + this.xValue[idx++] + Y4_VALUE;
            a = a << 30 | a >>> 2;

            c += (d << 5 | d >>> 27) + hFunc(e, a, b) + this.xValue[idx++] + Y4_VALUE;
            e = e << 30 | e >>> 2;

            b += (c << 5 | c >>> 27) + hFunc(d, e, a) + this.xValue[idx++] + Y4_VALUE;
            d = d << 30 | d >>> 2;

            a += (b << 5 | b >>> 27) + hFunc(c, d, e) + this.xValue[idx++] + Y4_VALUE;
            c = c << 30 | c >>> 2;
        }

        this.h1Value += a;
        this.h2Value += b;
        this.h3Value += c;
        this.h4Value += d;
        this.h5Value += e;

        //
        // reset start of the buffer.
        //
        this.xOff = 0;
        for (int i = 0; i < 16; i++) {
            this.xValue[i] = 0;
        }
    }
}
