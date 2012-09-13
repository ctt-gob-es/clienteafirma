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

/** Generador de huellas digitales SHA-256.
 * C&oacute;digo fuente proveniente de
 * <i>The Legion Of The Bouncy Castle (<a href="http://www.bouncycastle.org">http://www.bouncycastle.org</a>)</i> */
public final class SHA256Digest extends GeneralDigest {
    private static final int DIGEST_LENGTH = 32;

    private int h1Value, h2Value, h3Value, h4Value, h5Value, h6Value, h7Value, h8Value;

    private final int[] xValue = new int[64];
    private int xOff;

    /** Construye un generador de huellas digitales SHA-256. */
    public SHA256Digest() {
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
        Pack.intToBigEndian(this.h6Value, out, outOff + 20);
        Pack.intToBigEndian(this.h7Value, out, outOff + 24);
        Pack.intToBigEndian(this.h8Value, out, outOff + 28);

        reset();

        return DIGEST_LENGTH;
    }

    /** {@inheritDoc} */
    @Override
    public void reset() {
        super.reset();

        /* SHA-256 initial hash value
         * The first 32 bits of the fractional parts of the square roots
         * of the first eight prime numbers
         */

        this.h1Value = 0x6a09e667;
        this.h2Value = 0xbb67ae85;
        this.h3Value = 0x3c6ef372;
        this.h4Value = 0xa54ff53a;
        this.h5Value = 0x510e527f;
        this.h6Value = 0x9b05688c;
        this.h7Value = 0x1f83d9ab;
        this.h8Value = 0x5be0cd19;

        this.xOff = 0;
        for (int i = 0; i != this.xValue.length; i++) {
            this.xValue[i] = 0;
        }
    }

    @Override
    protected void processBlock() {
        //
        // expand 16 word block into 64 word blocks.
        //
        for (int t = 16; t <= 63; t++) {
            this.xValue[t] = theta1(this.xValue[t - 2]) + this.xValue[t - 7] + theta0(this.xValue[t - 15]) + this.xValue[t - 16];
        }

        //
        // set up working variables.
        //
        int a = this.h1Value;
        int b = this.h2Value;
        int c = this.h3Value;
        int d = this.h4Value;
        int e = this.h5Value;
        int f = this.h6Value;
        int g = this.h7Value;
        int h = this.h8Value;

        int t = 0;
        for (int i = 0; i < 8; i++) {
            // t = 8 * i
            h += sum1(e) + ch(e, f, g) + K_VALUE[t] + this.xValue[t];
            d += h;
            h += sum0(a) + maj(a, b, c);
            ++t;

            // t = 8 * i + 1
            g += sum1(d) + ch(d, e, f) + K_VALUE[t] + this.xValue[t];
            c += g;
            g += sum0(h) + maj(h, a, b);
            ++t;

            // t = 8 * i + 2
            f += sum1(c) + ch(c, d, e) + K_VALUE[t] + this.xValue[t];
            b += f;
            f += sum0(g) + maj(g, h, a);
            ++t;

            // t = 8 * i + 3
            e += sum1(b) + ch(b, c, d) + K_VALUE[t] + this.xValue[t];
            a += e;
            e += sum0(f) + maj(f, g, h);
            ++t;

            // t = 8 * i + 4
            d += sum1(a) + ch(a, b, c) + K_VALUE[t] + this.xValue[t];
            h += d;
            d += sum0(e) + maj(e, f, g);
            ++t;

            // t = 8 * i + 5
            c += sum1(h) + ch(h, a, b) + K_VALUE[t] + this.xValue[t];
            g += c;
            c += sum0(d) + maj(d, e, f);
            ++t;

            // t = 8 * i + 6
            b += sum1(g) + ch(g, h, a) + K_VALUE[t] + this.xValue[t];
            f += b;
            b += sum0(c) + maj(c, d, e);
            ++t;

            // t = 8 * i + 7
            a += sum1(f) + ch(f, g, h) + K_VALUE[t] + this.xValue[t];
            e += a;
            a += sum0(b) + maj(b, c, d);
            ++t;
        }

        this.h1Value += a;
        this.h2Value += b;
        this.h3Value += c;
        this.h4Value += d;
        this.h5Value += e;
        this.h6Value += f;
        this.h7Value += g;
        this.h8Value += h;

        //
        // reset the offset and clean out the word buffer.
        //
        this.xOff = 0;
        for (int i = 0; i < 16; i++) {
            this.xValue[i] = 0;
        }
    }

    /* SHA-256 functions */
    private static int ch(final int x, final int y, final int z) {
        return (x & y) ^ ((~x) & z);
    }

    private static int maj(final int x, final int y, final int z) {
        return (x & y) ^ (x & z) ^ (y & z);
    }

    private static int sum0(final int x) {
        return ((x >>> 2) | (x << 30)) ^ ((x >>> 13) | (x << 19)) ^ ((x >>> 22) | (x << 10));
    }

    private static int sum1(final int x) {
        return ((x >>> 6) | (x << 26)) ^ ((x >>> 11) | (x << 21)) ^ ((x >>> 25) | (x << 7));
    }

    private static int theta0(final int x) {
        return ((x >>> 7) | (x << 25)) ^ ((x >>> 18) | (x << 14)) ^ (x >>> 3);
    }

    private static int theta1(final int x) {
        return ((x >>> 17) | (x << 15)) ^ ((x >>> 19) | (x << 13)) ^ (x >>> 10);
    }

    /* SHA-256 Constants
     * (represent the first 32 bits of the fractional parts of the
     * cube roots of the first sixty-four prime numbers)
     */
    private static final int K_VALUE[] = {
            0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be,
            0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa,
            0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85,
            0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
            0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f,
            0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    };
}
