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

/** Base class for SHA-384 and SHA-512.
 * C&oacute;digo fuente proveniente de
 * <i>The Legion Of The Bouncy Castle (<a href="http://www.bouncycastle.org">http://www.bouncycastle.org</a>)</i> */
abstract class LongDigest implements ExtendedDigest {
    private static final int BYTE_LENGTH = 128;

    private final byte[] xBuf;
    private int xBufOff;

    private long byteCount1;
    private long byteCount2;

    protected long h1Value, h2Value, h3Value, h4Value, h5Value, h6Value, h7Value, h8Value;

    private final long[] wValue = new long[80];
    private int wOff;

    /** Constructor for variable length word */
    protected LongDigest() {
        this.xBuf = new byte[8];
        this.xBufOff = 0;

        reset();
    }

    /** Copy constructor. We are using copy constructors in place
     * of the Object.clone() interface as this interface is not
     * supported by J2ME. */
    protected LongDigest(final LongDigest t) {
        this.xBuf = new byte[t.xBuf.length];
        System.arraycopy(t.xBuf, 0, this.xBuf, 0, t.xBuf.length);

        this.xBufOff = t.xBufOff;
        this.byteCount1 = t.byteCount1;
        this.byteCount2 = t.byteCount2;

        this.h1Value = t.h1Value;
        this.h2Value = t.h2Value;
        this.h3Value = t.h3Value;
        this.h4Value = t.h4Value;
        this.h5Value = t.h5Value;
        this.h6Value = t.h6Value;
        this.h7Value = t.h7Value;
        this.h8Value = t.h8Value;

        System.arraycopy(t.wValue, 0, this.wValue, 0, t.wValue.length);
        this.wOff = t.wOff;
    }

    /** {@inheritDoc} */
    @Override
    public void update(final byte in) {
        this.xBuf[this.xBufOff++] = in;

        if (this.xBufOff == this.xBuf.length) {
            processWord(this.xBuf, 0);
            this.xBufOff = 0;
        }

        this.byteCount1++;
    }

    /** {@inheritDoc} */
    @Override
    public void update(final byte[] in, final int inOffset, final int length) {
    	int inOff = inOffset;
    	int len = length;
        //
        // fill the current word
        //
        while ((this.xBufOff != 0) && (len > 0)) {
            update(in[inOff]);

            inOff++;
            len--;
        }

        //
        // process whole words.
        //
        while (len > this.xBuf.length) {
            processWord(in, inOff);

            inOff += this.xBuf.length;
            len -= this.xBuf.length;
            this.byteCount1 += this.xBuf.length;
        }

        //
        // load in the remainder.
        //
        while (len > 0) {
            update(in[inOff]);

            inOff++;
            len--;
        }
    }

    void finish() {
        adjustByteCounts();

        final long lowBitLength = this.byteCount1 << 3;
        final long hiBitLength = this.byteCount2;

        //
        // add the pad bytes.
        //
        update((byte) 128);

        while (this.xBufOff != 0) {
            update((byte) 0);
        }

        processLength(lowBitLength, hiBitLength);

        processBlock();
    }

    /** {@inheritDoc} */
    @Override
    public void reset() {
        this.byteCount1 = 0;
        this.byteCount2 = 0;

        this.xBufOff = 0;
        for (int i = 0; i < this.xBuf.length; i++) {
            this.xBuf[i] = 0;
        }

        this.wOff = 0;
        for (int i = 0; i != this.wValue.length; i++) {
            this.wValue[i] = 0;
        }
    }

    /** {@inheritDoc} */
    @Override
    public int getByteLength() {
        return BYTE_LENGTH;
    }

    protected void processWord(final byte[] in, final int inOff) {
        this.wValue[this.wOff] = Pack.bigEndianToLong(in, inOff);

        if (++this.wOff == 16) {
            processBlock();
        }
    }

    /** adjust the byte counts so that byteCount2 represents the
     * upper long (less 3 bits) word of the byte count. */
    private void adjustByteCounts() {
        if (this.byteCount1 > 0x1fffffffffffffffL) {
            this.byteCount2 += (this.byteCount1 >>> 61);
            this.byteCount1 &= 0x1fffffffffffffffL;
        }
    }

    protected void processLength(final long lowW, final long hiW) {
        if (this.wOff > 14) {
            processBlock();
        }

        this.wValue[14] = hiW;
        this.wValue[15] = lowW;
    }

    protected void processBlock() {
        adjustByteCounts();

        //
        // expand 16 word block into 80 word blocks.
        //
        for (int t = 16; t <= 79; t++) {
            this.wValue[t] = sigma1(this.wValue[t - 2]) + this.wValue[t - 7] + sigma0(this.wValue[t - 15]) + this.wValue[t - 16];
        }

        //
        // set up working variables.
        //
        long a = this.h1Value;
        long b = this.h2Value;
        long c = this.h3Value;
        long d = this.h4Value;
        long e = this.h5Value;
        long f = this.h6Value;
        long g = this.h7Value;
        long h = this.h8Value;

        int t = 0;
        for (int i = 0; i < 10; i++) {
            // t = 8 * i
            h += sum1(e) + ch(e, f, g) + K_VALUE[t] + this.wValue[t++];
            d += h;
            h += sum0(a) + maj(a, b, c);

            // t = 8 * i + 1
            g += sum1(d) + ch(d, e, f) + K_VALUE[t] + this.wValue[t++];
            c += g;
            g += sum0(h) + maj(h, a, b);

            // t = 8 * i + 2
            f += sum1(c) + ch(c, d, e) + K_VALUE[t] + this.wValue[t++];
            b += f;
            f += sum0(g) + maj(g, h, a);

            // t = 8 * i + 3
            e += sum1(b) + ch(b, c, d) + K_VALUE[t] + this.wValue[t++];
            a += e;
            e += sum0(f) + maj(f, g, h);

            // t = 8 * i + 4
            d += sum1(a) + ch(a, b, c) + K_VALUE[t] + this.wValue[t++];
            h += d;
            d += sum0(e) + maj(e, f, g);

            // t = 8 * i + 5
            c += sum1(h) + ch(h, a, b) + K_VALUE[t] + this.wValue[t++];
            g += c;
            c += sum0(d) + maj(d, e, f);

            // t = 8 * i + 6
            b += sum1(g) + ch(g, h, a) + K_VALUE[t] + this.wValue[t++];
            f += b;
            b += sum0(c) + maj(c, d, e);

            // t = 8 * i + 7
            a += sum1(f) + ch(f, g, h) + K_VALUE[t] + this.wValue[t++];
            e += a;
            a += sum0(b) + maj(b, c, d);
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
        this.wOff = 0;
        for (int i = 0; i < 16; i++) {
            this.wValue[i] = 0;
        }
    }

    /* SHA-384 and SHA-512 functions (as for SHA-256 but for longs) */
    private static long ch(final long x, final long y, final long z) {
        return ((x & y) ^ ((~x) & z));
    }

    private static long maj(final long x, final long y, final long z) {
        return ((x & y) ^ (x & z) ^ (y & z));
    }

    private static long sum0(final long x) {
        return ((x << 36) | (x >>> 28)) ^ ((x << 30) | (x >>> 34)) ^ ((x << 25) | (x >>> 39));
    }

    private static long sum1(final long x) {
        return ((x << 50) | (x >>> 14)) ^ ((x << 46) | (x >>> 18)) ^ ((x << 23) | (x >>> 41));
    }

    private static long sigma0(final long x) {
        return ((x << 63) | (x >>> 1)) ^ ((x << 56) | (x >>> 8)) ^ (x >>> 7);
    }

    private static long sigma1(final long x) {
        return ((x << 45) | (x >>> 19)) ^ ((x << 3) | (x >>> 61)) ^ (x >>> 6);
    }

    /* SHA-384 and SHA-512 Constants
     * (represent the first 64 bits of the fractional parts of the
     * cube roots of the first sixty-four prime numbers)
     */
    private static final long K_VALUE[] = {
            0x428a2f98d728ae22L, 0x7137449123ef65cdL, 0xb5c0fbcfec4d3b2fL, 0xe9b5dba58189dbbcL, 0x3956c25bf348b538L, 0x59f111f1b605d019L,
            0x923f82a4af194f9bL, 0xab1c5ed5da6d8118L, 0xd807aa98a3030242L, 0x12835b0145706fbeL, 0x243185be4ee4b28cL, 0x550c7dc3d5ffb4e2L,
            0x72be5d74f27b896fL, 0x80deb1fe3b1696b1L, 0x9bdc06a725c71235L, 0xc19bf174cf692694L, 0xe49b69c19ef14ad2L, 0xefbe4786384f25e3L,
            0x0fc19dc68b8cd5b5L, 0x240ca1cc77ac9c65L, 0x2de92c6f592b0275L, 0x4a7484aa6ea6e483L, 0x5cb0a9dcbd41fbd4L, 0x76f988da831153b5L,
            0x983e5152ee66dfabL, 0xa831c66d2db43210L, 0xb00327c898fb213fL, 0xbf597fc7beef0ee4L, 0xc6e00bf33da88fc2L, 0xd5a79147930aa725L,
            0x06ca6351e003826fL, 0x142929670a0e6e70L, 0x27b70a8546d22ffcL, 0x2e1b21385c26c926L, 0x4d2c6dfc5ac42aedL, 0x53380d139d95b3dfL,
            0x650a73548baf63deL, 0x766a0abb3c77b2a8L, 0x81c2c92e47edaee6L, 0x92722c851482353bL, 0xa2bfe8a14cf10364L, 0xa81a664bbc423001L,
            0xc24b8b70d0f89791L, 0xc76c51a30654be30L, 0xd192e819d6ef5218L, 0xd69906245565a910L, 0xf40e35855771202aL, 0x106aa07032bbd1b8L,
            0x19a4c116b8d2d0c8L, 0x1e376c085141ab53L, 0x2748774cdf8eeb99L, 0x34b0bcb5e19b48a8L, 0x391c0cb3c5c95a63L, 0x4ed8aa4ae3418acbL,
            0x5b9cca4f7763e373L, 0x682e6ff3d6b2b8a3L, 0x748f82ee5defb2fcL, 0x78a5636f43172f60L, 0x84c87814a1f0ab72L, 0x8cc702081a6439ecL,
            0x90befffa23631e28L, 0xa4506cebde82bde9L, 0xbef9a3f7b2c67915L, 0xc67178f2e372532bL, 0xca273eceea26619cL, 0xd186b8c721c0c207L,
            0xeada7dd6cde0eb1eL, 0xf57d4f7fee6ed178L, 0x06f067aa72176fbaL, 0x0a637dc5a2c898a6L, 0x113f9804bef90daeL, 0x1b710b35131c471bL,
            0x28db77f523047d84L, 0x32caab7b40c72493L, 0x3c9ebe0a15c9bebcL, 0x431d67c49c100d4cL, 0x4cc5d4becb3e42b6L, 0x597f299cfc657e2aL,
            0x5fcb6fab3ad6faecL, 0x6c44198c4a475817L
    };
}
