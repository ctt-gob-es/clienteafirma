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

/** Generador de huellas digitales SHA-512.
 * C&oacute;digo fuente proveniente de
 * <i>The Legion Of The Bouncy Castle (<a href="http://www.bouncycastle.org">http://www.bouncycastle.org</a>)</i> */
public final class SHA512Digest extends LongDigest {
    private static final int DIGEST_LENGTH = 64;

    /** Construye un generador de huellas digitales SHA-512. */
    public SHA512Digest() {
        super();
        reset();
    }

    /** {@inheritDoc} */
    @Override
    public int getDigestSize() {
        return DIGEST_LENGTH;
    }

    /** {@inheritDoc} */
    @Override
    public int doFinal(final byte[] out, final int outOff) {
        finish();

        Pack.longToBigEndian(this.h1Value, out, outOff);
        Pack.longToBigEndian(this.h2Value, out, outOff + 8);
        Pack.longToBigEndian(this.h3Value, out, outOff + 16);
        Pack.longToBigEndian(this.h4Value, out, outOff + 24);
        Pack.longToBigEndian(this.h5Value, out, outOff + 32);
        Pack.longToBigEndian(this.h6Value, out, outOff + 40);
        Pack.longToBigEndian(this.h7Value, out, outOff + 48);
        Pack.longToBigEndian(this.h8Value, out, outOff + 56);

        reset();

        return DIGEST_LENGTH;
    }

    /** {@inheritDoc} */
    @Override
    public void reset() {
        super.reset();

        /* SHA-512 initial hash value
         * The first 64 bits of the fractional parts of the square roots
         * of the first eight prime numbers
         */
        this.h1Value = 0x6a09e667f3bcc908L;
        this.h2Value = 0xbb67ae8584caa73bL;
        this.h3Value = 0x3c6ef372fe94f82bL;
        this.h4Value = 0xa54ff53a5f1d36f1L;
        this.h5Value = 0x510e527fade682d1L;
        this.h6Value = 0x9b05688c2b3e6c1fL;
        this.h7Value = 0x1f83d9abfb41bd6bL;
        this.h8Value = 0x5be0cd19137e2179L;
    }
}
