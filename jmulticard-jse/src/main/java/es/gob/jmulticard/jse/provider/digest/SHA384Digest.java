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

/** Generador de huellas digitales SHA-384.
 * C&oacute;digo fuente proveniente de
 * <i>The Legion Of The Bouncy Castle (<a href="http://www.bouncycastle.org">http://www.bouncycastle.org</a>)</i> */
public final class SHA384Digest extends LongDigest {

    private static final int DIGEST_LENGTH = 48;

    /** Construye un generador de huellas digitales SHA-384. */
    public SHA384Digest() {
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

        reset();

        return DIGEST_LENGTH;
    }

    /** {@inheritDoc} */
    @Override
    public void reset() {
        super.reset();

        /* SHA-384 initial hash value
         * The first 64 bits of the fractional parts of the square roots
         * of the 9th through 16th prime numbers
         */
        this.h1Value = 0xcbbb9d5dc1059ed8l;
        this.h2Value = 0x629a292a367cd507l;
        this.h3Value = 0x9159015a3070dd17l;
        this.h4Value = 0x152fecd8f70e5939l;
        this.h5Value = 0x67332667ffc00b31l;
        this.h6Value = 0x8eb44a8768581511l;
        this.h7Value = 0xdb0c2e0d64f98fa7l;
        this.h8Value = 0x47b5481dbefa4fa4l;
    }
}
