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

/** Implementaci&oacute;n base de las huellas digitales SHA.
 * C&oacute;digo fuente proveniente de
 * <i>The Legion Of The Bouncy Castle (<a href="http://www.bouncycastle.org">http://www.bouncycastle.org</a>)</i> */
abstract class GeneralDigest implements ExtendedDigest {
    private static final int BYTE_LENGTH = 64;
    private final byte[] xBuf;
    private int xBufOff;

    private long byteCount;

    /** Standard constructor */
    protected GeneralDigest() {
        this.xBuf = new byte[4];
        this.xBufOff = 0;
    }

    /** {@inheritDoc} */
    @Override
    public void update(final byte in) {
        this.xBuf[this.xBufOff++] = in;

        if (this.xBufOff == this.xBuf.length) {
            processWord(this.xBuf, 0);
            this.xBufOff = 0;
        }

        this.byteCount++;
    }

    /** {@inheritDoc} */
    @Override
    public void update(final byte[] in, final int inOffset, final int length) {
    	int len = length;
    	int inOff = inOffset;
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
            this.byteCount += this.xBuf.length;
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

    /** Finaliza el proceso de huella digital. */
    void finish() {
        final long bitLength = (this.byteCount << 3);

        //
        // add the pad bytes.
        //
        update((byte) 128);

        while (this.xBufOff != 0) {
            update((byte) 0);
        }
        processLength(bitLength);
        processBlock();
    }

    @Override
    public void reset() {
        this.byteCount = 0;

        this.xBufOff = 0;
        for (int i = 0; i < this.xBuf.length; i++) {
            this.xBuf[i] = 0;
        }
    }

    @Override
    public int getByteLength() {
        return BYTE_LENGTH;
    }

    protected abstract void processWord(byte[] in, int inOff);

    protected abstract void processLength(long bitLength);

    protected abstract void processBlock();
}
