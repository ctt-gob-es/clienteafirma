package es.gob.afirma.core.misc;

import java.util.Arrays;

/** A very fast and memory efficient class to encode and decode to and from BASE64 in full accordance
 * with RFC 2045.<br><br>
 * On Windows XP sp1 with 1.4.2_04 and later ;), this encoder and decoder is about 10 times faster
 * on small arrays (10 - 1000 bytes) and 2-3 times as fast on larger arrays (10000 - 1000000 bytes)
 * compared to <code>sun.misc.Encoder()/Decoder()</code>.<br><br>
 *
 * On byte arrays the encoder is about 20% faster than Jakarta Commons Base64 Codec for encode and
 * about 50% faster for decoding large arrays. This implementation is about twice as fast on very small
 * arrays (&lt 30 bytes). If source/destination is a <code>String</code> this
 * version is about three times as fast due to the fact that the Commons Codec result has to be recoded
 * to a <code>String</code> from <code>byte[]</code>, which is very expensive.<br><br>
 *
 * This encode/decode algorithm doesn't create any temporary arrays as many other codecs do, it only
 * allocates the resulting array. This produces less garbage and it is possible to handle arrays twice
 * as large as algorithms that create a temporary array. (E.g. Jakarta Commons Codec). It is unknown
 * whether Sun's <code>sun.misc.Encoder()/Decoder()</code> produce temporary arrays but since performance
 * is quite low it probably does.<br><br>
 *
 * The encoder produces the same output as the Sun one except that the Sun's encoder appends
 * a trailing line separator if the last character isn't a pad. Unclear why but it only adds to the
 * length and is probably a side effect. Both are in conformance with RFC 2045 though.<br>
 * Commons codec seem to always att a trailing line separator.<br><br>
 *
 * <b>Note!</b>
 * The encode/decode method pairs (types) come in three versions with the <b>exact</b> same algorithm and
 * thus a lot of code redundancy. This is to not create any temporary arrays for transcoding to/from different
 * format types. The methods not used can simply be commented out.<br><br>
 *
 * There is also a "fast" version of all decode methods that works the same way as the normal ones, but
 * har a few demands on the decoded input. Normally though, these fast verions should be used if the source if
 * the input is known and it hasn't bee tampered with.<br><br>
 *
 * If you find the code useful or you find a bug, please send me a note at base64 @ miginfocom . com.
 *
 * Licence (BSD):
 *
 *
 * Copyright (c) 2004, Mikael Grev, MiG InfoCom AB. (base64 @ miginfocom . com)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright notice, this list
 * of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 * Neither the name of the MiG InfoCom AB nor the names of its contributors may be
 * used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * @version 2.2
 * @author Mikael Grev
 *         Date: 2004-aug-02
 *         Time: 11:31:11
 */

public class Base64 {

  private static final char[] CA = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray(); //$NON-NLS-1$
  private static final int[] IA = new int[256];
  static {
    Arrays.fill(IA, -1);
    for (int i = 0, iS = CA.length; i < iS; ++i) {
      IA[CA[i]] = i;
    }
    IA['='] = 0;
  }

  /** Encodes a raw byte array into a BASE64 <code>byte[]</code> representation i accordance with RFC 2045.
   * @param sArr The bytes to convert. If <code>null</code> or length 0 an empty array will be returned.
   * @return A BASE64 encoded array. Never <code>null</code>.
   */
  public final static String encode(final byte[] sArr) {
    // Check special case
	if (sArr == null) {
		return ""; //$NON-NLS-1$
	}
    final int sLen = sArr.length;
    if (sLen == 0) {
      return ""; //$NON-NLS-1$
    }

    final int eLen = (sLen / 3) * 3;                // Length of even 24-bits.
    final int cCnt = ((sLen - 1) / 3 + 1) << 2;     // Returned character count
    final int dLen = cCnt + ((cCnt - 1) / 76 << 1); // Length of returned array
    final byte[] dArr = new byte[dLen];

    // Encode even 24-bits
    for (int s = 0, d = 0, cc = 0; s < eLen;) {
      // Copy next three bytes into lower 24 bits of int, paying attension to sign.
      final int i = (sArr[s++] & 0xff) << 16 | (sArr[s++] & 0xff) << 8 | (sArr[s++] & 0xff);

      // Encode the int into four chars
      dArr[d++] = (byte) CA[(i >>> 18) & 0x3f];
      dArr[d++] = (byte) CA[(i >>> 12) & 0x3f];
      dArr[d++] = (byte) CA[(i >>> 6) & 0x3f];
      dArr[d++] = (byte) CA[i & 0x3f];

      // Add optional line separator
      if (++cc == 19 && d < dLen - 2) {
        dArr[d++] = '\r';
        dArr[d++] = '\n';
        cc = 0;
      }
    }

    // Pad and encode last bits if source isn't an even 24 bits.
    final int left = sLen - eLen; // 0 - 2.
    if (left > 0) {
      // Prepare the int
      final int i = ((sArr[eLen] & 0xff) << 10) | (left == 2 ? ((sArr[sLen - 1] & 0xff) << 2) : 0);

      // Set last four chars
      dArr[dLen - 4] = (byte) CA[i >> 12];
      dArr[dLen - 3] = (byte) CA[(i >>> 6) & 0x3f];
      dArr[dLen - 2] = left == 2 ? (byte) CA[i & 0x3f] : (byte) '=';
      dArr[dLen - 1] = '=';
    }
    return new String(dArr);
  }

  /** Decodes a BASE64 encoded <code>String</code>. All illegal characters will be ignored and can handle both strings with
   * and without line separators.<br>
   * <b>Note!</b> It can be up to about 2x the speed to call <code>decode(str.toCharArray())</code> instead. That
   * will create a temporary array though. This version will use <code>str.charAt(i)</code> to iterate the string.
   * @param str The source string. <code>null</code> or length 0 will return an empty array.
   * @return The decoded array of bytes. May be of length 0. Will be <code>null</code> if the legal characters
   * (including '=') isn't divideable by 4.  (I.e. definitely corrupted).
   */
  public final static byte[] decode(final String str) {
    // Check special case
	if (str == null) {
		return new byte[0];
	}
    final int sLen = str.length();
    if (sLen == 0) {
      return new byte[0];
    }

    // Count illegal characters (including '\r', '\n') to know what size the returned array will be,
    // so we don't have to reallocate & copy it later.
    int sepCnt = 0; // Number of separator characters. (Actually illegal characters, but that's a bonus...)
    for (int i = 0; i < sLen; ++i) {
      if (IA[str.charAt(i)] < 0) {
        ++sepCnt;
      }
    }

    // Check so that legal chars (including '=') are evenly divideable by 4 as specified in RFC 2045.
    if ((sLen - sepCnt) % 4 != 0) {
      return null;
    }

    // Count '=' at end
    int pad = 0;
    for (int i = sLen; i > 1 && IA[str.charAt(--i)] <= 0;) {
      if (str.charAt(i) == '=') {
        ++pad;
      }
    }

    final int len = ((sLen - sepCnt) * 6 >> 3) - pad;

    final byte[] dArr = new byte[len];       // Preallocate byte[] of exact length

    for (int s = 0, d = 0; d < len;) {
      // Assemble three bytes into an int from four "valid" characters.
      int i = 0;
      for (int j = 0; j < 4; ++j) {   // j only increased if a valid char was found.
        final int c = IA[str.charAt(s++)];
        if (c >= 0) {
          i |= c << (18 - j * 6);
        } else {
          --j;
        }
      }
      // Add the bytes
      dArr[d++] = (byte) (i >> 16);
      if (d < len) {
        dArr[d++]= (byte) (i >> 8);
        if (d < len) {
          dArr[d++] = (byte) i;
        }
      }
    }
    return dArr;
  }

}