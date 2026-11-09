/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

/**
 * BouncyCastle 1.85+ rejects UTCTime values that are actually GeneralizedTime
 * payloads (four-digit year, fractional seconds, numeric timezone) under tag
 * 0x17. Some archived CAdES-A fixtures and TSA tokens carry that mis-tag.
 * Retagging 0x17 → 0x18 restores parseability without changing the content
 * octets.
 */
/** Normalizes legacy ASN.1 time encodings rejected by BouncyCastle 1.85+. */
public final class Asn1LegacyTimeNormalizer {

	private static final int TAG_UTC_TIME = 0x17;
	private static final int TAG_GENERALIZED_TIME = 0x18;

	private Asn1LegacyTimeNormalizer() {
		// utility
	}

	/**
	 * Returns the same array when no rewrite is needed; otherwise a copy with
	 * mis-tagged UTCTime values retagged as GeneralizedTime.
	 *
	 * @param data DER/BER CMS or certificate bytes
	 * @return input or a rewritten copy
	 */
	public static byte[] retagMisencodedUtcTime(final byte[] data) {
		if (data == null || data.length < 3) {
			return data;
		}
		byte[] out = null;
		int i = 0;
		while (i < data.length - 2) {
			if ((data[i] & 0xff) != TAG_UTC_TIME) {
				i++;
				continue;
			}
			final int lengthByte = data[i + 1] & 0xff;
			if (lengthByte >= 0x80) {
				i++;
				continue;
			}
			final int len = lengthByte;
			final int payloadStart = i + 2;
			final int payloadEnd = payloadStart + len;
			if (payloadEnd > data.length || !looksLikeGeneralizedTime(data, payloadStart, len)) {
				i = payloadEnd > i ? payloadEnd : i + 1;
				continue;
			}
			if (out == null) {
				out = data.clone();
			}
			out[i] = (byte) TAG_GENERALIZED_TIME;
			i = payloadEnd;
		}
		return out != null ? out : data;
	}

	private static boolean looksLikeGeneralizedTime(final byte[] data, final int offset, final int len) {
		// UTCTime is yyMMddHHmm[ss]Z (≤17). A four-digit year needs GeneralizedTime.
		if (len < 15) {
			return false;
		}
		final byte y0 = data[offset];
		final byte y1 = data[offset + 1];
		if (!((y0 == '1' && y1 == '9') || (y0 == '2' && y1 == '0'))) {
			return false;
		}
		for (int j = 0; j < len; j++) {
			final int b = data[offset + j] & 0xff;
			if (b < 0x20 || b > 0x7e) {
				return false;
			}
		}
		return true;
	}
}
