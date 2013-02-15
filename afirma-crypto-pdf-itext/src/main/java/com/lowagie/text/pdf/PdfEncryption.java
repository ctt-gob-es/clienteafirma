/*
 * $Id: PdfEncryption.java 3707 2009-02-20 18:47:51Z xlv $
 *
 * Copyright 2001-2006 Paulo Soares
 *
 * The contents of this file are subject to the Mozilla Public License Version 1.1
 * (the "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the License.
 *
 * The Original Code is 'iText, a free JAVA-PDF library'.
 *
 * The Initial Developer of the Original Code is Bruno Lowagie. Portions created by
 * the Initial Developer are Copyright (C) 1999, 2000, 2001, 2002 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000, 2001, 2002 by Paulo Soares. All Rights Reserved.
 *
 * Contributor(s): all the names of the contributors are added in the source code
 * where applicable.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * LGPL license (the "GNU LIBRARY GENERAL PUBLIC LICENSE"), in which case the
 * provisions of LGPL are applicable instead of those above.  If you wish to
 * allow use of your version of this file only under the terms of the LGPL
 * License and not to allow others to use your version of this file under
 * the MPL, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the LGPL.
 * If you do not delete the provisions above, a recipient may use your version
 * of this file under either the MPL or the GNU LIBRARY GENERAL PUBLIC LICENSE.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MPL as stated above or under the terms of the GNU
 * Library General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library general Public License for more
 * details.
 *
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */

package com.lowagie.text.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.security.cert.Certificate;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.pdf.crypto.ARCFOUREncryption;

/**
 *
 * @author Paulo Soares (psoares@consiste.pt)
 * @author Kazuya Ujihara
 */
class PdfEncryption {

	private static final int STANDARD_ENCRYPTION_40 = 2;

	private static final int STANDARD_ENCRYPTION_128 = 3;

	private static final int AES_128 = 4;

	private static final byte[] pad = { (byte) 0x28, (byte) 0xBF, (byte) 0x4E,
			(byte) 0x5E, (byte) 0x4E, (byte) 0x75, (byte) 0x8A, (byte) 0x41,
			(byte) 0x64, (byte) 0x00, (byte) 0x4E, (byte) 0x56, (byte) 0xFF,
			(byte) 0xFA, (byte) 0x01, (byte) 0x08, (byte) 0x2E, (byte) 0x2E,
			(byte) 0x00, (byte) 0xB6, (byte) 0xD0, (byte) 0x68, (byte) 0x3E,
			(byte) 0x80, (byte) 0x2F, (byte) 0x0C, (byte) 0xA9, (byte) 0xFE,
			(byte) 0x64, (byte) 0x53, (byte) 0x69, (byte) 0x7A };

	private static final byte[] salt = { (byte) 0x73, (byte) 0x41, (byte) 0x6c,
			(byte) 0x54 };

	private static final byte[] metadataPad = { (byte) 255, (byte) 255,
			(byte) 255, (byte) 255 };

	/** The encryption key for a particular object/generation */
	private byte key[];

	/** The encryption key length for a particular object/generation */
	private int keySize;

	/** The global encryption key */
	private byte mkey[];

	/** Work area to prepare the object/generation bytes */
	private final byte extra[] = new byte[5];

	/** The message digest algorithm MD5 */
	private MessageDigest md5;

	/** The encryption key for the owner */
	private byte ownerKey[] = new byte[32];

	/** The encryption key for the user */
	byte userKey[] = new byte[32];

	/** The public key security handler for certificate encryption */
	private PdfPublicKeySecurityHandler publicKeyHandler = null;

	private int permissions;

	private byte documentID[];

	private static long seq = System.currentTimeMillis();

	private int revision;

	private final ARCFOUREncryption arcfour = new ARCFOUREncryption();

	/** The generic key length. It may be 40 or 128. */
	private int keyLength;

	private boolean encryptMetadata;

	/**
	 * Indicates if the encryption is only necessary for embedded files.
	 * @since 2.1.3
	 */
	private boolean embeddedFilesOnly;

	private int cryptoMode;

	public PdfEncryption() {
		try {
			this.md5 = MessageDigest.getInstance("MD5");
		} catch (final Exception e) {
			throw new ExceptionConverter(e);
		}
		this.publicKeyHandler = new PdfPublicKeySecurityHandler();
	}

	public PdfEncryption(final PdfEncryption enc) {
		this();
		this.mkey = enc.mkey.clone();
		this.ownerKey = enc.ownerKey.clone();
		this.userKey = enc.userKey.clone();
		this.permissions = enc.permissions;
		if (enc.documentID != null) {
			this.documentID = enc.documentID.clone();
		}
		this.revision = enc.revision;
		this.keyLength = enc.keyLength;
		this.encryptMetadata = enc.encryptMetadata;
		this.embeddedFilesOnly = enc.embeddedFilesOnly;
		this.publicKeyHandler = enc.publicKeyHandler;
	}

	public void setCryptoMode(int mode, final int kl) {
		this.cryptoMode = mode;
		this.encryptMetadata = (mode & PdfWriter.DO_NOT_ENCRYPT_METADATA) == 0;
		this.embeddedFilesOnly = (mode & PdfWriter.EMBEDDED_FILES_ONLY) != 0;
		mode &= PdfWriter.ENCRYPTION_MASK;
		switch (mode) {
		case PdfWriter.STANDARD_ENCRYPTION_40:
			this.encryptMetadata = true;
			this.embeddedFilesOnly = false;
			this.keyLength = 40;
			this.revision = STANDARD_ENCRYPTION_40;
			break;
		case PdfWriter.STANDARD_ENCRYPTION_128:
			this.embeddedFilesOnly = false;
			if (kl > 0) {
				this.keyLength = kl;
			} else {
				this.keyLength = 128;
			}
			this.revision = STANDARD_ENCRYPTION_128;
			break;
		case PdfWriter.ENCRYPTION_AES_128:
			this.keyLength = 128;
			this.revision = AES_128;
			break;
		default:
			throw new IllegalArgumentException("No valid encryption mode");
		}
	}

	public int getCryptoMode() {
		return this.cryptoMode;
	}

	public boolean isMetadataEncrypted() {
		return this.encryptMetadata;
	}

	/**
	 * Indicates if only the embedded files have to be encrypted.
	 * @return	if true only the embedded files will be encrypted
	 * @since	2.1.3
	 */
	public boolean isEmbeddedFilesOnly() {
		return this.embeddedFilesOnly;
	}

	/**
	 */
	private byte[] padPassword(final byte userPassword[]) {
		final byte userPad[] = new byte[32];
		if (userPassword == null) {
			System.arraycopy(pad, 0, userPad, 0, 32);
		} else {
			System.arraycopy(userPassword, 0, userPad, 0, Math.min(
					userPassword.length, 32));
			if (userPassword.length < 32) {
				System.arraycopy(pad, 0, userPad, userPassword.length,
						32 - userPassword.length);
			}
		}

		return userPad;
	}

	/**
	 */
	private byte[] computeOwnerKey(final byte userPad[], final byte ownerPad[]) {
		final byte ownerKey[] = new byte[32];

		final byte digest[] = this.md5.digest(ownerPad);
		if (this.revision == STANDARD_ENCRYPTION_128 || this.revision == AES_128) {
			final byte mkey[] = new byte[this.keyLength / 8];
			// only use for the input as many bit as the key consists of
			for (int k = 0; k < 50; ++k) {
				System.arraycopy(this.md5.digest(digest), 0, digest, 0, mkey.length);
			}
			System.arraycopy(userPad, 0, ownerKey, 0, 32);
			for (int i = 0; i < 20; ++i) {
				for (int j = 0; j < mkey.length; ++j) {
					mkey[j] = (byte) (digest[j] ^ i);
				}
				this.arcfour.prepareARCFOURKey(mkey);
				this.arcfour.encryptARCFOUR(ownerKey);
			}
		} else {
			this.arcfour.prepareARCFOURKey(digest, 0, 5);
			this.arcfour.encryptARCFOUR(userPad, ownerKey);
		}

		return ownerKey;
	}

	/**
	 *
	 * ownerKey, documentID must be setup
	 */
	private void setupGlobalEncryptionKey(final byte[] documentID, final byte userPad[],
			final byte ownerKey[], final int permissions) {
		this.documentID = documentID;
		this.ownerKey = ownerKey;
		this.permissions = permissions;
		// use variable keylength
		this.mkey = new byte[this.keyLength / 8];

		// fixed by ujihara in order to follow PDF reference
		this.md5.reset();
		this.md5.update(userPad);
		this.md5.update(ownerKey);

		final byte ext[] = new byte[4];
		ext[0] = (byte) permissions;
		ext[1] = (byte) (permissions >> 8);
		ext[2] = (byte) (permissions >> 16);
		ext[3] = (byte) (permissions >> 24);
		this.md5.update(ext, 0, 4);
		if (documentID != null) {
			this.md5.update(documentID);
		}
		if (!this.encryptMetadata) {
			this.md5.update(metadataPad);
		}

		final byte digest[] = new byte[this.mkey.length];
		System.arraycopy(this.md5.digest(), 0, digest, 0, this.mkey.length);

		// only use the really needed bits as input for the hash
		if (this.revision == STANDARD_ENCRYPTION_128 || this.revision == AES_128) {
			for (int k = 0; k < 50; ++k) {
				System.arraycopy(this.md5.digest(digest), 0, digest, 0, this.mkey.length);
			}
		}

		System.arraycopy(digest, 0, this.mkey, 0, this.mkey.length);
	}

	/**
	 *
	 * mkey must be setup
	 */
	// use the revision to choose the setup method
	private void setupUserKey() {
		if (this.revision == STANDARD_ENCRYPTION_128 || this.revision == AES_128) {
			this.md5.update(pad);
			final byte digest[] = this.md5.digest(this.documentID);
			System.arraycopy(digest, 0, this.userKey, 0, 16);
			for (int k = 16; k < 32; ++k) {
				this.userKey[k] = 0;
			}
			for (int i = 0; i < 20; ++i) {
				for (int j = 0; j < this.mkey.length; ++j) {
					digest[j] = (byte) (this.mkey[j] ^ i);
				}
				this.arcfour.prepareARCFOURKey(digest, 0, this.mkey.length);
				this.arcfour.encryptARCFOUR(this.userKey, 0, 16);
			}
		} else {
			this.arcfour.prepareARCFOURKey(this.mkey);
			this.arcfour.encryptARCFOUR(pad, this.userKey);
		}
	}

	// gets keylength and revision and uses revision to choose the initial values
	// for permissions
	public void setupAllKeys(final byte userPassword[], byte ownerPassword[],
			int permissions) {
		if (ownerPassword == null || ownerPassword.length == 0) {
			ownerPassword = this.md5.digest(createDocumentId());
		}
		permissions |= this.revision == STANDARD_ENCRYPTION_128 || this.revision == AES_128 ? 0xfffff0c0
				: 0xffffffc0;
		permissions &= 0xfffffffc;
		// PDF reference 3.5.2 Standard Security Handler, Algorithm 3.3-1
		// If there is no owner password, use the user password instead.
		final byte userPad[] = padPassword(userPassword);
		final byte ownerPad[] = padPassword(ownerPassword);

		this.ownerKey = computeOwnerKey(userPad, ownerPad);
		this.documentID = createDocumentId();
		setupByUserPad(this.documentID, userPad, this.ownerKey, permissions);
	}

	public static byte[] createDocumentId() {
		MessageDigest md5;
		try {
			md5 = MessageDigest.getInstance("MD5");
		} catch (final Exception e) {
			throw new ExceptionConverter(e);
		}
		final long time = System.currentTimeMillis();
		final long mem = Runtime.getRuntime().freeMemory();
		final String s = time + "+" + mem + "+" + seq++;
		return md5.digest(s.getBytes());
	}

	/**
	 */
	public void setupByUserPassword(final byte[] documentID, final byte userPassword[],
			final byte ownerKey[], final int permissions) {
		setupByUserPad(documentID, padPassword(userPassword), ownerKey,
				permissions);
	}

	/**
	 */
	private void setupByUserPad(final byte[] documentID, final byte userPad[],
			final byte ownerKey[], final int permissions) {
		setupGlobalEncryptionKey(documentID, userPad, ownerKey, permissions);
		setupUserKey();
	}

	/**
	 */
	public void setupByOwnerPassword(final byte[] documentID, final byte ownerPassword[],
			final byte userKey[], final byte ownerKey[], final int permissions) {
		setupByOwnerPad(documentID, padPassword(ownerPassword), userKey,
				ownerKey, permissions);
	}

	private void setupByOwnerPad(final byte[] documentID, final byte ownerPad[],
			final byte userKey[], final byte ownerKey[], final int permissions) {
		final byte userPad[] = computeOwnerKey(ownerKey, ownerPad); // userPad will
																// be set in
																// this.ownerKey
		setupGlobalEncryptionKey(documentID, userPad, ownerKey, permissions); // step
																				// 3
		setupUserKey();
	}

	public void setupByEncryptionKey(final byte[] key, final int keylength) {
		this.mkey = new byte[keylength / 8];
		System.arraycopy(key, 0, this.mkey, 0, this.mkey.length);
	}

	public void setHashKey(final int number, final int generation) {
		this.md5.reset(); // added by ujihara
		this.extra[0] = (byte) number;
		this.extra[1] = (byte) (number >> 8);
		this.extra[2] = (byte) (number >> 16);
		this.extra[3] = (byte) generation;
		this.extra[4] = (byte) (generation >> 8);
		this.md5.update(this.mkey);
		this.md5.update(this.extra);
		if (this.revision == AES_128) {
			this.md5.update(salt);
		}
		this.key = this.md5.digest();
		this.keySize = this.mkey.length + 5;
		if (this.keySize > 16) {
			this.keySize = 16;
		}
	}

	public static PdfObject createInfoId(byte id[]) {
		final ByteBuffer buf = new ByteBuffer(90);
		buf.append('[').append('<');
		for (int k = 0; k < 16; ++k) {
			buf.appendHex(id[k]);
		}
		buf.append('>').append('<');
		id = createDocumentId();
		for (int k = 0; k < 16; ++k) {
			buf.appendHex(id[k]);
		}
		buf.append('>').append(']');
		return new PdfLiteral(buf.toByteArray());
	}

	public PdfDictionary getEncryptionDictionary() {
		final PdfDictionary dic = new PdfDictionary();

		if (this.publicKeyHandler.getRecipientsSize() > 0) {
			PdfArray recipients = null;

			dic.put(PdfName.FILTER, PdfName.PUBSEC);
			dic.put(PdfName.R, new PdfNumber(this.revision));

			try {
				recipients = this.publicKeyHandler.getEncodedRecipients();
			} catch (final Exception f) {
				throw new ExceptionConverter(f);
			}

			if (this.revision == STANDARD_ENCRYPTION_40) {
				dic.put(PdfName.V, new PdfNumber(1));
				dic.put(PdfName.SUBFILTER, PdfName.ADBE_PKCS7_S4);
				dic.put(PdfName.RECIPIENTS, recipients);
			} else if (this.revision == STANDARD_ENCRYPTION_128 && this.encryptMetadata) {
				dic.put(PdfName.V, new PdfNumber(2));
				dic.put(PdfName.LENGTH, new PdfNumber(128));
				dic.put(PdfName.SUBFILTER, PdfName.ADBE_PKCS7_S4);
				dic.put(PdfName.RECIPIENTS, recipients);
			} else {
				dic.put(PdfName.R, new PdfNumber(AES_128));
				dic.put(PdfName.V, new PdfNumber(4));
				dic.put(PdfName.SUBFILTER, PdfName.ADBE_PKCS7_S5);

				final PdfDictionary stdcf = new PdfDictionary();
				stdcf.put(PdfName.RECIPIENTS, recipients);
				if (!this.encryptMetadata) {
					stdcf.put(PdfName.ENCRYPTMETADATA, PdfBoolean.PDFFALSE);
				}

				if (this.revision == AES_128) {
					stdcf.put(PdfName.CFM, PdfName.AESV2);
				} else {
					stdcf.put(PdfName.CFM, PdfName.V2);
				}
				final PdfDictionary cf = new PdfDictionary();
				cf.put(PdfName.DEFAULTCRYPTFILTER, stdcf);
				dic.put(PdfName.CF, cf);if (this.embeddedFilesOnly) {
					dic.put(PdfName.EFF, PdfName.DEFAULTCRYPTFILTER);
					dic.put(PdfName.STRF, PdfName.IDENTITY);
					dic.put(PdfName.STMF, PdfName.IDENTITY);
				}
				else {
					dic.put(PdfName.STRF, PdfName.DEFAULTCRYPTFILTER);
					dic.put(PdfName.STMF, PdfName.DEFAULTCRYPTFILTER);
				}
			}

			MessageDigest md = null;
			byte[] encodedRecipient = null;

			try {
				md = MessageDigest.getInstance("SHA-1");
				md.update(this.publicKeyHandler.getSeed());
				for (int i = 0; i < this.publicKeyHandler.getRecipientsSize(); i++) {
					encodedRecipient = this.publicKeyHandler.getEncodedRecipient(i);
					md.update(encodedRecipient);
				}
				if (!this.encryptMetadata) {
					md.update(new byte[] { (byte) 255, (byte) 255, (byte) 255,
							(byte) 255 });
				}
			} catch (final Exception f) {
				throw new ExceptionConverter(f);
			}

			final byte[] mdResult = md.digest();

			setupByEncryptionKey(mdResult, this.keyLength);
		} else {
			dic.put(PdfName.FILTER, PdfName.STANDARD);
			dic.put(PdfName.O, new PdfLiteral(PdfContentByte
					.escapeString(this.ownerKey)));
			dic.put(PdfName.U, new PdfLiteral(PdfContentByte
					.escapeString(this.userKey)));
			dic.put(PdfName.P, new PdfNumber(this.permissions));
			dic.put(PdfName.R, new PdfNumber(this.revision));

			if (this.revision == STANDARD_ENCRYPTION_40) {
				dic.put(PdfName.V, new PdfNumber(1));
			} else if (this.revision == STANDARD_ENCRYPTION_128 && this.encryptMetadata) {
				dic.put(PdfName.V, new PdfNumber(2));
				dic.put(PdfName.LENGTH, new PdfNumber(128));

			} else {
				if (!this.encryptMetadata) {
					dic.put(PdfName.ENCRYPTMETADATA, PdfBoolean.PDFFALSE);
				}
				dic.put(PdfName.R, new PdfNumber(AES_128));
				dic.put(PdfName.V, new PdfNumber(4));
				dic.put(PdfName.LENGTH, new PdfNumber(128));
				final PdfDictionary stdcf = new PdfDictionary();
				stdcf.put(PdfName.LENGTH, new PdfNumber(16));
				if (this.embeddedFilesOnly) {
					stdcf.put(PdfName.AUTHEVENT, PdfName.EFOPEN);
					dic.put(PdfName.EFF, PdfName.STDCF);
					dic.put(PdfName.STRF, PdfName.IDENTITY);
					dic.put(PdfName.STMF, PdfName.IDENTITY);
				}
				else {
					stdcf.put(PdfName.AUTHEVENT, PdfName.DOCOPEN);
					dic.put(PdfName.STRF, PdfName.STDCF);
					dic.put(PdfName.STMF, PdfName.STDCF);
				}
				if (this.revision == AES_128) {
					stdcf.put(PdfName.CFM, PdfName.AESV2);
				} else {
					stdcf.put(PdfName.CFM, PdfName.V2);
				}
				final PdfDictionary cf = new PdfDictionary();
				cf.put(PdfName.STDCF, stdcf);
				dic.put(PdfName.CF, cf);
			}
		}

		return dic;
	}

	public PdfObject getFileID() {
		return createInfoId(this.documentID);
	}

	public OutputStreamEncryption getEncryptionStream(final OutputStream os) {
		return new OutputStreamEncryption(os, this.key, 0, this.keySize, this.revision);
	}

	int calculateStreamSize(final int n) {
		if (this.revision == AES_128) {
			return (n & 0x7ffffff0) + 32;
		} else {
			return n;
		}
	}

	byte[] encryptByteArray(final byte[] b) {
		try {
			final ByteArrayOutputStream ba = new ByteArrayOutputStream();
			final OutputStreamEncryption os2 = getEncryptionStream(ba);
			os2.write(b);
			os2.finish();
			return ba.toByteArray();
		} catch (final IOException ex) {
			throw new ExceptionConverter(ex);
		}
	}

	public StandardDecryption getDecryptor() {
		return new StandardDecryption(this.key, 0, this.keySize, this.revision);
	}

	public byte[] decryptByteArray(final byte[] b) {
		try {
			final ByteArrayOutputStream ba = new ByteArrayOutputStream();
			final StandardDecryption dec = getDecryptor();
			byte[] b2 = dec.update(b, 0, b.length);
			if (b2 != null) {
				ba.write(b2);
			}
			b2 = dec.finish();
			if (b2 != null) {
				ba.write(b2);
			}
			return ba.toByteArray();
		} catch (final IOException ex) {
			throw new ExceptionConverter(ex);
		}
	}

	public void addRecipient(final Certificate cert, final int permission) {
		this.documentID = createDocumentId();
		this.publicKeyHandler.addRecipient(new PdfPublicKeyRecipient(cert,
				permission));
	}


}