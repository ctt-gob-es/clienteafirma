/*
 * $Id: StandardDecryption.java 3117 2008-01-31 05:53:22Z xlv $
 *
 * Copyright 2006 Paulo Soares
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

import com.lowagie.text.pdf.crypto.AESCipher;
import com.lowagie.text.pdf.crypto.ARCFOUREncryption;

class StandardDecryption {
    private ARCFOUREncryption arcfour;
    private AESCipher cipher;
    private byte[] key;
    private static final int AES_128 = 4;
    private final boolean aes;
    private boolean initiated;
    private final byte[] iv = new byte[16];
    private int ivptr;

    /** Creates a new instance of StandardDecryption */
    StandardDecryption(final byte key[], final int off, final int len, final int revision) {
        this.aes = revision == AES_128;
        if (this.aes) {
            this.key = new byte[len];
            System.arraycopy(key, off, this.key, 0, len);
        }
        else {
            this.arcfour = new ARCFOUREncryption();
            this.arcfour.prepareARCFOURKey(key, off, len);
        }
    }

    byte[] update(final byte[] b, int off, int len) {
        if (this.aes) {
            if (this.initiated) {
				return this.cipher.update(b, off, len);
			} else {
                final int left = Math.min(this.iv.length - this.ivptr, len);
                System.arraycopy(b, off, this.iv, this.ivptr, left);
                off += left;
                len -= left;
                this.ivptr += left;
                if (this.ivptr == this.iv.length) {
                    this.cipher = new AESCipher(false, this.key, this.iv);
                    this.initiated = true;
                    if (len > 0) {
						return this.cipher.update(b, off, len);
					}
                }
                return null;
            }
        }
        else {
            final byte[] b2 = new byte[len];
            this.arcfour.encryptARCFOUR(b, off, len, b2, 0);
            return b2;
        }
    }

    byte[] finish() {
        if (this.aes) {
            return this.cipher.doFinal();
        } else {
			return null;
		}
    }
}