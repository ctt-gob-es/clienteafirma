/*
 * $Id: PdfPublicKeySecurityHandler.java 3373 2008-05-12 16:21:24Z xlv $
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
 * the Initial Developer are Copyright (C) 1999-2007 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000-2007 by Paulo Soares. All Rights Reserved.
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

/**
 *     The below 2 methods are from pdfbox.
 *
 *     private DERObject createDERForRecipient(byte[] in, X509Certificate cert) ;
 *     private KeyTransRecipientInfo computeRecipientInfo(X509Certificate x509certificate, byte[] abyte0);
 *
 *     2006-11-22 Aiken Sam.
 */

/**
 * Copyright (c) 2003-2006, www.pdfbox.org
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of pdfbox; nor the names of its
 *    contributors may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * http://www.pdfbox.org
 *
 */

package com.lowagie.text.pdf;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.AlgorithmParameterGenerator;
import java.security.AlgorithmParameters;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DEROutputStream;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

/**
 * @author Aiken Sam (aikensam@ieee.org)
 */
class PdfPublicKeySecurityHandler {

    private static final int SEED_LENGTH = 20;

    private ArrayList recipients = null;

    private byte[] seed = new byte[SEED_LENGTH];

    public PdfPublicKeySecurityHandler() {
        KeyGenerator key;
        try {
            key = KeyGenerator.getInstance("AES");
            key.init(192, new SecureRandom());
            final SecretKey sk = key.generateKey();
            System.arraycopy(sk.getEncoded(), 0, this.seed, 0, SEED_LENGTH); // create the 20 bytes seed
        } catch (final NoSuchAlgorithmException e) {
            this.seed = SecureRandom.getSeed(SEED_LENGTH);
        }

        this.recipients = new ArrayList();
    }


    /*
     * Routine for decode output of PdfContentByte.escapeString(byte[] bytes).
     * It should be moved to PdfContentByte.
     */



    void addRecipient(final PdfPublicKeyRecipient recipient) {
        this.recipients.add(recipient);
    }

    protected byte[] getSeed() {
        return this.seed.clone();
    }
    /*
    public PdfPublicKeyRecipient[] getRecipients() {
        recipients.toArray();
        return (PdfPublicKeyRecipient[])recipients.toArray();
    }*/

    public int getRecipientsSize() {
        return this.recipients.size();
    }

    public byte[] getEncodedRecipient(final int index) throws IOException, GeneralSecurityException {
        //Certificate certificate = recipient.getX509();
        final PdfPublicKeyRecipient recipient = (PdfPublicKeyRecipient)this.recipients.get(index);
        byte[] cms = recipient.getCms();

        if (cms != null) {
			return cms;
		}

        final Certificate certificate  = recipient.getCertificate();
        int permission =  recipient.getPermission();//PdfWriter.AllowCopy | PdfWriter.AllowPrinting | PdfWriter.AllowScreenReaders | PdfWriter.AllowAssembly;
        final int revision = 3;

        permission |= revision==3 ? 0xfffff0c0 : 0xffffffc0;
        permission &= 0xfffffffc;
        permission += 1;

        final byte[] pkcs7input = new byte[24];

        final byte one = (byte)permission;
        final byte two = (byte)(permission >> 8);
        final byte three = (byte)(permission >> 16);
        final byte four = (byte)(permission >> 24);

        System.arraycopy(this.seed, 0, pkcs7input, 0, 20); // put this seed in the pkcs7 input

        pkcs7input[20] = four;
        pkcs7input[21] = three;
        pkcs7input[22] = two;
        pkcs7input[23] = one;

        final ASN1Primitive obj = createDERForRecipient(pkcs7input, (X509Certificate)certificate);

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();

        final DEROutputStream k = new DEROutputStream(baos);

        k.writeObject(obj);

        cms = baos.toByteArray();

        recipient.setCms(cms);

        return cms;
    }

    public PdfArray getEncodedRecipients() throws IOException,
                                         GeneralSecurityException {
        PdfArray EncodedRecipients = new PdfArray();
        byte[] cms = null;
        for (int i=0; i<this.recipients.size(); i++) {
			try {
			    cms = getEncodedRecipient(i);
			    EncodedRecipients.add(new PdfLiteral(PdfContentByte.escapeString(cms)));
			} catch (final GeneralSecurityException e) {
			    EncodedRecipients = null;
			} catch (final IOException e) {
			    EncodedRecipients = null;
			}
		}

        return EncodedRecipients;
    }

    private ASN1Primitive createDERForRecipient(final byte[] in, final X509Certificate cert)
        throws IOException,
               GeneralSecurityException
    {

        final String s = "1.2.840.113549.3.2";

        final AlgorithmParameterGenerator algorithmparametergenerator = AlgorithmParameterGenerator.getInstance(s);
        final AlgorithmParameters algorithmparameters = algorithmparametergenerator.generateParameters();
        final ByteArrayInputStream bytearrayinputstream = new ByteArrayInputStream(algorithmparameters.getEncoded("ASN.1"));
        final ASN1InputStream asn1inputstream = new ASN1InputStream(bytearrayinputstream);
        final ASN1Primitive derobject = asn1inputstream.readObject();
        final KeyGenerator keygenerator = KeyGenerator.getInstance(s);
        keygenerator.init(128);
        final SecretKey secretkey = keygenerator.generateKey();
        final Cipher cipher = Cipher.getInstance(s);
        cipher.init(1, secretkey, algorithmparameters);
        final byte[] abyte1 = cipher.doFinal(in);
        final DEROctetString deroctetstring = new DEROctetString(abyte1);
        final KeyTransRecipientInfo keytransrecipientinfo = computeRecipientInfo(cert, secretkey.getEncoded());
        final DERSet derset = new DERSet(new RecipientInfo(keytransrecipientinfo));
        final AlgorithmIdentifier algorithmidentifier = new AlgorithmIdentifier(new DERObjectIdentifier(s), derobject);
        final EncryptedContentInfo encryptedcontentinfo =
            new EncryptedContentInfo(PKCSObjectIdentifiers.data, algorithmidentifier, deroctetstring);
        final EnvelopedData env = new EnvelopedData(null, derset, encryptedcontentinfo, null);
        final ContentInfo contentinfo =
            new ContentInfo(PKCSObjectIdentifiers.envelopedData, env);
        return contentinfo.toASN1Primitive();
    }

    private KeyTransRecipientInfo computeRecipientInfo(final X509Certificate x509certificate, final byte[] abyte0)
        throws GeneralSecurityException, IOException
    {
        final ASN1InputStream asn1inputstream =
            new ASN1InputStream(new ByteArrayInputStream(x509certificate.getTBSCertificate()));
        final TBSCertificateStructure tbscertificatestructure =
            TBSCertificateStructure.getInstance(asn1inputstream.readObject());
        final AlgorithmIdentifier algorithmidentifier = tbscertificatestructure.getSubjectPublicKeyInfo().getAlgorithmId();
        final IssuerAndSerialNumber issuerandserialnumber =
            new IssuerAndSerialNumber(
                tbscertificatestructure.getIssuer(),
                tbscertificatestructure.getSerialNumber().getValue());
        final Cipher cipher = Cipher.getInstance(algorithmidentifier.getObjectId().getId());
        cipher.init(1, x509certificate);
        final DEROctetString deroctetstring = new DEROctetString(cipher.doFinal(abyte0));
        final RecipientIdentifier recipId = new RecipientIdentifier(issuerandserialnumber);
        return new KeyTransRecipientInfo( recipId, algorithmidentifier, deroctetstring);
    }
}
