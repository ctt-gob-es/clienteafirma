/*
 * eID Applet Project.
 * Copyright (C) 2009 FedICT.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version
 * 3.0 as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, see 
 * http://www.gnu.org/licenses/.
 */

/*
 * Copyright (C) 2009 FedICT.
 * This file is part of the eID Applet Project.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package es.gob.afirma.be.fedict.eid.applet.service.signer;

import java.io.InputStream;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.List;

import es.gob.afirma.be.fedict.eid.applet.service.signer.ooxml.AbstractOOXMLSignatureService;
import es.gob.afirma.be.fedict.eid.applet.service.signer.ooxml.OOXMLProvider;
import es.gob.afirma.misc.AOUtil;

public class AbstractOOXMLSignatureServiceContainer {

	public static void setUp() {
		OOXMLProvider.install();
	}

	private static class OOXMLTestSignatureService extends
			AbstractOOXMLSignatureService {

		@Override
		protected String getSignatureDigestAlgorithm() {
			return digestAlgorithm;
		}

		private final byte[] ooxml;

		private final String digestAlgorithm;

		public OOXMLTestSignatureService(InputStream ooxmlis, String digestAlgo) {
			try {
				this.ooxml = AOUtil.getDataFromInputStream(ooxmlis);
			} catch (Exception e) {
				throw new IllegalArgumentException(
						"No se ha podido leer el OOXML desde el InputStream de entrada");
			}
			if (digestAlgo == null)
				digestAlgorithm = "SHA1";
			else
				digestAlgorithm = digestAlgo;
		}

		@Override
		protected byte[] getOfficeOpenXMLDocument() {
			return this.ooxml;
		}

	}

	public byte[] sign(InputStream ooxml, List<X509Certificate> certChain,
			String digestAlgorithm, PrivateKey pk, int signerCount)
			throws Exception {

		OOXMLProvider.install();

		OOXMLTestSignatureService signatureService = new OOXMLTestSignatureService(
				ooxml, digestAlgorithm);

		// operate
		byte[] signedXML = signatureService.preSign(null, certChain, pk);

		// // setup: key material, signature value
		// KeyPair keyPair = PkiTestUtils.generateKeyPair();
		// Cipher cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");
		// cipher.init(Cipher.ENCRYPT_MODE, keyPair.getPrivate());
		// byte[] digestInfoValue = ArrayUtils.addAll(
		// PkiTestUtils.SHA1_DIGEST_INFO_PREFIX, digestInfo.digestValue);
		// byte[] signatureValue = cipher.doFinal(digestInfoValue);
		//
		// DateTime notBefore = new DateTime();
		// DateTime notAfter = new
		// DateTime(System.currentTimeMillis()+600000000);
		// X509Certificate certificate =
		// PkiTestUtils.generateCertificate(keyPair
		// .getPublic(), signerDn, notBefore, notAfter, null, keyPair
		// .getPrivate(), true, 0, null, null, new KeyUsage(
		// KeyUsage.nonRepudiation));
		//
		// // operate: postSign
		// signatureService.postSign(signatureValue,
		// Collections.singletonList(certificate));
		//
		return signatureService.outputSignedOfficeOpenXMLDocument(signedXML);

		// return null;

	}

}
