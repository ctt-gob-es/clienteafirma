/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package test.es.gob.afirma.signers.batch.server;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.batch.client.BatchSigner;

/** Pruebas del cliente de firma por lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestBatchSigner {

	private static final String CERT_PATH = "PFActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

//	private static final String SAMPLE_BATCH_XML =
//	"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" +
//	"<signbatch stoponerror=\"on\" algorithm=\"SHA1withRSA\"><singlesign Id=\"d5d7667b-f28e-4471-a182-97da7618eaa8\"><datasource>PEluaWNpbz5QYXJhIFBydWViYXM8L0luaWNpbz4=</datasource><format>XAdES</format><suboperation>sign</suboperation><extraparams>Zm9ybWF0PVhBZEVTIERldGFjaGVkCg==</extraparams><signsaver><class>es.gob.afirma.signers.batch.SignSaverFile</class><config>RmlsZU5hbWU9cHJ1ZWJhLnhtbA==</config></signsaver></singlesign></signbatch>";

	private static final String SAMPLE_BATCH_XML =
			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\r\n" + //$NON-NLS-1$
			"<signbatch stoponerror=\"false\" algorithm=\"SHA256withRSA\">\r\n" + //$NON-NLS-1$
			" <singlesign Id=\"7725374e-728d-4a33-9db9-3a4efea4cead\">\r\n" + //$NON-NLS-1$
			"  <datasource>http://www.google.com</datasource>\r\n" + //$NON-NLS-1$
			"  <format>XAdES</format>\r\n" + //$NON-NLS-1$
			"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
			"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD03NzI1Mzc0ZS03MjhkLTRhMzMtOWRiOS0zYTRlZmVhNGNlYWQNCg==</extraparams>\r\n" + //$NON-NLS-1$
			"  <signsaver>\r\n" + //$NON-NLS-1$
			"   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\r\n" + //$NON-NLS-1$
			"   <config>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpGaWxlTmFtZT1DXDpcXFVzZXJzXFx0b21hc1xcQXBwRGF0YVxcTG9jYWxcXFRlbXBcXEZJUk1BMS54bWwNCg==</config>\r\n" + //$NON-NLS-1$
			"  </signsaver>\r\n" + //$NON-NLS-1$
			" </singlesign>\r\n" + //$NON-NLS-1$
			" <singlesign Id=\"93d1531c-cd32-4c8e-8cc8-1f1cfe66f64a\">\r\n" + //$NON-NLS-1$
			"  <datasource>SG9sYSBNdW5kbw==</datasource>\r\n" + //$NON-NLS-1$
			"  <format>CAdES</format>\r\n" + //$NON-NLS-1$
			"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
			"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD05M2QxNTMxYy1jZDMyLTRjOGUtOGNjOC0xZjFjZmU2NmY2NGENCg==</extraparams>\r\n" + //$NON-NLS-1$
			"  <signsaver>\r\n" + //$NON-NLS-1$
			"   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\r\n" + //$NON-NLS-1$
			"   <config>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpGaWxlTmFtZT1DXDpcXFVzZXJzXFx0b21hc1xcQXBwRGF0YVxcTG9jYWxcXFRlbXBcXEZJUk1BMi54bWwNCg==</config>\r\n" + //$NON-NLS-1$
			"  </signsaver>\r\n" + //$NON-NLS-1$
			" </singlesign>\r\n" + //$NON-NLS-1$
			"</signbatch>"; //$NON-NLS-1$

	/** Prueba simple del cliente de firma por lote.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {

		final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

		final PrivateKeyEntry pke;
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final String res = BatchSigner.sign(
			Base64.encode(SAMPLE_BATCH_XML.getBytes(), true),
			"http://localhost:8080/afirma-server-triphase-signer/BatchPresigner", //$NON-NLS-1$
			"http://localhost:8080/afirma-server-triphase-signer/BatchPostsigner", //$NON-NLS-1$
			pke.getCertificateChain(),
			pke.getPrivateKey()
		);

		LOGGER.info(res);
	}

}
