/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.test.keystores;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/** Prueba manual del llavero de macOS con certificados personales en "Confiar siempre".
 * <p>
 * En JDK 17 y 21 un certificado con ajustes de confianza personalizados oculta su clave privada
 * (JDK-8320362), por lo que Autofirma no lo muestra. Esta prueba comprueba que
 * <code>AppleKeyStoreManager</code> lo recupera. Pasos:
 * </p>
 * <ol>
 *  <li>En Acceso a Llaveros, abrir un certificado personal con clave privada y establecer
 *      "Confiar siempre" en la secci&oacute;n Confianza.</li>
 *  <li>Quitar la anotaci&oacute;n <code>@Ignore</code> y ejecutar desde la ra&iacute;z del proyecto:
 *      <pre>mvn -pl afirma-core-keystores -am test -Dtest=TestMacKeyChainTrustSettings \
 *     -Dsurefire.failIfNoSpecifiedTests=false \
 *     -DargLine="--add-opens java.base/apple.security=ALL-UNNAMED" \
 *     -Des.gob.afirma.test.cert="TEXTO DEL CN DEL CERTIFICADO"</pre>
 *      Sin <code>-DargLine</code> la prueba reproduce el comportamiento anterior y debe fallar.</li>
 *  <li>Devolver el certificado a "Usar ajustes por omisi&oacute;n del sistema".</li>
 * </ol> */
public class TestMacKeyChainTrustSettings {

	private static final String EXPECTED_CERT_PROPERTY = "es.gob.afirma.test.cert"; //$NON-NLS-1$

	/** Comprueba que todos los certificados del llavero tienen clave privada recuperable y que,
	 * si se indica, el certificado esperado est&aacute; entre ellos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Requiere un llavero con un certificado personal en "Confiar siempre"
	public void testCertificateWithTrustSettingsIsAvailable() throws Exception {
		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			return;
		}
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, "Mac-Afirma", null, null); //$NON-NLS-1$
		final String[] aliases = ksm.getAliases();
		Assert.assertTrue("El llavero no contiene certificados con clave privada", aliases.length > 0); //$NON-NLS-1$

		final String expectedCert = System.getProperty(EXPECTED_CERT_PROPERTY);
		boolean expectedCertFound = false;
		for (final String alias : aliases) {
			final PrivateKeyEntry pke = ksm.getKeyEntry(alias);
			Assert.assertNotNull("No se ha podido recuperar la clave privada del alias '" + alias + "'", pke); //$NON-NLS-1$ //$NON-NLS-2$
			final X509Certificate cert = (X509Certificate) pke.getCertificate();
			System.out.println(alias + " -> " + cert.getSubjectX500Principal()); //$NON-NLS-1$
			if (expectedCert != null && cert.getSubjectX500Principal().getName().contains(expectedCert)) {
				expectedCertFound = true;
			}
		}
		if (expectedCert != null) {
			Assert.assertTrue("El certificado '" + expectedCert + "' no esta disponible", expectedCertFound); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
}
