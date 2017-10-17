package es.gob.afirma.crypto.jarverifier;

import java.io.InputStream;
import java.security.cert.X509Certificate;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;

/**
 * Prueba las funciones de extracci&oacute;n de firmas y certificados de la firma de un JAR,
 * as&iacute; como su importaci&oacute;n en el almac&eacute;n de confianza de Java.
 */
public class TestSignatureExtractor {

	/**
	 * Prueba las funciones de extraccion de certificados de la firma de un JAR.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testExtractSigningCertificatesFromJar() {
		final byte[] signature;
		try (
			final InputStream jarIs = TestSignatureExtractor.class.getResourceAsStream("/HolaMundo.jar"); //$NON-NLS-1$
		) {
			signature = JarSignatureCertExtractor.getJarSignature(jarIs);
		}
		catch (final Exception ioe) {
			Assert.fail("No se pudo extraer la firma del JAR: " + ioe); //$NON-NLS-1$
			return;
		}

		Assert.assertNotNull("No se ha podido extraer la firma del JAR", signature); //$NON-NLS-1$

		X509Certificate[] certChain = null;
		try {
			certChain = JarSignatureCertExtractor.getJarSignatureCertChain(signature);
		}
		catch (final Exception e) {
			Assert.fail("No se pudo extraer la cadena de certificados de la firma del JAR: " + e); //$NON-NLS-1$
		}

		Assert.assertNotNull("No se ha podido extraer la cadena de certificados de la firma", certChain); //$NON-NLS-1$

		Assert.assertTrue("La cadena de certificados extraida esta vacia", certChain.length > 0); //$NON-NLS-1$

		for (final X509Certificate cert : certChain) {
			System.out.println(AOUtil.getCN(cert));
		}
	}
}
