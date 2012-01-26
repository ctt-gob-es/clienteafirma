package es.gob.afirma.install;

import java.net.URL;

import org.junit.Test;

/** Pruebas gen&eacute;ricas del BootLoader.
 * @author Tom&aacute;s garc&iacute;a-Mer&aacute;s */
public final class TestBootLoader {

	/** Prueba las comprobaciones de entorno b&aacute;sicas.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void testIsNeeded() throws Exception {
		final CheckAndInstallMissingParts cimp = new CheckAndInstallMissingParts(
			BootPlatform.getOS(),
			BootPlatform.getJavaVersion(),
			null,
			new URL("http://www.google.com/") //$NON-NLS-1$
		);
		System.out.println("Se necesita Xalan: " + cimp.isEndorsedXalanNeeded()); //$NON-NLS-1$
		System.out.println("Se actualizacion de Java 5: " + cimp.isEndorsedJava5AFirmaDependenciesNeeded()); //$NON-NLS-1$
		System.out.println("Se necesita SunMSCAPI: " + cimp.isSunMSCAPINeeded()); //$NON-NLS-1$
		System.out.println("Se necesita SunPKCS11: " + CheckAndInstallMissingParts.isSunPKCS11Needed()); //$NON-NLS-1$
	}
}
