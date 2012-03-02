package es.gob.afirma.miniapplet;

import java.io.File;
import java.security.PrivilegedExceptionAction;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

/**
 * Acci&oacute;n para la comprobaci&oacute;n de la validez de la plataforma. Si falla en su
 * ejecuci&oacute;n lanza una {@code RuntimeException} con el mensaje explicativo del problema
 * y su posible soluci&oacute;n por parte del usuario.
 * @deprecated Se externaliza las comprobaciones de entorno.
 */
@Deprecated
final class VerifyPlatformAction implements PrivilegedExceptionAction<Void> {

	private static final String BC_VERSION = "1.46";  //$NON-NLS-1$

	private final String userAgent;

	/**
	 * Crea la acci&oacute;n de verificaci&oacute;n.
	 * @param userAgent Identificador del navegador Web utilizado.
	 */
	VerifyPlatformAction(final String userAgent) {
		this.userAgent = userAgent;
	}

	/** {@inheritDoc} */
	public Void run() throws InvalidExternalLibraryException {
//		this.verificaSunMSCAPINeeded();
//		this.verificaBCVersion();
		return null;
	}

	/**
	 * Comprueba si est&aacute; disponible el proveedor de seguridad SunMSCAPI. En caso de no estarlo
	 * lanza una excepci&oacute;n con la descripci&oacute;n del problema y c&oacute;mo podemos
	 * solucionarlo.
	 * @throws InvalidExternalLibraryException Cuando no se detecte la biblioteca SunMSCAPI.
	 */
	private void verificaSunMSCAPINeeded() throws InvalidExternalLibraryException {
		if (Platform.OS.WINDOWS.equals(Platform.getOS()) &&
		   ("64".equals(Platform.getJavaArch()) && //$NON-NLS-1$
		   (!Platform.BROWSER.FIREFOX.equals(Platform.getBrowser(this.userAgent))))) {
			try {
				AOUtil.classForName("sun.security.mscapi.SunMSCAPI"); //$NON-NLS-1$
				return;
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").severe("Se requiere instalar SunMSCAPI en Windows 64"); //$NON-NLS-1$ //$NON-NLS-2$
				final String sunmscapOri = MiniAppletMessages.getString("MSCapiJar.uri"); //$NON-NLS-1$
				final String sunmscapiDes = Platform.getJavaHome() + File.separator + "lib" + File.separator + "ext" + File.separator; //$NON-NLS-1$ //$NON-NLS-2$
				final String msCapiOri  = MiniAppletMessages.getString("MSCapiDll.uri"); //$NON-NLS-1$
				final String msCapiDes = Platform.getJavaHome() + File.separator + "bin" + File.separator; //$NON-NLS-1$
				throw new InvalidExternalLibraryException(
						"No se tienen instaladas las bibliotecas de SunMSCAPI.", //$NON-NLS-1$
						"VerifyPlatformAction.0", //$NON-NLS-1$
						new String[] {sunmscapOri, sunmscapiDes, msCapiOri, msCapiDes}
				);
			}
		}
	}

	/** Indica si la versi&oacute;n de BouncyCastle es la adecuada para ejecutar el MiniApplet.
	 * @throws InvalidExternalLibraryException Si se encuentra una versi&oacute;n no compatible
	 * en el CLASSPATH de la aplicaci&oacute;n */
	@SuppressWarnings("static-method")
	private void verificaBCVersion() throws InvalidExternalLibraryException {

		final String bcVersion = Platform.getBouncyCastleVersion();

		if (bcVersion == null) {
			throw new InvalidExternalLibraryException(
				"No se ha encontrado el proveedor BouncyCastleProvider", //$NON-NLS-1$
				"VerifyPlatformAction.3", //$NON-NLS-1$
				null
			);
		}

		if (BC_VERSION.compareTo(bcVersion) > 0) {
			String javaExtDir = Platform.getJavaExtDir();
			final String systemJavaExtDir = Platform.getSystemJavaExtDir();
			if (systemJavaExtDir != null && systemJavaExtDir.length() > 0 ) {
				javaExtDir = MiniAppletMessages.getString("VerifyPlatformAction.1", //$NON-NLS-1$
						new String[] {javaExtDir, systemJavaExtDir});
			}

			throw new InvalidExternalLibraryException(
				"Existe una biblioteca en el sistema, externa a la aplicacion, que puede causar un mal funcionamiento de la aplicacion", //$NON-NLS-1$
				"VerifyPlatformAction.2", //$NON-NLS-1$
				new String[] {bcVersion, BC_VERSION, javaExtDir}
			);
		}
	}
}
