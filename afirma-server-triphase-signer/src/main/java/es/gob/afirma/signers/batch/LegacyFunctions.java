package es.gob.afirma.signers.batch;

import java.util.Properties;

import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Clases con metodos que se mantienen por compatibilidad hacia atr&aacute;s y pueden ser eliminados
 * en futuras versiones del servicio.
 */
public class LegacyFunctions {

	private static final String XADES_FORMAT_PREFIX = "xades"; //$NON-NLS-1$

	private static final String EXTRAPARAM_MODE = "mode"; //$NON-NLS-1$

	/**
	 * Identifica cuando se ha configurado una firma con el formato XAdES y la
	 * propiedad {@code mode} con el valor {@code explicit}. Esta no es una firma
	 * correcta pero, por compatibilidad con los tipos de firmas del Applet pesado,
	 * se ha incluido aqu&iacute;.
	 * @param format Formato declarado para la firma.
	 * @param config Par&aacute;metros adicionales declarados para la firma.
	 * @return {@code true} si se configura una firma <i>XAdES explicit</i>,
	 *         {@code false} en caso contrario.
	 * @deprecated Uso temporal hasta que se elimine el soporte de firmas XAdES
	 *             expl&iacute;citas.
	 */
	@Deprecated
	public static boolean isXadesExplicitConfigurated(final String format, final Properties config) {
		return format != null
				&& format.toLowerCase().startsWith(XADES_FORMAT_PREFIX)
				&& config != null
				&& AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(config.getProperty(EXTRAPARAM_MODE));
	}
}
