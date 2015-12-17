package es.gob.afirma.signers.batch;

import java.io.InputStream;
import java.util.Properties;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.triphase.server.SignatureService;
import es.gob.afirma.triphase.signer.processors.CAdESTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.PAdESTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.XAdESTriPhasePreProcessor;

/** Constantes para la definici&oacute;n de una firma independiente.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SingleSignConstants {

	private static final String CONFIG_FILE = "config.properties"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_INSTALL_XMLDSIG = "alternative.xmldsig"; //$NON-NLS-1$

	private static final Properties config;

	static {
		try {
			final InputStream configIs = SignatureService.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
			if (configIs == null) {
				throw new RuntimeException("No se encuentra el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
			}
			config = new Properties();
			config.load(configIs);
			configIs.close();
		}
		catch(final Exception e) {
			throw new RuntimeException("Error en la carga del fichero de propiedades: " + e, e); //$NON-NLS-1$
		}
	}

	/** Tipo de operaci&oacute;n de firma. */
	public enum SignSubOperation {

		/** Firma. */
		SIGN("sign"), //$NON-NLS-1$

		/** Cofirma. */
		COSIGN("cosign"); //$NON-NLS-1$

		private final String name;

		private SignSubOperation(final String n) {
			this.name = n;
		}

		@Override
		public String toString() {
			return this.name;
		}

		/** Obtiene el tipo de operaci&oacute;n de firma a partir de su nombre.
		 * @param name Nombre del tipo de operaci&oacute;n de firma.
		 * @return Tipo de operaci&oacute;n de firma. */
		public static SignSubOperation getSubOperation(final String name) {
			if (SIGN.toString().equalsIgnoreCase(name)) {
				return SIGN;
			}
			if (COSIGN.toString().equalsIgnoreCase(name)) {
				return COSIGN;
			}
			throw new IllegalArgumentException(
				"Tipo de operacion (suboperation) de firma no soportado: " + name //$NON-NLS-1$
			);
		}
	}

	/** Formato de firma. */
	public enum SignFormat {

		/** CAdES. */
		CADES(AOSignConstants.SIGN_FORMAT_CADES),

		/** XAdES. */
		XADES(AOSignConstants.SIGN_FORMAT_XADES),

		/** PAdES. */
		PADES(AOSignConstants.SIGN_FORMAT_PADES);

		private final String name;

		private SignFormat(final String n) {
			this.name = n;
		}

		@Override
		public String toString() {
			return this.name;
		}

		/** Obtiene el formato de firma a partir de su nombre.
		 * @param name Nombre del formato de firma.
		 * @return Formato firma. */
		public static SignFormat getFormat(final String name) {
			if (name != null) {
				if (CADES.toString().equalsIgnoreCase(name.trim())) {
					return CADES;
				}
				if (XADES.toString().equalsIgnoreCase(name.trim())) {
					return XADES;
				}
				if (PADES.toString().equalsIgnoreCase(name.trim())) {
					return PADES;
				}
			}
			throw new IllegalArgumentException(
				"Tipo de formato de firma no soportado: " + name //$NON-NLS-1$
			);
		}
	}

	/** Algoritmo de firma. */
	public enum SignAlgorithm {

		/** SHA1withRSA. */
		SHA1WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA),

		/** SHA256withRSA. */
		SHA256WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA),

		/** SHA284withRSA. */
		SHA384WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA),

		/** SHA512withRSA. */
		SHA512WITHRSA(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA);

		private final String name;

		private SignAlgorithm(final String n) {
			this.name = n;
		}

		@Override
		public String toString() {
			return this.name;
		}

		/** Obtiene el algoritmo de firma a partir de su nombre.
		 * @param name Nombre del algoritmo de firma.
		 * @return Algoritmo firma. */
		public static SignAlgorithm getAlgorithm(final String name) {
			if (SHA1WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA1WITHRSA;
			}
			if (SHA256WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA256WITHRSA;
			}
			if (SHA384WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA384WITHRSA;
			}
			if (SHA512WITHRSA.toString().equalsIgnoreCase(name)) {
				return SHA512WITHRSA;
			}
			throw new IllegalArgumentException(
				"Tipo de algoritmo de firma no soportado: " + name //$NON-NLS-1$
			);
		}
	}

	static TriPhasePreProcessor getTriPhasePreProcessor(final SingleSign sSign) throws AOInvalidFormatException {
		if (sSign == null) {
			throw new IllegalArgumentException("La firma no puede ser nula"); //$NON-NLS-1$
		}
		switch(sSign.getSignFormat()) {
			case PADES:
				return new PAdESTriPhasePreProcessor();
			case CADES:
				return new CAdESTriPhasePreProcessor();
			case XADES:
				final boolean installXmlDSig = Boolean.parseBoolean(config.getProperty(CONFIG_PARAM_INSTALL_XMLDSIG, Boolean.FALSE.toString()));
				return new XAdESTriPhasePreProcessor(installXmlDSig);
			default:
				throw new AOInvalidFormatException("Formato de firma no soportado: " + sSign.getSignFormat()); //$NON-NLS-1$
		}
	}

}
