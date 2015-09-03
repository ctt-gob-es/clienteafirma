package es.gob.afirma.signers.batch;

import java.io.IOException;
import java.security.cert.X509Certificate;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.triphase.server.processors.TriPhasePreProcessor;

final class SingleSignPreProcessor {

	private SingleSignPreProcessor() {
		// No instanciable
	}

	/** Realiza el proceso de prefirma, incluyendo la descarga u obtenci&oacute;n de datos.
	 * @param sSign Firma sobre la que hay que hacer el preproceso.
	 * @param certChain Cadena de certificados del firmante.
	 * @param algorithm Algoritmo de firma.
	 * @return Nodo <code>firma</code> del XML de datos trif&aacute;sicos (sin ninguna etiqueta
	 *         antes ni despu&eacute;s).
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos. */
	static String doPreProcess(final SingleSign sSign,
			                   final X509Certificate[] certChain,
			                   final SingleSignConstants.SignAlgorithm algorithm) throws IOException,
			                                                                             AOException {
		final TriphaseData td = getPreSign(sSign, certChain, algorithm);
		final String tmp = td.toString();
		return tmp.substring(
			tmp.indexOf("<firmas>") + "<firmas>".length(), //$NON-NLS-1$ //$NON-NLS-2$
			tmp.indexOf("</firmas>") //$NON-NLS-1$
		);
	}

	private static TriphaseData getPreSign(final SingleSign sSign,
			                               final X509Certificate[] certChain,
			                               final SingleSignConstants.SignAlgorithm algorithm) throws IOException,
			                                                                                         AOException {
		if (certChain == null || certChain.length < 1) {
			throw new IllegalArgumentException(
				"La cadena de certificados del firmante no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}

		// Instanciamos el preprocesador adecuado
		final TriPhasePreProcessor prep = SingleSignConstants.getTriPhasePreProcessor(sSign);

		final byte[] docBytes = sSign.getData();

		switch(sSign.getSubOperation()) {
			case SIGN:
				return TriphaseData.parser(
					prep.preProcessPreSign(
						docBytes,
						algorithm.toString(),
						certChain,
						sSign.getExtraParams()
					)
				);
			case COSIGN:
				return TriphaseData.parser(
						prep.preProcessPreCoSign(
						docBytes,
						algorithm.toString(),
						certChain,
						sSign.getExtraParams()
					)
				);
			default:
				throw new UnsupportedOperationException(
					"Operacion no soportada: " + sSign.getSubOperation() //$NON-NLS-1$
				);
		}

	}


}
