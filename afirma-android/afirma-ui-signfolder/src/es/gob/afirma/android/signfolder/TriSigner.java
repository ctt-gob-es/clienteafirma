package es.gob.afirma.android.signfolder;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;

import org.xml.sax.SAXException;

import android.util.Log;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestResult;
import es.gob.afirma.android.signfolder.proxy.SignRequest;
import es.gob.afirma.android.signfolder.proxy.TriphaseRequest;
import es.gob.afirma.android.signfolder.proxy.TriphaseSignDocumentRequest;
import es.gob.afirma.android.signfolder.proxy.TriphaseSignDocumentRequest.TriphaseConfigData;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOPkcs1Signer;

/** Firmador trif&aacute;sico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class TriSigner {

	/** Firma de forma trif&aacute;sica una petici&oacute;n de firma.
	 * @param request Petici&oacute;n de firma.
	 * @param pk Clave privada para al firma.
	 * @param certificateChain Cadena de certificaci&oacute;n del certificado de firma.
	 * @param commMgr Gestor de las llamadas a servicios remotos.
	 * @return Resultado de la operaci&oacute;n.
	 * @throws CertificateEncodingException Cuando hay un error en la codificaci&oacute;n del certificado.
	 * @throws IOException Cuando hay un error de lectura/escritura de datos.
	 * @throws SAXException Cuando el XML est&aacute; mal formado. */
	static RequestResult sign(final SignRequest request, final PrivateKey pk,
			final X509Certificate[] certificateChain, final CommManager commMgr)
					throws CertificateEncodingException, IOException, SAXException {

		// *****************************************************************************************************
		// **************************** PREFIRMA ***************************************************************
		//******************************************************************************************************

		Log.i(SFConstants.LOG_TAG, "TriSigner - sign: == PREFIRMA =="); //$NON-NLS-1$

		// Mandamos a prefirmar y obtenemos los resultados
		final TriphaseRequest[] signRequest = commMgr.preSignRequests(
				request,
				certificateChain[0] // Solo el primero, no toda la cadena
				);

		// *****************************************************************************************************
		// ******************************* FIRMA ***************************************************************
		//******************************************************************************************************

		Log.i(SFConstants.LOG_TAG, "TriSigner - sign: == FIRMA =="); //$NON-NLS-1$

		// Recorremos las peticiones de firma
		for (int i = 0; i < signRequest.length; i++) {

			// Si fallo una sola firma de la peticion, esta es erronea al completo
			if (!signRequest[i].isStatusOk()) {
				Log.w(SFConstants.LOG_TAG, "Se encontro prefirma erronea, se aborta el proceso de firma. La traza de la excepcion es: " + signRequest[i].getException()); //$NON-NLS-1$
				return new RequestResult(request.getId(), false);
			}

			// Recorremos cada uno de los documentos de cada peticion de firma
			for(final TriphaseSignDocumentRequest docRequests : signRequest[i].getDocumentsRequests()) {
				// Firmamos las prefirmas y actualizamos los parciales de cada documento de cada peticion
				try {
					signPhase2(docRequests, pk, certificateChain);
				}
				catch(final Exception e) {
					Log.w(SFConstants.LOG_TAG, "Error en la fase de FIRMA: " + e); //$NON-NLS-1$
					e.printStackTrace();

					// Si un documento falla en firma toda la peticion se da por fallida
					return new RequestResult(request.getId(), false);
				}
			} // Documentos de peticion
		} // Peticiones

		// *****************************************************************************************************
		// **************************** POSTFIRMA **************************************************************
		//******************************************************************************************************

		Log.i(SFConstants.LOG_TAG, "TriSigner - sign: == POSTFIRMA =="); //$NON-NLS-1$

		// Mandamos a postfirmar y recogemos el resultado
		return commMgr.postSignRequests(
				signRequest,
				certificateChain[0] // Solo el primero, no toda la cadena)
				);
	}

	/**
	 * Obtiene el nombre de un algoritmo de firma que usa un algoritmo de huella espec&iacute;fico.
	 * @param mdAlgorithm Algoritmo de huella digital.
	 * @return Algoritmo de firma que utiliza el algoritmo de huella digital indicado.
	 */
	private static String getSignatureAlgorithm(final String mdAlgorithm) {
		return mdAlgorithm.replace("-", "") + "withRSA"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	/**
	 * Genera la firma PKCS#1 (segunda fase del proceso de firma trif&aacute;sica) y muta el objeto
	 * de petici&oacute;n de firma de un documento para almacenar el mismo el resultado.
	 * @param docRequest Petici&oacute;n de firma de un documento.
	 * @param key Clave privada de firma.
	 * @param certChain Cadena de certificaci&oacute;n del certificado de firma.
	 * @throws AOException Cuando ocurre alg&uacute;n error al generar el PKCS#1 o falta alg&uacute;n
	 * par&aacute;metro obligatorio dentro del resultado de prefirma de alguno de los documentos.
	 */
	private static void signPhase2(final TriphaseSignDocumentRequest docRequest, final PrivateKey key, final Certificate[] certChain) throws AOException {

		final String signatureAlgorithm = getSignatureAlgorithm(docRequest.getMessageDigestAlgorithm());

		final TriphaseConfigData config = docRequest.getPartialResult();

		// Es posible que se ejecute mas de una firma como resultado de haber proporcionado varios
		// identificadores de datos o en una operacion de contrafirma.

		int signCount = 1;
		if (config.getSignCount() != null) {
			signCount = config.getSignCount().intValue();
		}

		for (int i = 0; i < signCount; i++) {
			byte[] preSign;
			try {
				preSign = config.getPreSign(i);
			}
			catch (final IndexOutOfBoundsException e) {
				// Cuando la respuesta no indica el numero de firmas y no se ha devuelto ninguna
				Log.e(SFConstants.LOG_TAG, "No se ha devuelto ningun resultado de firma"); //$NON-NLS-1$
				preSign = null;
			}
			if (preSign == null) {
				throw new AOException("El servidor no ha devuelto la prefirma numero " + i); //$NON-NLS-1$
			}

			final byte[] pkcs1sign = new AOPkcs1Signer().sign(
					preSign,
					signatureAlgorithm,
					key,
					certChain,
					null // No hay parametros en PKCS#1
					);

			// Configuramos la peticion de postfirma indicando las firmas PKCS#1 generadas
			config.addPk1(pkcs1sign);

			if (config.isNeedPreSign() == null || !config.isNeedPreSign().booleanValue()) {
				config.setPreSign(i, null);
			}
		}
	}
}
