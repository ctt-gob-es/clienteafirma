package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.misc.protocol.UrlParametersForBatch;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.signers.batch.client.BatchSigner;
import es.gob.afirma.standalone.crypto.CypherDataManager;


final class ProtocolInvocationLauncherBatch {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$
	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private ProtocolInvocationLauncherBatch() {
		// No instanciable
	}

	/** Procesa un lote de firma en invocaci&oacute;n por protocolo.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param bySocket <code>true</code> para usar comunicaci&oacute;n por <i>socket</i> local,
	 *                 <code>false</code> para usar servidor intermedio.
	 * @return XML de respuesta del procesado.
	 * @throws SocketOperationException Si hay errores en la
	 *                                  comunicaci&oacute;n por <i>socket</i> local. */
	static String processBatch(final UrlParametersForBatch options,
			                   final boolean bySocket) throws SocketOperationException {

		final AOKeyStore aoks = AOKeyStore.getKeyStore(options.getDefaultKeyStore());
		if (aoks == null) {
			LOGGER.severe("No hay un KeyStore con el nombre: " + options.getDefaultKeyStore()); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_07);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_07);
		}

		final PasswordCallback pwc = aoks.getStorePasswordCallback(null);
		final AOKeyStoreManager ksm;
		try {
			ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
				aoks, // Store
				null, // Lib
				null, // Description
				pwc,  // PasswordCallback
				null  // Parent
			);
		}
		catch (final Exception e3) {
			LOGGER.severe("Error obteniendo el AOKeyStoreManager: " + e3); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_08);
			if (!bySocket){
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_08);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_08);
		}

		final CertFilterManager filterManager = new CertFilterManager(options.getExtraParams());
		final List<CertificateFilter> filters = filterManager.getFilters();
		final boolean mandatoryCertificate = filterManager.isMandatoryCertificate();
		final PrivateKeyEntry pke;
		try {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				ServiceInvocationManager.focusApplication();
			}
			final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
				ksm,
				null,
				true,
				true, // showExpiredCertificates
				true, // checkValidity
				filters,
				mandatoryCertificate
			);
			dialog.show();
			pke = ksm.getKeyEntry(
				dialog.getSelectedAlias()
			);
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.severe("Operacion cancelada por el usuario " + e); //$NON-NLS-1$
			if (!bySocket){
				throw new SocketOperationException(RESULT_CANCEL);
			}
			return RESULT_CANCEL;
		}
		catch(final AOCertificatesNotFoundException e) {
			LOGGER.severe("No hay certificados validos en el almacen: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_19);
			if (!bySocket){
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_19);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_19);
		}
		catch (final Exception e) {
			LOGGER.severe("Error al mostrar el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_08);
			if (!bySocket){
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_08);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_08);
		}

		String batchResult;
		try {
			batchResult = BatchSigner.sign(
				Base64.encode(options.getData(), true),
				options.getBatchPresignerUrl(),
				options.getBatchPostSignerUrl(),
				pke.getCertificateChain(),
				pke.getPrivateKey()
			);
			// Devuelve los datos sin codificar en el caso de peticion por socket, por lo que hay que codificarlo
			if (bySocket){
				batchResult = Base64.encode(batchResult.getBytes());
			}
		}
		catch(final Exception e) {
			LOGGER.log(
				Level.SEVERE,
				"Error en el proceso del lote de firmas: " + e, //$NON-NLS-1$
				e
			);
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_20);
			if (!bySocket){
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_20);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_20);
		}

		// Tenemos el XML de resultado del lote, lo subimos al servidor intermedio


		// Si hay clave de cifrado, ciframos
		if (options.getDesKey() != null) {
			try {
				batchResult = CypherDataManager.cipherData(batchResult.getBytes(), options.getDesKey());
			}
			catch (final Exception e) {
				LOGGER.severe("Error en el cifrado de los datos a enviar: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_12);
				if (!bySocket){
					throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_12);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_12);
			}
		}
		else {
			LOGGER.warning(
				"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
		}

		if (options.getStorageServletUrl() != null) {
			// Enviamos la firma cifrada al servicio remoto de intercambio
			try {
				sendData(batchResult, options);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al enviar los datos al servidor: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_11);
				if (!bySocket){
					throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_11);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_11);
			}
		}
		else {
			LOGGER.info(
				"Se omite el envio por red de los datos resultantes por no haberse proporcionado una URL de destino" //$NON-NLS-1$
			);
		}

		return batchResult;
	}

	public static void sendErrorToServer(final String data, final UrlParametersForBatch options){
		try {
			sendData(data, options);
		} catch (final IOException e1) {
			LOGGER.severe("Error al enviar los datos del error en la operacion firma por lotes al servidor" + e1); //$NON-NLS-1$
		}
	}

	private static void sendData(final String data, final UrlParametersForBatch options) throws IOException {

		final StringBuffer url = new StringBuffer(options.getStorageServletUrl().toString());
		url.append("?op=").append(METHOD_OP_PUT); //$NON-NLS-1$
		url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
		url.append("&id=").append(options.getId()); //$NON-NLS-1$
		url.append("&dat=").append(data); //$NON-NLS-1$

		// Llamamos al servicio para guardar los datos
		final byte[] result = UrlHttpManagerFactory.getInstalledManager().readUrl(url.toString(), UrlHttpMethod.POST);

		LOGGER.info(
			"Resultado del envio de datos de lote al servidor intermedio: " + new String(result) //$NON-NLS-1$
		);
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}
}
