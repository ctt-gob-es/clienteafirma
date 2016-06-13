package es.gob.afirma.standalone.protocol;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.cert.CertificateEncodingException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JFileChooser;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.misc.protocol.UrlParametersToSignAndSave;
import es.gob.afirma.core.misc.protocol.UrlParametersToSignAndSave.Operation;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.crypto.CypherDataManager;

final class ProtocolInvocationLauncherSignAndSave {

	private static final char CERT_SIGNATURE_SEPARATOR = '|';
	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$
	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	private ProtocolInvocationLauncherSignAndSave() {
		// No instanciable
	}

	static String process(final UrlParametersToSignAndSave options, final boolean bySocket) throws SocketOperationException {

		if (options == null) {
			LOGGER.severe("Las opciones de firma son nulas"); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_01);
			if (!bySocket){
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_01);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_01);
		}

		// En caso de que no se haya solicitado una operacion de multifirma con el formato AUTO
		// configuramos el servidor en base al nombre de formato
		AOSigner signer = null;
		if (!AOSignConstants.SIGN_FORMAT_AUTO.equalsIgnoreCase(options.getSignatureFormat())) {
			signer = AOSignerFactory.getSigner(options.getSignatureFormat());
			if (signer == null) {
				LOGGER.severe("No hay un firmador configurado para el formato: " + options.getSignatureFormat()); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_06);
				if (!bySocket) {
					throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_06);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_06);
			}
		}

		final AOKeyStore aoks = AOKeyStore.getKeyStore(options.getDefaultKeyStore());
		if (aoks == null) {
			LOGGER.severe("No hay un KeyStore con el nombre: " + options.getDefaultKeyStore()); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_07);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_07);
		}

		// Si no hay datos a firmar se los pedimos al usuario
		String selectedFilename = null;
		if (options.getData() == null) {

			final String dialogTilte = Operation.SIGN.equals(options.getOperation()) ?
					ProtocolMessages.getString("ProtocolLauncher.25") : //$NON-NLS-1$
						ProtocolMessages.getString("ProtocolLauncher.26"); //$NON-NLS-1$

			final File selectedDataFile;
			try {
				if (Platform.OS.MACOSX.equals(Platform.getOS())) {
					ServiceInvocationManager.focusApplication();
				}
				selectedDataFile = AOUIFactory.getLoadFiles(
					dialogTilte,
					new JFileChooser().getFileSystemView().getDefaultDirectory().toString(),
					null,
					null,
					ProtocolMessages.getString("ProtocolLauncher.27"), //$NON-NLS-1$
					false,
					false,
					AutoFirmaUtil.getDefaultDialogsIcon(),
					null
				)[0];
			}
			catch(final AOCancelledOperationException e) {
				LOGGER.info("carga de datos de firma cancelada por el usuario: " + e); //$NON-NLS-1$
				if (!bySocket){
					throw new SocketOperationException(getResultCancel());
				}
				return getResultCancel();
			}

			selectedFilename = selectedDataFile.getName();

			try {
				final byte[] data;
				try (
					final InputStream fis = new FileInputStream(selectedDataFile);
					final InputStream bis = new BufferedInputStream(fis);
				) {
					data = AOUtil.getDataFromInputStream(bis);
				}
				if (data == null) {
					throw new IOException("La lectura de datos para firmar ha devuelto un nulo"); //$NON-NLS-1$
				}
				options.setData(data);
			}
			catch(final Exception e) {
				LOGGER.severe("Error en la lectura de los datos a firmar: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(
					ProtocolInvocationLauncherErrorManager.SAF_00
				);
				if (!bySocket){
					throw new SocketOperationException(
						ProtocolInvocationLauncherErrorManager.SAF_00
					);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(
					ProtocolInvocationLauncherErrorManager.SAF_00
				);
			}
		}

		// En caso de haber programado el formato "AUTO", se selecciona el firmador a partir
		// de los datos (firma) proporcionados
		if (signer == null) {
			if (Operation.SIGN == options.getOperation()) {
				if (DataAnalizerUtil.isPDF(options.getData())) {
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_PADES);
					options.setSignFormat(AOSignConstants.SIGN_FORMAT_PADES);
				}
				else if (DataAnalizerUtil.isFacturae(options.getData())) {
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_FACTURAE);
					options.setSignFormat(AOSignConstants.SIGN_FORMAT_FACTURAE);
				}
				else if (DataAnalizerUtil.isXML(options.getData())) {
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_XADES);
					options.setSignFormat(AOSignConstants.SIGN_FORMAT_XADES);
				}
				else if (DataAnalizerUtil.isODF(options.getData())) {
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_ODF);
					options.setSignFormat(AOSignConstants.SIGN_FORMAT_ODF);
				}
				else if (DataAnalizerUtil.isOOXML(options.getData())) {
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_OOXML);
					options.setSignFormat(AOSignConstants.SIGN_FORMAT_OOXML);
				}
				else {
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_CADES);
					options.setSignFormat(AOSignConstants.SIGN_FORMAT_CADES);
				}
			}
			else {
				try {
					signer = AOSignerFactory.getSigner(options.getData());
				}
				catch (final IOException e) {
					LOGGER.severe(
							"No se han podido analizar los datos para determinar si son una firma: " + e //$NON-NLS-1$
							);
					// signer sera null
				}
			}

			if (signer == null) {
				LOGGER.severe(
					"Los datos no se corresponden con una firma electronica o no se pudieron analizar" //$NON-NLS-1$
				);
				ProtocolInvocationLauncherErrorManager.showError(
					ProtocolInvocationLauncherErrorManager.SAF_17
				);
				if (!bySocket){
					throw new SocketOperationException(
						ProtocolInvocationLauncherErrorManager.SAF_17
					);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(
					ProtocolInvocationLauncherErrorManager.SAF_17
				);
			}
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
			ProtocolInvocationLauncherErrorManager.showError(
				ProtocolInvocationLauncherErrorManager.SAF_08
			);
			if (!bySocket){
				throw new SocketOperationException(
					ProtocolInvocationLauncherErrorManager.SAF_08
				);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(
				ProtocolInvocationLauncherErrorManager.SAF_08
			);
		}

		LOGGER.info("Obtenido gestor de almacenes de claves: " + ksm); //$NON-NLS-1$

		final CertFilterManager filterManager = new CertFilterManager(options.getExtraParams());
		final List<CertificateFilter> filters = filterManager.getFilters();
		final boolean mandatoryCertificate = filterManager.isMandatoryCertificate();
		final PrivateKeyEntry pke;

		LOGGER.info("Cargando dialogo de seleccion de certificados..."); //$NON-NLS-1$

		try {
			ServiceInvocationManager.focusApplication();
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
			LOGGER.severe("Operacion cancelada por el usuario" + e); //$NON-NLS-1$
			if (!bySocket){
				throw new SocketOperationException(getResultCancel());
			}
			return getResultCancel();
		}
		catch(final AOCertificatesNotFoundException e) {
			LOGGER.severe("No hay certificados validos en el almacen: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_19);
			if (!bySocket){
				throw new SocketOperationException(
					ProtocolInvocationLauncherErrorManager.SAF_19
				);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(
				ProtocolInvocationLauncherErrorManager.SAF_19
			);
		}
		catch (final Exception e) {
			LOGGER.severe("Error al mostrar el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_08);
			if (!bySocket){
				throw new SocketOperationException(
					ProtocolInvocationLauncherErrorManager.SAF_08
				);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(
				ProtocolInvocationLauncherErrorManager.SAF_08
			);
		}

		LOGGER.info("Iniciando operacion de firma..."); //$NON-NLS-1$


		// XXX: Codigo de soporte de firmas XAdES explicitas (Eliminar cuando se abandone el soporte de XAdES explicitas)
		if (isXadesExplicitConfigurated(options.getSignatureFormat(), options.getExtraParams())) {
			LOGGER.warning(
				"Se ha pedido una firma XAdES explicita, este formato dejara de soportarse en proximas versiones" //$NON-NLS-1$
			);
			try {
				options.setData(MessageDigest.getInstance("SHA1").digest(options.getData())); //$NON-NLS-1$
				options.getExtraParams().setProperty("mimeType", "hash/sha1"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch (final Exception e) {
				LOGGER.warning("Error al generar la huella digital de los datos para firmar como 'XAdES explicit', " //$NON-NLS-1$
					+ "se realizara una firma XAdES corriente: " + e); //$NON-NLS-1$
			}
		}

		final byte[] sign;
		switch(options.getOperation()) {
			case SIGN:
				try {
					sign = signer.sign(
						options.getData(),
						options.getSignatureAlgorithm(),
						pke.getPrivateKey(),
						pke.getCertificateChain(),
						options.getExtraParams()
					);
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error al realizar la operacion firma: " + e, e); //$NON-NLS-1$
					ProtocolInvocationLauncherErrorManager.showError(
						ProtocolInvocationLauncherErrorManager.SAF_09
					);
					if (!bySocket){
						throw new SocketOperationException(
							ProtocolInvocationLauncherErrorManager.SAF_09
						);
					}
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(
						ProtocolInvocationLauncherErrorManager.SAF_09
					);
				}
				break;
			case COSIGN:
				try {
					sign = signer.cosign(
						options.getData(),
						options.getSignatureAlgorithm(),
						pke.getPrivateKey(),
						pke.getCertificateChain(),
						options.getExtraParams()
					);
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error al realizar la operacion cofirma: " + e, e); //$NON-NLS-1$
					ProtocolInvocationLauncherErrorManager.showError(
						ProtocolInvocationLauncherErrorManager.SAF_09
					);
					if (!bySocket){
						throw new SocketOperationException(
							ProtocolInvocationLauncherErrorManager.SAF_09
						);
					}
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(
						ProtocolInvocationLauncherErrorManager.SAF_09
					);
				}
				break;
			case COUNTERSIGN:
				try {
					sign = signer.countersign(
						options.getData(),
						options.getSignatureAlgorithm(),
						"tree".equalsIgnoreCase(options.getExtraParams().getProperty("target")) ? //$NON-NLS-1$ //$NON-NLS-2$
							CounterSignTarget.TREE :
								CounterSignTarget.LEAFS,
						null, // Targets
						pke.getPrivateKey(),
						pke.getCertificateChain(),
						options.getExtraParams()
					);
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error al realizar la contrafirma" + e, e); //$NON-NLS-1$
					ProtocolInvocationLauncherErrorManager.showError(
						ProtocolInvocationLauncherErrorManager.SAF_09
					);
					if (!bySocket){
						throw new SocketOperationException(
							ProtocolInvocationLauncherErrorManager.SAF_09
						);
					}
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(
						ProtocolInvocationLauncherErrorManager.SAF_09
					);
				}
				break;
			default:
				LOGGER.severe("Error al realizar la operacion firma"); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(
					ProtocolInvocationLauncherErrorManager.SAF_04
				);
				if (!bySocket){
					throw new SocketOperationException(
						ProtocolInvocationLauncherErrorManager.SAF_04
					);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(
					ProtocolInvocationLauncherErrorManager.SAF_04
				);
		}

		// Damos la opcion de guardar la firma generada
		try {
			AOUIFactory.getSaveDataToFile(
					sign,
					ProtocolMessages.getString("ProtocolLauncher.31"), //$NON-NLS-1$
					null,
					getFilename(options, selectedFilename, signer),
					null,
					null,
					null
					);
		}
		catch (final Exception e) {
			LOGGER.warning("Error en el guardado de datos. Devolvemos la firma: " + e); //$NON-NLS-1$
		}

		// Concatenamos el certificado utilizado para firmar y la firma con un separador
		// para que la pagina pueda recuperar ambos
		byte[] certEncoded;
		try {
			certEncoded = pke.getCertificateChain()[0].getEncoded();
		}
		catch (final CertificateEncodingException e) {
			LOGGER.severe("Error en la decodificacion del certificado de firma: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_18);
			if (!bySocket){
				throw new SocketOperationException(
					ProtocolInvocationLauncherErrorManager.SAF_18
				);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(
				ProtocolInvocationLauncherErrorManager.SAF_18
			);
		}

		// Si tenemos clave de cifrado, ciframos el certificado y la firma
		final StringBuilder dataToSend = new StringBuilder();

		if (options.getDesKey() != null) {
			try {
				// El CipherData devuelve los datos directamente en Base64
				dataToSend.append(CypherDataManager.cipherData(certEncoded, options.getDesKey()));
				dataToSend.append(CERT_SIGNATURE_SEPARATOR);
				dataToSend.append(CypherDataManager.cipherData(sign, options.getDesKey()));
			}
			catch (final Exception e) {
				LOGGER.severe("Error en el cifrado de los datos a enviar: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_12);
				if (!bySocket){
					throw new SocketOperationException(
						ProtocolInvocationLauncherErrorManager.SAF_12
					);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(
					ProtocolInvocationLauncherErrorManager.SAF_12
				);
			}
		}
		else {
			LOGGER.warning(
				"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
			dataToSend.append(Base64.encode(certEncoded, true));
			dataToSend.append(CERT_SIGNATURE_SEPARATOR);
			// Se hace una doble codigicacion Base64, una de los datos y otras del cifrado, que si bien este ultimo
			// no se realiza, si se mantiene la codificacion
			dataToSend.append(Base64.encode(sign, true));
		}

		if (!bySocket) {
			// Enviamos la firma cifrada al servicio remoto de intercambio
			try {
				sendData(dataToSend, options);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al enviar los datos al servidor: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_11);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(
					ProtocolInvocationLauncherErrorManager.SAF_11
				);
			}
		}
		else {
			LOGGER.info(
				"Se omite el envio por red de los datos resultantes por no haberse proporcionado una URL de destino" //$NON-NLS-1$
			);
		}

		return dataToSend.toString();
	}

	/**
	 * Genera un nombre de fichero.
	 * @param options Opciones proporcionadas en la operaci&oacute;n.
	 * @param filename Nombre del fichero firmado ({@code null} si no es conocido).
	 * @param signer Manejador utilizado para la firma-
	 * @return Nombre de fichero por defecto.
	 */
	private static String getFilename(final UrlParametersToSignAndSave options, final String filename, final AOSigner signer) {

		if (options.getFileName() != null) {
			return options.getFileName();
		}

		String name;
		if (filename != null) {
			final int dotPos = filename.lastIndexOf('.');
			if (dotPos > 0) {
				name = filename.substring(0, dotPos);
			}
			else {
				name = filename;
			}
		}
		else {
			name = ProtocolMessages.getString("ProtocolLauncher.30"); //$NON-NLS-1$
		}

		return signer.getSignedName(name, null);
	}

	public static void sendErrorToServer(final String data, final UrlParametersToSignAndSave options){
		try {
			sendData(new StringBuilder().append(data), options);
		} catch (final IOException e1) {
			LOGGER.severe("Error al enviar los datos del error en la operacion firma al servidor" + e1); //$NON-NLS-1$
		}
	}

	private static void sendData(final StringBuilder data, final UrlParametersToSignAndSave options) throws IOException {

		final StringBuffer url = new StringBuffer(options.getStorageServletUrl().toString());
		url.append("?op=").append(METHOD_OP_PUT); //$NON-NLS-1$
		url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
		url.append("&id=").append(options.getId()); //$NON-NLS-1$
		url.append("&dat=").append(data.toString()); //$NON-NLS-1$

		// Llamamos al servicio para guardar los datos
		final byte[] result = UrlHttpManagerFactory.getInstalledManager().readUrl(url.toString(), UrlHttpMethod.POST);

		LOGGER.info("Resultado: " + new String(result)); //$NON-NLS-1$
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}

	/** Identifica cuando se ha configurado una firma con el formato XAdES
	 * y la propiedad {@code mode} con el valor {@code explicit}. Esta no es una firma correcta,
	 * pero por compatibilidad con los tipos de firmas del Applet pesado se ha incluido aqu&iacute;.
	 * @param format Formato declarado para la firma.
	 * @param config Par&aacute;metros adicionales declarados para la firma.
	 * @return {@code true} si se configura una firma <i>XAdES explicit</i>, {@code false} en caso contrario.
	 * @deprecated Uso temporal hasta que se elimine el soporte de firmas XAdES expl&iacute;citas. */
	@Deprecated
	private static boolean isXadesExplicitConfigurated(final String format, final Properties config) {
		return format != null && format.toLowerCase().startsWith("xades") && config != null && //$NON-NLS-1$
				AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(config.getProperty("mode")) //$NON-NLS-1$
			;
	}
}
