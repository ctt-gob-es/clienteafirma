package es.gob.afirma.standalone;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JFileChooser;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.protocol.ParameterLocalAccessRequestedException;
import es.gob.afirma.core.misc.protocol.ParameterNeedsUpdatedVersionException;
import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParser;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.standalone.crypto.CypherDataManager;

/** Gestiona la ejecuci&oacute;n del Cliente Afirma en una invocaci&oacute;n
 * por protocolo y bajo un entorno compatible <code>Swing</code>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ProtocolInvocationLauncher {

	private static final boolean HEADLESS = Boolean.getBoolean(
		"es.gob.afirma.protocolinvocation.HeadLess" //$NON-NLS-1$
	);

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SAF_00 = "SAF_00"; //$NON-NLS-1$
	private static final String SAF_01 = "SAF_01"; //$NON-NLS-1$
	private static final String SAF_02 = "SAF_02"; //$NON-NLS-1$
	private static final String SAF_03 = "SAF_03"; //$NON-NLS-1$
	private static final String SAF_04 = "SAF_04"; //$NON-NLS-1$
	private static final String SAF_05 = "SAF_05"; //$NON-NLS-1$
	private static final String SAF_06 = "SAF_06"; //$NON-NLS-1$
	private static final String SAF_07 = "SAF_07"; //$NON-NLS-1$
	private static final String SAF_08 = "SAF_08"; //$NON-NLS-1$
	private static final String SAF_09 = "SAF_09"; //$NON-NLS-1$
	private static final String SAF_10 = "SAF_10"; //$NON-NLS-1$
	private static final String SAF_11 = "SAF_11"; //$NON-NLS-1$
	private static final String SAF_12 = "SAF_12"; //$NON-NLS-1$
	private static final String SAF_13 = "SAF_13"; //$NON-NLS-1$
	private static final String SAF_14 = "SAF_14"; //$NON-NLS-1$

	private static final Dictionary<String, String> ERRORS = new Hashtable<String, String>();
	static {
		ERRORS.put(SAF_00, ProtocolMessages.getString("ProtocolLauncher.0")); //$NON-NLS-1$
		ERRORS.put(SAF_01, ProtocolMessages.getString("ProtocolLauncher.1")); //$NON-NLS-1$
		ERRORS.put(SAF_02, ProtocolMessages.getString("ProtocolLauncher.2")); //$NON-NLS-1$
		ERRORS.put(SAF_03, ProtocolMessages.getString("ProtocolLauncher.3")); //$NON-NLS-1$
		ERRORS.put(SAF_04, ProtocolMessages.getString("ProtocolLauncher.4")); //$NON-NLS-1$
		ERRORS.put(SAF_05, ProtocolMessages.getString("ProtocolLauncher.5")); //$NON-NLS-1$
		ERRORS.put(SAF_06, ProtocolMessages.getString("ProtocolLauncher.6")); //$NON-NLS-1$
		ERRORS.put(SAF_07, ProtocolMessages.getString("ProtocolLauncher.7")); //$NON-NLS-1$
		ERRORS.put(SAF_08, ProtocolMessages.getString("ProtocolLauncher.8")); //$NON-NLS-1$
		ERRORS.put(SAF_09, ProtocolMessages.getString("ProtocolLauncher.9")); //$NON-NLS-1$
		ERRORS.put(SAF_10, ProtocolMessages.getString("ProtocolLauncher.10")); //$NON-NLS-1$
		ERRORS.put(SAF_11, ProtocolMessages.getString("ProtocolLauncher.11")); //$NON-NLS-1$
		ERRORS.put(SAF_12, ProtocolMessages.getString("ProtocolLauncher.12")); //$NON-NLS-1$
		ERRORS.put(SAF_13, ProtocolMessages.getString("ProtocolLauncher.13")); //$NON-NLS-1$
		ERRORS.put(SAF_14, ProtocolMessages.getString("ProtocolLauncher.14")); //$NON-NLS-1$
	}

	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$

	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static final String RESULT_OK = "OK"; //$NON-NLS-1$
	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	/** Lanza la aplicaci&oacute;n y realiza las acciones indicadas en la URL.
	 * @param urlString URL de invocaci&oacute;n por protocolo.
	 * @return Resultado de la operaci&oacute;n. */
	public static String launch(final String urlString)  {

		if (urlString == null) {
			showError(SAF_01);
			return null;
		}
		if (!urlString.startsWith("afirma://")) { //$NON-NLS-1$
			showError(SAF_02);
			return SAF_02;
		}
		if (urlString.startsWith("afirma://service?") || urlString.startsWith("afirma://service/?")) { //$NON-NLS-1$ //$NON-NLS-2$
			ServiceInvocationManager.startService(urlString);
			return RESULT_OK;
		}
		else if (urlString.startsWith("afirma://save?") || urlString.startsWith("afirma://save/?")) { //$NON-NLS-1$ //$NON-NLS-2$
			try {
				return processSave(ProtocolInvocationUriParser.getParametersToSave(urlString));
			}
			catch(final ParameterNeedsUpdatedVersionException e) {
				LOGGER.severe("Se necesita una version mas moderna de Firma Facil para procesar la peticion: " + e); //$NON-NLS-1$
				showError(SAF_14);
				return SAF_14;
			}
			catch(final ParameterLocalAccessRequestedException e) {
				LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
				showError(SAF_13);
				return SAF_13;
			}
			catch (final Exception e) {
				LOGGER.severe("Error en los parametros de guardado: " + e); //$NON-NLS-1$
				showError(SAF_03);
				return SAF_03;
			}
		}
		else if (urlString.startsWith("afirma://sign?")   || urlString.startsWith("afirma://sign/?") || //$NON-NLS-1$ //$NON-NLS-2$
				 urlString.startsWith("afirma://cosign?") || urlString.startsWith("afirma://cosign/?") || //$NON-NLS-1$ //$NON-NLS-2$
				 urlString.startsWith("afirma://countersign?") || urlString.startsWith("afirma://countersign/?") //$NON-NLS-1$ //$NON-NLS-2$
		) {
			try {
				return processSign(ProtocolInvocationUriParser.getParametersToSign(urlString));
			}
			catch(final ParameterNeedsUpdatedVersionException e) {
				LOGGER.severe("Se necesita una version mas moderna de Firma Facil para procesar la peticion: " + e); //$NON-NLS-1$
				showError(SAF_14);
				return SAF_14;
			}
			catch(final ParameterLocalAccessRequestedException e) {
				LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
				showError(SAF_13);
				return SAF_13;
			}
			catch (final Exception e) {
				LOGGER.severe("Error en los parametros de firma: " + e); //$NON-NLS-1$
				showError(SAF_03);
				return SAF_03;
			}
		}
		else {
			showError(SAF_04);
			return SAF_04;
		}

	}

	private static String processSign(final UrlParametersToSign options) {
		if (options == null) {
			LOGGER.severe("Las opciones de firma son nulas"); //$NON-NLS-1$
			showError(SAF_01);
			return SAF_01;
		}

		final AOSigner signer = AOSignerFactory.getSigner(options.getSignatureFormat());
		if (signer == null) {
			LOGGER.severe("No hay un firmador configurado para el formato: " + options.getSignatureFormat()); //$NON-NLS-1$
			showError(SAF_06);
			return SAF_06;
		}

		final AOKeyStore aoks = AOKeyStore.getKeyStore(options.getDefaultKeyStore());
		if (aoks == null) {
			LOGGER.severe("No hay un KeyStore con el nombre: " + options.getDefaultKeyStore()); //$NON-NLS-1$
			showError(SAF_07);
			return SAF_07;
		}

		// Si no hay datos a firmar se los pedimos al usuario
		if (options.getData() == null) {
			final File selectedDataFile;
			try {
				selectedDataFile = AOUIFactory.getLoadFiles(
					ProtocolMessages.getString("ProtocolLauncher.15"), //$NON-NLS-1$
					new JFileChooser().getFileSystemView().getDefaultDirectory().toString(),
					null,
					null,
					ProtocolMessages.getString("ProtocolLauncher.16"), //$NON-NLS-1$
					false,
					false,
					null,
					null
				)[0];
			}
			catch(final AOCancelledOperationException e) {
				LOGGER.info("carga de datos de firma cancelada por el usuario: " + e); //$NON-NLS-1$
				return RESULT_CANCEL;
			}

			try {
				final InputStream fis = new FileInputStream(selectedDataFile);
				final byte[] data = AOUtil.getDataFromInputStream(fis);
				fis.close();
				if (data == null) {
					throw new IOException("La lectura de datos para firmar ha devuelto un nulo"); //$NON-NLS-1$
				}
				options.setData(data);
			}
			catch(final Exception e) {
				LOGGER.severe("Error en la lectura de los datos a firmar: " + e); //$NON-NLS-1$
				showError(SAF_00);
				return SAF_00;
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
			showError(SAF_08);
			return SAF_08;
		}
		final PrivateKeyEntry pke;
		try {
			final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, true, false, true);
			dialog.show();
			pke = ksm.getKeyEntry(dialog.getSelectedAlias(), ksm.getType().getCertificatePasswordCallback(null));
		}
		catch (final AOCancelledOperationException e) {
			return RESULT_CANCEL;
		}
		catch (final Exception e) {
			LOGGER.severe("Error al mostrar el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
			showError(SAF_08);
			return SAF_08;
		}
		final byte[] sign;
		switch(options.getOperation()) {
			case UrlParametersToSign.OP_SIGN:
				try {
					sign = signer.sign(
						options.getData(),
						options.getSignatureAlgorithm(),
						pke.getPrivateKey(),
						pke.getCertificateChain(),
						options.getExtraParams()
					);
				}
				catch (final Exception e2) {
					LOGGER.severe("Error en el proceso de firma: " + e2); //$NON-NLS-1$
					showError(SAF_09);
					return SAF_09;
				}
				break;
			case UrlParametersToSign.OP_COSIGN:
				try {
					sign = signer.cosign(
						options.getData(),
						options.getSignatureAlgorithm(),
						pke.getPrivateKey(),
						pke.getCertificateChain(),
						options.getExtraParams()
					);
				}
				catch (final Exception e1) {
					LOGGER.severe("Error en el proceso de cofirma: " + e1); //$NON-NLS-1$
					showError(SAF_09);
					return SAF_09;
				}
				break;
			case UrlParametersToSign.OP_COUNTERSIGN:
				try {
					sign = signer.countersign(
						options.getData(),
						options.getSignatureAlgorithm(),
						"tree".equalsIgnoreCase(options.getExtraParams().getProperty("target")) ? CounterSignTarget.TREE : CounterSignTarget.LEAFS, //$NON-NLS-1$ //$NON-NLS-2$
						null, // Targets
						pke.getPrivateKey(),
						pke.getCertificateChain(),
						options.getExtraParams()
					);
				}
				catch (final Exception e) {
					LOGGER.severe("Error en el proceso de contrafirma: " + e); //$NON-NLS-1$
					showError(SAF_09);
					return SAF_09;
				}
				break;
			default:
				showError(SAF_04);
				return SAF_04;
		}

		// Ciframos la firmar resultante
		final String cipheredDataB64;
		if (options.getDesKey() != null) {
			try {
				cipheredDataB64 = CypherDataManager.cipherData(sign, options.getDesKey());
			}
			catch (final Exception e) {
				LOGGER.severe("Error en el cifrado de la firma: " + e); //$NON-NLS-1$
				showError(SAF_12);
				return SAF_12;
			}
		}
		else {
			LOGGER.warning(
				"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
			cipheredDataB64 = Base64.encode(sign);
		}

		if (options.getStorageServletUrl() != null) {
			// Enviamos la firma cifrada al servicio remoto de intercambio
			try {
				sendData(cipheredDataB64, options);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al enviar los datos al servidor: " + e); //$NON-NLS-1$
				showError(SAF_11);
				return SAF_11;
			}
		}
		else {
			LOGGER.info(
				"Se omite el envio por red de los datos resultantes por no haberse proporcionado una URL de destino" //$NON-NLS-1$
			);
		}

		return cipheredDataB64;

	}

	private static void sendData(final String dataB64, final UrlParametersToSign options) throws IOException {

		final StringBuffer url = new StringBuffer(options.getStorageServletUrl().toString());
		url.append("?op=").append(METHOD_OP_PUT); //$NON-NLS-1$
		url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
		url.append("&id=").append(options.getId()); //$NON-NLS-1$
		url.append("&dat=").append(dataB64); //$NON-NLS-1$

		// Llamamos al servicio para guardar los datos
		final byte[] result = UrlHttpManagerFactory.getInstalledManager().readUrlByPost(url.toString());

		LOGGER.info("Resultado: " + new String(result)); //$NON-NLS-1$
	}

	private static String processSave(final UrlParametersToSave  options) {
		try {
			AOUIFactory.getSaveDataToFile(
				options.getData(),
				options.getTitle(),
				System.getProperty("user.dir"), //$NON-NLS-1$
				options.getFileName(),
				options.getExtensions() != null ? options.getExtensions().split(",") : null, //$NON-NLS-1$
				options.getFileTypeDescription(),
				null
			);
		}
		catch(final AOCancelledOperationException e) {
			return RESULT_CANCEL;
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el salvado de datos: " + e); //$NON-NLS-1$
			showError(SAF_05);
			return SAF_05;
		}
		return RESULT_OK;
	}

	private static void showError(final String code) {
		final String desc = ProtocolMessages.getString("ProtocolLauncher.28") + "\n(" + code + ": " + ERRORS.get(code) + ")";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		if (!HEADLESS) {
			AOUIFactory.showErrorMessage(
				null,
				desc,
				ProtocolMessages.getString("ProtocolLauncher.29"), //$NON-NLS-1$
				AOUIFactory.ERROR_MESSAGE
			);
		}
		LOGGER.severe(desc);
		System.exit(Integer.parseInt("-" + code.replace("SAF_", ""))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

}
