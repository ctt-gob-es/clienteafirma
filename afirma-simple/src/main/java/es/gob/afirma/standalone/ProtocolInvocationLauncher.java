package es.gob.afirma.standalone;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.util.Arrays;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.List;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JFileChooser;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.protocol.ParameterLocalAccessRequestedException;
import es.gob.afirma.core.misc.protocol.ParameterNeedsUpdatedVersionException;
import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParser;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
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
import es.gob.afirma.standalone.crypto.CypherDataManager;
import es.gob.afirma.standalone.ui.MainMenu;

/** Gestiona la ejecuci&oacute;n del Cliente Afirma en una invocaci&oacute;n
 * por protocolo y bajo un entorno compatible <code>Swing</code>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ProtocolInvocationLauncher {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final boolean HEADLESS = Boolean.getBoolean(
		"es.gob.afirma.protocolinvocation.HeadLess" //$NON-NLS-1$
	);

	private static final char CERT_SIGNATURE_SEPARATOR = '|';

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
	private static final String SAF_15 = "SAF_15"; //$NON-NLS-1$
	private static final String SAF_16 = "SAF_16"; //$NON-NLS-1$
	private static final String SAF_17 = "SAF_17"; //$NON-NLS-1$
	private static final String SAF_18 = "SAF_18"; //$NON-NLS-1$
	private static final String SAF_19 = "SAF_19"; //$NON-NLS-1$

	private static final Dictionary<String, String> ERRORS = new Hashtable<>();
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
		ERRORS.put(SAF_15, ProtocolMessages.getString("ProtocolLauncher.15")); //$NON-NLS-1$
		ERRORS.put(SAF_16, ProtocolMessages.getString("ProtocolLauncher.16")); //$NON-NLS-1$
		ERRORS.put(SAF_17, ProtocolMessages.getString("ProtocolLauncher.17")); //$NON-NLS-1$
		ERRORS.put(SAF_18, ProtocolMessages.getString("ProtocolLauncher.18")); //$NON-NLS-1$
		ERRORS.put(SAF_19, ProtocolMessages.getString("ProtocolLauncher.19")); //$NON-NLS-1$
	}

	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$

	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static final String RESULT_OK = "OK"; //$NON-NLS-1$
	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final String FORMAT_AUTO = "auto"; //$NON-NLS-1$

	/** Lanza la aplicaci&oacute;n y realiza las acciones indicadas en la URL.
	 * @param urlString URL de invocaci&oacute;n por protocolo.
	 * @return Resultado de la operaci&oacute;n. */
	public static String launch(final String urlString)  {

	    // En OS X sobrecargamos el "Acerca de..." del sistema operativo, que tambien
	    // aparece en la invocacion por protocolo
	    if (Platform.OS.MACOSX.equals(Platform.getOS())) {
	    	com.apple.eawt.Application.getApplication().setAboutHandler(
                 new com.apple.eawt.AboutHandler() {
                     @Override
                     public void handleAbout(final com.apple.eawt.AppEvent.AboutEvent ae) {
                         MainMenu.showAbout(null);
                     }
                 }
            );
	    }

		if (urlString == null) {
			LOGGER.severe("No se ha proporcionado una URL para la invocacion"); //$NON-NLS-1$
			showError(SAF_01);
			return null;
		}
		if (!urlString.startsWith("afirma://")) { //$NON-NLS-1$
			LOGGER.severe("La URL de invocacion no comienza por 'afirma://'"); //$NON-NLS-1$
			showError(SAF_02);
			return SAF_02;
		}
		if (urlString.startsWith("afirma://service?") || urlString.startsWith("afirma://service/?")) { //$NON-NLS-1$ //$NON-NLS-2$
			LOGGER.info("Se inicia la invocacion por servicio"); //$NON-NLS-1$
			ServiceInvocationManager.startService(urlString);
			return RESULT_OK;
		}
		else if (urlString.startsWith("afirma://save?") || urlString.startsWith("afirma://save/?")) { //$NON-NLS-1$ //$NON-NLS-2$
			LOGGER.info("Se invoca a la aplicacion para el guardado de datos"); //$NON-NLS-1$

			try {
				UrlParametersToSave params = ProtocolInvocationUriParser.getParametersToSave(urlString);

				// Si se indica un identificador de fichero, es que la configuracion se tiene que
				// descargar desde el servidor intermedio
				if (params.getFileId() != null) {

					final StringBuilder dataUrl = new StringBuilder(params.getRetrieveServletUrl().toString()).
							append("?").append("op=get&v=1_0&id=").append(params.getFileId()); //$NON-NLS-1$ //$NON-NLS-2$
					final byte[] recoveredData =
							UrlHttpManagerFactory.getInstalledManager().readUrlByPost(dataUrl.toString());

					// Si los datos recibidos representan un error, detenemos la ejecucion
					if (recoveredData.length > 8 && new String(Arrays.copyOf(recoveredData, 8)).toLowerCase().startsWith("err-")) { //$NON-NLS-1$
						LOGGER.severe("Error al recuperar los datos del servidor intermedio: " + new String(recoveredData)); //$NON-NLS-1$
						showError(SAF_16);
						return SAF_16;
					}

					// Si no ha ocurrido un error, debemos haber recibido los datos cifrados
					byte[] xmlData;
					try {
						xmlData = CypherDataManager.decipherData(recoveredData, params.getDesKey());
					}
					catch (final Exception e) {
						LOGGER.severe("Error en el descifrado de los datos: " + e); //$NON-NLS-1$
						showError(SAF_15);
						return SAF_15;
					}

					params = ProtocolInvocationUriParser.getParametersToSave(xmlData);
				}
				return processSave(params);
			}
			catch(final ParameterNeedsUpdatedVersionException e) {
				LOGGER.severe("Se necesita una version mas moderna de AutoFirma para procesar la peticion: " + e); //$NON-NLS-1$
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
			LOGGER.info("Se invoca a la aplicacion para realizar una operacion de firma/multifirma"); //$NON-NLS-1$

			try {
				UrlParametersToSign params = ProtocolInvocationUriParser.getParametersToSign(urlString);

				// Si se indica un identificador de fichero, es que la configuracion se tiene que
				// descargar desde el servidor intermedio
				if (params.getFileId() != null) {

					final StringBuilder dataUrl = new StringBuilder(params.getRetrieveServletUrl().toString()).
							append("?").append("op=get&v=1_0&id=").append(params.getFileId()); //$NON-NLS-1$ //$NON-NLS-2$
					final byte[] recoveredData =
							UrlHttpManagerFactory.getInstalledManager().readUrlByPost(dataUrl.toString());

					// Si los datos recibidos representan un error, detenemos la ejecucion
					if (recoveredData.length > 8 && new String(Arrays.copyOf(recoveredData, 8)).toLowerCase().startsWith("err-")) { //$NON-NLS-1$
						LOGGER.severe("Error al recuperar los datos del servidor intermedio: " + new String(recoveredData)); //$NON-NLS-1$
						showError(SAF_16);
						return SAF_16;
					}

					// Si no ha ocurrido un error, debemos haber recibido los datos cifrados
					byte[] xmlData;
					try {
						xmlData = CypherDataManager.decipherData(recoveredData, params.getDesKey());
					}
					catch (final Exception e) {
						LOGGER.severe("Error en el descifrado de los datos: " + e); //$NON-NLS-1$
						showError(SAF_15);
						return SAF_15;
					}

					params = ProtocolInvocationUriParser.getParametersToSign(xmlData);
				}

				return processSign(params);
			}
			catch(final ParameterNeedsUpdatedVersionException e) {
				LOGGER.severe("Se necesita una version mas moderna de AutoFirma para procesar la peticion: " + e); //$NON-NLS-1$
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
			LOGGER.severe("No se ha identificado el motivo de la invocacion de la aplicacion"); //$NON-NLS-1$
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

		// En caso de que no se haya solicitado una operacion de multifirma con el formato AUTO
		// configuramos el servidor en base al nombre de formato
		AOSigner signer = null;
		if (!FORMAT_AUTO.equalsIgnoreCase(options.getSignatureFormat()) ||
				options.getOperation() != UrlParametersToSign.OP_COSIGN &&
				options.getOperation() != UrlParametersToSign.OP_COUNTERSIGN) {
			signer = AOSignerFactory.getSigner(options.getSignatureFormat());
			if (signer == null) {
				LOGGER.severe("No hay un firmador configurado para el formato: " + options.getSignatureFormat()); //$NON-NLS-1$
				showError(SAF_06);
				return SAF_06;
			}
		}

		final AOKeyStore aoks = AOKeyStore.getKeyStore(options.getDefaultKeyStore());
		if (aoks == null) {
			LOGGER.severe("No hay un KeyStore con el nombre: " + options.getDefaultKeyStore()); //$NON-NLS-1$
			showError(SAF_07);
			return SAF_07;
		}

		// Si no hay datos a firmar se los pedimos al usuario
		if (options.getData() == null) {

			final String dialogTilte = options.getOperation() == UrlParametersToSign.OP_SIGN ?
					ProtocolMessages.getString("ProtocolLauncher.25") : //$NON-NLS-1$
						ProtocolMessages.getString("ProtocolLauncher.26"); //$NON-NLS-1$

			final File selectedDataFile;
			try {
				selectedDataFile = AOUIFactory.getLoadFiles(
						dialogTilte,
						new JFileChooser().getFileSystemView().getDefaultDirectory().toString(),
						null,
						null,
						ProtocolMessages.getString("ProtocolLauncher.27"), //$NON-NLS-1$
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
				final byte[] data;
				try ( final InputStream fis = new FileInputStream(selectedDataFile); ) {
					data = AOUtil.getDataFromInputStream(fis);
				}
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

		// En caso de haber programado el formato "AUTO", se selecciona el firmador a partir
		// de los datos (firma) proporcionados
		if (signer == null) {
			try {
				signer = AOSignerFactory.getSigner(options.getData());
			} catch (final IOException e) {
				LOGGER.severe("Los datos proporcionados no se reconocen como una firma valida"); //$NON-NLS-1$
				showError(SAF_17);
				return SAF_17;
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

		final CertFilterManager filterManager = new CertFilterManager(options.getExtraParams());
		final List<CertificateFilter> filters = filterManager.getFilters();
		final boolean mandatoryCertificate = filterManager.isMandatoryCertificate();
		final PrivateKeyEntry pke;
		try {
			final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, true, true, true, filters, mandatoryCertificate);
			dialog.show();
			pke = ksm.getKeyEntry(dialog.getSelectedAlias(), ksm.getType().getCertificatePasswordCallback(null));
		}
		catch (final AOCancelledOperationException e) {
			return RESULT_CANCEL;
		}
		catch(final AOCertificatesNotFoundException e) {
			LOGGER.severe("No hay certificados validos en el almacen: " + e); //$NON-NLS-1$
			showError(SAF_19);
			return SAF_19;
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


		// Concatenamos el certificado utilizado para firmar y la firma con un separador
		// para que la pagina pueda recuperar ambos
		byte[] certEncoded;
		try {
			certEncoded = pke.getCertificateChain()[0].getEncoded();
		} catch (final CertificateEncodingException e) {
			LOGGER.severe("Error en la decodificacion del certificado de firma: " + e); //$NON-NLS-1$
			showError(SAF_18);
			return SAF_18;
		}

		// Si tenemos clave de cifrado, ciframos el certificado y la firma
		final StringBuilder dataToSend = new StringBuilder();
		if (options.getDesKey() != null) {
			try {
				dataToSend.append(CypherDataManager.cipherData(certEncoded, options.getDesKey()));
				dataToSend.append(CERT_SIGNATURE_SEPARATOR);
				dataToSend.append(CypherDataManager.cipherData(sign, options.getDesKey()));
			}
			catch (final Exception e) {
				LOGGER.severe("Error en el cifrado de los datos a enviar: " + e); //$NON-NLS-1$
				showError(SAF_12);
				return SAF_12;
			}
		}
		else {
			LOGGER.warning(
				"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
			dataToSend.append(Base64.encode(certEncoded, true));
			dataToSend.append(CERT_SIGNATURE_SEPARATOR);
			dataToSend.append(Base64.encode(sign, true));
		}

		if (options.getStorageServletUrl() != null) {
			// Enviamos la firma cifrada al servicio remoto de intercambio
			try {
				sendData(dataToSend, options);
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

		return dataToSend.toString();
	}

	private static void sendData(final StringBuilder data, final UrlParametersToSign options) throws IOException {

		final StringBuffer url = new StringBuffer(options.getStorageServletUrl().toString());
		url.append("?op=").append(METHOD_OP_PUT); //$NON-NLS-1$
		url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
		url.append("&id=").append(options.getId()); //$NON-NLS-1$
		url.append("&dat=").append(data.toString()); //$NON-NLS-1$

		// Llamamos al servicio para guardar los datos
		final byte[] result = UrlHttpManagerFactory.getInstalledManager().readUrlByPost(url.toString());

		LOGGER.info("Resultado: " + new String(result)); //$NON-NLS-1$
	}

	private static String processSave(final UrlParametersToSave  options) {
		try {
			AOUIFactory.getSaveDataToFile(
				options.getData(),
				options.getTitle(),
				null,
				options.getFileName(),
				options.getExtensions() != null ? new String[] { options.getExtensions() } : null,
				options.getFileTypeDescription(),
				null
			);
		}
		catch(final AOCancelledOperationException e) {
			return RESULT_CANCEL;
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el guardado de datos: " + e); //$NON-NLS-1$
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
