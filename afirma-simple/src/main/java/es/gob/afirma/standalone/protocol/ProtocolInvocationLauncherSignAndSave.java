/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.cert.CertificateEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.RuntimeConfigNeededException.RequestType;
import es.gob.afirma.core.RuntimePasswordNeededException;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.LockedKeyStoreException;
import es.gob.afirma.core.keystores.PinException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.UrlParametersToSignAndSave;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;
import es.gob.afirma.core.signers.OptionalDataInterface;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.keystores.filters.EncodedCertificateFilter;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.InvalidPdfException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.padestri.client.AOPDFTriPhaseSigner;
import es.gob.afirma.signers.pkcs7.ContainsNoDataException;
import es.gob.afirma.signers.xades.EFacturaAlreadySignedException;
import es.gob.afirma.signers.xades.InvalidEFacturaDataException;
import es.gob.afirma.signers.xml.InvalidXMLException;
import es.gob.afirma.signvalidation.InvalidSignatureException;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.EncryptingException;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginControlledException;
import es.gob.afirma.standalone.plugins.PluginInfo;
import es.gob.afirma.standalone.plugins.SignDataProcessor;
import es.gob.afirma.standalone.plugins.SignOperation;
import es.gob.afirma.standalone.plugins.SignOperation.Operation;
import es.gob.afirma.standalone.plugins.SignResult;
import es.gob.afirma.standalone.plugins.manager.PermissionChecker;
import es.gob.afirma.standalone.plugins.manager.PluginException;
import es.gob.afirma.standalone.so.macos.MacUtils;
import es.gob.afirma.standalone.ui.DataDebugDialog;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog.SignPdfDialogListener;

final class ProtocolInvocationLauncherSignAndSave {

	public static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static boolean showRubricIsCanceled = false;

	private ProtocolInvocationLauncherSignAndSave() {
		// No instanciable
	}

	/** Procesa una peticion de firma y guardado en invocaci&oacute;n por protocolo y obtiene
	 * la firma junto con una serie de metadatos en forma de cadena.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @return Resultado de la operaci&oacute;n o mensaje de error.
	 * @throws SocketOperationException Si hay errores en la comunicaci&oacute;n por
	 * <i>socket</i> local.
	 */
	static StringBuilder processSign(final UrlParametersToSignAndSave options,
			final int protocolVersion) throws SocketOperationException {
		if (options == null) {
			LOGGER.severe("Las opciones de firma son nulas"); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NULL_URI;
			throw new SocketOperationException(errorCode);
		}

        // Comprobamos si soportamos la version del protocolo indicada
		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(protocolVersion)) {
			LOGGER.severe(String.format(
					"Version de protocolo no soportada (%1d). Version actual: %2d. Hay que actualizar la aplicacion.", //$NON-NLS-1$
					Integer.valueOf(protocolVersion),
					Integer.valueOf(ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.getVersion())));
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE;
			throw new SocketOperationException(errorCode);
		}

        // Comprobamos si se exige una version minima del Cliente
        if (options.getMinimumClientVersion() != null) {
        	final String minimumRequestedVersion = options.getMinimumClientVersion();
        	final Version requestedVersion = new Version(minimumRequestedVersion);
        	if (requestedVersion.greaterThan(SimpleAfirma.getVersion())) {
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_MINIMUM_VERSION_NON_SATISTIED;
				throw new SocketOperationException(errorCode);
        	}
        }

		//TODO: Deshacer cuando se permita la generacion de firmas baseline
		options.getExtraParams().remove("profile");

		// Establecemos los parametros de la operacion
		final SignOperation operation = new SignOperation();
		operation.setData(options.getData());
		operation.setCryptoOperation(Operation.getOperation(options.getOperation()));
		operation.setAlgorithm(options.getSignatureAlgorithm());
		operation.setFormat(options.getSignatureFormat());
		operation.setExtraParams(options.getExtraParams());
		operation.setAnotherParams(options.getAnotherParams());

		// Determinamos que procesador se utilizara para tratar los datos. Este puede ser uno
		// derivado de un plugin que se active ante estos datos o el procesador nativo
		final SignDataProcessor processor = selectProcessor(
				protocolVersion,
				operation);
		processor.setCipherKey(options.getDesKey());

		final boolean needRefresh = false;
		final List<SignOperation> operations = processor.preProcess(operation);
		final boolean isMassiveSign = operations.size() > 1;
		final List<SignResult> results = new ArrayList<>(operations.size());
		for (int i = 0; i < operations.size(); i++) {
			final SignOperation op = operations.get(i);
			try {
				results.add(sign(op, options, isMassiveSign, needRefresh));
			}
			catch (final VisibleSignatureMandatoryException e) {
				LOGGER.log(Level.SEVERE, "No se cumplieron los requisitos para firma visible PDF: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_VISIBLE_SIGNATURE;
				throw new SocketOperationException(errorCode, e);
			}
			catch (final SocketOperationException e) {
				LOGGER.log(Level.SEVERE, "Se identifico un error en una operacion de firma", e); //$NON-NLS-1$
				// Salvo que el procesador indique que se permiten los errores, se relanza para
				// bloquear la ejecucion
				if (!processor.isErrorsAllowed()) {
					throw e;
				}
			}
		}

		StringBuilder dataToSend;
		try {
			dataToSend = processor.postProcess(results, operation);
		}
		catch (final EncryptingException e) {
			LOGGER.log(Level.SEVERE, "Error en el cifrado de los datos a enviar", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_ENCRIPTING_DATA;
			throw new SocketOperationException(errorCode);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error en el postprocesador de los datos a enviar", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_POSTPROCESSING_DATA;
			throw new SocketOperationException(errorCode);
		}

		return dataToSend;
	}

	private static SignDataProcessor selectProcessor(final int protocolVersion,
			final SignOperation operation) {

		List<AfirmaPlugin> plugins;
		try {
			plugins = SimpleAfirma.getPluginsManager().getPluginsLoadedList();
		} catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se pudo cargar el listado de plugins", e); //$NON-NLS-1$
			plugins = null;
		}

		if (plugins != null) {
			for (final AfirmaPlugin plugin : plugins) {
				try {
					final PluginInfo pluginInfo = plugin.getInfo();
					if (PermissionChecker.check(pluginInfo, Permission.INLINE_PROCESS)) {
						final SignDataProcessor processor = plugin.getInlineProcessor(protocolVersion);
						if (processor != null && processor.checkTrigger(operation)) {
							return processor;
						}
					}
				} catch (final PluginControlledException e) {
					LOGGER.log(Level.WARNING, "Error al evaluar el uso del plugin " //$NON-NLS-1$
							+ plugin.getClass().getName() + " para su uso en linea", e); //$NON-NLS-1$
				}
			}
		}

		return new NativeSignDataProcessor(protocolVersion);
	}

	private static SignResult sign(final SignOperation signOperation, final UrlParametersToSignAndSave options,
			final boolean isMassiveSign, final boolean needRefresh)
					throws SocketOperationException, VisibleSignatureMandatoryException {

		byte[] data = signOperation.getData();
		String format = signOperation.getFormat();
		final String algorithm = signOperation.getAlgorithm();
		Properties extraParams = signOperation.getExtraParams();
		if (extraParams == null) {
			extraParams = new Properties();
		}
		final Operation cryptoOperation = signOperation.getCryptoOperation();

		// En caso de que no se haya solicitado una operacion de multifirma con
		// el formato AUTO, configuramos el servidor en base al nombre de formato
		AOSigner signer = null;
		if (!AOSignConstants.SIGN_FORMAT_AUTO.equalsIgnoreCase(format)) {
			signer = AOSignerFactory.getSigner(format);
			if (signer == null) {
				LOGGER.severe("No hay un firmador configurado para el formato: " + format); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_FORMAT;
				throw new SocketOperationException(errorCode);
			}
		}

		final String lastSelectedKeyStore = KeyStorePreferencesManager.getLastSelectedKeystore();
		final boolean useDefaultStore = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS);

		// Si hay marcado un almacen como el ultimo seleccionado, lo usamos (este es el caso en el que se llaman
		// varias operaciones de firma dentro de la misma invocacion a la aplicacion)
		AOKeyStore aoks = null;
		if (lastSelectedKeyStore != null && !lastSelectedKeyStore.isEmpty()) {
			aoks = SimpleKeyStoreManager.getLastSelectedKeystore();
		}
		// Si no, si el usuario definio un almacen por defecto para usarlo en las llamadas a la aplicacion, lo usamos
		else if (useDefaultStore) {
			final String defaultStore = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
			if (!PreferencesManager.VALUE_KEYSTORE_DEFAULT.equals(defaultStore)) {
				aoks = SimpleKeyStoreManager.getKeyStore(defaultStore);
			}
		}
		// Si no, si en la llamada se definio el almacen que se debia usar, lo usamos
		else {
			aoks = SimpleKeyStoreManager.getKeyStore(options.getDefaultKeyStore());
		}

		// Si aun no se ha definido el almacen, se usara el por defecto para el sistema operativo
		if (aoks == null) {
			aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(Platform.getOS());
		}

		// Comprobamos si es necesario pedir datos de entrada al usuario
		boolean needRequestData = false;
		if (data == null) {
			if (signer instanceof OptionalDataInterface) {
				needRequestData = ((OptionalDataInterface) signer).needData(signOperation.getCryptoOperation().toString(), extraParams);
			} else {
				needRequestData = true;
			}
		}

		// Si el usuario lo pide (estableciendo una variable de entorno), mostramos los datos que va a firmar
		try {
			if (!needRequestData && Boolean.getBoolean("AFIRMA_SHOW_DATA_TO_SIGN")) { //$NON-NLS-1$
				final DataDebugDialog ddd = new DataDebugDialog(data);
				data = ddd.getData();
				ddd.dispose();
			}
		}
		catch(final Exception e) {
			LOGGER.warning("No se pueden mostrar los datos a firmar: " + e); //$NON-NLS-1$
		}

		// Nombre del fichero firmado. Tomara valor solo si es el usuario quien selecciona
		// el fichero a firmar
		String inputFilename = null;

		// Si se tienen que pedir los datos al usuario, se hace
		if (needRequestData) {
			final String dialogTitle = Operation.SIGN == cryptoOperation
					? ProtocolMessages.getString("ProtocolLauncher.25") //$NON-NLS-1$
					: ProtocolMessages.getString("ProtocolLauncher.26"); //$NON-NLS-1$

			final String fileExts = extraParams.getProperty(AfirmaExtraParams.LOAD_FILE_EXTS);

			final String fileDesc = extraParams.getProperty(AfirmaExtraParams.LOAD_FILE_DESCRIPTION,
					ProtocolMessages.getString("ProtocolLauncher.32")) + //$NON-NLS-1$
				(fileExts == null ? " (*.*)" : String.format(" (*.%1s)", fileExts.replace(",", ",*."))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

			final File selectedDataFile;
			try {
				if (Platform.OS.MACOSX.equals(Platform.getOS())) {
					MacUtils.focusApplication();
				}
				selectedDataFile = AOUIFactory.getLoadFiles(
					dialogTitle,
					extraParams.getProperty(AfirmaExtraParams.LOAD_FILE_CURRENT_DIR), // currentDir
					extraParams.getProperty(AfirmaExtraParams.LOAD_FILE_FILENAME), // fileName
					fileExts != null ? fileExts.split(",") : null, //$NON-NLS-1$
					fileDesc,
					false, // Select dir
					false, // Multiselect
					DesktopUtil.getDefaultDialogsIcon(),
					null //Parent
				)[0];
			} catch (final AOCancelledOperationException e) {
				LOGGER.info("Carga de datos de firma cancelada por el usuario: " + e); //$NON-NLS-1$
				throw new SocketOperationException(RESULT_CANCEL);
			}

			// Asignamos el nombre del fichero firmado para devolverlo a la aplicacion
			inputFilename = selectedDataFile.getName();

			try {
				try (
					final InputStream fis = new FileInputStream(selectedDataFile);
					final InputStream bis = new BufferedInputStream(fis);
				) {
					data = AOUtil.getDataFromInputStream(bis);
				}
				if (data == null) {
					throw new IOException("La lectura de datos para firmar ha devuelto un nulo"); //$NON-NLS-1$
				}
			} catch (final Exception e) {
				LOGGER.severe("Error en la lectura de los datos a firmar: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_READ_DATA;
				throw new SocketOperationException(errorCode);
			}
		}

		// En no haber fijado aun el firmador significa que se selecciono el formato AUTO y
		// es necesario identificar cual es el que se deberia usar
		if (signer == null) {
			format = ProtocolInvocationLauncherUtil.identifyFormatFromData(data, cryptoOperation);
			if (format == null) {
				LOGGER.severe(
					"Los datos no se corresponden con una firma electronica o no se pudieron analizar"); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNKNOWN_SIGNER;
				throw new SocketOperationException(errorCode);
			}
			signer = AOSignerFactory.getSigner(format);
		}

		// XXX: Codigo de soporte de firmas XAdES explicitas (Eliminar cuando se
		// abandone el soporte de XAdES explicitas)
		if (cryptoOperation == Operation.SIGN && isXadesExplicitConfigurated(format, extraParams)
				&& !format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_XADES_TRI)) {
			LOGGER.warning(
				"Se ha pedido una firma XAdES explicita, este formato dejara de soportarse en proximas versiones" //$NON-NLS-1$
			);
			try {
				data = MessageDigest.getInstance("SHA1").digest(data); //$NON-NLS-1$
				extraParams.setProperty("mimeType", "hash/sha1"); //$NON-NLS-1$ //$NON-NLS-2$
			} catch (final Exception e) {
				LOGGER.warning("Error al generar la huella digital de los datos para firmar como 'XAdES explicit', " //$NON-NLS-1$
					+ "se realizara una firma XAdES corriente: " + e); //$NON-NLS-1$
			}
		}

		// Si se ha pedido comprobar las firmas antes de agregarle la nueva firma, lo hacemos ahora
		if (data != null &&
				Boolean.parseBoolean(extraParams.getProperty(AfirmaExtraParams.CHECK_SIGNATURES))) {
			final SignValider validator = SignValiderFactory.getSignValider(signer);
			SignValidity validity = null;
			if (validator != null) {
				// Establecemos una validacion relajada para que nos informen cuando la firma presenta
				// problemas que requiririan la intervencion del usuario para operar con ella
				validator.setRelaxed(true);
				if (signer instanceof AOPDFSigner || signer instanceof AOPDFTriPhaseSigner) {
					configurePdfSignature(extraParams);
				}

				do {
					try {
						final List<SignValidity> validityList = validator.validate(data, extraParams);
						validity = validityList.get(0);
					} catch (final IOException e) {
						LOGGER.severe("Error al identificar la validez de la firma: " + e); //$NON-NLS-1$
						validity = new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.UNKOWN_ERROR);
					} catch (final RuntimeConfigNeededException e) {
						LOGGER.warning("La validacion de la firma ha revelado una situacion que requiere la intervencion del usuario: " + e); //$NON-NLS-1$

						// Se requiere confirmacion por parte del usuario
						if (Boolean.parseBoolean(extraParams.getProperty(AfirmaExtraParams.HEADLESS))) {
							LOGGER.severe("No se pueden mostrar dialogos para pedir datos del usuario. Se abortara la operacion"); //$NON-NLS-1$
							throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.ERROR_CONFIRMATION_NEEDED, e.getMessage(), e);
						}

						if (e.getRequestType() == RequestType.CONFIRM) {
							final int result = AOUIFactory.showConfirmDialog(null, SimpleAfirmaMessages.getString(e.getRequestorText()),
									SimpleAfirmaMessages.getString("SignPanelSignTask.4"), JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE); //$NON-NLS-1$
							if (result == JOptionPane.YES_OPTION) {
								extraParams.setProperty(e.getParam(), Boolean.TRUE.toString());
							}
							else {
								LOGGER.log(Level.SEVERE, "El usuario ha cancelado la operacion despues de la advertencia: " + SimpleAfirmaMessages.getString(e.getRequestorText())); //$NON-NLS-1$
								throw new SocketOperationException(RESULT_CANCEL);
							}
						}
						// Se requiere ua contrasena por parte del usuario
						else if (e.getRequestType() == RequestType.PASSWORD) {
							char[] p;
							try {
								p = AOUIFactory.getPassword(SimpleAfirmaMessages.getString(e.getRequestorText()), null);
							}
							catch (final AOCancelledOperationException ce) {
								p = null;
							}
							if (p != null) {
								if (e instanceof RuntimePasswordNeededException) {
	            					((RuntimePasswordNeededException) e).configure(extraParams, p);
	            				}
	            				else {
	            					extraParams.setProperty(e.getParam(), new String(p));
	            				}
							}
						}
						else {
							LOGGER.severe("No se puede gestionar la solicitud de datos necesaria para completar la firma"); //$NON-NLS-1$
							throw new SocketOperationException(e.getRequestorText(), e.getMessage(), e);
						}
					}
				} while (validity == null);

				// La comprobacion de la operacion se hace aqui ya que hay formatos que tambien
				// deben comprobar la validadez de las firmas previas para las operaciones de
				// firma (PAdES, OOXML, etc.).
				if (validity.getValidity() == SIGN_DETAIL_TYPE.KO &&
						(cryptoOperation != Operation.SIGN || validity.getError() != VALIDITY_ERROR.NO_SIGN)) {
					LOGGER.severe("La firma indicada no es valida: " + validity); //$NON-NLS-1$
					final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_SIGNATURE;
					throw new SocketOperationException(errorCode);
				}
			}
		}

		// Una vez se tienen todos los parametros necesarios expandimos los extraParams
		// de la operacion para obtener la configuracion final
		try {
			extraParams = ExtraParamsProcessor.expandProperties(
					extraParams,
					data,
					format);
		} catch (final IncompatiblePolicyException e) {
			LOGGER.info("Se ha indicado una politica no compatible: " + e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_POLICY;
			throw new SocketOperationException(errorCode, e);
		}

		final CertFilterManager filterManager = new CertFilterManager(extraParams);

		// Comprobamos si se necesita que el usuario seleccione el area de firma
		// visible.
		try {
			if (isRubricPositionRequired(format, extraParams)) {
				showRubricPositionDialog(data, extraParams, isMassiveSign);
				checkShowRubricDialogIsCalceled(extraParams);
			}
		} catch (final AOCancelledOperationException e) {
			LOGGER.info("El usuario ha cancelado el proceso de firma."); //$NON-NLS-1$
			throw new VisibleSignatureMandatoryException(
					"Es obligatorio mostrar la firma en el documento PDF"); //$NON-NLS-1$
		}

		PrivateKeyEntry pke = null;
		String keyStoreLib = null;

		// Identificamos si hay una clave preseleccionada que debemos usar. Si no, identificamos
		// la biblioteca por defecto que se usara si el almacen lo requiere
		if (options.getSticky() && !options.getResetSticky()
				&& ProtocolInvocationLauncher.getStickyKeyEntry() != null) {
			pke = ProtocolInvocationLauncher.getStickyKeyEntry();
		} else {
			if (useDefaultStore && (AOKeyStore.PKCS12.equals(aoks) || AOKeyStore.PKCS11.equals(aoks))) {
				keyStoreLib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
			} else {
				keyStoreLib = options.getDefaultKeyStoreLib();
			}
		}

		final boolean stickySignatory = options.getSticky();

		final SignOperationResult operationResult = selectCertAndSign(pke, aoks, keyStoreLib, filterManager, stickySignatory,
				data, algorithm, signer, cryptoOperation, extraParams);

		final byte[] signature = operationResult.getResult();
		pke = operationResult.getPke();

		// Si se pidio cachear la referencia a clave, se hace. Si no, se libera la que hubiese
		if (options.getSticky()) {
			ProtocolInvocationLauncher.setStickyKeyEntry(pke);
		} else {
			ProtocolInvocationLauncher.setStickyKeyEntry(null);
		}

		// Damos la opcion de guardar la firma generada
		final String fileExts = options.getExtraParams().getProperty(AfirmaExtraParams.SAVE_FILE_EXTS);

		final String fileDesc = options.getExtraParams().getProperty(AfirmaExtraParams.SAVE_FILE_DESCRIPTION, ProtocolMessages.getString("ProtocolLauncher.30")) +  //$NON-NLS-1$
			(fileExts == null ? " (*.*)" : String.format(" (*.%1s)", fileExts.replace(",", ",*."))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		try {
			AOUIFactory.getSaveDataToFile(
				signature,
				ProtocolMessages.getString("ProtocolLauncher.31"), // Titulo del dialogo //$NON-NLS-1$
				options.getExtraParams().getProperty(AfirmaExtraParams.SAVE_FILE_CURRENT_DIR), // Directorio de guardado
				getFilename(options, inputFilename, signer),
				Collections.singletonList(
					new GenericFileFilter(
						fileExts != null ? fileExts.split(",") : null, // Extensiones de fichero aceptadas //$NON-NLS-1$
						fileDesc // Descripcion del fichero
					)
				),
				null // Padre para la modalidad
			);
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
			throw new SocketOperationException(RESULT_CANCEL);
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el guardado de datos: " + e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_SAVE_DATA;
			throw new SocketOperationException(errorCode);
		}

		// Concatenamos el certificado utilizado para firmar y la firma con un separador
		// para que la pagina pueda recuperar ambos
		final byte[] certEncoded;
		try {
			certEncoded = pke.getCertificateChain()[0].getEncoded();
		} catch (final CertificateEncodingException e) {
			LOGGER.severe("Error en la decodificacion del certificado de firma: " + e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_DECODING_CERTIFICATE;
			throw new SocketOperationException(errorCode);
		}

		final SignResult result = new SignResult();
		result.setSignature(signature);
		result.setCertificate(certEncoded);
		Properties extraData = null;
		if (inputFilename != null) {
			extraData = new Properties();
			extraData.setProperty("filename", inputFilename); //$NON-NLS-1$
		}
		result.setDataFilename(extraData);

		return result;
	}

	private static SignOperationResult selectCertAndSign(final PrivateKeyEntry pkeSelected, final AOKeyStore aoks,
			final String keyStoreLib, final CertFilterManager filterManager, final boolean stickySignatory,
			final byte[] data, final String algorithm, final AOSigner signer,
			final SignOperation.Operation cryptoOperation, final Properties extraParams)
					throws SocketOperationException {

		PrivateKeyEntry pke = pkeSelected;

		if (pkeSelected == null) {
			final PasswordCallback pwc = aoks.getStorePasswordCallback(null);

			LOGGER.info("Obtenido gestor de almacenes de claves: " + aoks); //$NON-NLS-1$
			final AOKeyStoreManager ksm;
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(aoks, // Store
						keyStoreLib, // Lib
						null, // Description
						pwc, // PasswordCallback
						null // Parent
						);
			} catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "Error obteniendo el AOKeyStoreManager", e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_KEYSTORE;
				throw new SocketOperationException(errorCode, e);
			}

			LOGGER.info("Cargando dialogo de seleccion de certificados..."); //$NON-NLS-1$
			try {
				MacUtils.focusApplication();
				String libName = null;
				if (keyStoreLib != null) {
					final File file = new File(keyStoreLib);
					libName = file.getName();
				}
				final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
						ksm,
						null,
						true,
						true, // showExpiredCertificates
						true, // checkValidity
						filterManager.getFilters(),
						filterManager.isMandatoryCertificate(),
						libName);
				dialog.allowOpenExternalStores(filterManager.isExternalStoresOpeningAllowed());
				dialog.show();

				// Obtenemos el almacen del certificado seleccionado (que puede no ser el mismo
				// que se indico originalmente por haberlo cambiado desde el dialogo de
				// seleccion)
				final CertificateContext context = dialog.getSelectedCertificateContext();
				final KeyStoreManager currentKsm = context.getKeyStoreManager();
				pke = currentKsm.getKeyEntry(context.getAlias());
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
				throw new SocketOperationException(RESULT_CANCEL);
			}
			catch (final AOCertificatesNotFoundException e) {
				LOGGER.severe("No hay certificados validos en el almacen: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NO_CERTIFICATES_KEYSTORE;
				throw new SocketOperationException(errorCode);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al mostrar el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_KEYSTORE;
				throw new SocketOperationException(errorCode, e);
			}
		}

		// Seleccionamos el algoritmo de firma
		final String keyType = pke.getPrivateKey().getAlgorithm();
		String signatureAlgorithm;
		try {
			signatureAlgorithm = AOSignConstants.composeSignatureAlgorithmName(algorithm, keyType);
		}
		catch (final Exception e) {
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INCOMPATIBLE_KEY_TYPE;
			throw new SocketOperationException(errorCode, e);
		}

		// Si se pidio cachear la referencia a clave, se hace. Si no, se libera la que hubiese
		ProtocolInvocationLauncher.setStickyKeyEntry(stickySignatory ? pke : null);

		byte[] sign;
		try {
			sign = executeSign(signer, cryptoOperation, data, signatureAlgorithm, pke, extraParams);
		}
		catch (final LockedKeyStoreException e) {
			LOGGER.log(Level.SEVERE, "El almacen de claves esta bloqueado", e); //$NON-NLS-1$

			// En este caso no dejamos prefijado el certificado
			ProtocolInvocationLauncher.setStickyKeyEntry(null);

			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_LOCKED_KEYSTORE;
			throw new SocketOperationException(errorCode, e);
		}
		catch (final PinException e) {
			LOGGER.warning("PIN invalido. Reintentamos la operacion: " + e); //$NON-NLS-1$
			// Forzando el reinicio del almacen y la seleccion automatica de ese certificado
			List<CertificateFilter> filters = null;
			try {
				final byte[] certEncoded = pke.getCertificate().getEncoded();
				final CertificateFilter filter = new EncodedCertificateFilter(Base64.encode(certEncoded));
				filters = Collections.singletonList(filter);
			}
			catch (final Exception ex) {
				filters = null;
			}

			final CertFilterManager newFilterManager = new CertFilterManager(
					filters, filters != null, true);
			return selectCertAndSign(null, aoks, keyStoreLib, newFilterManager, stickySignatory,
					data, signatureAlgorithm, signer, cryptoOperation, extraParams);
		}

		return new SignOperationResult(sign, pke);
	}

	/**
	 * Ejecuta la operaci&oacute;n de firma con la configuraci&oacute;n data.
	 * @param signer Manejador de firma.
	 * @param cryptoOperation Operaci&oacute;n criptogr&aacute;fica de firma.
	 * @param data Datos/firma a firmar o multifirmar.
	 * @param algorithm Algoritmo de firma.
	 * @param pke Referencia a la clave privada de firma.
	 * @param extraParams Par&aacute;metros adicionales para la configuraci&oacute;m del formato de firma.
	 * @return Firma generada.
	 * @throws SocketOperationException Cuando ocurre alg&uacute;n error durante la operaci&oacute;n.
	 * @throws PinException Cuando se usa un PIN incorrecto para el almac&eacute;n de claves.
	 * @throws LockedKeyStoreException Cuando el almac&eacute;n de claves est&aacute; bloqueado.
	 */
	private static byte[] executeSign(final AOSigner signer, final Operation cryptoOperation, final byte[] data, final String algorithm,
			final PrivateKeyEntry pke, final Properties extraParams) throws SocketOperationException, PinException, LockedKeyStoreException {

		byte[] signature;
		try {
			try {
				switch (cryptoOperation) {
				case SIGN:
					signature = signer.sign(
							data,
							algorithm,
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParams
							);
					break;
				case COSIGN:
					signature = signer.cosign(
							data,
							algorithm,
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParams
							);
					break;
				case COUNTERSIGN:
					signature = signer.countersign(
							data,
							algorithm,
							"tree".equalsIgnoreCase(extraParams.getProperty(AfirmaExtraParams.TARGET)) ? //$NON-NLS-1$
									CounterSignTarget.TREE : CounterSignTarget.LEAFS,
									null, // Targets
									pke.getPrivateKey(),
									pke.getCertificateChain(),
									extraParams
							);
					break;
				default:
					LOGGER.severe("Error al realizar la operacion firma"); //$NON-NLS-1$
					final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_OPERATION;
					throw new SocketOperationException(errorCode);
				}
			}
			catch (final AOTriphaseException tex) {
				throw ProtocolInvocationLauncherUtil.getInternalException(tex);
			}
		}
		catch (final SocketOperationException | PinException | LockedKeyStoreException e) {
			throw e;
		}
		catch (final IllegalArgumentException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PARAMS;
			throw new SocketOperationException(errorCode, e);
		}
		catch (final AOTriphaseException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_RECOVER_SERVER_DOCUMENT;
			throw new SocketOperationException(errorCode, e);
		}
		catch (final InvalidPdfException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_PDF;
			throw new SocketOperationException(errorCode);
		}
		catch (final InvalidXMLException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_XML;
			throw new SocketOperationException(errorCode);
		}
		catch (final AOFormatFileException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_DATA;
			throw new SocketOperationException(errorCode);
		}
		catch (final InvalidEFacturaDataException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_FACTURAE;
			throw new SocketOperationException(errorCode);
		}
		catch (final EFacturaAlreadySignedException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_FACE_ALREADY_SIGNED;
			throw new SocketOperationException(errorCode);
		}
		catch (final ContainsNoDataException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_SIGN_WITHOUT_DATA;
			throw new SocketOperationException(errorCode);
		}
		catch (final AOInvalidFormatException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NO_SIGN_DATA;
			throw new SocketOperationException(errorCode);
		}
		catch (final RuntimeConfigNeededException e) {
			LOGGER.warning("No se puede completar la firma sin intervencion del usuario: " + e); //$NON-NLS-1$

			// Se requiere confirmacion por parte del usuario
			if (Boolean.parseBoolean(extraParams.getProperty(AfirmaExtraParams.HEADLESS))) {
				LOGGER.severe("No se pueden mostrar dialogos para pedir datos del usuario. Se abortara la operacion"); //$NON-NLS-1$
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.ERROR_CONFIRMATION_NEEDED, e.getMessage(), e);
			}

			if (e.getRequestType() == RequestType.CONFIRM) {
				final int result = AOUIFactory.showConfirmDialog(null, SimpleAfirmaMessages.getString(e.getRequestorText()),
						SimpleAfirmaMessages.getString("SignPanelSignTask.4"), JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE); //$NON-NLS-1$
				if (result == JOptionPane.YES_OPTION) {
					extraParams.setProperty(e.getParam(), Boolean.TRUE.toString());
					return executeSign(signer, cryptoOperation, data, algorithm, pke, extraParams);
				}
			}
			// Se requiere ua contrasena por parte del usuario
			else if (e.getRequestType() == RequestType.PASSWORD) {
				char[] p;
				try {
					p = AOUIFactory.getPassword(SimpleAfirmaMessages.getString(e.getRequestorText()), null);
				}
				catch (final AOCancelledOperationException ce) {
					p = null;
				}
				if (p != null) {
					if (e instanceof RuntimePasswordNeededException) {
    					((RuntimePasswordNeededException) e).configure(extraParams, p);
    				}
    				else {
    					extraParams.setProperty(e.getParam(), new String(p));
    				}
					return executeSign(signer, cryptoOperation, data, algorithm, pke, extraParams);
				}
			}
			else {
				LOGGER.severe("No se puede gestionar la solicitud de datos necesaria para completar la firma"); //$NON-NLS-1$
				throw new SocketOperationException(e.getRequestorText(), e.getMessage(), e);
			}

			LOGGER.severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
			throw new SocketOperationException(RESULT_CANCEL);
		}
		catch (final UnsupportedOperationException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_OPERATION;
			throw new SocketOperationException(errorCode);
		}
		catch (final InvalidSignatureException e) {
			LOGGER.log(Level.SEVERE, "La firma de entrada no es valida", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_SIGNATURE;
			throw new SocketOperationException(errorCode);
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.log(Level.SEVERE, "Operacion cancelada por el usuario", e); //$NON-NLS-1$
			throw new SocketOperationException(RESULT_CANCEL);
		}
		catch (final AOException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_SIGNATURE_FAILED;
			throw new SocketOperationException(errorCode, e);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_SIGNATURE_FAILED;
			throw new SocketOperationException(errorCode);
		}

		return signature;
	}

	private static void configurePdfSignature(final Properties extraParams) {
		final String allowPdfShadowAttackProp = extraParams.getProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK);
		if (!Boolean.parseBoolean(allowPdfShadowAttackProp) && !extraParams.containsKey(PdfExtraParams.PAGES_TO_CHECK_PSA)) {
			extraParams.setProperty(PdfExtraParams.PAGES_TO_CHECK_PSA, "10"); //$NON-NLS-1$
		}
	}

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
	private static boolean isXadesExplicitConfigurated(final String format, final Properties config) {
		return format != null
				&& format.toLowerCase().startsWith("xades") //$NON-NLS-1$
				&& config != null
				&& AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(config.getProperty(AfirmaExtraParams.MODE));
	}

	/**
	 * M&eacute;todo que comprueba si es necesario solicitar al usuario la
	 * posici&oacute;n de la firma visible. Se mostrar&aacute; el dialogo para
	 * seleccionar la zona donde colocar la firma siempre y cuando se cumplan las
	 * siguientes condiciones: 1) El documento a firmar debe ser un PDF. 2) No puede
	 * ser una firma por lotes. 3) En los par&aacute;metros adicionales debe venir
	 * la bandera "visibleSignature" con los posibles valores <i>want</i> y
	 * <i>optional</i>. 4) En los par&aacute;metros adicionales NO pueden venir los
	 * par&aacute;metros asociados a la posici&oacute;n de la firma ya establecidos,
	 * estos son: signaturePositionOnPageLowerLeftX,
	 * signaturePositionOnPageLowerLeftY, signaturePositionOnPageUpperRightX,
	 * signaturePositionOnPageUpperRightY.
	 *
	 * @param format Formato de firma.
	 * @param extraParams Propiedades de configuraci&oacute;n de firma.
	 * @return <i>true</i> si se debe mostrar el di&aacute;logo para la selecci&oacute;n de
	 *         la zona donde hacer la firma visible y <i>false</i> en caso
	 *         contrario.
	 */
	private static boolean isRubricPositionRequired(final String format, final Properties extraParams) {

		// Comprobamos que la firma sea PAdES.
		final List<String> formatPadesList = Arrays.asList(AOSignConstants.SIGN_FORMAT_PDF,
				AOSignConstants.SIGN_FORMAT_PDF_TRI, AOSignConstants.SIGN_FORMAT_PADES,
				AOSignConstants.SIGN_FORMAT_PADES_TRI, AOSignConstants.PADES_SUBFILTER_BES,
				AOSignConstants.PADES_SUBFILTER_BASIC);
		if (!formatPadesList.contains(format)) {
			return false;
		}

		// Comprobamos que se han incluido los parametros asociados a mostrar la
		// firma.
		if (!extraParams.containsKey(PdfExtraParams.VISIBLE_SIGNATURE)) {
			return false;
		}

		// Comprobamos que la bandera que indica si se debe solicitar la firma visible
		// tiene un valor valido.
		final String visibleSignature = extraParams.get(PdfExtraParams.VISIBLE_SIGNATURE).toString();
		if (!PdfExtraParams.VISIBLE_SIGNATURE_VALUE_WANT.equalsIgnoreCase(visibleSignature)
				&& !PdfExtraParams.VISIBLE_SIGNATURE_VALUE_OPTIONAL.equalsIgnoreCase(visibleSignature)) {
			return false;
		}

		return true;
	}

	/**
	 * M&eacute;todo que muestra el dialogo para seleccionar la posici&oacute;n de
	 * la firma.
	 * @param data Documento que se desea firmar.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n de la firma.
	 * @param isMassiveSign Si tiene valor {@code true} indica que es una firma masiva y {@code false} en caso contrario.
	 */
	private static void showRubricPositionDialog(final byte[] data, final Properties extraParams, final boolean isMassiveSign) {

		final AOPDFSigner signer = new AOPDFSigner();
		final boolean isSign = signer.isSign(data);
		final SignPdfDialogListener listener = new SignPdfListener(extraParams);
		final JDialog dialog = SignPdfDialog.getVisibleSignatureDialog(isSign, isMassiveSign, data, null, true, isCustomAppearance(extraParams),
				false, listener);
		dialog.setVisible(true);
	}

	/**
	 * M&eacute;todo que comprueba si el di&aacute;logo ha sido cancelado y act&uacute;a en
	 * consecuencia.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n de la firma.
	 */
	private static void checkShowRubricDialogIsCalceled(final Properties extraParams) {
		if (showRubricIsCanceled) {
			// Recuperamos el valor del atributo "visibleSignature".
			final String visibleSignature = extraParams.get(PdfExtraParams.VISIBLE_SIGNATURE) != null
					? extraParams.get(PdfExtraParams.VISIBLE_SIGNATURE).toString()
					: null;
			final boolean want = PdfExtraParams.VISIBLE_SIGNATURE_VALUE_WANT.equalsIgnoreCase(visibleSignature)
						? true : false;

			// Comprobamos si se han indicado la lista de atributos del area de firma
			// visible.
			final boolean existsAreaAttributes = extraParams.containsKey(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX)
					&& extraParams.containsKey(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY)
					&& extraParams.containsKey(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX)
					&& extraParams.containsKey(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY)
					&& (extraParams.containsKey(PdfExtraParams.SIGNATURE_PAGE) || extraParams.containsKey(PdfExtraParams.SIGNATURE_PAGES));

			if (want && !existsAreaAttributes) {
				throw new AOCancelledOperationException("Incluir la firma visible en el documento es obligatoria."); //$NON-NLS-1$
			}
		}
	}

	/**
	 * M&eacute;todo que comprueba si es necesario mostrar el dialogo de
	 * personalizaci&oacute;n de la firma visible.
	 *
	 * @param extraParams Propiedades de configuraci&oacute;n de firma.
	 * @return <i>true</i> si es necesario mostrar el dialogo y <i>falso</i> en caso
	 *         contrario.
	 */
	private static boolean isCustomAppearance(final Properties extraParams) {
		// Comprobamos que exista el parametro 'visibleAppearance'.
		boolean customizable = false;
		final String visibleAppearance = extraParams.getProperty(PdfExtraParams.VISIBLE_APPEARANCE);
		if (PdfExtraParams.VISIBLE_APPEARANCE_VALUE_CUSTOM.equalsIgnoreCase(visibleAppearance)) {
			customizable = true;
		}

		return customizable;
	}

	static class SignPdfListener implements SignPdfDialogListener {

		Properties extraParams;

		public SignPdfListener(final Properties extraParams) {
			this.extraParams = extraParams;
		}

		@Override
		public void propertiesCreated(final Properties newParams) {
			// Si el conjunto de propiedades es vacio, significa que se ha cancelado el
			// proceso.
			if (newParams.isEmpty()) {
				ProtocolInvocationLauncherSign.showRubricIsCanceled = true;
			} else {
				updateOptions(this.extraParams, newParams);
			}
		}

		/**
		 * M&eacute;todo que actualiza los par&aacute;metros adicionales asociados a la
		 * posici&oacute;n de la firma visible.
		 *
		 * @param extraParams Conjunto de propiedades de configuraci&oacute;n de la firma.
		 * @param newParams Conjunto de nuevas propiedades que establecer en la firma.
		 */
		static void updateOptions(final Properties extraParams, final Properties newParams) {
			// Atributos asociados al area de firma.
			extraParams.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX,
					newParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX));
			extraParams.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY,
					newParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY));
			extraParams.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX,
					newParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX));
			extraParams.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY,
					newParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY));
			extraParams.setProperty(PdfExtraParams.SIGNATURE_PAGE,
					newParams.getProperty(PdfExtraParams.SIGNATURE_PAGE));

			// Atributos asociados al aspecto de la firma.
			if (newParams.getProperty(PdfExtraParams.LAYER2_TEXT) != null) {
				extraParams.setProperty(PdfExtraParams.LAYER2_TEXT,
						newParams.getProperty(PdfExtraParams.LAYER2_TEXT));
			}
			if (newParams.getProperty(PdfExtraParams.LAYER2_FONTFAMILY) != null) {
				extraParams.setProperty(PdfExtraParams.LAYER2_FONTFAMILY,
						newParams.getProperty(PdfExtraParams.LAYER2_FONTFAMILY));
			}
			if (newParams.getProperty(PdfExtraParams.LAYER2_FONTSIZE) != null) {
				extraParams.setProperty(PdfExtraParams.LAYER2_FONTSIZE,
						newParams.getProperty(PdfExtraParams.LAYER2_FONTSIZE));
			}
			if (newParams.getProperty(PdfExtraParams.SIGNATURE_ROTATION) != null) {
				extraParams.setProperty(PdfExtraParams.SIGNATURE_ROTATION,
						newParams.getProperty(PdfExtraParams.SIGNATURE_ROTATION));
			}
			if (newParams.getProperty(PdfExtraParams.LAYER2_FONTSTYLE) != null) {
				extraParams.setProperty(PdfExtraParams.LAYER2_FONTSTYLE,
						newParams.getProperty(PdfExtraParams.LAYER2_FONTSTYLE));
			}
			if (newParams.getProperty(PdfExtraParams.LAYER2_FONTCOLOR) != null) {
				extraParams.setProperty(PdfExtraParams.LAYER2_FONTCOLOR,
						newParams.getProperty(PdfExtraParams.LAYER2_FONTCOLOR));
			}
			if (newParams.getProperty(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE) != null) {
				extraParams.setProperty(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE,
						newParams.getProperty(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE));
			}
		}
	}

	/** Genera un nombre de fichero.
	 * @param options Opciones proporcionadas en la operaci&oacute;n.
	 * @param filename Nombre del fichero firmado ({@code null} si no es conocido).
	 * @param signer Manejador utilizado para la firma-
	 * @return Nombre de fichero por defecto. */
	private static String getFilename(final UrlParametersToSignAndSave options,
			                          final String filename,
			                          final AOSigner signer) {

		if (options.getFileName() != null) {
			return options.getFileName();
		}

		final String name;
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
}
