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
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;
import es.gob.afirma.core.signers.OptionalDataInterface;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.signers.pades.BadPdfPasswordException;
import es.gob.afirma.signers.pades.InvalidPdfException;
import es.gob.afirma.signers.pades.PdfHasUnregisteredSignaturesException;
import es.gob.afirma.signers.pades.PdfIsCertifiedException;
import es.gob.afirma.signers.pades.PdfIsPasswordProtectedException;
import es.gob.afirma.signers.xades.EFacturaAlreadySignedException;
import es.gob.afirma.signers.xades.InvalidEFacturaDataException;
import es.gob.afirma.signers.xml.InvalidXMLException;
import es.gob.afirma.signvalidation.InvalidSignatureException;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.EncryptingException;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PermissionChecker;
import es.gob.afirma.standalone.plugins.PluginControlledException;
import es.gob.afirma.standalone.plugins.PluginException;
import es.gob.afirma.standalone.plugins.PluginInfo;
import es.gob.afirma.standalone.plugins.PluginsManager;
import es.gob.afirma.standalone.plugins.SignDataProcessor;
import es.gob.afirma.standalone.plugins.SignOperation;
import es.gob.afirma.standalone.plugins.SignOperation.Operation;
import es.gob.afirma.standalone.plugins.SignResult;
import es.gob.afirma.standalone.so.macos.MacUtils;

final class ProtocolInvocationLauncherSign {

	public static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherSign() {
		// No instanciable
	}

	/** Procesa una peticion de firma en invocaci&oacute;n por protocolo y obtiene
	 * la firma junto con una serie de metadatos en forma de cadena.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @param bySocket <code>true</code> para usar comunicaci&oacute;n por <i>socket</i> local,
	 *                 <code>false</code> para usar servidor intermedio.
	 * @return Resultado de la operaci&oacute;n o mensaje de error.
	 * @throws SocketOperationException Si hay errores en la
	 *                                  comunicaci&oacute;n por <i>socket</i> local. */
	static StringBuilder processSign(final UrlParametersToSign options,
			final int protocolVersion) throws SocketOperationException {

		if (options == null) {
			LOGGER.severe("Las opciones de firma son nulas"); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NULL_URI;
			throw new SocketOperationException(errorCode);
		}

        // Comprobamos si soportamos la version del protocolo indicada
		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(protocolVersion)) {
			LOGGER.severe(
				String.format(
					"Version de protocolo no soportada (%1s). Version actual: %2d. Hay que actualizar la aplicacion.", //$NON-NLS-1$
					Integer.valueOf(protocolVersion),
					Integer.valueOf(ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.getVersion())
				)
			);
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE;
			throw new SocketOperationException(errorCode);
		}

        // Comprobamos si se exige una version minima del Cliente
        if (options.getMinimunClientVersion() != null) {
        	final String minimumRequestedVersion = options.getMinimunClientVersion();
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
				ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.getVersion(),
				operation);
		processor.setCipherKey(options.getDesKey());

		final List<SignOperation> operations = processor.preProcess(operation);
		final List<SignResult> results = new ArrayList<>(operations.size());
		for (int i = 0; i < operations.size(); i++) {
			final SignOperation op = operations.get(i);
			try {
				results.add(signOperation(op, options));
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
			plugins = PluginsManager.getInstance().getPluginsLoadedList();
		} catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se pudo cargar el listado de plugins", e); //$NON-NLS-1$
			return new NativeSignDataProcessor(protocolVersion);
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

	private static SignResult signOperation(final SignOperation signOperation, final UrlParametersToSign options)
			throws SocketOperationException {

		byte[] data = signOperation.getData();
		String format = signOperation.getFormat();
		final String algorithm = signOperation.getAlgorithm();
		Properties extraParams = signOperation.getExtraParams();
		final Operation cryptoOperation = signOperation.getCryptoOperation();

		// En caso de que no se haya solicitado una operacion de multifirma con
		// el formato AUTO configuramos el servidor en base al nombre de formato
		AOSigner signer = null;
		if (!AOSignConstants.SIGN_FORMAT_AUTO.equalsIgnoreCase(format)) {
			signer = AOSignerFactory.getSigner(format);
			if (signer == null) {
				LOGGER.severe("No hay un firmador configurado para el formato: " + format); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_FORMAT;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				throw new SocketOperationException(errorCode);
			}
		}

		final AOKeyStore aoks = AOKeyStore.getKeyStore(options.getDefaultKeyStore());
		if (aoks == null) {
			LOGGER.severe("No hay un KeyStore con el nombre: " + options.getDefaultKeyStore()); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_FIND_KEYSTORE;
			throw new SocketOperationException(errorCode);
		}

		// Comprobamos si es necesario pedir datos de entrada al usuario
		boolean needRequestData = false;
		if (data == null) {
			if (signer != null && signer instanceof OptionalDataInterface) {
				needRequestData = ((OptionalDataInterface) signer).needData(extraParams);
			}
			else {
				needRequestData = true;
			}
		}

		// Nombre del fichero firmado. Tomara valor solo si es ekl usuario quien selecciona
		// el fichero a firmar
		String inputFilename = null;

		// Si se tienen que pedir los datos al usuario, se hace
		if (needRequestData) {
			final String dialogTitle = Operation.SIGN == cryptoOperation ?
				ProtocolMessages.getString("ProtocolLauncher.25") : //$NON-NLS-1$
					ProtocolMessages.getString("ProtocolLauncher.26"); //$NON-NLS-1$

			final String fileExts = extraParams.getProperty(AfirmaExtraParams.LOAD_FILE_EXTS);

			final String fileDesc = extraParams.getProperty(AfirmaExtraParams.LOAD_FILE_DESCRIPTION, ProtocolMessages.getString("ProtocolLauncher.32")) +  //$NON-NLS-1$
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
					false,
					false,
					AutoFirmaUtil.getDefaultDialogsIcon(), null
				)[0];
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Carga de datos de firma cancelada por el usuario: " + e); //$NON-NLS-1$
				throw new SocketOperationException(RESULT_CANCEL);
			}

			// Asignamos el nombre del fichero firmado para devolverlo a la aplicacion
			inputFilename = selectedDataFile.getName();

			try {
				try (final InputStream fis = new FileInputStream(selectedDataFile);
						final InputStream bis = new BufferedInputStream(fis);) {
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

		// En caso de haber programado el formato "AUTO", se define el formato de firma
		if (signer == null) {
			format = identifyFormatFromData(data, cryptoOperation);
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
		if (cryptoOperation == Operation.SIGN && isXadesExplicitConfigurated(format, extraParams)) {
			LOGGER.warning(
				"Se ha pedido una firma XAdES explicita, este formato dejara de soportarse en proximas versiones" //$NON-NLS-1$
			);
			try {
				data = MessageDigest.getInstance("SHA1").digest(data); //$NON-NLS-1$
				extraParams.setProperty("mimeType", "hash/sha1"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch (final Exception e) {
				LOGGER.warning("Error al generar la huella digital de los datos para firmar como 'XAdES explicit', " //$NON-NLS-1$
					+ "se realizara una firma XAdES corriente: " + e); //$NON-NLS-1$
			}
		}

		// Si se ha pedido comprobar las firmas antes de agregarle la nueva firma, lo hacemos ahora
		if (data != null &&
				Boolean.parseBoolean(extraParams.getProperty(AfirmaExtraParams.CHECK_SIGNATURES))) {
			final SignValider validator = SignValiderFactory.getSignValider(signer);
			if (validator != null) {
				SignValidity validity;
				try {
					validity = validator.validate(data);
				} catch (final IOException e) {
					LOGGER.severe("Error al identificar la validez de la firma: " + e); //$NON-NLS-1$
					validity = new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.UNKOWN_ERROR);
				}
				// La comprobacion de la operacion se hace aqui ya que hay formatos que tambien
				// deben comprobar la validadez de las firmas previas para las operaciones de
				// firma (PAdES, OOXML, etc.).
				if (validity.getValidity() == SIGN_DETAIL_TYPE.KO &&
						!(cryptoOperation == Operation.SIGN && validity.getError() == VALIDITY_ERROR.NO_SIGN)) {
					LOGGER.severe("La firma indicada no es valida"); //$NON-NLS-1$
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
		}
		catch (final IncompatiblePolicyException e1) {
			LOGGER.info("Se ha indicado una politica no compatible: " + e1); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_POLICY;
			throw new SocketOperationException(errorCode);
		}

		final CertFilterManager filterManager = new CertFilterManager(extraParams);
		final List<CertificateFilter> filters = filterManager.getFilters();
		final boolean mandatoryCertificate = filterManager.isMandatoryCertificate();
		final PrivateKeyEntry pke;

		if (options.getSticky() && !options.getResetSticky() && ProtocolInvocationLauncher.getStickyKeyEntry() != null) {
			pke = ProtocolInvocationLauncher.getStickyKeyEntry();
		}
		else {
			final PasswordCallback pwc = aoks.getStorePasswordCallback(null);
			final String aoksLib = options.getDefaultKeyStoreLib();
			final AOKeyStoreManager ksm;
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(aoks, // Store
					aoksLib, // Lib
					null, // Description
					pwc, // PasswordCallback
					null // Parent
				);
			}
			catch (final Exception e3) {
				LOGGER.severe("Error obteniendo el AOKeyStoreManager: " + e3); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_KEYSTORE;
				throw new SocketOperationException(errorCode);
			}

			LOGGER.info("Obtenido gestor de almacenes de claves: " + ksm); //$NON-NLS-1$

			LOGGER.info("Cargando dialogo de seleccion de certificados..."); //$NON-NLS-1$

			try {
				MacUtils.focusApplication();
				final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
					ksm,
					null,
					true,
					true, // showExpiredCertificates
					true, // checkValidity
					filters,
					mandatoryCertificate
				);
				dialog.allowOpenExternalStores(filterManager.isExternalStoresOpeningAllowed());
				dialog.show();

				// Obtenemos el almacen del certificado seleccionado (que puede no ser el mismo
		    	// que se indico originalmente por haberlo cambiado desde el dialogo de seleccion)
				// y de ahi sacamos la referencia a la clave
				final CertificateContext context = dialog.getSelectedCertificateContext();
		    	final KeyStoreManager currentKsm = context.getKeyStoreManager();
				pke = currentKsm.getKeyEntry(context.getAlias());

				if (options.getSticky()) {
					ProtocolInvocationLauncher.setStickyKeyEntry(pke);
				}
				else {
					ProtocolInvocationLauncher.setStickyKeyEntry(null);
				}
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
				throw new SocketOperationException(errorCode);
			}
		}

		final byte[] sign;
		try {
			try {
				switch (cryptoOperation) {
				case SIGN:
					sign = signer.sign(
							data,
							algorithm,
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParams
							);
					break;
				case COSIGN:
					sign = signer.cosign(
							data,
							algorithm,
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParams
							);
					break;
				case COUNTERSIGN:
					sign = signer.countersign(
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
		catch (final SocketOperationException e) {
			throw e;
		}
		catch (final IllegalArgumentException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PARAMS;
			throw new SocketOperationException(errorCode, e.getMessage());
		}
		catch (final AOTriphaseException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_RECOVER_SERVER_DOCUMENT;
			throw new SocketOperationException(errorCode, e.getMessage());
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
		catch (final AOInvalidFormatException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NO_SIGN_DATA;
			throw new SocketOperationException(errorCode);
		}
		catch (final BadPdfPasswordException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_WRONG_PASSWORD;
			throw new SocketOperationException(errorCode);
		}
		catch (final PdfHasUnregisteredSignaturesException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_UNREG_SIGN;
			throw new SocketOperationException(errorCode);
		}
		catch (final PdfIsCertifiedException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_CERTIFIED;
			throw new SocketOperationException(errorCode);
		}
		catch (final PdfIsPasswordProtectedException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_WRONG_PASSWORD;
			throw new SocketOperationException(errorCode);
		}
		catch (final UnsupportedOperationException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_OPERATION;
			throw new SocketOperationException(errorCode);
		}
		catch (final InvalidSignatureException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
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
			throw new SocketOperationException(errorCode, e.getMessage());
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_SIGNATURE_FAILED;
			throw new SocketOperationException(errorCode);
		}

		// Concatenamos el certificado utilizado para firmar y la firma con un separador
		// para que la pagina pueda recuperar ambos
		final byte[] certEncoded;
		try {
			certEncoded = pke.getCertificateChain()[0].getEncoded();
		}
		catch (final CertificateEncodingException e) {
			LOGGER.severe("Error en la decodificacion del certificado de firma: " + e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_DECODING_CERTIFICATE;
			throw new SocketOperationException(errorCode);
		}

		final SignResult result = new SignResult();
		result.setSignature(sign);
		result.setCertificate(certEncoded);
		Properties extraData = null;
		if (inputFilename != null) {
			extraData = new Properties();
			extraData.setProperty("filename", inputFilename); //$NON-NLS-1$
		}
		result.setDataFilename(extraData);

		return result;
	}

	/**
	 * Identifica el formato firma que debe generar a partir de los datos y el tipo de
	 * operaci&oacute;n. En caso de configurarse la operacion de firma, habremos recibido
	 * simples datos y seleccionaremos segun su formato. En caso contrario, la operaci&oacute;n
	 * ser&aacute; cofirma o contrafirma, habremos recibido una firma y usaremos el mismo formato
	 * que tenga esta.
	 * @param data Datos a firmar o firma a multifirmar.
	 * @param cryptoOperation Operaci&oacute;n que debe realizarse (firma, cofirma o contrafirma).
	 * @return Formato de firma.
	 */
	private static String identifyFormatFromData(final byte[] data, final Operation cryptoOperation) {

		String format;
		if (Operation.SIGN == cryptoOperation) {
			if (DataAnalizerUtil.isPDF(data)) {
				format = AOSignConstants.SIGN_FORMAT_PADES;
			}
			else if (DataAnalizerUtil.isFacturae(data)) {
				format = AOSignConstants.SIGN_FORMAT_FACTURAE;
			}
			else if (DataAnalizerUtil.isXML(data)) {
				format = AOSignConstants.SIGN_FORMAT_XADES;
			}
			else if (DataAnalizerUtil.isODF(data)) {
				format = AOSignConstants.SIGN_FORMAT_ODF;
			}
			else if (DataAnalizerUtil.isOOXML(data)) {
				format = AOSignConstants.SIGN_FORMAT_OOXML;
			}
			else {
				format = AOSignConstants.SIGN_FORMAT_CADES;
			}
		}
		else {
			try {
				final AOSigner signer = AOSignerFactory.getSigner(data);
				format = AOSignerFactory.getSignFormat(signer);
			}
			catch (final IOException e) {
				LOGGER.severe(
						"No se han podido analizar los datos para determinar si son una firma: " + e); //$NON-NLS-1$
				format = null;
			}
		}

		return format;
	}

	/** Identifica cuando se ha configurado una firma con el formato XAdES y la
	 * propiedad {@code mode} con el valor {@code explicit}.
	 * Esta no es una firma correcta pero, por compatibilidad con los tipos de firmas del
	 * Applet pesado, se ha incluido aqu&iacute;.
	 * @param format Formato declarado para la firma.
	 * @param config Par&aacute;metros adicionales declarados para la firma.
	 * @return {@code true} si se configura una firma <i>XAdES explicit</i>,
	 *         {@code false} en caso contrario.
	 * @deprecated Uso temporal hasta que se elimine el soporte de firmas XAdES
	 *             expl&iacute;citas. */
	@Deprecated
	private static boolean isXadesExplicitConfigurated(final String format, final Properties config) {
		return format != null && format.toLowerCase().startsWith("xades") && config != null && //$NON-NLS-1$
			AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(config.getProperty(AfirmaExtraParams.MODE));
	}
}
