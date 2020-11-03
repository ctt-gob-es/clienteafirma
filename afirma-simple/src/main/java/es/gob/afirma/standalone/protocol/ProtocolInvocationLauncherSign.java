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
import java.nio.charset.StandardCharsets;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.cert.CertificateEncodingException;
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
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign.Operation;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.core.signers.CounterSignTarget;
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
import es.gob.afirma.standalone.crypto.CypherDataManager;
import es.gob.afirma.standalone.so.macos.MacUtils;

final class ProtocolInvocationLauncherSign {

	private static final char RESULT_SEPARATOR = '|';

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

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
	static String processSign(final UrlParametersToSign options,
			final int protocolVersion,
			final boolean bySocket) throws SocketOperationException {

		if (options == null) {
			LOGGER.severe("Las opciones de firma son nulas"); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NULL_URI;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
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
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

        // Comprobamos si se exige una version minima del Cliente
        if (options.getMinimunClientVersion() != null) {
        	final String minimumRequestedVersion = options.getMinimunClientVersion();
        	final Version requestedVersion = new Version(minimumRequestedVersion);
        	if (requestedVersion.greaterThan(SimpleAfirma.getVersion())) {
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_MINIMUM_VERSION_NON_SATISTIED;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
        	}
        }

		//TODO: Deshacer cuando se permita la generacion de firmas baseline
		options.getExtraParams().remove("profile");




		// En caso de que no se haya solicitado una operacion de multifirma con
		// el formato AUTO
		// configuramos el servidor en base al nombre de formato
		AOSigner signer = null;
		if (!AOSignConstants.SIGN_FORMAT_AUTO.equalsIgnoreCase(options.getSignatureFormat())) {
			signer = AOSignerFactory.getSigner(options.getSignatureFormat());
			if (signer == null) {
				LOGGER.severe("No hay un firmador configurado para el formato: " + options.getSignatureFormat()); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_FORMAT;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		}

		final AOKeyStore aoks = AOKeyStore.getKeyStore(options.getDefaultKeyStore());
		if (aoks == null) {
			LOGGER.severe(
				"No hay un KeyStore con el nombre: " + options.getDefaultKeyStore() //$NON-NLS-1$
			);
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_FIND_KEYSTORE;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

		// Comprobamos si es necesario pedir datos de entrada al usuario
		boolean needRequestData = false;
		if (options.getData() == null) {
			if (signer != null && signer instanceof OptionalDataInterface) {
				needRequestData = ((OptionalDataInterface) signer).needData(options.getExtraParams());
			}
			else {
				needRequestData = true;
			}
		}

		// Nombre dl fichero firmado. Tomara valor solo si es ekl usuario quien selecciona
		// el fichero a firmar
		String inputFilename = null;

		// Si se tienen que pedir los datos al usuario, se hace
		if (needRequestData) {
			final String dialogTitle = Operation.SIGN.equals(options.getOperation()) ?
				ProtocolMessages.getString("ProtocolLauncher.25") : //$NON-NLS-1$
					ProtocolMessages.getString("ProtocolLauncher.26"); //$NON-NLS-1$

			final String fileExts = options.getExtraParams().getProperty(AfirmaExtraParams.LOAD_FILE_EXTS);

			final String fileDesc = options.getExtraParams().getProperty(AfirmaExtraParams.LOAD_FILE_DESCRIPTION, ProtocolMessages.getString("ProtocolLauncher.32")) +  //$NON-NLS-1$
				(fileExts == null ? " (*.*)" : String.format(" (*.%1s)", fileExts.replace(",", ",*."))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

			final File selectedDataFile;
			try {
				if (Platform.OS.MACOSX.equals(Platform.getOS())) {
					MacUtils.focusApplication();
				}
				selectedDataFile = AOUIFactory.getLoadFiles(
					dialogTitle,
					options.getExtraParams().getProperty(AfirmaExtraParams.LOAD_FILE_CURRENT_DIR), // currentDir
					options.getExtraParams().getProperty(AfirmaExtraParams.LOAD_FILE_FILENAME), // fileName
					fileExts != null ? fileExts.split(",") : null, //$NON-NLS-1$
					fileDesc,
					false,
					false,
					AutoFirmaUtil.getDefaultDialogsIcon(), null
				)[0];
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Carga de datos de firma cancelada por el usuario: " + e); //$NON-NLS-1$
				if (!bySocket) {
					throw new SocketOperationException(getResultCancel());
				}
				return getResultCancel();
			}

			// Asignamos el nombre del fichero firmado para devolverlo a la aplicacion
			inputFilename = selectedDataFile.getName();

			try {
				final byte[] data;
				try (final InputStream fis = new FileInputStream(selectedDataFile);
						final InputStream bis = new BufferedInputStream(fis);) {
					data = AOUtil.getDataFromInputStream(bis);
				}
				if (data == null) {
					throw new IOException("La lectura de datos para firmar ha devuelto un nulo"); //$NON-NLS-1$
				}
				options.setData(data);
			} catch (final Exception e) {
				LOGGER.severe("Error en la lectura de los datos a firmar: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_READ_DATA;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		}

		// En caso de haber programado el formato "AUTO", se selecciona el firmador a partir
		// de los datos proporcionados. En caso de configurarse la operacion de firma,
		// habremos recibido simples datos y seleccionaremos segun su formato. En caso contrario
		// (cofirma o contrafirma) habremos recibido una firma y usaremos el mismo formato que
		// tenga esta.
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
					options.setSignFormat(AOSignerFactory.getSignFormat(signer));
				}
				catch (final IOException e) {
					LOGGER.severe("No se han podido analizar los datos para determinar si son una firma: " + e //$NON-NLS-1$
					);
					// signer sera null
				}
			}

			if (signer == null) {
				LOGGER.severe(
					"Los datos no se corresponden con una firma electronica o no se pudieron analizar" //$NON-NLS-1$
				);
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNKNOWN_SIGNER;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		}

		// XXX: Codigo de soporte de firmas XAdES explicitas (Eliminar cuando se
		// abandone el soporte de XAdES explicitas)
		if (options.getOperation() == Operation.SIGN && isXadesExplicitConfigurated(options.getSignatureFormat(), options.getExtraParams())) {
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

		// Si se ha pedido comprobar las firmas antes de agregarle la nueva firma, lo hacemos ahora
		if (options.getData() != null &&
				Boolean.parseBoolean(options.getExtraParams().getProperty(AfirmaExtraParams.CHECK_SIGNATURES))) {
			final SignValider validator = SignValiderFactory.getSignValider(signer);
			if (validator != null) {
				SignValidity validity;
				try {
					validity = validator.validate(options.getData());
				} catch (final IOException e) {
					LOGGER.severe("Error al identificar la validez de la firma: " + e); //$NON-NLS-1$
					validity = new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.UNKOWN_ERROR);
				}
				if (validity.getValidity() == SIGN_DETAIL_TYPE.KO &&
						!(options.getOperation() == Operation.SIGN && validity.getError() == VALIDITY_ERROR.NO_SIGN)) {
					LOGGER.severe("La firma indicada no es valida"); //$NON-NLS-1$
					final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_SIGNATURE;
					ProtocolInvocationLauncherErrorManager.showError(errorCode);
					if (!bySocket){
						throw new SocketOperationException(errorCode);
					}
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
				}
			}
		}

		// Una vez se tienen todos los parametros necesarios expandimos los extraParams
		// de la operacion para obtener la configuracion final
		try {
			options.expandExtraParams();
		}
		catch (final IncompatiblePolicyException e1) {
			LOGGER.info("Se ha indicado una politica no compatible: " + e1); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_POLICY;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

		final CertFilterManager filterManager = new CertFilterManager(options.getExtraParams());
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
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
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
				if (!bySocket) {
					throw new SocketOperationException(getResultCancel());
				}
				return getResultCancel();
			}
			catch (final AOCertificatesNotFoundException e) {
				LOGGER.severe("No hay certificados validos en el almacen: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NO_CERTIFICATES_KEYSTORE;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al mostrar el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_KEYSTORE;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		}

		final byte[] sign;
		try {
			try {
				switch (options.getOperation()) {
				case SIGN:
					sign = signer.sign(
							options.getData(),
							options.getSignatureAlgorithm(),
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							options.getExtraParams()
							);
					break;
				case COSIGN:
					sign = signer.cosign(
							options.getData(),
							options.getSignatureAlgorithm(),
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							options.getExtraParams()
							);
					break;
				case COUNTERSIGN:
					sign = signer.countersign(
							options.getData(),
							options.getSignatureAlgorithm(),
							"tree".equalsIgnoreCase(options.getExtraParams().getProperty(AfirmaExtraParams.TARGET)) ? //$NON-NLS-1$
									CounterSignTarget.TREE : CounterSignTarget.LEAFS,
									null, // Targets
									pke.getPrivateKey(),
									pke.getCertificateChain(),
									options.getExtraParams()
							);
					break;
				default:
					LOGGER.severe("Error al realizar la operacion firma"); //$NON-NLS-1$
					final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_OPERATION;
					ProtocolInvocationLauncherErrorManager.showError(errorCode);
					if (!bySocket){
						throw new SocketOperationException(errorCode);
					}
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
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
			ProtocolInvocationLauncherErrorManager.showErrorDetail(errorCode, e.getMessage());
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final AOTriphaseException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_RECOVER_SERVER_DOCUMENT;
			ProtocolInvocationLauncherErrorManager.showErrorDetail(errorCode, e.getMessage());
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return errorCode + ": " + e.getMessage(); //$NON-NLS-1$
		}
		catch (final InvalidPdfException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_PDF;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final InvalidXMLException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_XML;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final AOFormatFileException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_DATA;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final InvalidEFacturaDataException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_FACTURAE;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final EFacturaAlreadySignedException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_FACE_ALREADY_SIGNED;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final AOInvalidFormatException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NO_SIGN_DATA;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final BadPdfPasswordException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_WRONG_PASSWORD;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final PdfHasUnregisteredSignaturesException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_UNREG_SIGN;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final PdfIsCertifiedException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_CERTIFIED;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final PdfIsPasswordProtectedException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PDF_WRONG_PASSWORD;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final UnsupportedOperationException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_OPERATION;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final InvalidSignatureException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_INVALID_SIGNATURE;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.log(Level.SEVERE, "Operacion cancelada por el usuario", e); //$NON-NLS-1$
			if (!bySocket) {
				throw new SocketOperationException(getResultCancel());
			}
			return getResultCancel();
		}
		catch (final AOException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_SIGNATURE_FAILED;
			ProtocolInvocationLauncherErrorManager.showErrorDetail(errorCode, e.getMessage());
			if (!bySocket) {
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_SIGNATURE_FAILED;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
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
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

		// Si tenemos clave de cifrado, ciframos el certificado y la firma
		final StringBuilder dataToSend = new StringBuilder();

		if (options.getDesKey() != null) {
			try {
				// El CipherData devuelve los datos directamente en Base64
				dataToSend.append(CypherDataManager.cipherData(certEncoded, options.getDesKey()));
				dataToSend.append(RESULT_SEPARATOR);
				dataToSend.append(CypherDataManager.cipherData(sign, options.getDesKey()));

				// A partir del protocolo version 3, si se cargo un fichero, se devuelve el nombre
				if (inputFilename != null && protocolVersion >= 3) {
					dataToSend.append(RESULT_SEPARATOR);
					dataToSend.append(CypherDataManager.cipherData(buildExtraDataResult(inputFilename)
							.getBytes(StandardCharsets.UTF_8), options.getDesKey()));
				}
			}
			catch (final Exception e) {
				LOGGER.severe("Error en el cifrado de los datos a enviar: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_ENCRIPTING_DATA;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		}
		else {
			LOGGER.warning(
				"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
			dataToSend.append(Base64.encode(certEncoded, true));
			dataToSend.append(RESULT_SEPARATOR);
			// Se hace una doble codificacion Base64, una de los datos y otras
			// del cifrado. La codificacion se realiza incluso si el cifrado
			// no se hiciera
			dataToSend.append(Base64.encode(sign, true));

			// A partir del protocolo version 3, si se cargo un fichero, se devuelve el nombre
			if (inputFilename != null && protocolVersion >= 3) {
				dataToSend.append(RESULT_SEPARATOR);
				dataToSend.append(Base64.encode(buildExtraDataResult(inputFilename)
						.getBytes(StandardCharsets.UTF_8), true));
			}
		}

		if (!bySocket) {
			// Enviamos la firma cifrada al servicio remoto de intercambio y detenemos la espera
			// activa si se encontraba vigente
			synchronized (IntermediateServerUtil.getUniqueSemaphoreInstance()) {
				final Thread waitingThread = ProtocolInvocationLauncher.getActiveWaitingThread();
				if (waitingThread != null) {
					waitingThread.interrupt();
				}
				try {
					IntermediateServerUtil.sendData(dataToSend, options.getStorageServletUrl().toString(), options.getId());
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error al enviar los datos al servidor", e); //$NON-NLS-1$
					ProtocolInvocationLauncherErrorManager.showError(
						ProtocolInvocationLauncherErrorManager.ERROR_SENDING_SIGNATURE
					);
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(
						ProtocolInvocationLauncherErrorManager.ERROR_SENDING_SIGNATURE
					);
				}
			}
		}
		else {
			LOGGER.info(
				"Se omite el envio por red de los datos resultantes por no haberse proporcionado una URL de destino" //$NON-NLS-1$
			);
		}

		return dataToSend.toString();
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
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

	/**
	 * Construye una cadena de texto con un objeto JSON de datos extra que enviar en la respuesta.
	 * @param filename Nombre de fichero.
	 * @return Cadena con el JSON de datos extra.
	 */
	private static String buildExtraDataResult(final String filename) {
		if (filename == null) {
			return null;
		}
		return "{\"filename\":\"" + filename + "\"}"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}
