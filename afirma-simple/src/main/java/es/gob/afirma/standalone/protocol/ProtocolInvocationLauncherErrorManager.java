/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.util.Dictionary;
import java.util.Hashtable;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.so.macos.MacUtils;

/** Gestiona los errores de la ejecuci&oacute;n del Cliente Afirma en una invocaci&oacute;n
 * por protocolo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class ProtocolInvocationLauncherErrorManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final boolean HEADLESS = Boolean.getBoolean(
		"es.gob.afirma.protocolinvocation.HeadLess" //$NON-NLS-1$
	);

	static final String ERROR_CANNOT_READ_DATA = "SAF_00"; //$NON-NLS-1$
	static final String ERROR_NULL_URI = "SAF_01"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_PROTOCOL = "SAF_02"; //$NON-NLS-1$
	static final String ERROR_PARAMS = "SAF_03"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_OPERATION = "SAF_04"; //$NON-NLS-1$
	static final String ERROR_CANNOT_SAVE_DATA = "SAF_05"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_FORMAT = "SAF_06"; //$NON-NLS-1$
	static final String ERROR_CANNOT_FIND_KEYSTORE = "SAF_07"; //$NON-NLS-1$
	static final String ERROR_CANNOT_ACCESS_KEYSTORE = "SAF_08"; //$NON-NLS-1$
	static final String ERROR_SIGNATURE_FAILED = "SAF_09"; //$NON-NLS-1$
	static final String ERROR_NO_CERTIFICATES_SYSTEM = "SAF_10"; //$NON-NLS-1$
	static final String ERROR_SENDING_RESULT = "SAF_11"; //$NON-NLS-1$
	static final String ERROR_ENCRIPTING_DATA = "SAF_12"; //$NON-NLS-1$
	static final String ERROR_LOCAL_ACCESS_BLOCKED = "SAF_13"; //$NON-NLS-1$
	static final String ERROR_OBSOLETE_APP = "SAF_14"; //$NON-NLS-1$
	static final String ERROR_DECRYPTING_DATA = "SAF_15"; //$NON-NLS-1$
	static final String ERROR_RECOVERING_DATA = "SAF_16"; //$NON-NLS-1$
	static final String ERROR_UNKNOWN_SIGNER = "SAF_17"; //$NON-NLS-1$
	static final String ERROR_DECODING_CERTIFICATE = "SAF_18"; //$NON-NLS-1$
	static final String ERROR_NO_CERTIFICATES_KEYSTORE = "SAF_19"; //$NON-NLS-1$
	static final String ERROR_LOCAL_BATCH_SIGN = "SAF_20"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_PROCEDURE = "SAF_21"; //$NON-NLS-1$
	static final String ERROR_UNSOPPORTED_WEB_PROCEDURE = "SAF_22"; //$NON-NLS-1$
	static final String ERROR_INVALID_POLICY = "SAF_23"; //$NON-NLS-1$
	static final String ERROR_RECOVERING_LOG = "SAF_24"; //$NON-NLS-1$
	static final String ERROR_CANNOT_LOAD_DATA = "SAF_25"; //$NON-NLS-1$
	static final String ERROR_CONTACT_BATCH_SERVICE = "SAF_26"; //$NON-NLS-1$
	static final String ERROR_BATCH_SIGNATURE = "SAF_27"; //$NON-NLS-1$
	static final String ERROR_INVALID_PDF = "SAF_28"; //$NON-NLS-1$
	static final String ERROR_INVALID_XML = "SAF_29"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA = "SAF_30"; //$NON-NLS-1$
	static final String ERROR_NO_SIGN_DATA = "SAF_31"; //$NON-NLS-1$
	static final String ERROR_FACE_ALREADY_SIGNED = "SAF_32"; //$NON-NLS-1$
	static final String ERROR_PDF_WRONG_PASSWORD = "SAF_33"; //$NON-NLS-1$
	static final String ERROR_PDF_UNREG_SIGN = "SAF_34"; //$NON-NLS-1$
	static final String ERROR_PDF_CERTIFIED = "SAF_35"; //$NON-NLS-1$
	static final String ERROR_CANNOT_FIND_SSL_KEYSTORE = "SAF_36"; //$NON-NLS-1$
	static final String ERROR_CANNOT_ACCESS_SSL_KEYSTORE = "SAF_37"; //$NON-NLS-1$
	static final String ERROR_INVALID_FACTURAE = "SAF_38"; //$NON-NLS-1$
	static final String ERROR_INVALID_SIGNATURE = "SAF_39"; //$NON-NLS-1$
	static final String ERROR_RECOVER_SERVER_DOCUMENT = "SAF_40"; //$NON-NLS-1$
	static final String ERROR_MINIMUM_VERSION_NON_SATISTIED = "SAF_41"; //$NON-NLS-1$
	static final String ERROR_POSTPROCESSING_DATA = "SAF_42"; //$NON-NLS-1$
	static final String ERROR_VISIBLE_SIGNATURE = "SAF_43"; //$NON-NLS-1$
	static final String ERROR_SIGN_WITHOUT_DATA = "SAF_44"; //$NON-NLS-1$
	static final String ERROR_CANNOT_OPEN_SOCKET = "SAF_45"; //$NON-NLS-1$
	static final String ERROR_INVALID_SESSION_ID = "SAF_46"; //$NON-NLS-1$
	static final String ERROR_EXTERNAL_REQUEST_TO_SOCKET = "SAF_47"; //$NON-NLS-1$
	static final String ERROR_PDF_SHADOW_ATTACK = "SAF_48"; //$NON-NLS-1$
	static final String ERROR_SIGNING_LTS_SIGNATURE = "SAF_49"; //$NON-NLS-1$
	static final String ERROR_CONFIRMATION_NEEDED = "SAF_50"; //$NON-NLS-1$
	static final String ERROR_INCOMPATIBLE_KEY_TYPE = "SAF_51"; //$NON-NLS-1$
	static final String ERROR_LOCKED_KEYSTORE = "SAF_52"; //$NON-NLS-1$

	private static final Dictionary<String, String> ERRORS = new Hashtable<>();

	static {
		ERRORS.put(ERROR_CANNOT_READ_DATA, ProtocolMessages.getString("ProtocolLauncher.0")); //$NON-NLS-1$
		ERRORS.put(ERROR_NULL_URI, ProtocolMessages.getString("ProtocolLauncher.1")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_PROTOCOL, ProtocolMessages.getString("ProtocolLauncher.2")); //$NON-NLS-1$
		ERRORS.put(ERROR_PARAMS, ProtocolMessages.getString("ProtocolLauncher.3")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_OPERATION, ProtocolMessages.getString("ProtocolLauncher.4")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_SAVE_DATA, ProtocolMessages.getString("ProtocolLauncher.5")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_FORMAT, ProtocolMessages.getString("ProtocolLauncher.6")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_FIND_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.7")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_ACCESS_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.8")); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGNATURE_FAILED, ProtocolMessages.getString("ProtocolLauncher.9")); //$NON-NLS-1$
		ERRORS.put(ERROR_NO_CERTIFICATES_SYSTEM, ProtocolMessages.getString("ProtocolLauncher.10")); //$NON-NLS-1$
		ERRORS.put(ERROR_SENDING_RESULT, ProtocolMessages.getString("ProtocolLauncher.11")); //$NON-NLS-1$
		ERRORS.put(ERROR_ENCRIPTING_DATA, ProtocolMessages.getString("ProtocolLauncher.12")); //$NON-NLS-1$
		ERRORS.put(ERROR_LOCAL_ACCESS_BLOCKED, ProtocolMessages.getString("ProtocolLauncher.13")); //$NON-NLS-1$
		ERRORS.put(ERROR_OBSOLETE_APP, ProtocolMessages.getString("ProtocolLauncher.14")); //$NON-NLS-1$
		ERRORS.put(ERROR_DECRYPTING_DATA, ProtocolMessages.getString("ProtocolLauncher.15")); //$NON-NLS-1$
		ERRORS.put(ERROR_RECOVERING_DATA, ProtocolMessages.getString("ProtocolLauncher.16")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNKNOWN_SIGNER, ProtocolMessages.getString("ProtocolLauncher.17")); //$NON-NLS-1$
		ERRORS.put(ERROR_DECODING_CERTIFICATE, ProtocolMessages.getString("ProtocolLauncher.18")); //$NON-NLS-1$
		ERRORS.put(ERROR_NO_CERTIFICATES_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.19")); //$NON-NLS-1$
		ERRORS.put(ERROR_LOCAL_BATCH_SIGN, ProtocolMessages.getString("ProtocolLauncher.20")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_PROCEDURE, ProtocolMessages.getString("ProtocolLauncher.21")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSOPPORTED_WEB_PROCEDURE, ProtocolMessages.getString("ProtocolLauncher.22")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_POLICY, ProtocolMessages.getString("ProtocolLauncher.33")); //$NON-NLS-1$
		ERRORS.put(ERROR_RECOVERING_LOG, ProtocolMessages.getString("ProtocolLauncher.34")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_LOAD_DATA, ProtocolMessages.getString("ProtocolLauncher.35")); //$NON-NLS-1$
		ERRORS.put(ERROR_CONTACT_BATCH_SERVICE, ProtocolMessages.getString("ProtocolLauncher.36")); //$NON-NLS-1$
		ERRORS.put(ERROR_BATCH_SIGNATURE, ProtocolMessages.getString("ProtocolLauncher.37")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_PDF, ProtocolMessages.getString("ProtocolLauncher.38")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_XML, ProtocolMessages.getString("ProtocolLauncher.39")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_DATA, ProtocolMessages.getString("ProtocolLauncher.40")); //$NON-NLS-1$
		ERRORS.put(ERROR_NO_SIGN_DATA, ProtocolMessages.getString("ProtocolLauncher.41")); //$NON-NLS-1$
		ERRORS.put(ERROR_FACE_ALREADY_SIGNED, ProtocolMessages.getString("ProtocolLauncher.42")); //$NON-NLS-1$
		ERRORS.put(ERROR_PDF_WRONG_PASSWORD, ProtocolMessages.getString("ProtocolLauncher.43")); //$NON-NLS-1$
		ERRORS.put(ERROR_PDF_UNREG_SIGN, ProtocolMessages.getString("ProtocolLauncher.44")); //$NON-NLS-1$
		ERRORS.put(ERROR_PDF_CERTIFIED, ProtocolMessages.getString("ProtocolLauncher.45")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_FIND_SSL_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.46")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_ACCESS_SSL_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.47")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_FACTURAE, ProtocolMessages.getString("ProtocolLauncher.48")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_SIGNATURE, ProtocolMessages.getString("ProtocolLauncher.49")); //$NON-NLS-1$
		ERRORS.put(ERROR_RECOVER_SERVER_DOCUMENT, ProtocolMessages.getString("ProtocolLauncher.50")); //$NON-NLS-1$
		ERRORS.put(ERROR_MINIMUM_VERSION_NON_SATISTIED, ProtocolMessages.getString("ProtocolLauncher.53")); //$NON-NLS-1$
		ERRORS.put(ERROR_POSTPROCESSING_DATA, ProtocolMessages.getString("ProtocolLauncher.54")); //$NON-NLS-1$
		ERRORS.put(ERROR_VISIBLE_SIGNATURE, ProtocolMessages.getString("ProtocolLauncher.55")); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGN_WITHOUT_DATA, ProtocolMessages.getString("ProtocolLauncher.56")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_OPEN_SOCKET, ProtocolMessages.getString("ProtocolLauncher.57")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_SESSION_ID, ProtocolMessages.getString("ProtocolLauncher.58")); //$NON-NLS-1$
		ERRORS.put(ERROR_EXTERNAL_REQUEST_TO_SOCKET, ProtocolMessages.getString("ProtocolLauncher.59")); //$NON-NLS-1$
		ERRORS.put(ERROR_PDF_SHADOW_ATTACK, ProtocolMessages.getString("ProtocolLauncher.63")); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGNING_LTS_SIGNATURE, ProtocolMessages.getString("ProtocolLauncher.64")); //$NON-NLS-1$
		ERRORS.put(ERROR_CONFIRMATION_NEEDED, ProtocolMessages.getString("ProtocolLauncher.65")); //$NON-NLS-1$
		ERRORS.put(ERROR_INCOMPATIBLE_KEY_TYPE, ProtocolMessages.getString("ProtocolLauncher.66")); //$NON-NLS-1$
		ERRORS.put(ERROR_LOCKED_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.67")); //$NON-NLS-1$
	}

	static void showError(final String code) {
		showError(code, null);
	}

	static void showError(final String code, final Throwable t) {
		showError(code, ERRORS.get(code), t);
	}

	static void showError(final String code, final String message, final Throwable t) {
		final String desc = ProtocolMessages.getString("ProtocolLauncher.28") + "\n(" + code + ": " + message + ")";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		if (!HEADLESS) {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MacUtils.focusApplication();
			}
			AOUIFactory.showErrorMessage(
				desc,
				ProtocolMessages.getString("ProtocolLauncher.29"), //$NON-NLS-1$
				AOUIFactory.ERROR_MESSAGE,
				t
			);
		}
		LOGGER.severe(desc);
	}

	static void showErrorDetail(final String code, final String detail) {
		showErrorDetail(code, ERRORS.get(code), detail);
	}

	static void showErrorDetail(final String code, final String message, final String detail) {
		showError(code, message + "\n" + detail, null); //$NON-NLS-1$
	}

	static void showErrorDetail(final String code, final Throwable t) {
		showError(code, t);
	}

	static void showErrorDetail(final String code, final String message, final Throwable t) {
		showError(code, message, t);
	}

	static String getErrorMessage(final String code) {
		return code + ": " + ERRORS.get(code); //$NON-NLS-1$
	}
}
