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

import es.gob.afirma.core.AOControlledException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.KeyStoreErrorCode;
import es.gob.afirma.signers.batch.client.BatchErrorCode;
import es.gob.afirma.signers.pades.common.PdfErrorCode;
import es.gob.afirma.signers.pkcs7.BinaryErrorCode;
import es.gob.afirma.signers.xades.XAdESErrorCode;
import es.gob.afirma.signers.xml.XMLErrorCode;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.so.macos.MacUtils;
import es.gob.afirma.standalone.ui.ProgressInfoDialogManager;

/** Gestiona los errores de la ejecuci&oacute;n del Cliente Afirma en una invocaci&oacute;n
 * por protocolo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class ProtocolInvocationLauncherErrorManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String AUTOFIRMA_ERROR_PREFIX = "AF"; //$NON-NLS-1$

	private static final String DEFAULT_ERROR_PREFIX = "err-00:="; //$NON-NLS-1$
	private static final String CANCELLATION_ERROR_PREFIX = "err-11:="; //$NON-NLS-1$

	private static final boolean HEADLESS = Boolean.getBoolean(
		"es.gob.afirma.protocolinvocation.HeadLess" //$NON-NLS-1$
	);

	static final String CANCEL_RESPONSE = "CANCEL"; //$NON-NLS-1$

	private static final String ERROR_CANNOT_READ_DATA = "SAF_00"; //$NON-NLS-1$
	private static final String ERROR_NULL_URI = "SAF_01"; //$NON-NLS-1$
	private static final String ERROR_UNSUPPORTED_PROTOCOL = "SAF_02"; //$NON-NLS-1$
	private static final String ERROR_PARAMS = "SAF_03"; //$NON-NLS-1$
	private static final String ERROR_UNSUPPORTED_OPERATION = "SAF_04"; //$NON-NLS-1$
	private static final String ERROR_CANNOT_SAVE_DATA = "SAF_05"; //$NON-NLS-1$
	private static final String ERROR_UNSUPPORTED_FORMAT = "SAF_06"; //$NON-NLS-1$
	private static final String ERROR_CANNOT_ACCESS_KEYSTORE = "SAF_08"; //$NON-NLS-1$
	private static final String ERROR_SIGNATURE_FAILED = "SAF_09"; //$NON-NLS-1$
	private static final String ERROR_SENDING_RESULT = "SAF_11"; //$NON-NLS-1$
	private static final String ERROR_ENCRIPTING_DATA = "SAF_12"; //$NON-NLS-1$
	private static final String ERROR_LOCAL_ACCESS_BLOCKED = "SAF_13"; //$NON-NLS-1$
	private static final String ERROR_OBSOLETE_APP = "SAF_14"; //$NON-NLS-1$
	private static final String ERROR_DECRYPTING_DATA = "SAF_15"; //$NON-NLS-1$
	private static final String ERROR_RECOVERING_DATA = "SAF_16"; //$NON-NLS-1$
	private static final String ERROR_UNKNOWN_SIGNER = "SAF_17"; //$NON-NLS-1$
	private static final String ERROR_DECODING_CERTIFICATE = "SAF_18"; //$NON-NLS-1$
	private static final String ERROR_NO_CERTIFICATES_KEYSTORE = "SAF_19"; //$NON-NLS-1$
	private static final String ERROR_LOCAL_BATCH_SIGN = "SAF_20"; //$NON-NLS-1$
	private static final String ERROR_UNSUPPORTED_PROCEDURE = "SAF_21"; //$NON-NLS-1$
	private static final String ERROR_INVALID_POLICY = "SAF_23"; //$NON-NLS-1$
	private static final String ERROR_CANNOT_LOAD_DATA = "SAF_25"; //$NON-NLS-1$
	private static final String ERROR_CONTACT_BATCH_SERVICE = "SAF_26"; //$NON-NLS-1$
	private static final String ERROR_BATCH_SIGNATURE = "SAF_27"; //$NON-NLS-1$
	private static final String ERROR_INVALID_PDF = "SAF_28"; //$NON-NLS-1$
	private static final String ERROR_INVALID_XML = "SAF_29"; //$NON-NLS-1$
	private static final String ERROR_INVALID_DATA = "SAF_30"; //$NON-NLS-1$
	private static final String ERROR_NO_SIGN_DATA = "SAF_31"; //$NON-NLS-1$
	private static final String ERROR_FACE_ALREADY_SIGNED = "SAF_32"; //$NON-NLS-1$
	private static final String ERROR_PDF_WRONG_PASSWORD = "SAF_33"; //$NON-NLS-1$
	private static final String ERROR_PDF_UNREG_SIGN = "SAF_34"; //$NON-NLS-1$
	private static final String ERROR_PDF_CERTIFIED = "SAF_35"; //$NON-NLS-1$
	private static final String ERROR_INVALID_FACTURAE = "SAF_38"; //$NON-NLS-1$
	private static final String ERROR_INVALID_SIGNATURE = "SAF_39"; //$NON-NLS-1$
	private static final String ERROR_MINIMUM_VERSION_NON_SATISTIED = "SAF_41"; //$NON-NLS-1$
	private static final String ERROR_VISIBLE_SIGNATURE = "SAF_43"; //$NON-NLS-1$
	private static final String ERROR_SIGN_WITHOUT_DATA = "SAF_44"; //$NON-NLS-1$
	private static final String ERROR_CANNOT_OPEN_SOCKET = "SAF_45"; //$NON-NLS-1$
	private static final String ERROR_INVALID_SESSION_ID = "SAF_46"; //$NON-NLS-1$
	private static final String ERROR_PDF_SHADOW_ATTACK = "SAF_48"; //$NON-NLS-1$
	private static final String ERROR_SIGNING_LTS_SIGNATURE = "SAF_49"; //$NON-NLS-1$
	private static final String ERROR_INCOMPATIBLE_KEY_TYPE = "SAF_51"; //$NON-NLS-1$
	private static final String ERROR_LOCKED_KEYSTORE = "SAF_52"; //$NON-NLS-1$

	// Error utilizado cuando se deba usar el protocolo antiguo que pero no haya un error definido
	private static final String ERROR_UNKNOWN = "SAF_53"; //$NON-NLS-1$

	private static final Dictionary<String, String> ERRORS = new Hashtable<>();

	static {
		// El texto asociado al codigo de cancelacion es la propia respuesta de cancelacion
		ERRORS.put(CANCEL_RESPONSE, CANCEL_RESPONSE);

		// Asociamos
		ERRORS.put(ERROR_CANNOT_READ_DATA, ProtocolMessages.getString("ProtocolLauncher.0")); //$NON-NLS-1$
		ERRORS.put(ERROR_NULL_URI, ProtocolMessages.getString("ProtocolLauncher.1")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_PROTOCOL, ProtocolMessages.getString("ProtocolLauncher.2")); //$NON-NLS-1$
		ERRORS.put(ERROR_PARAMS, ProtocolMessages.getString("ProtocolLauncher.3")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_OPERATION, ProtocolMessages.getString("ProtocolLauncher.4")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_SAVE_DATA, ProtocolMessages.getString("ProtocolLauncher.5")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_FORMAT, ProtocolMessages.getString("ProtocolLauncher.6")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_ACCESS_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.8")); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGNATURE_FAILED, ProtocolMessages.getString("ProtocolLauncher.9")); //$NON-NLS-1$
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
		ERRORS.put(ERROR_INVALID_POLICY, ProtocolMessages.getString("ProtocolLauncher.33")); //$NON-NLS-1$
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
		ERRORS.put(ERROR_INVALID_FACTURAE, ProtocolMessages.getString("ProtocolLauncher.48")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_SIGNATURE, ProtocolMessages.getString("ProtocolLauncher.49")); //$NON-NLS-1$
		ERRORS.put(ERROR_MINIMUM_VERSION_NON_SATISTIED, ProtocolMessages.getString("ProtocolLauncher.53")); //$NON-NLS-1$
		ERRORS.put(ERROR_VISIBLE_SIGNATURE, ProtocolMessages.getString("ProtocolLauncher.55")); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGN_WITHOUT_DATA, ProtocolMessages.getString("ProtocolLauncher.56")); //$NON-NLS-1$
		ERRORS.put(ERROR_CANNOT_OPEN_SOCKET, ProtocolMessages.getString("ProtocolLauncher.57")); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_SESSION_ID, ProtocolMessages.getString("ProtocolLauncher.58")); //$NON-NLS-1$
		ERRORS.put(ERROR_PDF_SHADOW_ATTACK, ProtocolMessages.getString("ProtocolLauncher.63")); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGNING_LTS_SIGNATURE, ProtocolMessages.getString("ProtocolLauncher.64")); //$NON-NLS-1$
		ERRORS.put(ERROR_INCOMPATIBLE_KEY_TYPE, ProtocolMessages.getString("ProtocolLauncher.66")); //$NON-NLS-1$
		ERRORS.put(ERROR_LOCKED_KEYSTORE, ProtocolMessages.getString("ProtocolLauncher.68")); //$NON-NLS-1$
		ERRORS.put(ERROR_UNKNOWN, ProtocolMessages.getString("ProtocolLauncher.69")); //$NON-NLS-1$
	}

	private static final Dictionary<ErrorCode, String> OLD_ERRORS_ASSOCIATION = new Hashtable<>();

	static {
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Functional.CANCELLED_OPERATION, CANCEL_RESPONSE);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Internal.LOADING_DATA_ERROR, ERROR_CANNOT_READ_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.REQUEST_URI_NOT_FOUND, ERROR_NULL_URI);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.UNSUPPORTED_REQUEST_SCHEME, ERROR_UNSUPPORTED_PROTOCOL);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.PORTS_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.INVALID_FORMAT_SIGN_BATCH_PARAM, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.INVALID_FORMAT_SIGNATURE_PARAM, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.DATA_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.DATA_TO_SAVE_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.FILE_EXTENSION_TO_SAVE_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.FILENAME_TO_SAVE_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.ID_SESSION_TO_SIGN_BATCH_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_COUNTERSIGNATURE_INDEX, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_PARAMS_TO_PRESIGN, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_PARAMS_TO_POSTSIGN, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_PRESIGN_BATCH_URL, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_POSTSIGN_BATCH_URL, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_RETRIEVE_URL_TO_SIGN, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_SESSION_ID_TO_SAVE, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_SESSION_ID_TO_SELECT_CERT, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_SESSION_ID_TO_SIGN, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_SESSION_ID_TO_SIGN_BATCH, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_STORAGE_URL_TO_SAVE, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_STORAGE_URL_TO_SELECT_CERT, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_STORAGE_URL_TO_SIGN, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_STORAGE_URL_TO_SIGN_BATCH, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.INVALID_TIMESTAMP_HASH_ALGORITHM, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.PRESIGN_BATCH_URL_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.POSTSIGN_BATCH_URL_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.RETRIEVE_URL_TO_SAVE_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.RETRIEVE_URL_TO_SIGN_CANT_BE_LOCAL, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.RETRIEVE_URL_TO_SIGN_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.SIGNATURE_ALGORITHM_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.SIGNATURE_FORMAT_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.STORAGE_URL_TO_SAVE_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.STORAGE_URL_TO_SELECT_CERT_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.STORAGE_URL_TO_SIGN_BATCH_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.STORAGE_URL_TO_SIGN_NOT_FOUND, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.UNSUPPORTED_CIPHER_KEY, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.UNSUPPORTED_COUNTERSIGN_CONFIG, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.UNSUPPORTED_POLICY_HASH_ALGORITHM, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.UNSUPPORTED_SIGNATURE_ALGORITHM, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.MALFORMED_REQUEST_TO_SOCKET, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(BatchErrorCode.Request.INVALID_PARAMS_TO_PRESIGN_JSON_BATCH, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(BatchErrorCode.Request.INVALID_PARAMS_TO_PRESIGN_XML_BATCH, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.UNSUPPORTED_OPERATION, ERROR_UNSUPPORTED_OPERATION);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.CANT_SAVE_FILE, ERROR_CANNOT_SAVE_DATA);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.UNSUPPORTED_SIGNATURE_FORMAT, ERROR_UNSUPPORTED_FORMAT);
		OLD_ERRORS_ASSOCIATION.put(KeyStoreErrorCode.Internal.LOADING_KEYSTORE_INTERNAL_ERROR, ERROR_CANNOT_ACCESS_KEYSTORE);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Internal.UNKNOWN_SIGNING_ERROR, ERROR_SIGNATURE_FAILED);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Internal.SIGNING_PKCS1_ERROR, ERROR_SIGNATURE_FAILED);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.UNKNOWN_SIGNING_BY_SOCKETS_ERROR, ERROR_SIGNATURE_FAILED);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.SENDING_RESULT_OPERATION, ERROR_SENDING_RESULT);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.SENDING_RESULT_OPERATION_BY_SOCKET, ERROR_SENDING_RESULT);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ENCRIPTING_SIGNATURE, ERROR_ENCRIPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ENCRIPTING_SIGNING_CERT, ERROR_ENCRIPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ENCRIPTING_SIGNATURE_EXTRA_DATA, ERROR_ENCRIPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ENCRIPTING_SELECTED_CERT, ERROR_ENCRIPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ENCRIPTING_BATCH_RESULT, ERROR_ENCRIPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ENCRIPTING_BATCH_SIGNING_CERT, ERROR_ENCRIPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ENCRYPTING_PARAMS_ERROR, ERROR_ENCRIPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.LOCAL_RETRIEVE_URL, ERROR_LOCAL_ACCESS_BLOCKED);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.LOCAL_PRESIGN_BATCH_URL, ERROR_LOCAL_ACCESS_BLOCKED);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.LOCAL_POSTSIGN_BATCH_URL, ERROR_LOCAL_ACCESS_BLOCKED);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.LOCAL_STORAGE_URL_TO_SAVE, ERROR_LOCAL_ACCESS_BLOCKED);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.LOCAL_STORAGE_URL_TO_SELECT_CERT, ERROR_LOCAL_ACCESS_BLOCKED);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.LOCAL_STORAGE_URL_TO_SIGN, ERROR_LOCAL_ACCESS_BLOCKED);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Request.LOCAL_STORAGE_URL_TO_SIGN_BATCH, ERROR_LOCAL_ACCESS_BLOCKED);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.NEEDS_UPDATED_VERSION, ERROR_OBSOLETE_APP);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.DECRYPTING_PARAMS_ERROR, ERROR_DECRYPTING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.ERROR_RECIVED_FROM_CLIENT, ERROR_RECOVERING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.RECIVING_DATA_OF_SIGN_OPERATION, ERROR_RECOVERING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.RECIVING_DATA_OF_CERT_OPERATION, ERROR_RECOVERING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.RECIVING_DATA_OF_BATCH_OPERATION, ERROR_RECOVERING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.RECIVING_DATA_OF_LOAD_OPERATION, ERROR_RECOVERING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.RECIVING_DATA_OF_SAVE_OPERATION, ERROR_RECOVERING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.RECIVING_DATA_OF_SIGN_AND_SAVE_OPERATION, ERROR_RECOVERING_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Functional.CANT_IDENTIFY_SIGNATURE_FORMAT, ERROR_UNKNOWN_SIGNER);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Internal.ENCODING_SIGNING_CERTIFICATE, ERROR_DECODING_CERTIFICATE);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Functional.NO_CERTS_FOUND_SIGNING, ERROR_NO_CERTIFICATES_KEYSTORE);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Functional.NO_CERTS_FOUND_SELECTING_CERT, ERROR_NO_CERTIFICATES_KEYSTORE);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Functional.NO_CERTS_FOUND_SIGNING_BATCH, ERROR_NO_CERTIFICATES_KEYSTORE);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.INTERNAL_LOCAL_BATCH_ERROR, ERROR_LOCAL_BATCH_SIGN);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.UNSUPPORTED_PROTOCOL_VERSION, ERROR_UNSUPPORTED_PROCEDURE);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Functional.SIGNING_WITH_POLICY_INCOMPATIBILITY, ERROR_INVALID_POLICY);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.CANT_LOAD_FILE, ERROR_CANNOT_LOAD_DATA);
		OLD_ERRORS_ASSOCIATION.put(BatchErrorCode.Communication.JSON_BATCH_PRESIGN_COMMUNICATION_ERROR, ERROR_CONTACT_BATCH_SERVICE);
		OLD_ERRORS_ASSOCIATION.put(BatchErrorCode.Communication.XML_BATCH_PRESIGN_COMMUNICATION_ERROR, ERROR_CONTACT_BATCH_SERVICE);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.INTERNAL_JSON_BATCH_ERROR, ERROR_BATCH_SIGNATURE);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.INTERNAL_XML_BATCH_ERROR, ERROR_BATCH_SIGNATURE);
		OLD_ERRORS_ASSOCIATION.put(BatchErrorCode.ThirdParty.XML_BATCH_PRESIGN_ERROR, ERROR_BATCH_SIGNATURE);
		OLD_ERRORS_ASSOCIATION.put(BatchErrorCode.ThirdParty.XML_BATCH_POSTSIGN_ERROR, ERROR_BATCH_SIGNATURE);
		OLD_ERRORS_ASSOCIATION.put(PdfErrorCode.Functional.PDF_DOCUMENT_NEEDED, ERROR_INVALID_PDF);
		OLD_ERRORS_ASSOCIATION.put(XMLErrorCode.Functional.XML_DOCUMENT_NEEDED, ERROR_INVALID_XML);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Functional.INVALID_FORMAT_FILE, ERROR_INVALID_DATA);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Functional.COMPATIBLE_SIGNATURE_NOT_FOUND, ERROR_NO_SIGN_DATA);
		OLD_ERRORS_ASSOCIATION.put(XAdESErrorCode.Functional.FACTURAE_ALREADY_SIGNED, ERROR_FACE_ALREADY_SIGNED);
		OLD_ERRORS_ASSOCIATION.put(PdfErrorCode.Internal.SIGNING_PDF_WITH_BAD_PASSWORD, ERROR_PDF_WRONG_PASSWORD);
		OLD_ERRORS_ASSOCIATION.put(PdfErrorCode.Internal.SIGNING_PDF_WITHOUT_PASSWORD, ERROR_PDF_WRONG_PASSWORD);
		OLD_ERRORS_ASSOCIATION.put(PdfErrorCode.Internal.SIGNING_PDF_WITH_UNREGISTER_SIGNATURES, ERROR_PDF_UNREG_SIGN);
		OLD_ERRORS_ASSOCIATION.put(PdfErrorCode.Internal.SIGNING_PDF_WITH_CERTIFIED_SIGN, ERROR_PDF_CERTIFIED);
		OLD_ERRORS_ASSOCIATION.put(XAdESErrorCode.Functional.FACTURAE_NEEDED, ERROR_INVALID_FACTURAE);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Functional.INVALID_SIGNATURE, ERROR_INVALID_SIGNATURE);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Functional.MINIMUM_VERSION_NON_SATISTIED, ERROR_MINIMUM_VERSION_NON_SATISTIED);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Functional.VISIBLE_SIGNATURE_IS_MANDATORY, ERROR_VISIBLE_SIGNATURE);
		OLD_ERRORS_ASSOCIATION.put(BinaryErrorCode.Functional.SIGNATURE_DOESNT_CONTAIN_DATA, ERROR_SIGN_WITHOUT_DATA);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.SOCKET_INITIALIZING_ERROR, ERROR_CANNOT_OPEN_SOCKET);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Request.INVALID_SESSION_ID, ERROR_INVALID_SESSION_ID);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Internal.INVALID_SIGNING_KEY, ERROR_INCOMPATIBLE_KEY_TYPE);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Functional.KEYSTORE_LOCKED, ERROR_LOCKED_KEYSTORE);
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Functional.SMARTCARD_LOCKED, ERROR_LOCKED_KEYSTORE);

		// Codigos sin asociacion directa, pero que se devolvian antes
		OLD_ERRORS_ASSOCIATION.put(ErrorCode.Internal.UNKNOWN_SIGNING_ERROR, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.UNKNOWN_SELECTING_CERT_ERROR, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.UNKNOWN_BATCH_ERROR, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.UNKNOWN_LOADING_DATA_ERROR, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Internal.UNKNOWN_SAVING_DATA_ERROR, ERROR_PARAMS);
		OLD_ERRORS_ASSOCIATION.put(SimpleErrorCode.Communication.READING_FROM_SOCKET, ERROR_PARAMS);
	}

	static void showError(final ProtocolVersion protocolVersion, final AOControlledException controlledException) {
		showError(protocolVersion, controlledException.getErrorCode());
	}

	private static final ProtocolVersion PROTOCOL_VERSION_WITH_ERROR_CODES = ProtocolVersion.getInstance(ProtocolVersion.VERSION_4_1);
	
	static void showError(final ProtocolVersion protocolVersion, final ErrorCode errorCode) {
		showError(protocolVersion, errorCode, null);
	}

	static void showError(final ProtocolVersion protocolVersion, final ErrorCode errorCode, String ... params) {

		ProgressInfoDialogManager.hideProgressDialog();

		 String message = getText(errorCode);
		
		 if (params != null) {
	            for (int i = 0; i < params.length; i++) {
	                if (params[i] != null) {
	                    message = message.replace("%" + i, params[i]); //$NON-NLS-1$
	                }
	            }
	        }

		if (HEADLESS) {
			LOGGER.warning("No se ha encontrado interfaz grafica para mostrar el error " //$NON-NLS-1$
					+ AUTOFIRMA_ERROR_PREFIX + errorCode.getCode() + ": " + message); //$NON-NLS-1$
		}
		else {
			// En macOS reclamamos el foco
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MacUtils.focusApplication();
			}

			// Mostramos el error al usuario. Lo haremos de una forma u otra segun si el protocolo lo soporta o no
			final String title = protocolVersion != null && protocolVersion.hasSupportTo(PROTOCOL_VERSION_WITH_ERROR_CODES)
					? ProtocolMessages.getString("ProtocolLauncher.67") //$NON-NLS-1$
					: ProtocolMessages.getString("ProtocolLauncher.29", getErrorCodeWithPrefix(errorCode)); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				message,
				title,
				AOUIFactory.ERROR_MESSAGE,
				null
			);
		}
	}

	static String getErrorMessage(final ProtocolVersion protocolVersion, final ErrorCode errorCode) {

		String message = null;

		// Si se utiliza el protocolo 4.1 o superior, se utiliza el formato de mensaje nuevo, pero
		// con una cabecera compatible con el formato antiguo para mantener la compatibilidad
		if (protocolVersion != null && protocolVersion.hasSupportTo(PROTOCOL_VERSION_WITH_ERROR_CODES)) {
			// Establecemos una cabecera de error compatible con la usada en versiones anteriores del protocolo.
			// Aunque no transmite informacion, permite que el receptor del error lo identifique como tal
			final String code = AUTOFIRMA_ERROR_PREFIX + errorCode.getCode();
			final String prefix = errorCode.equals(ErrorCode.Functional.CANCELLED_OPERATION)
					? CANCELLATION_ERROR_PREFIX
							: DEFAULT_ERROR_PREFIX;

			message = prefix + code + " - " + errorCode.getDescription(); //$NON-NLS-1$
		}
		// En caso contrario, el codigo de error tiene un valor antiguo
		// asociado, se utiliza el formato de mensaje antiguo
		else {
			String code = OLD_ERRORS_ASSOCIATION.get(errorCode);
			if (code == null) {
				code = ERROR_UNKNOWN;
			}

			if (CANCEL_RESPONSE.equals(code)) {
				message = code;
			} else {
				message = code + ": " + ERRORS.get(code); //$NON-NLS-1$
			}
		}

		return message;
	}

	private static String getText(final ErrorCode errorCode) {

		String message;
		final String textKey = "Error." + errorCode.getCode(); //$NON-NLS-1$
		if (errorCode.checkType(ErrorCode.ERROR_HARDWARE)) {
			message = ProtocolMessages.getTargetString(textKey, "Error.1XXXXX"); //$NON-NLS-1$
		} else if (errorCode.checkType(ErrorCode.ERROR_INTERNAL)) {
			message = ProtocolMessages.getTargetString(textKey, "Error.2XXXXX"); //$NON-NLS-1$
		} if (errorCode.checkType(ErrorCode.ERROR_THIRD_PARTY)) {
			message = ProtocolMessages.getTargetString(textKey, "Error.3XXXXX"); //$NON-NLS-1$
		} else if (errorCode.checkType(ErrorCode.ERROR_COMMUNICATION)) {
			message = ProtocolMessages.getTargetString(textKey, "Error.4XXXXX"); //$NON-NLS-1$
		} else if (errorCode.checkType(ErrorCode.ERROR_FUNCTIONAL)) {
			message = ProtocolMessages.getTargetString(textKey, "Error.5XXXXX"); //$NON-NLS-1$
		} else if (errorCode.checkType(ErrorCode.ERROR_REQUEST)) {
			if (errorCode.checkType(ErrorCode.ERROR_REQUEST_FROM_BROWSER)) {
				message = ProtocolMessages.getTargetString(textKey, "Error.62XXXX"); //$NON-NLS-1$
			} else {
				message = ProtocolMessages.getTargetString(textKey, "Error.6XXXXX"); //$NON-NLS-1$
			}
		} else {
			message = ProtocolMessages.getTargetString(textKey, "Error"); //$NON-NLS-1$
		}

		if (message == null) {
			message = errorCode.getDescription();
		}

		return message;
	}

	/**
	 * Devuelve el codigo de error con el prefijo que le corresponde.
	 * @param errorCode C&oacute;digo de error.
	 * @return C&oacute;digo con el prefijo.
	 */
	public static String getErrorCodeWithPrefix(final ErrorCode errorCode) {
		return AUTOFIRMA_ERROR_PREFIX + errorCode.getCode();
	}
}
