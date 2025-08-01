package es.gob.afirma.standalone.protocol;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidSignatureFormatException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.pades.InvalidPdfException;
import es.gob.afirma.signers.pades.common.BadPdfPasswordException;
import es.gob.afirma.signers.pades.common.PdfHasUnregisteredSignaturesException;
import es.gob.afirma.signers.pades.common.PdfIsCertifiedException;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;
import es.gob.afirma.signers.pkcs7.ContainsNoDataException;
import es.gob.afirma.signers.xades.EFacturaAlreadySignedException;
import es.gob.afirma.signers.xades.InvalidEFacturaDataException;
import es.gob.afirma.signers.xml.InvalidXMLException;
import es.gob.afirma.signvalidation.InvalidSignatureException;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.plugins.SignOperation;
import es.gob.afirma.standalone.protocol.SingleSignOperation.Operation;

public class LocalBatchSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String DONE_SIGN = "DONE_AND_SAVED"; //$NON-NLS-1$
	private static final String SKIPPED_SIGN = "SKIPPED"; //$NON-NLS-1$
	private static final String ERROR_SIGN = "ERROR_PRE"; //$NON-NLS-1$

	private static final String EXTRAPARAM_HEADLESS = "headless"; //$NON-NLS-1$

	/**
	 * Metodo encargado de firmar todas las firmas monof&aacute;sicas que conforman un lote.
	 * @param batchConfig Configuraci&oacute;n del lote de firma.
	 * @param pke Clave privada usada por el certificado.
	 * @return Resultado del proceso.
	 */
	public static String signLocalBatch(final BatchSignOperation batchConfig, final PrivateKeyEntry pke) {

		final List<LocalSingleBatchResult> results = new ArrayList<>();

		boolean errorOcurred = false;
		for (final SingleSignOperation singleConfig : batchConfig.getSigns()) {

			if (errorOcurred && batchConfig.isStopOnError()) {
				results.add(new LocalSingleBatchResult(singleConfig.getDocId()));
			} else {
				try {
					final byte[] signature = processSign(singleConfig, pke);
					results.add(new LocalSingleBatchResult(singleConfig.getDocId(), signature));
				}
				catch (final SocketOperationException e) {
					errorOcurred = true;
					LOGGER.severe("Error al procesar la firma de lotes monofasica con ID: " + LoggerUtil.getTrimStr(singleConfig.getDocId()) + "Excepcion :" + e); //$NON-NLS-1$ //$NON-NLS-2$

					// Si se debia detener la ejecucion en caso de error, ademas de
					// no hacer mas firmas, se cambiara el estado de las firmas previas
					if (batchConfig.isStopOnError()) {
						for (final LocalSingleBatchResult singleResult : results) {
							singleResult.setResult(SKIPPED_SIGN);
							singleResult.setSignature(null);
						}
					}

					// Agreamos ahora el error de la firma actual
					results.add(new LocalSingleBatchResult(singleConfig.getDocId(), e.getMessage()));
				}
			}
		}



		return JSONBatchManager.buildBatchResultJson(results);
	}

	private static byte[] processSign(final SingleSignOperation singleConfig, final PrivateKeyEntry pke) throws SocketOperationException {

		final Operation cryptoOperation = singleConfig.getCryptoOperation();
		final byte[] data = singleConfig.getData();
		final String algorithm = singleConfig.getAlgorithm();
    	final String keyType = pke.getPrivateKey().getAlgorithm();

    	// Seleccionamos el algoritmo de firma
		String signatureAlgorithm;
		try {
			signatureAlgorithm = AOSignConstants.composeSignatureAlgorithmName(algorithm, keyType);
		}
		catch (final Exception e) {
			final ErrorCode errorCode = ErrorCode.Internal.INVALID_SIGNING_KEY;
			throw new SocketOperationException(e, errorCode);
		}

		final String format = singleConfig.getFormat();
		final Properties extraParams = singleConfig.getExtraParams();

		// En caso de que no se haya solicitado una operacion de multifirma con
		// el formato AUTO, configuramos el servidor en base al nombre de formato
		AOSigner signer = null;
		if (!AOSignConstants.SIGN_FORMAT_AUTO.equalsIgnoreCase(format)) {
			signer = AOSignerFactory.getSigner(format);
			if (signer == null) {
				LOGGER.severe("No se soporta el formato de firma indicado: " + LoggerUtil.getTrimStr(format)); //$NON-NLS-1$
				final ErrorCode errorCode = ErrorCode.Request.UNSUPPORTED_SIGNATURE_FORMAT;
				throw new SocketOperationException(errorCode);
			}
		}

		// No haber fijado aun el firmador significa que se selecciono el formato AUTO y
		// es necesario identificar cual es el que se deberia usar
		if (signer == null) {
			final String signFormat = ProtocolInvocationLauncherUtil.identifyFormatFromData(data, SignOperation.Operation.valueOf(cryptoOperation.name()));
			if (signFormat == null) {
				LOGGER.severe("Los datos no se corresponden con una firma electronica o no se pudieron analizar"); //$NON-NLS-1$
				final ErrorCode errorCode = SimpleErrorCode.Functional.CANT_IDENTIFY_SIGNATURE_FORMAT;
				throw new SocketOperationException(errorCode);
			}
			signer = AOSignerFactory.getSigner(signFormat);
		}

		// Hacemos una copia del extraParam y le agregamos la propiedad "headless" para
		// evitar que se muestren dialogos graficos que interrumpan la ejecucion
		final Properties extraParamsCopy = new Properties();
		if (extraParams != null) {
			extraParamsCopy.putAll(extraParams);
		}
		extraParamsCopy.setProperty(EXTRAPARAM_HEADLESS, Boolean.TRUE.toString());

		final byte[] sign;
		try {
			try {
				switch (cryptoOperation) {
				case SIGN:
					sign = signer.sign(
							data,
							signatureAlgorithm,
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParamsCopy
							);
					break;
				case COSIGN:
					sign = signer.cosign(
							data,
							signatureAlgorithm,
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParamsCopy
							);
					break;
				case COUNTERSIGN:
					sign = signer.countersign(
							data,
							signatureAlgorithm,
							CounterSignTarget.getTarget(extraParamsCopy.getProperty(AfirmaExtraParams.TARGET)) == CounterSignTarget.TREE ?
									CounterSignTarget.TREE : CounterSignTarget.LEAFS,
									null, // Targets
									pke.getPrivateKey(),
									pke.getCertificateChain(),
									extraParamsCopy
							);
					break;
				default:
					LOGGER.severe("Error al realizar la operacion firma"); //$NON-NLS-1$
					throw new SocketOperationException(SimpleErrorCode.Request.UNSUPPORTED_OPERATION);
				}
			}
			catch (final AOTriphaseException tex) {
				throw ProtocolInvocationLauncherUtil.getInternalException(tex);
			}
		} catch (final SocketOperationException e) {
			throw e;
		}
		catch (final IllegalArgumentException e) {
			LOGGER.log(Level.SEVERE, "Se ha recibido algun parametro de firma invalido o no compatible", e); //$NON-NLS-1$
			final ErrorCode errorCode = SimpleErrorCode.Request.INVALID_FORMAT_SIGNATURE_PARAM;
			throw new SocketOperationException(e, errorCode);
		}
		catch (final AOTriphaseException e) {
			LOGGER.log(Level.SEVERE, "Error durante la operacion de firma trifasica", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final InvalidPdfException e) {
			LOGGER.log(Level.SEVERE, "Los datos no son un documento PDF", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final InvalidXMLException e) {
			LOGGER.log(Level.SEVERE, "Los datos no son un XML", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final AOFormatFileException e) {
			LOGGER.log(Level.SEVERE, "El formato de los datos no es valido para esta operacion", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final InvalidEFacturaDataException e) {
			LOGGER.log(Level.SEVERE, "Los datos no son una factura electronica", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final EFacturaAlreadySignedException e) {
			LOGGER.log(Level.SEVERE, "La factura electronica ya estaba firmada", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final ContainsNoDataException e) {
			LOGGER.log(Level.SEVERE, "La firma no contiene los datos a firmar ni un hash que se pueda reutilizar", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final AOInvalidSignatureFormatException e) {
			LOGGER.log(Level.SEVERE, "La firma no es compatible con el formato de firma utilizado", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final PdfHasUnregisteredSignaturesException e) {
			LOGGER.log(Level.SEVERE, "El PDF tiene firmas sin registrar", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final PdfIsCertifiedException e) {
			LOGGER.log(Level.SEVERE, "El PDF esta certificado", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final BadPdfPasswordException | PdfIsPasswordProtectedException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final UnsupportedOperationException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma", e); //$NON-NLS-1$
			throw new SocketOperationException(e, SimpleErrorCode.Request.UNSUPPORTED_OPERATION);
		}
		catch (final InvalidSignatureException e) {
			LOGGER.log(Level.SEVERE, "La firma de entrada no es valida", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final AOException e) {
			LOGGER.log(Level.SEVERE, "Error al realizar la operacion de firma local de lote", e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error desconocido al realizar la operacion de firma local de lote", e); //$NON-NLS-1$
			final ErrorCode errorCode = SimpleErrorCode.Internal.INTERNAL_LOCAL_BATCH_ERROR;
			throw new SocketOperationException(e, errorCode);
		}

		return sign;
	}

	static class LocalSingleBatchResult {

		private String docId;
		private String result;
		private String description;
		private byte[] signature;



		public LocalSingleBatchResult(final String docId) {
			this.docId = docId;
			this.result = SKIPPED_SIGN;
		}

		public LocalSingleBatchResult(final String docId, final byte[] signature) {
			this.docId = docId;
			this.result = DONE_SIGN;
			this.signature = signature;
		}

		public LocalSingleBatchResult(final String docId, final String description) {
			this.docId = docId;
			this.result = ERROR_SIGN;
			this.description = description;
		}

		public void setDocId(final String docId) {
			this.docId = docId;
		}
		public String getDocId() {
			return this.docId;
		}

		public void setResult(final String result) {
			this.result = result;
		}

		public String getResult() {
			return this.result;
		}

		public void setDescription(final String description) {
			this.description = description;
		}

		public String getDescription() {
			return this.description;
		}

		public void setSignature(final byte[] signature) {
			this.signature = signature;
		}

		public byte[] getSignature() {
			return this.signature;
		}
	}
}
