/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.AccessController;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.security.cert.CertificateEncodingException;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.UIManager;

import org.mozilla.universalchardet.UniversalDetector;

import com.dmurph.tracking.AnalyticsConfigData;
import com.dmurph.tracking.JGoogleAnalyticsTracker;
import com.dmurph.tracking.JGoogleAnalyticsTracker.GoogleAnalyticsVersion;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;
import es.gob.afirma.crypto.jarverifier.JarSignatureCertExtractor;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.SmartCardException;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.signers.batch.client.BatchSigner;

/** MiniApplet de firma del proyecto Afirma. */
public final class MiniAfirmaApplet extends JApplet implements MiniAfirma {

	private static final long serialVersionUID = -4364574240099120486L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String GOOGLE_ANALYTICS_TRACKING_CODE = "UA-41615516-2"; //$NON-NLS-1$

	private static final String EOF = "%%EOF%%"; //$NON-NLS-1$
	private static final String DEFAULT_CHUNK_ENCODING = "ISO-8859-1";  //$NON-NLS-1$

	private static final String APPLET_PARAM_USER_AGENT = "userAgent"; //$NON-NLS-1$

	private static final String APPLET_PARAM_SYSTEM_PROPERTIES = "system_properties"; //$NON-NLS-1$

	private static final String APPLET_PARAM_LOCALE = "locale"; //$NON-NLS-1$

	private static final String APPLET_PARAM_USER_KEYSTORE = "keystore"; //$NON-NLS-1$

	private static final String SIGNATURE_FORMAT_AUTO = "AUTO"; //$NON-NLS-1$

	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$
	private static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$
	private static final String CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	/** N&uacute;mero m&aacute;ximo de caracteres que se devuelven en cualquiera de los
	 * m&eacute;todos del Applet. */
	private static final int BUFFER_SIZE = 1024 * 1024;	// 1 Mb

	private final StringBuilder dataStore = new StringBuilder();

	/** Flujo de datos utilizado para la lectura de datos en fragmentos. */
	private InputStream chunkedReturnStream;

	/** Clave privada fijada para reutilizarse en operaciones sucesivas. */
	private PrivateKeyEntry stickyKeyEntry = null;

	private boolean stickySignatory = false;

	/** Identificador del navegador Web que carga el applet. */
	private String userAgent = null;

	/** Tipo de almac&eacute;n de claves del que extraer los certificados. */
	private AOKeyStore keystoreType = null;

	/** Ruta de la biblioteca o almac&eacute;n del que extraer los certificados. */
	private String keystoreLib = null;

	/** Mensaje del &uacute;ltimo error producido. */
	private String errorMessage = null;

	/** Tipoe del &uacute;ltimo error producido. */
	private String errorType = null;

	static {
		try {
			LogManager.install(App.MINIAPPLET);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No ha sido posible instalar el gestor de registro: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** {@inheritDoc} */
	@Override
	public void setStickySignatory(final boolean sticky) {
		this.stickySignatory = sticky;
		if (!sticky) {
			this.stickyKeyEntry = null;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String sign(final String algorithm,
					   final String format,
					   final String extraParams) throws PrivilegedActionException,
			                                            IOException,
			                                            AOException,
			                                            CertificateEncodingException,
			                                            IncompatiblePolicyException {

		LOGGER.info("Solicitada firma con algoritmo " + algorithm + " y formato " + format); //$NON-NLS-1$ //$NON-NLS-2$

		clearError();

		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);

		byte[] dataBinary;
		if (this.dataStore.length() > 0) {
			try {
				dataBinary = Base64.decode(this.dataStore.toString());
			}
			catch (final IOException e) {
				setError(e, "Los datos proporcionados est\u00E1n mal codificados en base 64"); //$NON-NLS-1$
				throw e;
			}
			catch (final OutOfMemoryError e) {
				setError(e, "Error de falta de memoria durante la carga de los datos"); //$NON-NLS-1$
				throw e;
			}
			finally {
				this.dataStore.setLength(0);
			}
		}
		else {
			final String fileExts = params.getProperty("filenameExts", null); //$NON-NLS-1$
			final String fileDesc = fileExts == null ?
					MiniAppletMessages.getString("MiniAfirmaApplet.1") : //$NON-NLS-1$
						String.format(
								MiniAppletMessages.getString("MiniAfirmaApplet.6"), //$NON-NLS-1$
								fileExts.replace(",", ",*.")); //$NON-NLS-1$ //$NON-NLS-2$

			try {
				dataBinary = AccessController.doPrivileged(new GetFileContentAction(
						MiniAppletMessages.getString("MiniAfirmaApplet.0"), //$NON-NLS-1$
						fileExts != null ? fileExts.split(",") : null, //$NON-NLS-1$
						fileDesc,
						this));
			}
			catch (final AOCancelledOperationException e) {
				setError(e);
				throw e;
			}
			catch (final java.util.concurrent.CancellationException e) {
				final AOCancelledOperationException ce = new AOCancelledOperationException("Operacion cancelada por el usuario", e); //$NON-NLS-1$
				setError(ce);
				throw ce;
			}
			catch (final PrivilegedActionException e) {
				setError(e);
				throw e;
			}
			catch (final OutOfMemoryError e) {
				setError(e, "Error de falta de memoria durante la carga del fichero de datos"); //$NON-NLS-1$
				throw e;
			}
			catch (final Error e) {
				setError(e);
				throw e;
			}
		}

		// Informacion sobre los parametros adicionales indicados
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final PrintStream ps = new PrintStream(baos);
		params.list(ps);
		LOGGER.info("Recibidos los siguientes parametros adicionales:\n" + baos.toString()); //$NON-NLS-1$

		try {
			String signatureFormat = MiniAfirmaApplet.cleanParam(format);
			final AOSigner signer = MiniAfirmaApplet.selectSigner(signatureFormat, dataBinary, null);
			if (SIGNATURE_FORMAT_AUTO.equalsIgnoreCase(signatureFormat)) {
				signatureFormat = ExtraParamsProcessor.getSignFormat(signer);
				ExtraParamsProcessor.configAutoFormat(signer, dataBinary, params);
			}

			// XXX: Codigo para la identificacion de firmas XAdES enveloped explicita
			// (Eliminar cuando se abandone el soporte de XAdES explicitas)
			if (isXadesEnvelopedExplicit(signatureFormat, params)) {
				final IllegalArgumentException e = new IllegalArgumentException(
						"El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
				setError(e);
				throw e;
			}

			// XXX: Codigo de soporte de firmas XAdES explicitas (Eliminar cuando se abandone el soporte de XAdES explicitas)
			if (isXadesExplicitConfigurated(signatureFormat, params)) {
				LOGGER.warning(
					"Se ha pedido una firma XAdES explicita, este formato dejara de soportarse en proximas versiones" //$NON-NLS-1$
				);
				try {
					dataBinary = MessageDigest.getInstance("SHA1").digest(dataBinary); //$NON-NLS-1$
					params.setProperty("mimeType", "hash/sha1"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				catch (final Exception e) {
					LOGGER.warning("Error al generar la huella digital de los datos para firmar como 'XAdES explicit', " //$NON-NLS-1$
						+ "se realizara una firma XAdES corriente: " + e); //$NON-NLS-1$
				}
			}

			final PrivateKeyEntry pke = selectPrivateKey(params);
			final byte[] signature = AccessController.doPrivileged(
					new SignAction(
						signer,
						dataBinary,
						MiniAfirmaApplet.cleanParam(algorithm),
						pke,
						ExtraParamsProcessor.expandProperties(params, dataBinary, signatureFormat)
					)
				);

			// El base 64 es un 30% mayor que los datos originales, asi que calculamos el tamano al alza en base al tamano de la firma
			// (normalmente mayor que el del certificado)
			final StringBuilder result = new StringBuilder((int) Math.floor(signature.length * 1.4));
			return chunkReturn(
				result.
					append(Base64.encode(pke.getCertificate().getEncoded())).
					append('|').
					append(Base64.encode(signature)).toString()
			);
		}
		catch (final IncompatiblePolicyException e) {
			setError(e);
			throw e;
		}
		catch (final AOFormatFileException e) {
			setError(e);
			throw e;
		}
		catch (final CertificateEncodingException e) {
			setError(e);
			throw e;
		}
		catch (final SmartCardException e) {
			setError(e);
			throw e;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		catch (final OutOfMemoryError e) {
			setError(e, "Error de falta de memoria durante la operaci\u00F3n de firma"); //$NON-NLS-1$
			throw e;
		}
		catch (final Error e) {
			setError(e);
			throw new AOException("Ocurrio un error grave durante la operaci\u00F3n de firma", e); //$NON-NLS-1$
		}

	}

	/** {@inheritDoc} */
	@Override
	public String coSign(final String dataB64,
			             final String algorithm,
			             final String format,
			             final String extraParams) throws PrivilegedActionException,
			             							      IOException,
				                                          AOException,
				                                          CertificateEncodingException,
				                                          IncompatiblePolicyException {
		clearError();

		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);

		byte[] signature;
		if (this.dataStore.length() > 0) {
			try {
				signature = Base64.decode(this.dataStore.toString());
			}
			catch (final IOException e) {
				setError(e, "La firma proporcionada est\u00E1 mal codificada en base 64"); //$NON-NLS-1$
				throw e;
			}
			catch (final OutOfMemoryError e) {
				setError(e, "Error de falta de memoria durante la carga de la firma"); //$NON-NLS-1$
				throw e;
			}
			finally {
				this.dataStore.setLength(0);
			}
		}
		else {

			final String fileExts = params.getProperty("filenameExts", null); //$NON-NLS-1$
			final String fileDesc = fileExts == null ?
					MiniAppletMessages.getString("MiniAfirmaApplet.1") : //$NON-NLS-1$
						String.format(
								MiniAppletMessages.getString("MiniAfirmaApplet.6"), //$NON-NLS-1$
								fileExts.replace(",", ",*.")); //$NON-NLS-1$ //$NON-NLS-2$

			try {
				signature = AccessController.doPrivileged(new GetFileContentAction(
						MiniAppletMessages.getString("MiniAfirmaApplet.2"), //$NON-NLS-1$
						fileExts != null ? fileExts.split(",") : null, //$NON-NLS-1$,
						fileDesc,
						this));
			}
			catch (final AOCancelledOperationException e) {
				setError(e);
				throw e;
			}
			catch (final java.util.concurrent.CancellationException e) {
				final AOCancelledOperationException ce = new AOCancelledOperationException("Operacion cancelada por el usuario", e); //$NON-NLS-1$
				setError(ce);
				throw ce;
			}
			catch (final OutOfMemoryError e) {
				setError(e, "Error de falta de memoria durante la carga del fichero de firma"); //$NON-NLS-1$
				throw e;
			}
			catch (final PrivilegedActionException e) {
				setError(e);
				throw e;
			}
			catch (final Error e) {
				setError(e);
				throw new AOException("Ocurrio un error grave durante la operaci\u00F3n de firma", e); //$NON-NLS-1$
			}
		}

		final byte[] dataBinary;
		try {
			dataBinary = dataB64 == null ? null : Base64.decode(dataB64);
		}
		catch (final IOException e) {
			setError(e, "Los datos proporcionados est\u00E1n mal codificados en base 64"); //$NON-NLS-1$
			throw e;
		}
		catch (final OutOfMemoryError e) {
			setError(e, "Error de falta de memoria durante la carga de los datos"); //$NON-NLS-1$
			throw e;
		}
		catch (final Error e) {
			setError(e);
			throw new AOException("Ocurrio un error grave durante la operaci\u00F3n de firma", e); //$NON-NLS-1$
		}

		try {
			String signatureFormat = format;
			final AOSigner signer = MiniAfirmaApplet.selectSigner(MiniAfirmaApplet.cleanParam(signatureFormat), null, signature);
			if (SIGNATURE_FORMAT_AUTO.equalsIgnoreCase(signatureFormat)) {
				signatureFormat = ExtraParamsProcessor.getSignFormat(signer);
				ExtraParamsProcessor.configAutoFormat(signer, signature, params);
			}

			final PrivateKeyEntry pke = selectPrivateKey(params);
			final byte[] cosignature = AccessController.doPrivileged(
				new CoSignAction(
					signer,
					signature,
					dataBinary,
					MiniAfirmaApplet.cleanParam(algorithm),
					pke,
					ExtraParamsProcessor.expandProperties(params, dataBinary, signatureFormat)
				)
			);

			// El base 64 es un 30% mayor que los datos originales, asi que calculamos el tamano al alza en base al tamano de la firma
			// (normalmente mayor que el del certificado)
			final StringBuilder result = new StringBuilder((int) Math.floor(cosignature.length * 1.4));
			return chunkReturn(result.
				append(Base64.encode(pke.getCertificate().getEncoded())).
				append('|').
				append(Base64.encode(cosignature)).toString()
			);

		}
		catch (final IncompatiblePolicyException e) {
			setError(e);
			throw e;
		}
		catch (final AOFormatFileException e) {
			setError(e);
			throw e;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final CertificateEncodingException e) {
			setError(e);
			throw e;
		}
		catch (final SmartCardException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		catch (final OutOfMemoryError e) {
			setError(e, "Error de falta de memoria durante la operaci\u00F3n de cofirma"); //$NON-NLS-1$
			throw e;
		}
		catch (final Error e) {
			setError(e);
			throw new AOException("Ocurrio un error grave durante la operaci\u00F3n de cofirma", e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String counterSign(final String algorithm,
			                  final String format,
			                  final String extraParams) throws PrivilegedActionException,
			                  								   IOException,
					                                           AOException,
					                                           CertificateEncodingException,
					                                           IncompatiblePolicyException {
		clearError();

		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);

		byte[] signature;
		if (this.dataStore.length() > 0) {
			try {
				signature = Base64.decode(this.dataStore.toString());
			}
			catch (final IOException e) {
				setError(e, "La firma proporcionada est\u00E1 mal codificada en base 64"); //$NON-NLS-1$
				throw e;
			}
			catch (final OutOfMemoryError e) {
				setError(e, "Error de falta de memoria durante la carga de la firma"); //$NON-NLS-1$
				throw e;
			}
			finally {
				this.dataStore.setLength(0);
			}
		}
		else {

			final String fileExts = params.getProperty("filenameExts", null); //$NON-NLS-1$
			final String fileDesc = fileExts == null ?
					MiniAppletMessages.getString("MiniAfirmaApplet.1") : //$NON-NLS-1$
						String.format(
								MiniAppletMessages.getString("MiniAfirmaApplet.6"), //$NON-NLS-1$
								fileExts.replace(",", ",*.")); //$NON-NLS-1$ //$NON-NLS-2$

			try {
				signature = AccessController.doPrivileged(new GetFileContentAction(
						MiniAppletMessages.getString("MiniAfirmaApplet.2"), //$NON-NLS-1$
						fileExts != null ? fileExts.split(",") : null, //$NON-NLS-1$,
						fileDesc,
						this));
			}
			catch (final AOCancelledOperationException e) {
				setError(e);
				throw e;
			}
			catch (final java.util.concurrent.CancellationException e) {
				final AOCancelledOperationException ce = new AOCancelledOperationException("Operacion cancelada por el usuario", e); //$NON-NLS-1$
				setError(ce);
				throw ce;
			}
			catch (final PrivilegedActionException e) {
				setError(e);
				throw e;
			}
			catch (final OutOfMemoryError e) {
				setError(e, "Error de falta de memoria durante la carga del fichero de firma"); //$NON-NLS-1$
				throw e;
			}
		}

		try {
			String signatureFormat = format;
			final AOSigner signer = MiniAfirmaApplet.selectSigner(MiniAfirmaApplet.cleanParam(signatureFormat), null, signature);
			if (SIGNATURE_FORMAT_AUTO.equalsIgnoreCase(signatureFormat)) {
				signatureFormat = ExtraParamsProcessor.getSignFormat(signer);
				ExtraParamsProcessor.configAutoFormat(signer, signature, params);
			}

			final PrivateKeyEntry pke = selectPrivateKey(params);
			final byte[] countersignature = AccessController.doPrivileged(
				new CounterSignAction(
						signer,
						signature,
						MiniAfirmaApplet.cleanParam(algorithm),
						pke,
						ExtraParamsProcessor.expandProperties(params, null, signatureFormat)
				)
			);

			// El base 64 es un 30% mayor que los datos originales, asi que calculamos el tamano al alza en base al tamano de la firma
			// (normalmente mayor que el del certificado)
			final StringBuilder result = new StringBuilder((int) Math.floor(countersignature.length * 1.4));
			return chunkReturn(result.
					append(Base64.encode(pke.getCertificate().getEncoded())).
					append('|').
					append(Base64.encode(countersignature)).toString());
		}
		catch (final IncompatiblePolicyException e) {
			setError(e);
			throw e;
		}
		catch (final AOFormatFileException e) {
			setError(e);
			throw e;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final CertificateEncodingException e) {
			setError(e);
			throw e;
		}
		catch (final SmartCardException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		catch (final OutOfMemoryError e) {
			setError(e, "Error de falta de memoria durante la operaci\u00F3n de contrafirma"); //$NON-NLS-1$
			throw e;
		}
		catch (final Error e) {
			setError(e);
			throw new AOException("Ocurrio un error grave durante la operaci\u00F3n de contrafirma", e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public boolean saveDataToFile(final String title,
			                      final String fileName,
			                      final String extension,
			                      final String description) throws PrivilegedActionException,
			                                                       IOException {
		clearError();

		if (this.dataStore.length() < 1) {
			LOGGER.warning(
				"Se ha solicitado guardar en disco un contenido nulo, se ignorara la peticion" //$NON-NLS-1$
			);
			return false;
		}

		final String titleDialog = MiniAfirmaApplet.cleanParam(title);

		final String depuredExts = MiniAfirmaApplet.cleanParam(extension);
		final String[] exts = depuredExts == null ? null : new String[] { depuredExts };

		final String descFiles = MiniAfirmaApplet.cleanParam(description);

		try {
			AccessController.doPrivileged(
				new SaveFileAction(
						titleDialog,
						Base64.decode(this.dataStore.toString()),
						exts,
						descFiles,
						MiniAfirmaApplet.cleanParam(fileName),
						this
				)
			);
			return true;
		}
		catch (final AOCancelledOperationException e) {
			return false;
		}
		catch (final java.util.concurrent.CancellationException e) {
			return false;
		}
		catch (final IOException e) {
			setError(e, "Los datos proporcionados est\u00E1n mal codificados en base 64"); //$NON-NLS-1$
			throw e;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		finally {
			this.dataStore.setLength(0);
		}
	}

	@Override
	public String signAndSaveToFile(final String op, final String algorithm, final String format,
			final String extraParams, final String fileName)
			throws PrivilegedActionException, IOException, AOException,
			CertificateEncodingException, IncompatiblePolicyException {

		String result;
		String saveDialogtitle;
		if (op == null || CRYPTO_OPERATION_SIGN.equalsIgnoreCase(op)) {
			result = sign(algorithm, format, extraParams);
			saveDialogtitle = MiniAppletMessages.getString("MiniAfirmaApplet.3"); //$NON-NLS-1$
		}
		else if (CRYPTO_OPERATION_COSIGN.equalsIgnoreCase(op)) {
			result = coSign(null, algorithm, format, extraParams);
			saveDialogtitle = MiniAppletMessages.getString("MiniAfirmaApplet.4"); //$NON-NLS-1$
		}
		else if (CRYPTO_OPERATION_COUNTERSIGN.equalsIgnoreCase(op)) {
			result = counterSign(algorithm, format, extraParams);
			saveDialogtitle = MiniAppletMessages.getString("MiniAfirmaApplet.5"); //$NON-NLS-1$
		}
		else {
			throw new UnsupportedOperationException("La operacion criptografica no soportada: " + op); //$NON-NLS-1$
		}

		// Establecemos los datos que se deben guardar
		this.dataStore.append(result.substring(result.indexOf('|') + 1));

		// Solicitamos el guardado
		try {
			saveDataToFile(saveDialogtitle, fileName, null, MiniAppletMessages.getString("MiniAfirmaApplet.1")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo completar el guardado de la firma: " + e); //$NON-NLS-1$
		}

		return result;
	}

	/** {@inheritDoc} */
	@Override
	public String getFileNameContentBase64(final String title,
			                               final String extensions,
			                               final String description,
			                               final String filePath) throws PrivilegedActionException, IOException {
		clearError();
		// Se llama a setError() desde getFileNameContent, si es preciso, por lo qye no es necesario repetirlo aqui
		return chunkReturn(getFileNameContent(title, extensions, description, filePath, true));
	}

	/** Permite seleccionar al usuario un fichero y devuelve la tupla con el
	 * nombre de fichero y su contenido separados por ('|').
	 * @param title T&iacute;tulo de la ventana de selecci&oacute;n.
	 * @param extensions Extensiones de fichero permitidas separadas por coma (',').
	 * @param description Descripci&oacute;n del tipo de fichero.
	 * @param filePath Ruta por defecto.
	 * @param asBase64 Si es {@code true} devuelve el contenido en Base64, si es {@code false}
	 * 				lo devuelve en texto plano.
	 * @return Tuplas "NombreFichero|Contenido".
	 * @throws PrivilegedActionException Cuando ocurre alg&uacute;n error durante la operaci&oacute;n.
	 */
	private String getFileNameContent(final String title,
			                          final String extensions,
			                          final String description,
		                              final String filePath,
			                          final boolean asBase64) throws PrivilegedActionException {

		clearError();

		final String titleDialog = MiniAfirmaApplet.cleanParam(title);
		final String cleanExts = MiniAfirmaApplet.cleanParam(extensions);
		final String[] exts = cleanExts == null ? null : cleanExts.split(","); //$NON-NLS-1$
		final String descFiles = MiniAfirmaApplet.cleanParam(description);
		final String path = MiniAfirmaApplet.cleanParam(filePath);

		try {
			return AccessController.doPrivileged(new GetFileNameContentAction(
					titleDialog, exts, descFiles, path, false, asBase64, this))[0];
		}
		catch (final AOCancelledOperationException e) {
			setError(e);
			throw e;
		}
		catch (final java.util.concurrent.CancellationException e) {
			final AOCancelledOperationException ce = new AOCancelledOperationException("Operacion cancelada por el usuario", e); //$NON-NLS-1$
			setError(ce);
			throw ce;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		catch (final OutOfMemoryError e) {
			setError(e, "Error de falta de memoria durante la carga del fichero"); //$NON-NLS-1$
			throw e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String[] getMultiFileNameContentBase64(final String title,
			                                      final String extensions,
			                                      final String description,
			                                      final String filePath) throws PrivilegedActionException {
		return getMultiFileNameContent(title, extensions, description, filePath,true);
	}

	/** Permite seleccionar al usuario un conjunto de ficheros y devuelve las tuplas con cada
	 * nombre de fichero y contenido separados por ('|').
	 * @param title T&iacute;tulo de la ventana de selecci&oacute;n.
	 * @param extensions Extensiones de fichero permitidas separadas por coma (',').
	 * @param description Descripci&oacute;n del tipo de fichero.
	 * @param filePath Ruta por defecto.
	 * @param asBase64 Si es {@code true} devuelve el contenido en Base64, si es {@code false}
	 * 				lo devuelve en texto plano.
	 * @return Listado de tuplas "NombreFichero|Contenido".
	 * @throws PrivilegedActionException Cuando ocurre alg&uacute;n error durante la operaci&oacute;n.
	 */
	private String[] getMultiFileNameContent(final String title,
			                                 final String extensions,
			                                 final String description,
			                                 final String filePath,
			                                 final boolean asBase64) throws PrivilegedActionException {
		clearError();

		final String titleDialog = MiniAfirmaApplet.cleanParam(title);
		final String cleanExts = MiniAfirmaApplet.cleanParam(extensions);
		final String[] exts = cleanExts == null ? null : cleanExts.split(","); //$NON-NLS-1$
		final String descFiles = MiniAfirmaApplet.cleanParam(description);
		final String path = MiniAfirmaApplet.cleanParam(filePath);

		try {
			return AccessController.doPrivileged(new GetFileNameContentAction(
					titleDialog, exts, descFiles, path, true, asBase64, this));
		}
		catch (final AOCancelledOperationException e) {
			setError(e);
			throw e;
		}
		catch (final java.util.concurrent.CancellationException e) {
			final AOCancelledOperationException ce = new AOCancelledOperationException("Operacion cancelada por el usuario", e); //$NON-NLS-1$
			setError(ce);
			throw ce;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		catch (final OutOfMemoryError e) {
			setError(e, "Error de falta de memoria durante la carga de los ficheros"); //$NON-NLS-1$
			throw e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getTextFromBase64(final String base64Data, final String charset) throws IOException {
		clearError();
		if (base64Data == null) {
			return null;
		}
		final String cleanCharset = getCharset(MiniAfirmaApplet.cleanParam(charset), base64Data);
		try {
			return chunkReturn(new String(Base64.decode(base64Data), cleanCharset));
		}
		catch (final IOException e) {
			setError(e);
			throw e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getBase64FromText(final String plainText, final String charset) throws UnsupportedEncodingException {
		clearError();
		if (plainText == null) {
			return null;
		}
		final String cleanCharset = getCharset(
			MiniAfirmaApplet.cleanParam(charset),
			null
		);
		try {
			return Base64.encode(plainText.getBytes(cleanCharset));
		}
		catch (final UnsupportedEncodingException e) {
			setError(e);
			throw e;
		}
	}

	/** Obtiene la URL base del Applet.
	 * Al contrario que el <a href="http://docs.oracle.com/javase/7/docs/api/java/applet/Applet.html#getCodeBase%28%29">m&eacute;todo original</a>,
	 * este funciona incluso en despliegues locales (<a href="http://bugs.java.com/bugdatabase/view_bug.do?bug_id=8017250">Bug 8017250 de Java</a>). */
	@Override
	public URL getCodeBase() {
		URL codebase = null;
		try {
			codebase = super.getCodeBase();
		}
		catch (final Exception e) { /* Ignorada */ }
		if (codebase != null) {
			return codebase;
		}
		try {
			return new URL(
				MiniAfirmaApplet.class.getResource("/miniappletmessages.properties").toString().replace("/miniappletmessages.properties", "") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			);
		}
		catch (final MalformedURLException e) {
			throw new IllegalStateException("No se puede determinar el codebase del Aeplet: " + e, e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public void init() {

		// Establecemos propiedades del sistema en base a argumentos de Java
		setSystemProperties(getParameter(APPLET_PARAM_SYSTEM_PROPERTIES));

		// Establecemos la localizacion
		setLocale(getParameter(APPLET_PARAM_LOCALE));

		// Configuramos el almacen en base al navegador
		this.userAgent = getParameter(APPLET_PARAM_USER_AGENT);

		final String keystoreParam = MiniAfirmaApplet.cleanParam(
			getParameter(APPLET_PARAM_USER_KEYSTORE)
		);
		if (keystoreParam != null && !keystoreParam.equals("null")) { //$NON-NLS-1$
			final int separatorPos = keystoreParam.indexOf(':');
			try {
				if (separatorPos == -1) {
					this.keystoreType = AOKeyStore.valueOf(keystoreParam);
				}
				else if (keystoreParam.length() > 1) {
					this.keystoreType = AOKeyStore.valueOf(keystoreParam.substring(0, separatorPos).trim());
					if (separatorPos < keystoreParam.length() -1 &&
							keystoreParam.substring(separatorPos + 1).trim().length() > 0) {
						this.keystoreLib = keystoreParam.substring(separatorPos + 1).trim();
					}
				}
			}
			catch (final Exception e) {
				LOGGER.warning(
					"Se ha intentado cargar un almacen de certificados no soportado, se cargara el por defecto: " + e //$NON-NLS-1$
				);
			}
		}

		MiniAfirmaApplet.configureLookAndFeel();

		// Google Analytics
		if (
				!Boolean.getBoolean("es.gob.afirma.doNotSendAnalytics") && //$NON-NLS-1$
				!Boolean.parseBoolean(System.getenv("es.gob.afirma.doNotSendAnalytics")) //$NON-NLS-1$
			) {
			new Thread(new Runnable() {

				@Override
				public void run() {
					try {
						final AnalyticsConfigData config = new AnalyticsConfigData(GOOGLE_ANALYTICS_TRACKING_CODE);
						final JGoogleAnalyticsTracker tracker = new JGoogleAnalyticsTracker(config, GoogleAnalyticsVersion.V_4_7_2);
						tracker.trackPageView(
								getCodeBase().toString(),
								"MiniApplet Cliente @firma " + getVersion(), //$NON-NLS-1$
								getCodeBase().getHost().toString()
								);
					}
					catch(final Exception e) {
						LOGGER.warning("Error registrando datos en Google Analytics: " + e); //$NON-NLS-1$
					}
				}
			}).start();
		}

		LOGGER.info("Miniapplet Afirma " + getVersion()); //$NON-NLS-1$

		LOGGER.info("Sistema operativo: " + System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Version del SO: " + System.getProperty("os.version")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Version de Java: " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Arquitectura del JRE: " + Platform.getJavaArch()); //$NON-NLS-1$
		LOGGER.info("Java Vendor: " + System.getProperty("java.vm.vendor")); //$NON-NLS-1$ //$NON-NLS-2$

		LOGGER.info("Localizacion por defecto: " + Locale.getDefault()); //$NON-NLS-1$

		LOGGER.info("Tamano actual en memoria: " + Runtime.getRuntime().totalMemory()/(1024*1024) + "MB"); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Tamano maximo de memoria: " + Runtime.getRuntime().maxMemory()/(1024*1024) + "MB"); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Memoria actualmente libre: " + Runtime.getRuntime().freeMemory()/(1024*1024) + "MB"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** {@inheritDoc} */
	@Override
	public void start() {
		super.start();

		try {
			AccessController.doPrivileged(new PrivilegedExceptionAction<Void>() {
				@Override
				public Void run() throws Exception {
					JarSignatureCertExtractor.insertJarSignerOnCACerts(this);
					return null;
				}
			});

		} catch (final Exception e) {
			LOGGER.warning(
					"No se ha podido insertar la cadena de confianza del certificado de firma del applet en el almacen de confianza de Java: " + e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getErrorMessage() {
		return this.errorMessage;
	}

	/** {@inheritDoc} */
	@Override
	public String getErrorType() {
		return this.errorType;
	}

	/** Establece el error en base a la excepci&oacute;n recibida.
	 * @param e Excepci&oacute;n que produjo el error. */
	private void setError(final Throwable e) {
		setError(e, null);
	}

	/** Establece el mensaje de error recibido y el tipo de error en base a la excepci&oacute;n.
	 * Si no se indica un mensaje este tambi&eacute;n se toma de la excepci&oacute;n.
	 * @param e Excepci&oacute;n que produjo el error.
	 * @param message Mensaje descriptivo del error. */
	private void setError(final Throwable e, final String message) {
		Throwable ex = e;
		if (e instanceof PrivilegedActionException && e.getCause() != null) {
			ex = e.getCause();
		}

		this.errorMessage = message != null ? message :
			ex.getLocalizedMessage() != null ? ex.getLocalizedMessage() :
			ex.getMessage() != null ? ex.getMessage() :
				ex.toString();

		this.errorType = ex.getClass().getCanonicalName();

		// Mostramos la traza de error a traves del log
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final PrintWriter writer = new PrintWriter(baos);
		e.printStackTrace(writer);
		writer.flush();
		writer.close();

		LOGGER.severe(new String(baos.toByteArray()));
	}

	/** Elimina el tipo y mensaje de error establecido previamente. */
	private void clearError() {
		this.errorType = null;
		this.errorMessage = null;
	}

	@Override
	public void setKeyStore(final String ksType) throws AOException {

		LOGGER.info("Se configura el almacen de certificados " + ksType); //$NON-NLS-1$

		clearError();

		if (ksType == null || ksType.length() == 0) {
			this.keystoreType = null;
			this.keystoreLib = null;
		}
		else {
			final int separatorPos = ksType.indexOf(':');
			try {
				if (separatorPos == -1) {
					this.keystoreType = AOKeyStore.valueOf(ksType);
				}
				else if (ksType.length() > 1) {
					this.keystoreType = AOKeyStore.valueOf(ksType.substring(0, separatorPos).trim());
					if (separatorPos < ksType.length() -1 &&
							ksType.substring(separatorPos + 1).trim().length() > 0) {
						this.keystoreLib = ksType.substring(separatorPos + 1).trim();
					}
				}
			}
			catch (final Exception e) {
				LOGGER.severe("El tipo de almacen indicado (" + ksType + ") no existe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				setError(e, "El tipo de almacen indicado no existe: " + ksType); //$NON-NLS-1$
				throw new AOException("El tipo de almacen indicado no existe: " + ksType, e); //$NON-NLS-1$
			}
		}
	}

	/** Permite que el usuario seleccione un certificado del almac&eacute;n configurado, o
	 * el por defecto si no se indic&oacute;, y devuelve su clave privada.
	 * @param params Configuraci&oacute;n general establecida para la operaci&oacute;n.
	 * @return Clave privada asociada al certificado seleccionado.
	 * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
	 * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n. */
	private PrivateKeyEntry selectPrivateKey(final Properties params) throws PrivilegedActionException {
		if (this.stickySignatory && this.stickyKeyEntry != null) {
			return this.stickyKeyEntry;
		}
		final SelectPrivateKeyAction selectPrivateKeyAction;
		if (this.keystoreType == null) {
			selectPrivateKeyAction = new SelectPrivateKeyAction(
					Platform.getOS(),
					Platform.getBrowser(this.userAgent),
					new CertFilterManager(params),
					this);
		}
		else {
			selectPrivateKeyAction = new SelectPrivateKeyAction(
					this.keystoreType,
					this.keystoreLib,
					new CertFilterManager(params),
					this);
		}
		final PrivateKeyEntry keyEntry = AccessController.doPrivileged(selectPrivateKeyAction);
		if (this.stickySignatory) {
			this.stickyKeyEntry = keyEntry;
		}

		return keyEntry;
	}

	/**
	 * Devuelve un manejador de firma compatible con un formato de firma o, de no establecerse, con una firma electr&oacute;nica concreta.
	 * @param format Formato de firma.
	 * @param data Datos a firmar
	 * @param sign Firma electr&oacute;nica.
	 * @return Manejador de firma.
	 * @throws AOFormatFileException Cuando el formato o la firma no estan soportados
	 * @throws PrivilegedActionException Cuando se produce un error durante la lectura de los datos. */
	private static AOSigner selectSigner(final String format,
			 							 final byte[] data,
			                             final byte[] sign) throws AOFormatFileException,
			                                                       PrivilegedActionException {

		if (format == null) {
			throw new IllegalArgumentException(
				"No se ha indicado el formato para la operacion de firma" //$NON-NLS-1$
			);
		}

		final AOSigner signer;
		if (!SIGNATURE_FORMAT_AUTO.equalsIgnoreCase(format)) {
			signer = MiniAfirmaApplet.getSigner(format);
			if (signer == null) {
				throw new AOFormatFileException("No esta soportado el formato de firma: " + format); //$NON-NLS-1$
			}
		}
		else if (data != null) {
			signer = MiniAfirmaApplet.getSigner(format, data);
		}
		else if (sign != null) {
			signer = MiniAfirmaApplet.getSigner(sign);
			if (signer == null) {
				throw new IllegalArgumentException(
					"Los datos introducidos no se corresponden con una firma soportada" //$NON-NLS-1$
				);
			}
		}
		else {
			throw new IllegalArgumentException(
					"No se han introducido datos para la seleccion del signer" //$NON-NLS-1$
					);
		}

		return signer;
	}

	/** Recupera un manejador de firma compatible con el formato indicado. Si no se encuentra uno
	 * compatible, se devuelve {@code null}.
	 * @param format Nombre de un formato de firma.
	 * @return Manejador de firma.
	 * @throws PrivilegedActionException Cuando ocurre un problema de seguridad.
	 */
	private static AOSigner getSigner(final String format) throws PrivilegedActionException {
		return AccessController.doPrivileged(new SelectSignerAction(format));
	}

	/** Recupera un manejador de firma compatible con el formato indicado. Si este es {@code AUTO}, se
	 * calcular&aacute; en base a los datos proporcionados. Si no se encuentra uno compatible, se
	 * devuelve {@code null}.
	 * @param format Nombre de un formato de firma.
	 * @return Manejador de firma.
	 * @throws PrivilegedActionException Cuando ocurre un problema de seguridad.
	 */
	private static AOSigner getSigner(final String format, final byte[] data) throws PrivilegedActionException {
		return AccessController.doPrivileged(new SelectSignerAction(format, data));
	}

	/** Recupera un manejador de firma compatible con la firma indicada. Si no se encuentra uno
	 * compatible, se devuelve {@code null}.
	 * @param signature Firma electr&oacute;nica.
	 * @return Manejador de firma.
	 * @throws PrivilegedActionException Cuando ocurre un problema de seguridad.
	 */
	private static AOSigner getSigner(final byte[] signature) throws PrivilegedActionException {
		return AccessController.doPrivileged(new SelectSignerAction(signature));
	}

	/** Configura la apariencia de los di&aacute;logos Java siguiendo la configuraci&oacute;n
	 * establecida en el sistema.
	 */
	private static void configureLookAndFeel() {
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido establecer el Look&Feel: " + e); //$NON-NLS-1$
		}

		// Nos aseguramos de que los dialogos salgan decorados
		javax.swing.JDialog.setDefaultLookAndFeelDecorated(true);
		javax.swing.JFrame.setDefaultLookAndFeelDecorated(true);
	}

	/** Limpia los par&aacute;metros recibidos en las funciones del applet sustituyendo
	 * cadenas vac&iacute;as por nulos y eliminando los espacios en blanco al principio
	 * y final de las cadenas
	 * @param appletParam Par&aacute;metro que se desea limpiar.
	 * @return Par&aacute;metro preparado para su uso. */
	private static String cleanParam(final String appletParam) {
		return appletParam == null || appletParam.trim().length() < 1 ?
				null : appletParam.trim();
	}

	/**
	 * En caso de indicarse una expresi&oacute;n clave como juego de caracteres, se calcula el
	 * juego de caracteres en base al algoritmo determinado para esa expresi&oacute;n. Si no
	 * es una expresi&oacute;n reconocida se devuelve la propia entrada. Las expresiones
	 * permitidas son:
	 * <ul>
	 *   <li><b>auto</b>: Devuelve el juego de caracteres detectado en el texto proporcionado.
	 *   Si no puede detectarse, se devolver&aacute; el juego de caracteres del sistema.</li>
	 *   <li><b>default</b>: Devuelve el juego de caracteres por defecto del sistema.</li>
	 * </ul>
	 * @param charset Expresi&oacute;n clave o juego de caracteres.
	 * @param textBase64 Texto auxiliar para determinar el juego de caracteres.
	 * @return Juego de caracteres introducido u obtenido a partir de la expresi&oacute;n
	 * clave.
	 */
	private static String getCharset(final String charset, final String textBase64) {
		if ("auto".equalsIgnoreCase(charset)) { //$NON-NLS-1$
			if (textBase64 == null) {
				LOGGER.warning(
					"No se puede detectar el juego de caracteres de un texto nulo, se devolvera la codificacion por defecto" //$NON-NLS-1$
				);
				return getCharset("default", null); //$NON-NLS-1$
			}
			final byte[] text;
			try {
				text = Base64.decode(textBase64);
			}
			catch (final IOException e) {
				LOGGER.warning("Los datos proporcionados est\u00E1n mal codificados en base 64"); //$NON-NLS-1$
				return getCharset("default", null); //$NON-NLS-1$
			}
			final UniversalDetector detector = new UniversalDetector();
			detector.handleData(text, 0, text.length);
			detector.dataEnd();
			if (detector.isDone()) {
				final String detectedCharset = detector.getDetectedCharset();
				if (detectedCharset != null) {
					return detectedCharset;
				}
			}
			// Si no se ha podido detectar, devolvemos la codificacion por defecto
			LOGGER.warning(
					"No se ha podido autodetectar el juego de caracteres, se devolvera el por defecto del sistema" //$NON-NLS-1$
			);
			return getCharset("default", null); //$NON-NLS-1$
		}
		else if ("default".equalsIgnoreCase(charset) || charset == null) { //$NON-NLS-1$
			return AccessController.doPrivileged(new GetPropertyAction("file.encoding")); //$NON-NLS-1$
		}
		return charset;
	}

	/** Imprime en el logger el texto "MiniApplet cargado y en ejecuci&oacute;n". El uso de
	 * este m&eacute;todo permite determinar si el applet se encuentra inicializado. */
	@Override
	public String echo() {
		final String javaInfo =
				"Java vendor: " + System.getProperty("java.vm.vendor") + '\n' + //$NON-NLS-1$ //$NON-NLS-2$
				"Java version: " + System.getProperty("java.version") + '\n' + //$NON-NLS-1$ //$NON-NLS-2$
				"Java architecture: " + Platform.getJavaArch(); //$NON-NLS-1$

		LOGGER.info("MiniApplet cargado y en ejecuci\u00F3n"); //$NON-NLS-1$
		LOGGER.info(javaInfo);

		return javaInfo;
	}

	private byte[] readChunk() throws IOException {
		if (this.chunkedReturnStream == null) {
			return new byte[0];
		}

		final byte[] buf;
		boolean finished = false;
		final int available = this.chunkedReturnStream.available();
		if (available > BUFFER_SIZE) {
			buf = new byte[BUFFER_SIZE];
		}
		else if (available > 0){
			buf = new byte[available];
			finished = true;
		}
		else {
			this.chunkedReturnStream = null;
			return new byte[0];
		}
		final int readed = this.chunkedReturnStream.read(buf);
		if (readed != buf.length) {
			this.chunkedReturnStream = null;
			throw new IOException("Lectura incompleta de la porcion de datos resultantes"); //$NON-NLS-1$
		}

		if (finished) {
			this.chunkedReturnStream.close();
			this.chunkedReturnStream = null;
		}

		return buf;
	}

	private String chunkReturn(final String completeReturn) throws IOException {
		if (completeReturn == null || completeReturn.length() <= BUFFER_SIZE) {
			return completeReturn;
		}
		LOGGER.info(
			"El resultado es demasiado grande (" + //$NON-NLS-1$
			completeReturn.length() +
			" caracteres), se ha devuelto un resultado parcial y debe solicitarse el resto en llamadas adicionales" //$NON-NLS-1$
		);
		this.chunkedReturnStream = new ByteArrayInputStream(completeReturn.getBytes(DEFAULT_CHUNK_ENCODING));
		return new String(readChunk(), DEFAULT_CHUNK_ENCODING);
	}

	@Override
	public String getRemainingData() throws IOException {
		final byte[] chunk = readChunk();
		if (chunk == null) {
			throw new IOException("Se ha producido una lectura parcial nula"); //$NON-NLS-1$
		}
		if (chunk.length == 0) {
			LOGGER.info("Se ha terminado de devolver un resultado grande"); //$NON-NLS-1$
			return EOF;
		}
		LOGGER.info("Devolucion parcial de " + chunk.length + " caracteres"); //$NON-NLS-1$ //$NON-NLS-2$
		return new String(chunk, DEFAULT_CHUNK_ENCODING);
	}

	@Override
	public void addData(final String data) {
		if (data == null) {
			this.dataStore.setLength(0);
		}
		else {
			this.dataStore.append(data);
			LOGGER.info("Anadida porcion de datos de longitud: " + data.length()); //$NON-NLS-1$
		}
	}

	/** Recupera el identificador del numero de version del MiniApplet a partir de su Manifest.
	 * @return Identificador de la versi&oacute;n. */
	static String getVersion() {
		try {
			final InputStream manifestIs = MiniAfirmaApplet.class.getClassLoader().getResourceAsStream("miniapplet.version"); //$NON-NLS-1$
			final Properties metadata = new Properties();
			metadata.load(manifestIs);
			manifestIs.close();

			return metadata.getProperty("version", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido leer el numero de version del MiniApplet del fichero de version: " + e); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
	}

	/**
	 * Establece las propiedades del sistema indicadas como argumentos y separadas por '-D'.
	 * Las propiedades deben establecerse como clave=valor.
	 * @param systemProperties Texto con las propiedades que establecer.
	 */
	private static void setSystemProperties(final String systemProperties) {

		LOGGER.info("setSystemProperties: " + systemProperties); //$NON-NLS-1$

		if (systemProperties != null && systemProperties.trim().length() > 0) {
			final String[] properties = systemProperties.split("-D");  //$NON-NLS-1$
			for (final String property : properties) {
				final int equalIndex = property.indexOf('=');
				if (equalIndex != -1) {
					LOGGER.info("Establecemos la propiedad del sistema: " + property.trim()); //$NON-NLS-1$
					System.setProperty(property.substring(0, equalIndex).trim(), property.substring(equalIndex + 1).trim());
				}
			}
		}
	}

	/** Establece la localizaci&oacute;n por defecto de la aplicaci&oacute;n.
	 * @param locale C&oacute;digo ISO del idioma a establecer. */
	private static void setLocale(final String locale) {

		LOGGER.info("setLocale: " + locale); //$NON-NLS-1$

		if (locale == null || locale.trim().length() == 0) {
			return;
		}

		final String[] localeComponents = locale.replace('-', '_').split("_"); //$NON-NLS-1$
		if (localeComponents.length == 1) {
			Locale.setDefault(new Locale(localeComponents[0]));
		}
		else if (localeComponents.length == 2) {
			Locale.setDefault(new Locale(localeComponents[0], localeComponents[1]));
		}
		else if (localeComponents.length > 2) {
			Locale.setDefault(new Locale(localeComponents[0], localeComponents[1], localeComponents[2]));
		}
	}

	@Override
	public String getCurrentLog() {
		return AccessController.doPrivileged(new GetCurrentLogAction());
	}

	/**
	 * Este metodo sirve para identificar cuando se ha configurado una firma con el formato XAdES
	 * de tipo enveloped y la propiedad {@code mode} con el valor {@code explicit}. Este tipo de firma
	 * no existe.
	 * @param format Formato declarado para la firma.
	 * @param config Par&aacute;metros adicionales declarados para la firma.
	 * @return {@code true} si se configura una firma XAdES Enveloped explicit, {@code false} en caso contrario.
	 * @deprecated Uso temporal hasta que se elimine el soporte de firmas XAdES explicitas.
	 */
	@Deprecated
	private static boolean isXadesEnvelopedExplicit(final String format, final Properties config) {
		return isXadesExplicitConfigurated(format, config) &&
				AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equalsIgnoreCase(config.getProperty("format")) //$NON-NLS-1$
			;
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

	@Override
	public String signBatch(final String batchB64,
			                final String batchPreSignerUrl,
			                final String batchPostSignerUrl,
			                final String extraParams) throws AOException,
			                                                 PrivilegedActionException {
		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
		final PrivateKeyEntry pke;
		try {
			pke = selectPrivateKey(params);
		}
		catch (final PrivilegedActionException e) {
			LOGGER.severe("error al seleccionar certificado: " + e); //$NON-NLS-1$
			setError(e);
			throw e;
		}
		catch (final SmartCardException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		catch (final OutOfMemoryError e) {
			setError(e, "Error de falta de memoria durante la operaci\u00F3n de firma de lote"); //$NON-NLS-1$
			throw e;
		}
		catch (final Error e) {
			setError(e);
			throw new AOException("Ocurrio un error grave durante la operaci\u00F3n de firma de lote", e); //$NON-NLS-1$
		}

		try {

			return Base64.encode(
					AccessController.doPrivileged(
							new PrivilegedExceptionAction<byte[]>() {
								@Override
								public byte[] run() throws Exception {
									return BatchSigner.sign(
											batchB64,
											batchPreSignerUrl,
											batchPostSignerUrl,
											pke.getCertificateChain(),
											pke.getPrivateKey()
											).getBytes("utf-8"); //$NON-NLS-1$
								}
							})
					);
		}
		catch (final Exception e) {
			if (e.getCause() instanceof CertificateEncodingException) {
				LOGGER.severe("Error al codificar el certificado al realizar la firma por lotes: " + e); //$NON-NLS-1$
				final AOException aoe = new AOException(e);
				setError(e);
				throw aoe;
			}
			LOGGER.severe("Error al realizar la firma por lotes: " + e); //$NON-NLS-1$
			final AOException aoe = new AOException(e);
			setError(e);
			throw aoe;
		}
	}

	@Override
	public String selectCertificate(final String extraParams) throws AOException, PrivilegedActionException, CertificateEncodingException {

		LOGGER.info("Solicitada la seleccion de un certificado"); //$NON-NLS-1$

		clearError();

		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);

		// Informacion sobre los parametros adicionales indicados
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final PrintStream ps = new PrintStream(baos);
		params.list(ps);
		LOGGER.info("Recibidos los siguientes parametros adicionales:\n" + baos.toString()); //$NON-NLS-1$

		try {
			final PrivateKeyEntry pke = selectPrivateKey(params);

			return Base64.encode(pke.getCertificate().getEncoded());
		}
		catch (final CertificateEncodingException e) {
			setError(e);
			throw e;
		}
		catch (final SmartCardException e) {
			setError(e);
			throw e;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
		catch (final Error e) {
			setError(e);
			throw new AOException("Ocurrio un error grave durante la seleccion de un certificado de firma", e); //$NON-NLS-1$
		}
	}
}
