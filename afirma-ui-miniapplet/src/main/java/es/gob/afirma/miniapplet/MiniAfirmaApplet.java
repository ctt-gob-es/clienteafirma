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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.security.AccessController;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedActionException;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.UIManager;

import org.mozilla.universalchardet.UniversalDetector;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.keystores.main.common.AOKeyStore;

/** MiniApplet de firma del proyecto Afirma. */
public final class MiniAfirmaApplet extends JApplet implements MiniAfirma {

	/** Clave privada fijada para reutilizarse en operaciones sucesivas. */
	private PrivateKeyEntry stickyKeyEntry = null;

	private boolean stickySignatory = false;

	private static final long serialVersionUID = -4364574240099120486L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String APPLET_PARAM_USER_AGENT = "userAgent"; //$NON-NLS-1$

	private static final String APPLET_PARAM_USER_KEYSTORE = "keystore"; //$NON-NLS-1$

	/** Identificador del navegador Web que carga el applet. */
	private String userAgent = null;

	/** Tipo de almac&eacute;n de claves del que extraer los certificados. */
	private AOKeyStore keystoreType = null;

	/** Ruta de la biblioteca o almac&eacute;n del que extraer los certificados. */
	private String keystoreLib = null;

	/** Ruta recomendada para la apertura de di&aacute;logos se seleccion y guardado de ficheros. */
	private final File pathHint = new File(Platform.getUserHome());

	/** Mensaje del &uacute;ltimo error producido. */
	private String errorMessage = null;

	/** Tipoe del &uacute;ltimo error producido. */
	private String errorType = null;

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
	public String sign(final String dataB64,
					   final String algorithm,
					   final String format,
					   final String extraParams) throws PrivilegedActionException,
			                                            IOException,
			                                            AOException {
		this.clearError();

		final byte[] dataBinary;
		if (dataB64 != null) {
			try {
				dataBinary = Base64.decode(dataB64);
			}
			catch (final IOException e) {
				setError(e, "Los datos proporcionados est\u00E1n mal codificados en base 64"); //$NON-NLS-1$
				throw e;
			}
		}
		else {
			try {
				dataBinary = AccessController.doPrivileged(new GetFileContentAction(
						MiniAppletMessages.getString("MiniAfirmaApplet.0"), null, //$NON-NLS-1$
						MiniAppletMessages.getString("MiniAfirmaApplet.1"), this)); //$NON-NLS-1$
			}
			catch (final AOCancelledOperationException e) {
				setError(e);
				throw e;
			}
			catch (final PrivilegedActionException e) {
				setError(e);
				throw e;
			}
		}

		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);

		try {
			return Base64.encode(AccessController.doPrivileged(new SignAction(
				MiniAfirmaApplet.selectSigner(MiniAfirmaApplet.cleanParam(format), null),
				dataBinary,
				MiniAfirmaApplet.cleanParam(algorithm),
				this.selectPrivateKey(params),
				ExtraParamsProcessor.expandProperties(params, dataBinary, format)
			)));
		}
		catch (final AOFormatFileException e) {
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
			throw new AOException("Ocurrio un error grave durante la operacion de firma", e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String coSign(final String signB64,
			             final String dataB64,
			             final String algorithm,
			             final String format,
			             final String extraParams) throws PrivilegedActionException,
			             							      IOException,
				                                          AOException {
		this.clearError();

		byte[] sign;
		if (signB64 != null) {
			try {
				sign = Base64.decode(signB64);
			}
			catch (final IOException e) {
				setError(e, "La firma proporcionada est\u00E1 mal codificada en base 64"); //$NON-NLS-1$
				throw e;
			}
		}
		else {
			try {
				sign = AccessController.doPrivileged(new GetFileContentAction(
						MiniAppletMessages.getString("MiniAfirmaApplet.2"), null, //$NON-NLS-1$
						MiniAppletMessages.getString("MiniAfirmaApplet.1"), this)); //$NON-NLS-1$
			}
			catch (final AOCancelledOperationException e) {
				setError(e);
				throw e;
			}
			catch (final PrivilegedActionException e) {
				setError(e);
				throw e;
			}
		}

		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
		final byte[] dataBinary;
		try {
			dataBinary = dataB64 == null ? null : Base64.decode(dataB64);
		}
		catch (final IOException e) {
			setError(e, "Los datos proporcionados est\u00E1n mal codificados en base 64"); //$NON-NLS-1$
			throw e;
		}

		try {
			return Base64.encode(AccessController.doPrivileged(new CoSignAction(
					MiniAfirmaApplet.selectSigner(MiniAfirmaApplet.cleanParam(format), sign),
					sign,
					dataBinary,
					MiniAfirmaApplet.cleanParam(algorithm),
					this.selectPrivateKey(params),
					ExtraParamsProcessor.expandProperties(params, dataBinary, format)
			)));
		}
		catch (final AOFormatFileException e) {
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
			throw new AOException("Ocurrio un error grave durante la operacion de cofirma", e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String counterSign(final String signB64,
			                  final String algorithm,
			                  final String format,
			                  final String extraParams) throws PrivilegedActionException,
			                  								   IOException,
					                                           AOException {
		this.clearError();

		byte[] sign;
		if (signB64 != null) {
			try {
				sign = Base64.decode(signB64);
			}
			catch (final IOException e) {
				setError(e, "La firma proporcionada est\u00E1 mal codificada en base 64"); //$NON-NLS-1$
				throw e;
			}
		}
		else {
			try {
				sign = AccessController.doPrivileged(new GetFileContentAction(
						MiniAppletMessages.getString("MiniAfirmaApplet.2"), null, //$NON-NLS-1$
						MiniAppletMessages.getString("MiniAfirmaApplet.1"), this)); //$NON-NLS-1$
			}
			catch (final AOCancelledOperationException e) {
				setError(e);
				throw e;
			}
			catch (final PrivilegedActionException e) {
				setError(e);
				throw e;
			}
		}

		final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);

		try {
			return Base64.encode(AccessController.doPrivileged(new CounterSignAction(
					MiniAfirmaApplet.selectSigner(MiniAfirmaApplet.cleanParam(format), sign),
					sign,
					MiniAfirmaApplet.cleanParam(algorithm),
					this.selectPrivateKey(params),
					ExtraParamsProcessor.expandProperties(params, null, format)
			)));
		}
		catch (final AOFormatFileException e) {
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
			throw new AOException("Ocurrio un error grave durante la operacion de contrafirma", e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getSignersStructure(final String signB64) throws IOException,
	                                                               PrivilegedActionException,
	                                                               AOFormatFileException {
		this.clearError();

		final byte[] sign;
		if (signB64 != null) {
			try {
				sign = Base64.decode(signB64);
			}
			catch (final IOException e) {
				setError(e, "La firma proporcionada est\u00E1 mal codificada en base 64"); //$NON-NLS-1$
				throw e;
			}
		}
		else {
			try {
				sign = AccessController.doPrivileged(new GetFileContentAction(
						MiniAppletMessages.getString("MiniAfirmaApplet.2"), null, //$NON-NLS-1$
						MiniAppletMessages.getString("MiniAfirmaApplet.1"), this)); //$NON-NLS-1$
			}
			catch (final AOCancelledOperationException e) {
				setError(e);
				throw e;
			}
			catch (final PrivilegedActionException e) {
				setError(e);
				throw e;
			}
		}

		final AOSigner signer;
		try {
			signer = MiniAfirmaApplet.getSigner(sign);
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}

		if (signer == null) {
			final AOFormatFileException e = new AOFormatFileException(
				"Los datos introducidos no se corresponden con una firma soportada" //$NON-NLS-1$
			);
			setError(e);
			throw e;
		}

		try {
			return AOUtil.showTreeAsString(signer.getSignersStructure(sign, false), null, null);
		} catch (final AOInvalidFormatException ex) {
			final AOFormatFileException e = new AOFormatFileException(
					"Los datos introducidos no se corresponden con una firma soportada", ex //$NON-NLS-1$
				);
				setError(e);
				throw e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public boolean saveDataToFile(final String data,
			                      final String title,
			                      final String fileName,
			                      final String extension,
			                      final String description) throws PrivilegedActionException,
			                                                       IOException {

		this.clearError();

		if (data == null) {
			LOGGER.warning(
				"Se ha solicitado guardar en disco un contenido nulo, se ignorara la peticion" //$NON-NLS-1$
			);
			return false;
		}

		final String titleDialog = MiniAfirmaApplet.cleanParam(title);

		final String depuredExts = MiniAfirmaApplet.cleanParam(extension);
		final String[] exts = depuredExts == null ? null : new String[] { depuredExts };

		final String descFiles = MiniAfirmaApplet.cleanParam(description);

		final String depuredFileName = MiniAfirmaApplet.cleanParam(fileName);
		final File fileHint = depuredFileName == null ?
				this.pathHint : new File(this.pathHint, fileName);

		try {
			AccessController.doPrivileged(
				new SaveFileAction(
						titleDialog,
						Base64.decode(data),
						exts,
						descFiles,
						fileHint,
						this
				)
			);
			return true;
		}
		catch (final AOCancelledOperationException e) {
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
	}

	/** {@inheritDoc} */
	@Override
	public String getFileNameContentBase64(final String title,
			                               final String extensions,
			                               final String description) throws PrivilegedActionException {
		this.clearError();
		// Se llama a setError() desde getFileNameContent, no es necesario repetirlo aqui
		return this.getFileNameContent(title, extensions, description, true);
	}

	private String getFileNameContent(final String title,
			                          final String extensions,
			                          final String description,
			                          final boolean asBase64) throws PrivilegedActionException {

		this.clearError();

		final String titleDialog = MiniAfirmaApplet.cleanParam(title);
		final String cleanExts = MiniAfirmaApplet.cleanParam(extensions);
		final String[] exts = cleanExts == null ? null : cleanExts.split(","); //$NON-NLS-1$
		final String descFiles = MiniAfirmaApplet.cleanParam(description);

		try {
			return AccessController.doPrivileged(new GetFileNameContentAction(
					titleDialog, exts, descFiles, false, asBase64, this))[0];
		}
		catch (final AOCancelledOperationException e) {
			return null;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String[] getMultiFileNameContentBase64(final String title,
			                                      final String extensions,
			                                      final String description) throws IOException,
			                                                                       PrivilegedActionException {
		return this.getMultiFileNameContent(title, extensions, description, true);
	}

	/** Permite seleccionar al usuario un conjunto de ficheros y devuelve las tuplas con cada
	 * nombre de fichero y contenido separados por ('|').
	 * @param title T&iacute;tulo de la ventana de selecci&oacute;n.
	 * @param extensions Extensiones de fichero permitidas separadas por coma (',').
	 * @param description Descripci&oacute;n del tipo de fichero.
	 * @param asBase64 Si es {@code true} devuelve el contenido en Base64, si es {@code false}
	 * 				lo devuelve en texto plano.
	 * @return Listado de tuplas "NombreFichero|Contenido".
	 * @throws PrivilegedActionException Cuando ocurre alg&uacute;n error durante la operaci&oacute;n.
	 */
	private String[] getMultiFileNameContent(final String title,
			                                 final String extensions,
			                                 final String description,
			                                 final boolean asBase64) throws PrivilegedActionException {
		this.clearError();

		final String titleDialog = MiniAfirmaApplet.cleanParam(title);
		final String cleanExts = MiniAfirmaApplet.cleanParam(extensions);
		final String[] exts = cleanExts == null ? null : cleanExts.split(","); //$NON-NLS-1$
		final String descFiles = MiniAfirmaApplet.cleanParam(description);

		try {
			return AccessController.doPrivileged(new GetFileNameContentAction(
					titleDialog, exts, descFiles, true, asBase64, this));
		}
		catch (final AOCancelledOperationException e) {
			return null;
		}
		catch (final PrivilegedActionException e) {
			setError(e);
			throw e;
		}
		catch (final RuntimeException e) {
			setError(e);
			throw e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getTextFromBase64(final String base64Data, final String charset) throws IOException {
		this.clearError();
		if (base64Data == null) {
			return null;
		}
		final String cleanCharset = getCharset(MiniAfirmaApplet.cleanParam(charset), base64Data);
		try {
			return new String(Base64.decode(base64Data), cleanCharset);
		}
		catch (final IOException e) {
			setError(e);
			throw e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getBase64FromText(final String plainText, final String charset) throws UnsupportedEncodingException {
		this.clearError();
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

	/** {@inheritDoc} */
	@Override
	public void init() {
		this.userAgent = this.getParameter(APPLET_PARAM_USER_AGENT);

		final String keystoreParam = MiniAfirmaApplet.cleanParam(
			this.getParameter(APPLET_PARAM_USER_KEYSTORE)
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
		LOGGER.info("Miniapplet Afirma"); //$NON-NLS-1$
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
	 * @param e Excepci&oacute;n que produjo el error. */
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

	/** Devuelve un manejador de firma compatible con un formato de firma o, de no establecerse, con
	 * una firma electr&oacute;nica concreta.
	 * @param format Formato de firma.
	 * @param sign Firma electr&oacute;nica.
	 * @return Manejador de firma.
	 * @throws AOFormatFileException Cuando el formato o la firma no estan soportados.
	 * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
	 * @throws IOException Cuando se produce un error durante la lectura de los datos.
	 * @throws NullPointerException Cuando no se indica ni formato ni firma como par&aacute;nmetro.
	 */
	private static AOSigner selectSigner(final String format,
			                             final byte[] sign) throws AOFormatFileException,
			                                                       PrivilegedActionException, IOException {
		final AOSigner signer;
		if (format != null) {
			signer = MiniAfirmaApplet.getSigner(format);
			if (signer == null) {
				throw new AOFormatFileException("El formato de firma indicado no esta soportado"); //$NON-NLS-1$
			}
			if (sign != null &&  !signer.isSign(sign)) {
				throw new AOFormatFileException(
					"La firma electronica no es compatible con el formato de firma indicado" //$NON-NLS-1$
				);
			}
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
				"No se ha indicado el formato ni la firma que se desea tratar" //$NON-NLS-1$
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

	/**
	 * Imprime en el logger el texto "MiniApplet cargado y en ejecuci&oacute;n". El uso de
	 * este m&eacute;todo permite determinar si el applet se encuentra inicializado.
	 */
	@Override
	public void echo() {
		LOGGER.info("MiniApplet cargado y en ejecuci\u00F3n"); //$NON-NLS-1$
	}
}
