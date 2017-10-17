/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.logging.Logger;

import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.SingleSign.ProcessResult.Result;
import es.gob.afirma.signers.batch.SingleSignConstants.SignFormat;
import es.gob.afirma.signers.batch.SingleSignConstants.SignSubOperation;

/** Firma electr&oacute;nica &uacute;nica dentro de un lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SingleSign {

	private static final String PROP_ID = "SignatureId"; //$NON-NLS-1$

	private static final String XML_ATTRIBUTE_ID = "Id"; //$NON-NLS-1$

	private static final String XML_ELEMENT_DATASOURCE = "datasource"; //$NON-NLS-1$
	private static final String XML_ELEMENT_FORMAT = "format"; //$NON-NLS-1$
	private static final String XML_ELEMENT_SUBOPERATION = "suboperation"; //$NON-NLS-1$
	private static final String XML_ELEMENT_SIGNSAVER = "signsaver"; //$NON-NLS-1$
	private static final String XML_ELEMENT_SIGNSAVER_CLASSNAME = "class"; //$NON-NLS-1$
	private static final String XML_ELEMENT_SIGNSAVER_CONFIG = "config"; //$NON-NLS-1$
	private static final String XML_ELEMENT_EXTRAPARAMS = "extraparams"; //$NON-NLS-1$

	private static final String HTTP_SCHEME = "http://"; //$NON-NLS-1$
	private static final String HTTPS_SCHEME = "https://"; //$NON-NLS-1$
	private static final String FTP_SCHEME = "ftp://"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final Properties extraParams;

	private final String dataSource;

	private final SignFormat format;

	private final String id;

	private final SignSubOperation subOperation;

	private final SignSaver signSaver;

	private ProcessResult processResult = new ProcessResult(Result.NOT_STARTED, null);

	/**
	 * Recupera los par&aacute;metros de configuraci&oacute;n del formato de firma.
	 * @return Configuraci&oacute;n del formato de firma.
	 */
	public Properties getExtraParams() {
		return this.extraParams;
	}

	/**
	 * Recupera el formato de firma.
	 * @return Formato de firma.
	 */
	public SignFormat getSignFormat() {
		return this.format;
	}

	SignSubOperation getSubOperation() {
		return this.subOperation;
	}

	private static void checkDataSource(final String dataSource) {
		if (dataSource == null) {
			throw new IllegalArgumentException(
				"El origen de los datos no puede ser nulo" //$NON-NLS-1$
			);
		}
		for (final String allowed : BatchConfigManager.getAllowedSources()) {
			if ("base64".equalsIgnoreCase(allowed) && Base64.isBase64(dataSource)) { //$NON-NLS-1$
				return;
			}
			if (allowed.endsWith("*")) { //$NON-NLS-1$
				if (dataSource.startsWith(allowed.replace("*", ""))) { //$NON-NLS-1$ //$NON-NLS-2$
					return;
				}
			}
			else {
				if (dataSource.equals(allowed)) {
					return;
				}
			}
		}
		throw new SecurityException("Origen de datos no valido"); //$NON-NLS-1$
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder(" <singlesign "); //$NON-NLS-1$
		sb.append(XML_ATTRIBUTE_ID);
		sb.append("=\""); //$NON-NLS-1$
		sb.append(getId());
		sb.append("\">\n  <"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_DATASOURCE);
		sb.append(">"); //$NON-NLS-1$
		sb.append(this.dataSource);
		sb.append("</"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_DATASOURCE);
		sb.append(">\n  <"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_FORMAT);
		sb.append(">"); //$NON-NLS-1$
		sb.append(getSignFormat().toString());
		sb.append("</"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_FORMAT);
		sb.append(">\n  <"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SUBOPERATION);
		sb.append(">"); //$NON-NLS-1$
		sb.append(getSubOperation().toString());
		sb.append("</"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SUBOPERATION);
		sb.append(">\n  <"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_EXTRAPARAMS);
		sb.append(">"); //$NON-NLS-1$
		try {
			sb.append(AOUtil.properties2Base64(getExtraParams()));
		}
		catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error convirtiendo los parametros adicionales de la firma '" + getId() + "' a Base64: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		sb.append("</"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_EXTRAPARAMS);
		sb.append(">\n  <"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SIGNSAVER);
		sb.append(">\n   <"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SIGNSAVER_CLASSNAME);
		sb.append(">"); //$NON-NLS-1$
		sb.append(this.signSaver.getClass().getName());
		sb.append("</"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SIGNSAVER_CLASSNAME);
		sb.append(">\n   <"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SIGNSAVER_CONFIG);
		sb.append(">"); //$NON-NLS-1$
		try {
			sb.append(AOUtil.properties2Base64(this.signSaver.getConfig()));
		}
		catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error convirtiendo la configuracion del objeto de guardado de la firma '" + getId() + "' a Base64: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		sb.append("</"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SIGNSAVER_CONFIG);
		sb.append(">\n  </"); //$NON-NLS-1$
		sb.append(XML_ELEMENT_SIGNSAVER);
		sb.append(">\n </singlesign>"); //$NON-NLS-1$

		return sb.toString();
	}

	/** Realiza el proceso de prefirma, incluyendo la descarga u obtenci&oacute;n de datos.
	 * @param certChain Cadena de certificados del firmante.
	 * @param algorithm Algoritmo de firma.
	 * @return Nodo <code>firma</code> del XML de datos trif&aacute;sicos (sin ninguna etiqueta
	 *         antes ni despu&eacute;s).
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos. */
	String doPreProcess(final X509Certificate[] certChain,
			            final SingleSignConstants.SignAlgorithm algorithm) throws IOException,
			                                                                      AOException {
		return SingleSignPreProcessor.doPreProcess(this, certChain, algorithm);
	}

	/** Obtiene la tarea de preproceso de firma para ser ejecutada en paralelo.
	 * @param certChain Cadena de certificados del firmante.
	 * @param algorithm Algoritmo de firma.
	 * @return Tarea de preproceso de firma para ser ejecutada en paralelo. */
	Callable<String> getPreProcessCallable(final X509Certificate[] certChain,
                                                  final SingleSignConstants.SignAlgorithm algorithm) {
		return new Callable<String>() {
			@Override
			public String call() throws IOException, AOException {
				return doPreProcess(certChain, algorithm);
			}
		};
	}

	/** Realiza el proceso de postfirma, incluyendo la subida o guardado de datos.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos relativos <b>&uacute;nicamente</b> a esta firma.
	 *           Debe serializarse como un XML con esta forma (ejemplo):
	 *           <pre>
	 *            &lt;xml&gt;
	 *             &lt;firmas&gt;
	 *              &lt;firma Id="53820fb4-336a-47ee-b7ba-f32f58e5cfd6"&gt;
	 *               &lt;param n="PRE"&gt;MYICXDAYBgk[...]GvykA=&lt;/param&gt;
	 *               &lt;param n="PK1"&gt;dC2dIILB9HV[...]xT1bY=&lt;/param&gt;
	 *               &lt;param n="NEED_PRE"&gt;true&lt;/param&gt;
	 *              &lt;/firma&gt;
	 *             &lt;/firmas&gt;
	 *            &lt;/xml&gt;
	 *           </pre>
	 * @param algorithm Algoritmo de firma.
	 * @param batchId Identificador del lote de firma.
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario. */
	void doPostProcess(final X509Certificate[] certChain,
			                  final TriphaseData td,
			                  final SingleSignConstants.SignAlgorithm algorithm,
			                  final String batchId) throws IOException,
			                                               AOException,
			                                               NoSuchAlgorithmException {
		SingleSignPostProcessor.doPostProcess(
			this, certChain, td, algorithm, batchId
		);
	}

	/** Obtiene la tarea de postproceso de firma para ser ejecutada en paralelo.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos relativos <b>&uacute;nicamente</b> a esta firma.
	 *           Debe serializarse como un XML con esta forma (ejemplo):
	 *           <pre>
	 *            &lt;xml&gt;
	 *             &lt;firmas&gt;
	 *              &lt;firma Id="53820fb4-336a-47ee-b7ba-f32f58e5cfd6"&gt;
	 *               &lt;param n="PRE"&gt;MYICXDAYBgk[...]GvykA=&lt;/param&gt;
	 *               &lt;param n="PK1"&gt;dC2dIILB9HV[...]xT1bY=&lt;/param&gt;
	 *               &lt;param n="NEED_PRE"&gt;true&lt;/param&gt;
	 *              &lt;/firma&gt;
	 *             &lt;/firmas&gt;
	 *            &lt;/xml&gt;
	 *           </pre>
	 * @param algorithm Algoritmo de firma.
	 * @param batchId Identificador del lote de firma.
	 * @return Tarea de postproceso de firma para ser ejecutada en paralelo. */
	Callable<CallableResult> getPostProcessCallable(final X509Certificate[] certChain,
			                                                          final TriphaseData td,
			                                                          final SingleSignConstants.SignAlgorithm algorithm,
			                                                          final String batchId) {
		return new Callable<CallableResult>() {
			@Override
			public CallableResult call() {
				try {
					doPostProcess(certChain, td, algorithm, batchId);
				}
				catch(final Exception e) {
					return new CallableResult(getId(), e);
				}
				return new CallableResult(getId());
			}
		};

	}

	/**
	 * Construye la operaci&oacute;n simple de firma.
	 * @param singleSignNode
	 * @param xmlEncoding Codificaci&oacute;n declarada en el XML. Si no se conoce, ser&aacute; {@code null}.
	 * @throws DOMException
	 * @throws IOException
	 */
	SingleSign(final Node singleSignNode, final Charset charset) throws DOMException, IOException {
		if (!(singleSignNode instanceof Element)) {
			throw new IllegalArgumentException(
				"El nodo de definicion de la firma debe ser un Elemento DOM" //$NON-NLS-1$
			);
		}
		final Element el = (Element)singleSignNode;

		this.dataSource = el.getElementsByTagName(XML_ELEMENT_DATASOURCE).item(0).getTextContent();

		this.extraParams = new Properties();
		final NodeList tmpNl = el.getElementsByTagName(XML_ELEMENT_EXTRAPARAMS);
		if (tmpNl != null && tmpNl.getLength() > 0) {
			loadProperties(this.extraParams, Base64.decode(tmpNl.item(0).getTextContent()), charset);
		}

		this.format = SignFormat.getFormat(
			el.getElementsByTagName(XML_ELEMENT_FORMAT).item(0).getTextContent()
		);

		this.id = el.getAttribute(XML_ATTRIBUTE_ID);
		if (this.id == null || "".equals(this.id)) { //$NON-NLS-1$
			throw new IllegalArgumentException(
				"Es obligatorio establecer identificadores unicos de firma" //$NON-NLS-1$
			);
		}
		this.extraParams.put(PROP_ID, getId());

		this.subOperation = SignSubOperation.getSubOperation(
			el.getElementsByTagName(XML_ELEMENT_SUBOPERATION).item(0).getTextContent()
		);

		final Element signSaverElement = (Element) el.getElementsByTagName(XML_ELEMENT_SIGNSAVER).item(0);
		final SignSaver ssaver;
		try {
			ssaver = (SignSaver) Class.forName(
				signSaverElement.getElementsByTagName(XML_ELEMENT_SIGNSAVER_CLASSNAME).item(0).getTextContent()
			).newInstance();
		}
		catch (final Exception ex) {
			throw new IOException(
				"No se ha podido instanciar el objeto de guardado: " + ex, ex //$NON-NLS-1$
			);
		}
		ssaver.init(
			AOUtil.base642Properties(
				signSaverElement.getElementsByTagName(XML_ELEMENT_SIGNSAVER_CONFIG).item(0).getTextContent()
			)
		);

		this.signSaver = ssaver;
	}

	/**
	 * Carga propiedades de una cadena cuidando de si hay saltos de
	 * l&iacute;nea protegidos como parte de una propiedad.
	 * @param properties Objeto en el que cargar las propiedades.
	 * @param params Propiedades que se deben cargar.
	 * @param charset Juego de caracteres para hacer la interpretaci&oacute;n de bytes.
	 * @throws IOException Cuando ocurre un error durante la carga de las propiedades.
	 */
	private static void loadProperties(Properties properties, byte[] params, Charset charset) throws IOException {
		properties.load(new ByteArrayInputStream(
				new String(params, charset)
					.replace("\\\\n", "*$bn$*").replace("\\n", "\n").replace("*$bn$*", "\\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
					.getBytes(charset)));
	}

	/** Crea una definici&oacute;n de tarea de firma electr&oacute;nica &uacute;nica.
	 * @param id Identificador de la firma.
	 * @param dataSrc Datos a firmar.
	 * @param fmt Formato de firma.
	 * @param subOp Tipo de firma a realizar.
	 * @param xParams Opciones adicionales de la firma.
	 * @param ss Objeto para guardar la firma una vez completada. */
	public SingleSign(final String id,
			          final String dataSrc,
			          final SignFormat fmt,
			          final SignSubOperation subOp,
			          final Properties xParams,
			          final SignSaver ss) {

		if (dataSrc == null) {
			throw new IllegalArgumentException(
				"El origen de los datos a firmar no puede ser nulo" //$NON-NLS-1$
			);
		}

		if (fmt == null) {
			throw new IllegalArgumentException(
				"El formato de firma no puede ser nulo" //$NON-NLS-1$
			);
		}

		if (ss == null) {
			throw new IllegalArgumentException(
				"El objeto de guardado de firma no puede ser nulo" //$NON-NLS-1$
			);
		}

		this.dataSource = dataSrc;
		this.extraParams = xParams != null ? xParams : new Properties();
		this.format = fmt;

		this.id = id != null ? id : UUID.randomUUID().toString();
		this.extraParams.put(PROP_ID, getId());

		this.subOperation = subOp;
		this.signSaver = ss;
	}

	void save(final byte[] dataToSave) throws IOException {
		this.signSaver.saveSign(this, dataToSave);
	}

	Callable<CallableResult> getSaveCallable(final TempStore ts, final String batchId) {
		return new Callable<CallableResult>() {
			@Override
			public CallableResult call() {
				try {
					save(ts.retrieve(SingleSign.this, batchId));
				}
				catch(final Exception e) {
					return new CallableResult(getId(), e);
				}
				return new CallableResult(getId());
			}
		};
	}

	/**
	 * Recupera el identificador asignado en el lote a la firma.
	 * @return Identificador.
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * Recupera los datos que se deben procesar.
	 * @param stored {@code} true, indica que en caso de tratarse de datos remotos, estos ya
	 * se estar&aacute;n cargados en un temporal y deben tomarse de este; {@code false} indica
	 * que se deber&aacute;n cargar los datos desde la fuente y, en caso de ser remotos, se
	 * crear&aacute; un temporal para ellos.
	 * @return Datos.
	 * @throws IOException Cuando no se pueden obtener los datos en caso de que estos sean remotos.
	 */
	public byte[] getData(final boolean stored) throws IOException {
		// Si se nos solicita un fichero remoto, calculamos cual seria el fichero
		// temporal que le corresponderia
		String tempResource = null;
		if (this.dataSource.startsWith(HTTP_SCHEME) || this.dataSource.startsWith(HTTPS_SCHEME) || this.dataSource.startsWith(FTP_SCHEME)) {
			try {
				tempResource = getTempFileName(this.dataSource, this.id);
			}
			catch (final Exception e) {
				LOGGER.warning("No se puede calcular el nombre de un temporal para un recurso remoto: " + e); //$NON-NLS-1$
				tempResource = null;
			}
		}

		// Si se indica que este fichero ya se almaceno
		// y deberia haber un recurso local, lo cargamos
		byte[] data = null;
		if (stored && tempResource != null) {
			try {
				final TempStore tempStore = TempStoreFactory.getTempStore();
				data = tempStore.retrieve(tempResource);
				tempStore.delete(tempResource);
			}
			catch (final Exception e) {
				LOGGER.warning(String.format("No se puede recuperar el recurso temporal %0s, se cargara de la fuente original: " + e, tempResource)); //$NON-NLS-1$
			}
		}

		// Si no, lo descargamos de la fuente original
		if (data == null) {
			checkDataSource(this.dataSource);
			data = DataDownloader.downloadData(this.dataSource);
		}

		// Finalmente, si se habia indicado que no habia recurso temporal
		// pero deberia haberlo, lo creamos
		if (!stored && tempResource != null) {
			TempStoreFactory.getTempStore().store(data, tempResource);
		}

		return data;
	}

	private static String getTempFileName(String source, String signId) throws NoSuchAlgorithmException {
		return Base64.encode(MessageDigest.getInstance("SHA-1").digest((source + signId).getBytes()), true); //$NON-NLS-1$
	}

	void setProcessResult(final ProcessResult pResult) {
		this.processResult = pResult;
	}

	ProcessResult getProcessResult() {
		this.processResult.setId(getId());
		return this.processResult;
	}

	void rollbackSave() {
		this.signSaver.rollback(this);
	}

	static class CallableResult {

		private final String signId;
		private final Exception exception;

		CallableResult(final String id) {
			this.signId = id;
			this.exception = null;
		}

		CallableResult(final String id, final Exception e) {
			this.signId = id;
			this.exception = e;
		}

		boolean isOk() {
			return this.exception == null;
		}

		Exception getError() {
			return this.exception;
		}

		String getSignatureId() {
			return this.signId;
		}
	}

	static final class ProcessResult {

		enum Result {
			NOT_STARTED,
			DONE_AND_SAVED,
			DONE_BUT_NOT_SAVED_YET,
			DONE_BUT_SAVED_SKIPPED,
			DONE_BUT_ERROR_SAVING,
			ERROR_PRE,
			ERROR_POST,
			SKIPPED,
			SAVE_ROLLBACKED;
		}

		private final Result result;
		private final String description;
		private String signId;

		boolean wasSaved() {
			return Result.DONE_AND_SAVED.equals(this.result);
		}

		static final ProcessResult PROCESS_RESULT_OK_UNSAVED = new ProcessResult(Result.DONE_BUT_NOT_SAVED_YET, null);
		static final ProcessResult PROCESS_RESULT_SKIPPED    = new ProcessResult(Result.SKIPPED,                null);
		static final ProcessResult PROCESS_RESULT_DONE_SAVED = new ProcessResult(Result.DONE_AND_SAVED,         null);
		static final ProcessResult PROCESS_RESULT_ROLLBACKED = new ProcessResult(Result.SAVE_ROLLBACKED,        null);

		ProcessResult(final Result r, final String d) {
			if (r == null) {
				throw new IllegalArgumentException(
					"El resultado no puede ser nulo" //$NON-NLS-1$
				);
			}
			this.result = r;
			this.description = d != null ? d : ""; //$NON-NLS-1$
		}

		@Override
		public String toString() {
			return "<signresult id=\"" + this.signId + "\" result=\"" + this.result + "\" description=\"" + this.description + "\"/>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}

		void setId(final String id) {
			this.signId = id;
		}

	}

}
