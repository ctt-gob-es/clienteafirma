/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch.xml;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchConfigManager;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.SignSaver;
import es.gob.afirma.signers.batch.SingleSignConstants;
import es.gob.afirma.signers.batch.SingleSignConstants.SignFormat;
import es.gob.afirma.signers.batch.SingleSignConstants.SignSubOperation;
import es.gob.afirma.signers.batch.TempStore;
import es.gob.afirma.signers.batch.TempStoreFactory;

/** Firma electr&oacute;nica &uacute;nica dentro de un lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public class SingleSign {

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

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	protected Properties extraParams;

	protected String dataRef;

	protected SignFormat format;

	protected String id;

	protected SignSubOperation subOperation;

	private SignSaver signSaver;

	private ProcessResult processResult = new ProcessResult(ProcessResult.Result.NOT_STARTED, null, false);

	/** Crea una definici&oacute;n de tarea de firma electr&oacute;nica &uacute;nica.
	 * @param id Identificador de la firma. */
	public SingleSign(final String id) {
		this.id =  id;
		this.extraParams = new Properties();
		// El identificador de la firma debe transmitirse al firmador trifasico a traves
		// de los extraParams para que este lo utilice y asi podamos luego asociar la
		// firma con los datos a los que corresponden
		this.extraParams.put(PROP_ID, getId());
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

		this.dataRef = dataSrc;
		this.format = fmt;

		this.id = id != null ? id : UUID.randomUUID().toString();

		// El identificador de la firma debe transmitirse al firmador trifasico a traves
		// de los extraParams para que este lo utilice y asi podamos luego asociar la
		// firma con los datos a los que corresponden
		this.extraParams = xParams != null ? xParams : new Properties();
		this.extraParams.put(PROP_ID, getId());

		this.subOperation = subOp;
		this.signSaver = ss;
	}

	public SingleSign() {
		// No hace nada
	}

	void save(final byte[] dataToSave) throws IOException {
		this.signSaver.saveSign(this, dataToSave);
	}

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

	public SignSubOperation getSubOperation() {
		return this.subOperation;
	}

	public String getDataRef() {
		return this.dataRef;
	}

	public void setExtraParams(final Properties extraParams) {
		// El identificador de la firma debe transmitirse al firmador trifasico a traves
		// de los extraParams para que este lo utilice y asi podamos luego asociar la
		// firma con los datos a los que corresponden
		this.extraParams = extraParams != null ? extraParams : new Properties();
		this.extraParams.put(PROP_ID, getId());
	}

	public void setDataRef(final String dataRef) {
		this.dataRef = dataRef;
	}

	public void setFormat(final SignFormat format) {
		this.format = format;
	}

	public void setSubOperation(final SignSubOperation subOperation) {
		this.subOperation = subOperation;
	}

	void setSignSaver(final SignSaver signSaver) {
		this.signSaver = signSaver;
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
			else if (dataSource.equals(allowed)) {
				return;
			}
		}
		throw new SecurityException(
			"Origen de datos no valido (" + dataSource + "), los origenes validos son: " + //$NON-NLS-1$ //$NON-NLS-2$
				Arrays.asList(BatchConfigManager.getAllowedSources())
		);
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
		sb.append(this.dataRef);
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
			LOGGER.severe(
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
			LOGGER.severe(
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
			            final SingleSignConstants.DigestAlgorithm algorithm) throws IOException,
			                                                                      AOException {
		return SingleSignPreProcessor.doPreProcess(this, certChain, algorithm);
	}

	/** Obtiene la tarea de preproceso de firma para ser ejecutada en paralelo.
	 * @param certChain Cadena de certificados del firmante.
	 * @param algorithm Algoritmo de firma.
	 * @return Tarea de preproceso de firma para ser ejecutada en paralelo. */
	Callable<String> getPreProcessCallable(final X509Certificate[] certChain,
                                                  final SingleSignConstants.DigestAlgorithm algorithm) {
		return new PreProcessCallable(this, certChain, algorithm);
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
			                  final SingleSignConstants.DigestAlgorithm algorithm,
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
			                                                          final SingleSignConstants.DigestAlgorithm algorithm,
			                                                          final String batchId) {
		return new PostProcessCallable(this, certChain, td, algorithm, batchId);
	}

	public Callable<CallableResult> getSaveCallable(final TempStore ts, final String batchId) {

		return new SaveCallable(this, this.signSaver, ts, batchId);
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
	 * @param stored {@code true}, indica que en caso de tratarse de datos remotos, estos ya
	 * estar&aacute;n cargados en un temporal y deben tomarse de este; {@code false} indica
	 * que se deber&aacute;n cargar los datos desde la fuente y, en caso de ser remotos, se
	 * crear&aacute; un temporal para ellos.
	 * @return Datos.
	 * @throws IOException Cuando no se pueden obtener los datos en caso de que estos sean remotos.
	 */
	public byte[] getData(final boolean stored) throws IOException {
		// Si se nos solicita un fichero remoto, calculamos cual seria el fichero
		// temporal que le corresponderia
		String tempResource = null;
		if (this.dataRef.startsWith(HTTP_SCHEME) || this.dataRef.startsWith(HTTPS_SCHEME) || this.dataRef.startsWith(FTP_SCHEME)) {
			try {
				tempResource = getTempFileName(this.dataRef, this.id);
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
				LOGGER.warning(String.format("No se puede recuperar el recurso temporal %s, se cargara de la fuente original: " + e, tempResource)); //$NON-NLS-1$
			}
		}

		// Si no, lo descargamos de la fuente original
		if (data == null) {
			checkDataSource(this.dataRef);
			data = DataDownloader.downloadData(this.dataRef);
		}

		// Comprobamos si se ha establecido un tamano maximo de documento y si este lo supera
		final long maxDocSize = BatchConfigManager.getBatchXmlDocSize();
		if (maxDocSize > 0 && data.length > maxDocSize) {
			LOGGER.severe("El tamano del documento supera el permitido: " + maxDocSize); //$NON-NLS-1$
			throw new IOException(
					"El tamano del documento supera el permitido: " + maxDocSize); //$NON-NLS-1$
		}

		// Finalmente, si se habia indicado que no habia recurso temporal
		// pero deberia haberlo, lo creamos
		if (!stored && tempResource != null) {
			TempStoreFactory.getTempStore().store(data, tempResource);
		}

		return data;
	}

	private static String getTempFileName(final String source, final String signId) throws NoSuchAlgorithmException {
		return Base64.encode(MessageDigest.getInstance("SHA-1").digest((source + signId).getBytes()), true); //$NON-NLS-1$
	}

	public void setProcessResult(final ProcessResult pResult) {
		this.processResult = pResult;
	}

	public ProcessResult getProcessResult() {
		if (this.processResult != null) {
			this.processResult.setId(getId());
		}
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

	static class PreProcessCallable implements Callable<String> {
		private final SingleSign ss;
		private final X509Certificate[] certChain;
		private final SingleSignConstants.DigestAlgorithm algorithm;

		public PreProcessCallable(final SingleSign ss, final X509Certificate[] certChain,
                final SingleSignConstants.DigestAlgorithm algorithm) {
			this.ss = ss;
			this.certChain = certChain;
			this.algorithm = algorithm;
		}

		@Override
		public String call() throws Exception {
			return SingleSignPreProcessor.doPreProcess(this.ss, this.certChain, this.algorithm);
		}
	}

	static class PostProcessCallable implements Callable<CallableResult> {

		private final SingleSign ss;
		private final X509Certificate[] certChain;
		private final TriphaseData td;
		private final SingleSignConstants.DigestAlgorithm algorithm;
		private final String batchId;

		public PostProcessCallable(final SingleSign ss, final X509Certificate[] certChain,
                final TriphaseData td, final SingleSignConstants.DigestAlgorithm algorithm,
                final String batchId) {
			this.ss = ss;
			this.certChain = certChain;
			this.td = td;
			this.algorithm = algorithm;
			this.batchId = batchId;
		}

		@Override
		public CallableResult call() {
			try {
				SingleSignPostProcessor.doPostProcess(this.ss, this.certChain, this.td, this.algorithm, this.batchId);
			}
			catch(final Exception e) {
				return new CallableResult(this.ss.getId(), e);
			}
			return new CallableResult(this.ss.getId());
		}
	}

	static class SaveCallable implements Callable<CallableResult> {

		private final SingleSign ss;
		private final SignSaver signSaver;
		private final TempStore ts;
		private final String batchId;

		public SaveCallable(final SingleSign ss, final SignSaver signSaver, final TempStore ts, final String batchId) {
			this.ss = ss;
			this.signSaver = signSaver;
			this.ts = ts;
			this.batchId = batchId;
		}

		@Override
		public CallableResult call() {
			try {
				final byte[] dataToSave = this.ts.retrieve(this.ss, this.batchId);
				this.signSaver.saveSign(this.ss, dataToSave);
			}
			catch(final Exception e) {
				return new CallableResult(this.ss.getId(), e);
			}
			return new CallableResult(this.ss.getId());
		}
	}
}
