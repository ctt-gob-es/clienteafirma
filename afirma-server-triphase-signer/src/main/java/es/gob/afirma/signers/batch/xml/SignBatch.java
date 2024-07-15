/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch.xml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchException;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.SingleSignConstants;
import es.gob.afirma.signers.batch.TempStore;
import es.gob.afirma.signers.batch.TempStoreFactory;

/** Lote de firmas electr&oacute;nicas.
 * Un ejemplo de representaci&oacute;n XML de un lote podr&iacute;a ser:
 * <pre>
 * &lt;?xml version="1.0" encoding="UTF-8" ?&gt;
 * &lt;signbatch stoponerror="false" algorithm="SHA256" concurrenttimeout="9223372036854775807" Id="LOTE001"&gt;
 *  &lt;singlesign Id="7725374e-728d-4a33-9db9-3a4efea4cead"&gt;
 *   &lt;datasource&gt;http://google.com&lt;/datasource&gt;
 *   &lt;format&gt;XAdES&lt;/format&gt;
 *   &lt;suboperation&gt;sign&lt;/suboperation&gt;
 *   &lt;extraparams&gt;Iw0KI1RodSBKYW4gMTQgMTU6Mzc6MTcgQ0VUIDIwMTYNClNpZ25hdHVyZUlkPTc3MjUzNzRlLTcyOGQtNGEzMy05ZGI5LTNhNGVmZWE0Y2VhZA0K&lt;/extraparams&gt;
 *   &lt;signsaver&gt;
 *    &lt;class&gt;es.gob.afirma.signers.batch.SignSaverFile&lt;/class&gt;
 *    &lt;config&gt;Iw0KI1RodSBKYW4gMTQgMTU6Mzc6MTcgQ0VUIDIwMTYNCkZpbGVOYW1lPUNcOlxcVXNlcnNcXHRvbWFzXFxBcHBEYXRhXFxMb2NhbFxcVGVtcFxcRklSTUExLnhtbA0K&lt;/config&gt;
 *   &lt;/signsaver&gt;
 *  &lt;/singlesign&gt;
 *  &lt;singlesign Id="93d1531c-cd32-4c8e-8cc8-1f1cfe66f64a"&gt;
 *   &lt;datasource&gt;SG9sYSBNdW5kbw==&lt;/datasource&gt;
 *   &lt;format&gt;CAdES&lt;/format&gt;
 *   &lt;suboperation&gt;sign&lt;/suboperation&gt;
 *   &lt;extraparams&gt;Iw0KI1RodSBKYW4gMTQgMTU6Mzc6MTcgQ0VUIDIwMTYNClNpZ25hdHVyZUlkPTkzZDE1MzFjLWNkMzItNGM4ZS04Y2M4LTFmMWNmZTY2ZjY0YQ0K&lt;/extraparams&gt;
 *   &lt;signsaver&gt;
 *    &lt;class&gt;es.gob.afirma.signers.batch.SignSaverFile&lt;/class&gt;
 *    &lt;config&gt;Iw0KI1RodSBKYW4gMTQgMTU6Mzc6MTcgQ0VUIDIwMTYNCkZpbGVOYW1lPUNcOlxcVXNlcnNcXHRvbWFzXFxBcHBEYXRhXFxMb2NhbFxcVGVtcFxcRklSTUEyLnhtbA0K&lt;/config&gt;
 *   &lt;/signsaver&gt;
 *  &lt;/singlesign&gt;
 * &lt;/signbatch&gt;
 * </pre>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public abstract class SignBatch {

	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Lista de firmas a procesar. */
	protected final List<SingleSign> signs;

	protected final SingleSignConstants.DigestAlgorithm algorithm;

	private String id;
	String getId() {
		return this.id;
	}
	void setId(final String i) {
		if (i != null) {
			this.id = i;
		}
	}

	protected long concurrentTimeout = Long.MAX_VALUE;

	/** Obtiene el algoritmo de firma.
	 * @return Algoritmo de firma. */
	public SingleSignConstants.DigestAlgorithm getSignAlgorithm() {
		return this.algorithm;
	}

	/** Indica si se debe parar al encontrar un error o por el contrario se debe continuar con el proceso. */
	protected boolean stopOnError = false;

	/** Ejecuta el preproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @return Datos trif&aacute;sicos de pre-firma del lote.
	 * @throws BatchException Si hay errores irrecuperables en el proceso. */
	public abstract String doPreBatch(final X509Certificate[] certChain) throws BatchException;

	/** Ejecuta el postproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos del preproceso.
	 *           Debe contener los datos de todas y cada una de las firmas del lote.
	 * @return Registro del resultado general del proceso por lote, en un XML (<a href="../doc-files/resultlog-scheme.html">descripci&oacute;n
	 *         del formato</a>).
	 * @throws BatchException Si hay errores irrecuperables en el postproceso. */
	public abstract String doPostBatch(final X509Certificate[] certChain,
                                       final TriphaseData td) throws BatchException;

	/** Crea un lote de firmas a partir de su definici&oacute;n XML.
	 * @param xml XML de definici&oacute;n de lote de firmas (<a href="./doc-files/batch-scheme.html">descripci&oacute;n
	 *            del formato</a>).
	 * @throws IOException Si hay problemas en el tratamiento de datoso en el an&aacute;lisis del XML. */
	protected SignBatch(final byte[] xml) throws IOException {

		if (xml == null || xml.length < 1) {
			throw new IllegalArgumentException(
				"El XML de definicion de lote de firmas no puede ser nulo ni vacio" //$NON-NLS-1$
			);
		}

		// Definimos un manejador que extraera la informacion del XML
		final SignBatchXmlHandler handler = new SignBatchXmlHandler();

		try (final InputStream is = new ByteArrayInputStream(xml)) {

			final SAXParserFactory spf = SAXParserFactory.newInstance();
			spf.setNamespaceAware(true);
			final SAXParser saxParser = spf.newSAXParser();
			final XMLReader xmlReader = saxParser.getXMLReader();

			xmlReader.setContentHandler(handler);
			xmlReader.parse(new InputSource(is));
		}
		catch (final Exception e) {
			LOGGER.severe("Error al cargar el fichero XML de definicion de lote: " + e + //$NON-NLS-1$
					"\n" + LoggerUtil.getTrimStr(new String(xml, DEFAULT_CHARSET))); //$NON-NLS-1$
			throw new IOException("Error al cargar el fichero XML de definicion de lote: " + e, e); //$NON-NLS-1$
		}

		final SignBatchConfig config = handler.getBatchConfig();

		this.id = config.getId() != null ? config.getId() : UUID.randomUUID().toString();
		this.algorithm = config.getAlgorithm();
		this.concurrentTimeout = config.getConcurrentTimeout();
		this.stopOnError = config.isStopOnError();
		this.signs = config.getSingleSigns();
	}

	protected SignBatch(final List<SingleSign> signatures,
			            final SingleSignConstants.DigestAlgorithm algo,
			            final boolean soe) {

		if (signatures == null) {
			throw new IllegalArgumentException(
				"La lista de firmas del lote no puede ser nula" //$NON-NLS-1$
			);
		}
		if (algo == null) {
			throw new IllegalArgumentException(
				"El algoritmo de firma no puede ser nulo" //$NON-NLS-1$
			);
		}
		this.signs = signatures;
		this.stopOnError = soe;
		this.algorithm = algo;
		this.id = UUID.randomUUID().toString();
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder(
			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<signbatch stoponerror=\"" //$NON-NLS-1$
		);
		sb.append(Boolean.toString(this.stopOnError));
		sb.append("\" algorithm=\""); //$NON-NLS-1$
		sb.append(this.algorithm.getName());
		sb.append("\" concurrenttimeout=\""); //$NON-NLS-1$
		sb.append(this.concurrentTimeout);
		sb.append("\" Id=\""); //$NON-NLS-1$
		sb.append(this.id);
		sb.append("\">\n"); //$NON-NLS-1$
		for (final SingleSign ss : this.signs) {
			sb.append(ss.toString());
			sb.append('\n');
		}
		sb.append("</signbatch>\n"); //$NON-NLS-1$
		return sb.toString();
	}

	/** Indica si el proceso por lote debe detenerse cuando se encuentre un error.
	 * @param soe <code>true</code> si el proceso por lote debe detenerse cuando se encuentre un error,
	 *            <code>false</code> si se debe continuar con el siguiente elemento del lote cuando se
	 *            produzca un error. */
	public void setStopOnError(final boolean soe) {
		this.stopOnError = soe;
	}

	/** Obtiene el <i>log</i> con el resultado del proceso del lote.
	 * @return <i>Log</i> en formato XML con el resultado del proceso del lote. */
	protected String getResultLog() {
		// Iniciamos el log de retorno
		final StringBuilder ret = new StringBuilder("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<signs>\n"); //$NON-NLS-1$
		for (final SingleSign ss : this.signs) {
			ret.append(" "); //$NON-NLS-1$
			ret.append(printProcessResult(ss.getProcessResult()));
			ret.append("\n"); //$NON-NLS-1$
		}
		ret.append("</signs>"); //$NON-NLS-1$
		return ret.toString();
	}

	private static String printProcessResult(final ProcessResult result) {
		return "<signresult id=\"" + result.getId() //$NON-NLS-1$
			+ "\" result=\"" + result.getResult() //$NON-NLS-1$
			+ "\" description=\"" + (result.getDescription() != null ? result.getDescription() : "") + "\"/>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	/** Borra todos los ficheros temporales usados en el proceso del lote. */
	protected void deleteAllTemps() {
		final TempStore ts = TempStoreFactory.getTempStore();
		for (final SingleSign ss : this.signs) {
			ts.delete(ss, getId());
		}
	}
}
