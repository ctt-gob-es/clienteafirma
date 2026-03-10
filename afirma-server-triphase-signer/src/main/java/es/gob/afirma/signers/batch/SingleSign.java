/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch;

import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.signers.batch.SingleSignConstants.SignFormat;
import es.gob.afirma.signers.batch.SingleSignConstants.SignSubOperation;
import es.gob.afirma.triphase.server.ConfigManager;

/** Firma electr&oacute;nica &uacute;nica dentro de un lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public class SingleSign {

	private static final String PROP_ID = "SignatureId"; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger(ConfigManager.LOGGER_NAME);

	protected Properties extraParams;

	protected String dataRef;

	protected SignFormat format;

	protected String id;

	protected SignSubOperation subOperation;

	private ProcessResult processResult = new ProcessResult(ProcessResult.Result.NOT_STARTED, null, false);

	/**
	 * Impide la instanciaci&oacute;n de la clase salvo por parte de subclases.
	 */
	protected SingleSign() {
		// No hace nada
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

	/**
	 * Recupera el identificador asignado en el lote a la firma.
	 * @return Identificador.
	 */
	public String getId() {
		return this.id;
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
}
