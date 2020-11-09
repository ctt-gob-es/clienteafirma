/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

import java.util.Map;

/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n. */
public final class UrlParametersToLoad extends UrlParameters {

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que indica si el proceso de carga permite la selecci&oacute;n de uno o varios ficheros. */
	private static final String MULTILOAD_PARAM = "multiload"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que indica el t&iacute;tulo del dialogo. */
	private static final String TITLE_PARAM = "title"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que indica las extensiones de ficheros permitidos. */
	private static final String EXTENSIONS_PARAM = "exts"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que indica la descripcion del tipo de fichero. */
	private static final String DESCRIPTION_PARAM = "desc"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que indica la ruta por defecto. */
	private static final String FILEPATH_PARAM = "filePath"; //$NON-NLS-1$


	private String minimumProtocolVersion;

	/**
	 * Atributo que representa si se trata de una operaci&oacute;n de carga ({@code false}}) o multicarga ({@code true}})
	 */
	private boolean multiload;

	/**
	 * Atributo que representa el t&iacute;tulo de la ventana de di&aacute;logo de selecci&oacute;n de fichero
	 */
	private String title;
	/**
	 * Atributo que representa las extensiones de fichero permitidas
	 */
	private String extensions;
	/**
	 * Atributo que representa la descripci&oacute;n de los tipos de ficheros que se pueden cargar
	 */
	private String description;
	/**
	 * Atributo que representa la ruta por defecto de ficheros para cargar
	 */
	private String filepath;

	/**
	 * Construye el conjunto de par&aacute;metros vac&iacute;o.
	 */
	public UrlParametersToLoad() {
		setMinimumProtocolVersion(null);
		setMultiload(false);
		setTitle(null);
		setExtensions(null);
		setDescription(null);
		setFilepath(null);
	}

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumProtocolVersion() {
		return this.minimumProtocolVersion;
	}

	/** Establece la versi&oacute;n m&iacute;nima exigida del protocolo de comunicaci&oacute;n.
	 * @param minVer Versi&oacute;n m&iacute;nima del protocolo.
	 */
	void setMinimumProtocolVersion(final String minVer) {
		this.minimumProtocolVersion = minVer;
	}


	/** Establece el tipo de operaci&oacute;n de la operaci&oacute;n de carga/multicarga.
	 * @param multiload Valor del tipo de &oacute;n: ({@code false}}) para carga o
	 *                  ({@code true}}) para multicarga. */
	public void setMultiload(final boolean multiload) {
		this.multiload = multiload;
	}

	/** Obtiene la opci&oacute;n de configuraci&oacute;n de la operaci&oacute;n de carga/multicarga.
	 * @return multiload Valor del tipo de &oacute;n: ({@code false}}) para carga o
	 *                   ({@code true}}) para multicarga. */
	public boolean getMultiload() {
		return this.multiload;
	}


	/** Obtiene el t&iacute;tulo del di&aacute;logo de carga de datos.
	 * @return T&iacute;tulo del di&aacute;logo de carga de datos. */
	public String getTitle() {
		return this.title;
	}

	/** Establece el t&iacute;tulo del di&aacute;logo de carga de datos.
	 * @param title T&iacute;tulo del di&aacute;logo de carga de datos. */
	public void setTitle(final String title) {
		this.title = title;
	}

	/** Obtiene, como una lista separada por comas, las extensiones permitidas para el
	 * fichero(s) a cargar.
	 * @return Lista separada por comas con las extensiones para el fichero(s) a cargar. */
	public String getExtensions() {
		return this.extensions;
	}

	/** Establece las extensiones permitidas para el fichero(s) a cargar.
	 * Deben indicarse como una lista separada por comas
	 * @param extensions Extensiones permitidas, indicadas como una lista separada por comas. */
	public void setExtensions(final String extensions) {
		this.extensions = extensions;
	}

	/** Obtiene la descripci&oacute;n del tipo de fichero a cargar.
	 * @return Descripci&oacute;n del tipo de fichero a cargar. */
	public String getDescription() {
		return this.description;
	}

	/** Establece la descripci&oacute;n del tipo de fichero a cargar.
	 * @param description Descripci&oacute;n del tipo de fichero a cargar. */
	public void setDescription(final String description) {
		this.description = description;
	}

	/** Obtiene la ruta por defecto para cargar ficheros.
	 * @return Ruta por defecto para cargar ficheros. */
	public String getFilepath() {
		return this.filepath;
	}

	/** Establece la ruta por defecto para cargar ficheros.
	 * @param filepath Ruta por defecto para cargar ficheros. */
	public void setFilepath(final String filepath) {
		this.filepath = filepath;
	}

	/**Establece los par&aacute;metros propios de la operaci&oacute;n de carga/multicarga.
	 * @param params Mapa de valores obtenidos de la URL de invocaci&oacute;n de la operaci&oacute;n. */
	public void setLoadParameters(final Map<String, String> params) {

		// Version minima requerida del protocolo que se debe soportar
		if (params.containsKey(VER_PARAM)) {
			setMinimumProtocolVersion(params.get(VER_PARAM));
		}
		else {
			setMinimumProtocolVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}

		// Parametro que indica si se debe utilizar el dialogo de seleccion
		// simple o multiple de ficheros
		if (params.containsKey(MULTILOAD_PARAM)) {
			setMultiload(Boolean.parseBoolean(params.get(MULTILOAD_PARAM)));
		}
		else {
			setMultiload(false);
		}

		if (params.containsKey(TITLE_PARAM)) {
			setTitle(params.get(TITLE_PARAM));
		}
		else {
			setTitle(null);
		}

		if (params.containsKey(EXTENSIONS_PARAM)) {
			setExtensions(params.get(EXTENSIONS_PARAM));
		}
		else {
			setExtensions(null);
		}

		if (params.containsKey(DESCRIPTION_PARAM)) {
			setDescription(params.get(DESCRIPTION_PARAM));
		}
		else {
			setDescription(null);
		}

		if (params.containsKey(FILEPATH_PARAM) && !"".equals(params.get(FILEPATH_PARAM))) { //$NON-NLS-1$
			setFilepath(params.get(FILEPATH_PARAM));
		}
		else {

			setFilepath(null);
		}
	}

}
