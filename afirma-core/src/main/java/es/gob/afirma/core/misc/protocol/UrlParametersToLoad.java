/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
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
	private static final String EXTENSIONS_PARAM = "extensions"; //$NON-NLS-1$
	
	/** Par&aacute;metro de entrada que indica la descripción del tipo de fichero. */
	private static final String DESCRIPTION_PARAM = "description"; //$NON-NLS-1$
	
	/** Par&aacute;metro de entrada que indica la ruta por defecto. */
	private static final String FILEPATH_PARAM = "filePath"; //$NON-NLS-1$
	
				
	private String minimumVersion;
		
	/**
	 * Attribute that represents the configuration for the load/multiload option.
	 */
	private Boolean multiload;
	
	/**
	 * The title param from invocation URL
	 */
	private String title;
	/**
	 * The extensions param from invocation URL
	 */
	private String extensions;
	/**
	 * The description param from invocation URL
	 */
	private String description;
	/**
	 * The filepath param from invocation URL
	 */
	private String filepath;
	
	
	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumVersion() {
		return this.minimumVersion;
	}
	
	UrlParametersToLoad() {
		setMinimumVersion(null);
		setMultiload(null);
		setTitle(null);
		setExtensions(null);
		setDescription(null);
		setFilepath(null);
	}
	
	void setMinimumVersion(final String minVer) {
		this.minimumVersion = minVer;
	}
				
	/**
	 * Setter method for the multiload param
	 * @param multiload The new multiload to set
	 */
	public void setMultiload(final Boolean multiload) {
		this.multiload = multiload;
	}
	
	/**
	 * Getter method for the multiload attribute
	 * @return The multiload param
	 */
	public Boolean getMultiload() {
		return this.multiload;
	}
	
		
	/**
	 * Getter method for the title attribute
	 * @return The title param
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Setter method for the title param
	 * @param title The new title to set
	 */
	public void setTitle(final String title) {
		this.title = title;
	}

	/**
	 * Getter method for the extensions param
	 * @return The extensions param
	 */
	public String getExtensions() {
		return extensions;
	}

	/**
	 * Setter method for the extensions param
	 * @param extensions The new extensions to set
	 */
	public void setExtensions(final String extensions) {
		this.extensions = extensions;
	}

	/**
	 * Getter method for the description param
	 * @return The description param
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Setter method for the description para,
	 * @param description The new description to set
	 */
	public void setDescription(final String description) {
		this.description = description;
	}

	/**
	 * Getter method for the filepath param
	 * @return The filepath param
	 */
	public String getFilepath() {
		return filepath;
	}

	/**
	 * Setter method for the filepath param
	 * @param filepath The new filepath to set
	 */
	public void setFilepath(final String filepath) {
		this.filepath = filepath;
	}

	void setLoadParameters(final Map<String, String> params) throws ParameterException {
		
		// Version minima requerida del protocolo que se debe soportar
		if (params.containsKey(VER_PARAM)) {
			setMinimumVersion(params.get(VER_PARAM));
		}
		else {
			setMinimumVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}
		
		// Parámetro que indica si se debe utilizar el diálogo de selección
		// simple o múltiple de ficheros
		if (params.containsKey(MULTILOAD_PARAM)) {
			setMultiload(Boolean.valueOf(params.get(MULTILOAD_PARAM)));
		}
		else {
			setMultiload(Boolean.FALSE);
		}
				
		if (params.containsKey(TITLE_PARAM)) {
						
			setTitle(params.get(TITLE_PARAM));
		} else {
			setTitle(null);
		}
		
		if (params.containsKey(EXTENSIONS_PARAM)) {
			setExtensions(params.get(EXTENSIONS_PARAM));
		} else {
			setExtensions(null);
		}
		
		if (params.containsKey(DESCRIPTION_PARAM)) {
			setDescription(params.get(DESCRIPTION_PARAM));
		} else {
			setDescription(null);
		}
		
		if (params.containsKey(FILEPATH_PARAM)) {
			setFilepath(params.get(FILEPATH_PARAM));
		} else {
			setFilepath(null);
		}
	}	

}
