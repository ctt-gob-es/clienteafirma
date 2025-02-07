package es.gob.afirma.standalone.protocol;

/**
 * ExtraParams directamente utilizados por Autofirma para configurar su comportamiento.
 */
class AfirmaExtraParams {

	/** Comprobar las firmas antes de cofirmar o contrafirmar. */
	static final String CHECK_SIGNATURES = "checkSignatures"; //$NON-NLS-1$

	/** Extensiones de fichero a cargar. */
	static final String LOAD_FILE_EXTS = "filenameExts"; //$NON-NLS-1$

	/** Descripci&oacute;n de ficheros a cargar. */
	static final String LOAD_FILE_DESCRIPTION = "filenameDescription"; //$NON-NLS-1$

	/** Directorio del di&aacute;logo de carga de fichero. */
	static final String LOAD_FILE_CURRENT_DIR = "filenameCurrentDir"; //$NON-NLS-1$

	/** Nombre de fichero por defecto del di&aacute;logo de carga de fichero. */
	static final String LOAD_FILE_FILENAME = "filenameActualName"; //$NON-NLS-1$

	/** Nodos objetivo de contrafirma (tree o leafs). */
	static final String TARGET = "target"; //$NON-NLS-1$

	/** Modo de firma (implicit o explicit). */
	static final String MODE = "mode"; //$NON-NLS-1$

	/** Extensiones de fichero a guardar. */
	static final String SAVE_FILE_EXTS = "filenameSaveExts"; //$NON-NLS-1$

	/** Descripci&oacute;n del fichero a guardar. */
	static final String SAVE_FILE_DESCRIPTION = "filenameSaveDescription"; //$NON-NLS-1$

	/** Directorio del di&aacute;logo de guardado de fichero. */
	static final String SAVE_FILE_CURRENT_DIR = "filenameSaveCurrentDir"; //$NON-NLS-1$

	/** Directorio del di&aacute;logo de guardado de fichero. */
	static final String HEADLESS = "headless"; //$NON-NLS-1$
}
