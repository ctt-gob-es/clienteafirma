package es.gob.afirma.ui.utils;

class ContextOptionsConfig {
	/** Clave de la raz&oacute;n de firma. */
	static final String OPTION_REASON = "signReason";
	
	/** Clave del lugar en el que se realiza de firma (com&uacute;nmente, la poblaci&oacute;n). */
	static final String OPTION_PRODUCTION_PLACE = "signProductionPlace";
	
	/** Clave del contacto del firmante. */
	static final String OPTION_SIGNER_CONTACT = "signerContact";
	
	/**
	 * Establece una opci&oacute;n de configuraci&oacute;n para PDF.
	 * @param optionKey Clave de la opci&oacute;n.
	 * @param optionValue Valor de la configuraci&oacute;n.
	 */
	static void setOption(String optionKey, String optionValue) {
		GeneralConfig.setOption(optionKey, optionValue);
	}
	
	/**
	 * Recupera un dato de la configuraci&oacute;n.
	 * @param optionKey Clave de la configuraci&oacute;n que queremos recuperar.
	 * @return Configuraci&oacute;n.
	 */
	static String getOption(String optionKey) {
		return GeneralConfig.getOption(optionKey);
	}
}
