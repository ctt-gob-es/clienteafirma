package es.gob.afirma.ui.utils;

final class ContextOptionsConfig {

	private ContextOptionsConfig() {
		// No permitimos la instanciacion
	}

    /** Clave del lugar en el que se realiza de firma (com&uacute;nmente, la poblaci&oacute;n). */
    static final String OPTION_PRODUCTION_PLACE = "signProductionPlace";

    /** Clave de la raz&oacute;n de firma. */
    static final String OPTION_REASON = "signReason";

    /** Clave del contacto del firmante. */
    static final String OPTION_SIGNER_CONTACT = "signerContact";

    /** Recupera un dato de la configuraci&oacute;n.
     * @param optionKey Clave de la configuraci&oacute;n que queremos recuperar.
     * @return Configuraci&oacute;n. */
    static String getOption(final String optionKey) {
        return GeneralConfig.getOption(optionKey);
    }

    /** Establece una opci&oacute;n de configuraci&oacute;n para PDF.
     * @param optionKey Clave de la opci&oacute;n.
     * @param optionValue Valor de la configuraci&oacute;n. */
    static void setOption(final String optionKey, final String optionValue) {
        GeneralConfig.setOption(optionKey, optionValue);
    }
}
