/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.util.Properties;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.ui.principal.AccessibilityOptionsPane;
import es.gob.afirma.ui.principal.MainOptionsPane;

/** Configuraci&oacute;n global de la aplicaci&oacute;n. */
public final class GeneralConfig {

    /** Opciones de configuraci&oacute;n general. */
    private static Properties configOptions = new Properties();
    /** Opciones de configuraci&oacute;n de firma. */
    private static Properties configSignatureOptions = new Properties();

    /** Recupera la configuraci&oacute;n de la ventana de opciones.
     * @return Configuraci&oacute;n de la ventana de opciones. */
    public static Properties getConfig() {
        final Properties config = new Properties();
        config.putAll(configOptions);
        return config;
    }

    /** Recupera la relaci&oacute;n de propiedades de firma predefinidas para las operaciones.
     * @return Propiedades de firma. */
    private static Properties getDefaultSignatureConfig() {
        final Properties defaultConfig = new Properties();
        defaultConfig.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        return defaultConfig;
    }

    /** Recupera el valor de una de las opciones de configuraci&oacute;n.
     * @param keyOption Clave de la opci&oacute;n deseada.
     * @return Valor de la opci&oacute;n. */
    public static String getOption(final String keyOption) {
        return configOptions.getProperty(keyOption);
    }

    /** Recupera el algoritmo de firma configurado.
     * @return Algoritmo de firma. */
    public static String getSignAlgorithm() {
        return configOptions.getProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM, MainOptionsPane.DEFAULT_DEFAULT_ALGORITHM);
    }

    /** Recupera la configuraci&oacute;n a establecer a las firmas.
     * @return Configuraci&oacute;n para las firmas. */
    public static Properties getSignConfig() {
        final Properties config = new Properties();
        config.putAll(configSignatureOptions);
        return config;
    }

    /** Indica si est&aacute; activada ka vista avanzada de configuraci&oacute;n.
     * @return <code>true</code> si est&aacute; activada ka vista avanzada de configuraci&oacute;n,
     *         <code>false</code> en caso contrario */
    public static boolean isAvanzados() {
        return Boolean.parseBoolean(configOptions.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "true")); //$NON-NLS-1$
    }

    /** Indica si el ususario ha activado o desactivado la opcion de cursor de texto grande
     * @return boolean Indicando el estado de la opcion */
    public static boolean isBigCaret() {
        return Boolean.parseBoolean(configOptions.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, "false")); //$NON-NLS-1$
    }

    /** Indica si el ususario ha activado o desactivado la opcion de tama&ntilde;o de fuente grande
     * @return boolean Indicando el estado de la opcion */
    public static boolean isBigFontSize() {
        return Boolean.parseBoolean(configOptions.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE, "false")); //$NON-NLS-1$
    }

    /** Indica si el ususario ha activado o desactivado la opcion de fuente en negrita
     * @return boolean Indicando el estado de la opcion */
    public static boolean isFontBold() {
        return Boolean.parseBoolean(configOptions.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, "false")); //$NON-NLS-1$
    }

    /** Indica si el ususario ha activado o desactivado la opcion de alto contraste
     * @return boolean Indicando el estado de la opcion */
    public static boolean isHighContrast() {
        return Boolean.parseBoolean(configOptions.getProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, "false")); //$NON-NLS-1$
    }

    /** Indica si el ususario ha activado o desactivado la opcion de maximizar todas las ventanas
     * @return boolean Indicando el estado de la opcion */
    public static boolean isMaximized() {
        return Boolean.parseBoolean(configOptions.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, "false")); //$NON-NLS-1$
    }

    /** Indica si el ususario ha activado o desactivado la opcion de remarcar elementos con foco
     * @return boolean Indicando el estado de la opcion */
    public static boolean isRemarked() {
        return Boolean.parseBoolean(configOptions.getProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, "false")); //$NON-NLS-1$
    }

    /** Carga la nueva configuraci&oacute;n de la ventana de opciones.
     * @param config Configuraci&oacute;n. */
    public static void loadConfig(final Properties config) {
        configOptions = new Properties();
        configOptions.putAll(config);

        loadSignatureConfig(config);
    }

    /** Carga la nueva configuraci&oacute;n de firma.
     * @param config Configuraci&oacute;n que se desea cargar. */
    private static void loadSignatureConfig(final Properties config) {
        configSignatureOptions = new Properties();
        configSignatureOptions.putAll(getDefaultSignatureConfig());

        if (config.containsKey(MainOptionsPane.MAIN_ALGORITHM_XML) && Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ALGORITHM_XML))) {
            configSignatureOptions.setProperty(
               "referencesDigestMethod", //$NON-NLS-1$
               config.getProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM, MainOptionsPane.DEFAULT_DEFAULT_ALGORITHM)
            );
        }
        if (config.containsKey(MainOptionsPane.MAIN_POLICY_ESTABLISHED) && Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED))) {
            configSignatureOptions.setProperty("policyIdentifier", config.getProperty(MainOptionsPane.MAIN_POLICY_IDENTIFIER, "")); //$NON-NLS-1$ //$NON-NLS-2$
            configSignatureOptions.setProperty("policyQualifier", config.getProperty(MainOptionsPane.MAIN_POLICY_QUALIFIER, "")); //$NON-NLS-1$ //$NON-NLS-2$
            configSignatureOptions.setProperty("policyIdentifierHash", config.getProperty(MainOptionsPane.MAIN_POLICY_HASH, "")); //$NON-NLS-1$ //$NON-NLS-2$
            configSignatureOptions.setProperty("policyIdentifierHashAlgorithm", config.getProperty(MainOptionsPane.MAIN_POLICY_HASH) != null ? MainOptionsPane.DEFAULT_POLICY_HASH_ALGORITHM : ""); //$NON-NLS-1$ //$NON-NLS-2$
        }

        if (config.containsKey(ContextOptionsConfig.OPTION_REASON) && config.getProperty(ContextOptionsConfig.OPTION_REASON).trim().length() > 0) {
            configSignatureOptions.setProperty("signReason", config.getProperty(ContextOptionsConfig.OPTION_REASON).trim()); //$NON-NLS-1$
        }
        if (config.containsKey(ContextOptionsConfig.OPTION_PRODUCTION_PLACE) && config.getProperty(ContextOptionsConfig.OPTION_PRODUCTION_PLACE)
                .trim()
                .length() > 0) {
        	configSignatureOptions.setProperty("signatureProductionCity", config.getProperty(ContextOptionsConfig.OPTION_PRODUCTION_PLACE).trim()); //$NON-NLS-1$
        }
        if (config.containsKey(ContextOptionsConfig.OPTION_SIGNER_CONTACT) && config.getProperty(ContextOptionsConfig.OPTION_SIGNER_CONTACT)
                .trim()
                .length() > 0) {
        	configSignatureOptions.setProperty("signerContact", config.getProperty(ContextOptionsConfig.OPTION_SIGNER_CONTACT).trim()); //$NON-NLS-1$
        }
        if (config.containsKey(ContextOptionsConfig.OPTION_PADES_FORMAT)) {
        	configSignatureOptions.setProperty("signatureSubFilter", config.getProperty(ContextOptionsConfig.OPTION_PADES_FORMAT)); //$NON-NLS-1$
        }
    }

    /** Establece una opci&oacute;n de configuraci&oacute;n.
     * @param optionKey Nombre de la opci&oacute;n de configuraci&oacute;n
     * @param optionValue Valor opci&oacute;n de configuraci&oacute;n */
    public static void setOption(final String optionKey, final String optionValue) {
        configOptions.setProperty(optionKey, optionValue);
    }

    private GeneralConfig() {
        // No permitimos la instanciacion
    }
}
