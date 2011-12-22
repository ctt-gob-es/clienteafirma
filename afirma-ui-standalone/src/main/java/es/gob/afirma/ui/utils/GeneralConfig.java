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
import es.gob.afirma.ui.principal.ContextOptionsPane;
import es.gob.afirma.ui.principal.MainOptionsPane;

/** Configuraci&oacute;n global de la aplicaci&oacute;n. */
public class GeneralConfig {

    /** Opciones de configuraci&oacute;n general. */
    private static Properties configOptions = new Properties();
    /** Opciones de configuraci&oacute;n de firma. */
    private static Properties configSignatureOptions = new Properties();
    /** Estancia unica de la clase de configuraci&oacute;n. */
    private static GeneralConfig instance = null;

    public static final String OPTION_ADVANCED_VIEW = "advancedView"; //$NON-NLS-1$

    public static final String OPTION_ALGORITHM = "algoritmo"; //$NON-NLS-1$

    public static final String OPTION_USE_ALGORITHM_XML = "algoritmoXML"; //$NON-NLS-1$

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

    /** Recupera la configuraci&oacute;n de firma.
     * @return Devuelve la configurac&oacute;n general de firma configurada. */
    public static GeneralConfig getInstance() {
        if (instance == null) {
            instance = new GeneralConfig();
        }

        return instance;
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

    public static String getSignContactInfo() {
        return configOptions.getProperty(ContextOptionsPane.KEY_CONTACT_INFO, ""); //$NON-NLS-1$
    }

    public static String getSignPolicyOid() {
        return configOptions.getProperty(MainOptionsPane.MAIN_POLICY_OID, ""); //$NON-NLS-1$
    }

    public static String getSignPolicyUrl() {
        return configOptions.getProperty(MainOptionsPane.MAIN_POLICY_URL, ""); //$NON-NLS-1$
    }

    public static String getSignProductionPlace() {
        return configOptions.getProperty(ContextOptionsPane.KEY_PRODUCTION_PLACE, ""); //$NON-NLS-1$
    }

    public static String getSignSubject() {
        return configOptions.getProperty(ContextOptionsPane.KEY_SUBJECT, ""); //$NON-NLS-1$
    }

    public static boolean isAvanzados() {
        return Boolean.parseBoolean(configOptions.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "false")); //$NON-NLS-1$
    }

    /** Indica si el ususario ha activado o desactivado la opcion de desactivar accesibilidad en ventanas de seleci&oacute;n de archivos
     * @return boolean Indicando el estado de la opcion */
    // public static boolean isAccessibility() {
    //		return Boolean.parseBoolean(configOptions.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_ACCESSIBILITY, "true")); //$NON-NLS-1$
    // }

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

    public static boolean isSignAlgorithmUsedToXml() {
        return Boolean.parseBoolean(configOptions.getProperty(MainOptionsPane.MAIN_ALGORITHM_XML, "false")); //$NON-NLS-1$
    }

    public static boolean isSignPolicyEstablished() {
        return Boolean.parseBoolean(configOptions.getProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, "false")); //$NON-NLS-1$
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

        if (config.contains(MainOptionsPane.MAIN_ALGORITHM_XML)) {
            configSignatureOptions.setProperty("referencesDigestMethod",
                                               config.getProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM, MainOptionsPane.DEFAULT_DEFAULT_ALGORITHM));
        }
        if (config.contains(MainOptionsPane.MAIN_POLICY_ESTABLISHED)) {
            configSignatureOptions.setProperty("policyQualifier", config.getProperty(MainOptionsPane.MAIN_POLICY_OID, ""));
            configSignatureOptions.setProperty("policyIdentifier", config.getProperty(MainOptionsPane.MAIN_POLICY_URL, ""));
        }

        if (config.contains(ContextOptionsConfig.OPTION_REASON) && config.getProperty(ContextOptionsConfig.OPTION_REASON).trim().length() > 0) {
            configSignatureOptions.setProperty("signReason", config.getProperty(ContextOptionsConfig.OPTION_REASON).trim());
        }
        if (config.contains(ContextOptionsConfig.OPTION_PRODUCTION_PLACE) && config.getProperty(ContextOptionsConfig.OPTION_PRODUCTION_PLACE)
                .trim()
                .length() > 0) {
            config.setProperty("signatureProductionCity", config.getProperty(ContextOptionsConfig.OPTION_PRODUCTION_PLACE).trim());
        }
        if (config.contains(ContextOptionsConfig.OPTION_SIGNER_CONTACT) && config.getProperty(ContextOptionsConfig.OPTION_SIGNER_CONTACT)
                .trim()
                .length() > 0) {
            config.setProperty("signerContact", config.getProperty(ContextOptionsConfig.OPTION_SIGNER_CONTACT).trim());
        }
    }

    public static void setOption(final String optionKey, final String optionValue) {
        configOptions.setProperty(optionKey, optionValue);
    }

    private GeneralConfig() {}
}
