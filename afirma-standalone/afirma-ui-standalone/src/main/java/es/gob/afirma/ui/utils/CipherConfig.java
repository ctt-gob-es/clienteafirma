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

import java.util.Vector;

import es.gob.afirma.ciphers.jce.AOSunJCECipher;
import es.gob.afirma.core.ciphers.AOCipher;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;

/** Clase para configurar un cifrador dado un algoritmo */
public final class CipherConfig {

    private static class CipherConfigures {

        private final AOCipherConfig config;
        AOCipherConfig getConfig() {
        	return this.config;
        }

        private final AOCipher provider;
        AOCipher getProvider() {
        	return this.provider;
        }

        CipherConfigures(final AOCipherConfig config, final AOCipher provider) {
            this.config = config;
            this.provider = provider;
        }
    }
    private static CipherConfigures[] configures = null;
    /** Listado de proveedores de cifrado soportados (AOCipher). Para agregar nuevos proveedores
     * basta con agregarlos al array. */
    private static final AOCipher[] SUPPORTED_CIPHER_PROVIDERS = new AOCipher[] {
                                                                                 new AOSunJCECipher()
    };

    static {
        final Vector<CipherConfigures> configs = new Vector<>();
        for (final AOCipher cipher : SUPPORTED_CIPHER_PROVIDERS) {
            for (final AOCipherConfig config : cipher.getSupportedConfigs()) {
                configs.add(new CipherConfigures(config, cipher));
            }
        }
        configures = new CipherConfigures[configs.size()];
        configs.copyInto(configures);
    }

    private AOCipher cipher;

    private AOCipherConfig config;

    /** Crea una nueva configuraci&oacute;n de cifrado.
     * @param algoritmo Algoritmo de cifrado */
    public CipherConfig(final String algoritmo) {
        configurarCipher(algoritmo);
    }

    /** Configura un cifrador dado un algoritmo
     * @param algoritmo */
    private void configurarCipher(final String algoritmo) {
        // Recogemos la configuracion actual
        this.config = new AOCipherConfig(
    		AOCipherAlgorithm.getValueOf(algoritmo),
    		null,
    		null
		);

        // Iniciamos el cifrador
        this.cipher = getCipher(this.config);
    }

    /** Obtiene el cifrador.
     * @return Cifrador correspondiente a esta configuraci&oacute;n */
    public AOCipher getCipher() {
        return this.cipher;
    }

    /** Recupera el proveedor de cifrado que nos ofrece la configuraci&oacute;n indicada. En caso
     * de no soportarse esta configuracion, se devolvera <code>null</code>.
     * @param algorithmConfig Configuraci&oacute;n de cifrado.
     * @return Proveedor que soporta la configuraci&oacute;n indicada. */
    public static AOCipher getCipher(final AOCipherConfig algorithmConfig) {
        for (final CipherConfigures configure : configures) {
            if (configure.getConfig().equals(algorithmConfig)) {
                return configure.getProvider();
            }
        }
        return null;
    }

    /** Obtiene la configuraci&oacute;n de cifrado.
     * @return Configuraci&oacute;n de cifrado */
    public AOCipherConfig getConfig() {
        return this.config;
    }
}
