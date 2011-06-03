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

import es.gob.afirma.ciphers.AOCipher;
import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.ciphers.AOSunJCECipher;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;

/**
 * Clase para configurar un cifrador dado un algoritmo
 */
public class CipherConfig {

	private AOCipherConfig config;
	private AOCipher cipher;
	private AOCipherAlgorithm cipherAlgoritm;

	public CipherConfig (String algoritmo) {;
	configurarCipher(algoritmo);
	}

	public AOCipherConfig getConfig() {
		return config;
	}

	public AOCipher getCipher() {
		return cipher;
	}

	private static CipherConfigures[] configures = null;

	/**
	 * Listado de proveedores de cifrado soportados (AOCipher). Para agregar nuevos proveedores
	 * basta con agregarlos al array.
	 */
	private static final AOCipher[] SUPPORTED_CIPHER_PROVIDERS = new AOCipher[] {
		new AOSunJCECipher()
	};

	static {
		final Vector<CipherConfigures> configs = new Vector<CipherConfigures>();
		for(AOCipher cipher : SUPPORTED_CIPHER_PROVIDERS) {
			for(AOCipherConfig config : cipher.getSupportedConfigs()) {
				configs.add(new CipherConfigures(config, cipher));
			}
		}
		configures = new CipherConfigures[configs.size()];
		configs.copyInto(configures);
	}

	private static class CipherConfigures {
		AOCipherConfig config = null;
		AOCipher provider = null;

		CipherConfigures(AOCipherConfig config, AOCipher provider) {
			this.config = config;
			this.provider = provider;
		}
	}

	/**
	 * Configura un cifrador dado un algoritmo
	 * @param algoritmo
	 */
	private void configurarCipher(String algoritmo) {
		cipherAlgoritm = AOCipherAlgorithm.getValueOf(algoritmo);
        // Recogemos la configuracion actual
        config = new AOCipherConfig(cipherAlgoritm,null,null);
        
        // Iniciamos el cifrador
        cipher = getCipher(config);
	}
	
	/**
	 * Recupera el proveedor de cifrado que nos ofrece la configuraci&oacute;n indicada. En caso
	 * de no soportarse esta configuracion, se devolvera <code>null</code>.
	 * @param algorithmConfig Configuraci&oacute;n de cifrado.
	 * @return Proveedor que soporta la configuraci&oacute;n indicada.
	 */
	public AOCipher getCipher(AOCipherConfig algorithmConfig) {
		for(CipherConfigures configure : configures) {
			if(configure.config.equals(algorithmConfig)) return configure.provider;
		}
		return null;
	}
}
