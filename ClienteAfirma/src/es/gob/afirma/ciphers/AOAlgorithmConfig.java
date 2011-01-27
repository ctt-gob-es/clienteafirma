/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.ciphers;

import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOConstants.AOCipherPadding;

/**
 * Confuiguraciones de algoritmo / modo de bloque / relleno para cifrado.
 */
public final class AOAlgorithmConfig {
	
	private final AOCipherAlgorithm algo;
	private final AOCipherBlockMode mode;
	private final AOCipherPadding padding;
	
	/**
	 * Construyye una configuraci&oacute;n de cifrado.
	 * @param a Algoritmo de cifrado
	 * @param m Modo de bloque para el cifrado
	 * @param p Relleno (<i>padding</i>) del cifrado
	 */
	public AOAlgorithmConfig(AOCipherAlgorithm a, AOCipherBlockMode m, AOCipherPadding p) {
		// En caso de nulos tomamos defectos y algunas combinaciones predefinidas
		if (a == null) a = AOCipherAlgorithm.getDefault();
		if (m == null) {
			if (
				a.equals(AOCipherAlgorithm.PBEWITHMD5ANDDES) ||
				a.equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE) ||
				a.equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40)
			) m = AOCipherBlockMode.CBC;
			else m = AOCipherBlockMode.ECB;
		}
		if (p==null) {
			if (a.equals(AOCipherAlgorithm.ARCFOUR)) p = AOCipherPadding.NOPADDING;
			else p = AOCipherPadding.PKCS5PADDING;
		}
		algo = a;
		mode = m;
		padding = p;
	}
	
	@Override
	public String toString() {
		return new StringBuilder(algo.getName())
			.append("/")
			.append(mode.getName())
			.append("/")
			.append(padding.getName())
				.toString();
	}
	
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AOAlgorithmConfig)) return false;
		return ((AOAlgorithmConfig)obj).algo.equals(algo) && ((AOAlgorithmConfig)obj).mode.equals(mode) && ((AOAlgorithmConfig)obj).padding.equals(padding);
	}
	
	@Override
	public int hashCode() {
		return this.algo.hashCode()+this.mode.hashCode()+this.padding.hashCode();
	}
	
	/**
	 * Obtiene el algoritmo de cifrado.
	 * @return Algoritmo de cifrado
	 */
	public AOCipherAlgorithm getAlgorithm() {
		return algo;
	}
	
	/**
	 * Obtiene el modo de bloque (<i>block mode</i>) de cifrado.
	 * @return Modo de bloque de cifrado
	 */
	public AOCipherBlockMode getBlockMode() {
		return mode;
	}
	
	/**
	 * Obtiene el relleno (<i>padding</i>) del cifrado.
	 * @return Relleno del cifrado
	 */
	public AOCipherPadding getPadding() {
		return padding;
	}

}
