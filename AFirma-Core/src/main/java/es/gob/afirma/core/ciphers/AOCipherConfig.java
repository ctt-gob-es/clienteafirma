/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.ciphers;

import java.security.NoSuchAlgorithmException;

import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherPadding;

/** Confuiguraciones de algoritmo / modo de bloque / relleno para cifrado. */
public final class AOCipherConfig {

    /** Algoritmo de cifrado. */
    private AOCipherAlgorithm algo;

    /** Modo de bloque para el cifrado. */
    private AOCipherBlockMode mode;

    /** Padding que aplicar al cifrado. */
    private AOCipherPadding padding;

    /** Construyye una configuraci&oacute;n de cifrado.
     * @param ciphAlgo
     *        Algoritmo de cifrado
     * @param ciphMode
     *        Modo de bloque para el cifrado
     * @param ciphPadding
     *        Relleno (<i>padding</i>) del cifrado */
    public AOCipherConfig(AOCipherAlgorithm ciphAlgo, AOCipherBlockMode ciphMode, AOCipherPadding ciphPadding) {
        // En caso de nulos tomamos defectos y algunas combinaciones
        // predefinidas
        if (ciphAlgo == null) {
            ciphAlgo = AOCipherAlgorithm.getDefault();
        }
        if (ciphMode == null) {
            if (ciphAlgo.equals(AOCipherAlgorithm.PBEWITHMD5ANDDES) || ciphAlgo.equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE)
                || ciphAlgo.equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40)) {
                ciphMode = AOCipherBlockMode.CBC;
            }
            else {
                ciphMode = AOCipherBlockMode.ECB;
            }
        }
        if (ciphPadding == null) {
            if (ciphAlgo.equals(AOCipherAlgorithm.ARCFOUR)) {
                ciphPadding = AOCipherPadding.NOPADDING;
            }
            else {
                ciphPadding = AOCipherPadding.PKCS5PADDING;
            }
        }
        this.algo = ciphAlgo;
        this.mode = ciphMode;
        this.padding = ciphPadding;
    }

    /** Genera un objeto AOCipherConfig a partir de una cadena que siga uno de
     * los siguientes patrones: <list>
     * <ul>
     * Algoritmo/ModoBloque/Padding
     * </ul>
     * <ul>
     * Algoritmo
     * </ul>
     * </list> Si s&oacute;lo se especifica el algoritmo de cifrado, se tomara
     * el modo y el padding configurados por defecto para ese algoritmo.
     * @return AOCipherConfig con una configuraci&oacute; espec&iacute;fica de
     *         cifrado
     * @param cipherConfig
     *        Configuraci&oacute;n de cifrado.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo no est&aacute; soportado. */
    public static AOCipherConfig parse(final String cipherConfig) throws NoSuchAlgorithmException {
        // Desmenbramos el algoritmo por si se ha indicado el modo de bloque y
        // el padding

        AOCipherAlgorithm cipAlgo = null;
        AOCipherBlockMode cipBlockMode = null;
        AOCipherPadding cipPadding = null;
        final String[] algoConfig = cipherConfig.split("/"); //$NON-NLS-1$
        cipAlgo = AOCipherAlgorithm.getValueOf(algoConfig[0]);
        if (cipAlgo == null) {
            throw new NoSuchAlgorithmException();
        }

        // Establecemos el resto de la configuracion
        if (algoConfig.length == 3) {
            cipBlockMode = AOCipherBlockMode.getValueOf(algoConfig[1]);
            cipPadding = AOCipherPadding.getValueOf(algoConfig[2]);
        }
        return new AOCipherConfig(cipAlgo, cipBlockMode, cipPadding);
    }

    @Override
    public String toString() {
        final StringBuilder config = new StringBuilder(this.algo.getName());
        if (this.mode != null && this.padding != null) {
            config.append("/").append(this.mode.getName()).append("/").append(this.padding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return config.toString();
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof AOCipherConfig)) {
            return false;
        }
        return ((AOCipherConfig) obj).algo.equals(this.algo) && ((AOCipherConfig) obj).mode.equals(this.mode) && ((AOCipherConfig) obj).padding.equals(this.padding);
    }

    @Override
    public int hashCode() {
        return this.algo.hashCode() + this.mode.hashCode() + this.padding.hashCode();
    }

    /** Obtiene el algoritmo de cifrado.
     * @return Algoritmo de cifrado */
    public AOCipherAlgorithm getAlgorithm() {
        return this.algo;
    }

    /** Obtiene el modo de bloque (<i>block mode</i>) de cifrado.
     * @return Modo de bloque de cifrado */
    public AOCipherBlockMode getBlockMode() {
        return this.mode;
    }

    /** Obtiene el relleno (<i>padding</i>) del cifrado.
     * @return Relleno del cifrado */
    public AOCipherPadding getPadding() {
        return this.padding;
    }

    /** Establece el algoritmo de cifrado.
     * @param algo
     *        Algoritmo. */
    public void setAlgorithm(final AOCipherAlgorithm algo) {
        this.algo = algo;
    }

    /** Establece el modo de bloque para el cifrado.
     * @param mode
     *        Modo de bloque. */
    public void setBlockMode(final AOCipherBlockMode mode) {
        this.mode = mode;
    }

    /** Establece el padding para el cifrado.
     * @param padding
     *        Padding. */
    public void setPadding(final AOCipherPadding padding) {
        this.padding = padding;
    }
}
