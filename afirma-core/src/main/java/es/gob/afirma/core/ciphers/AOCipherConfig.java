/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
  * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.ciphers;

import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherPadding;

/** Confuiguraciones de algoritmo / modo de bloque / relleno para cifrado.
 * <p>Los algoritmos marcados como obsoletos (DES, PBE con MD5 o SHA-1, RC2-40 y
 * HmacMD5/HmacSHA1) solo se aceptan al <b>crear</b> cifrados nuevos si se activa
 * expresamente <code>-DallowLegacyCiphers=true</code>. Para <b>descifrar</b> contenido
 * heredado se usa {@link #forDeciphering(AOCipherAlgorithm, AOCipherBlockMode, AOCipherPadding)},
 * que siempre los admite y deja un aviso en el log.</p> */
public final class AOCipherConfig {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Algoritmos obsoletos de los que ya se ha avisado al descifrar. */
    private static final Set<String> WARNED_LEGACY_FOR_DECIPHER = Collections.synchronizedSet(new HashSet<String>());

    /** Algoritmo de cifrado. */
    private AOCipherAlgorithm algo;

    /** Modo de bloque para el cifrado. */
    private AOCipherBlockMode mode;

    /** Padding que aplicar al cifrado. */
    private AOCipherPadding padding;

    /** Indica si la configuracion se creo para descifrar contenido heredado. */
    private final boolean forDeciphering;

    /** Construyye una configuraci&oacute;n de cifrado para crear cifrados nuevos.
     * @param algo
     *        Algoritmo de cifrado
     * @param mode
     *        Modo de bloque para el cifrado
     * @param padd
     *        Relleno (<i>padding</i>) del cifrado */
    public AOCipherConfig(final AOCipherAlgorithm algo, final AOCipherBlockMode mode, final AOCipherPadding padd) {
        this(algo, mode, padd, false);
    }

    /** Construye una configuraci&oacute;n de cifrado.
     * @param algo Algoritmo de cifrado.
     * @param mode Modo de bloque (si es nulo se usa el de creacion, CBC).
     * @param padd Relleno.
     * @param deciphering Si es {@code true}, la configuracion es para descifrar contenido
     *                    heredado y se admiten los algoritmos obsoletos. */
    private AOCipherConfig(final AOCipherAlgorithm algo, final AOCipherBlockMode mode,
                           final AOCipherPadding padd, final boolean deciphering) {

        // En caso de nulos tomamos defectos y algunas combinaciones
        // predefinidas
        final AOCipherAlgorithm ciphAlgo = algo != null ? algo : AOCipherAlgorithm.getDefault();

        if (ciphAlgo.isLegacy() && !deciphering && !CipherConstants.isLegacyAllowed()) {
            throw new IllegalArgumentException(
                "El algoritmo de cifrado '" + ciphAlgo.getName() + "' esta obsoleto y no se permite crear " + //$NON-NLS-1$ //$NON-NLS-2$
                "cifrados nuevos con el. Use -D" + CipherConstants.ALLOW_LEGACY_CIPHERS_PROPERTY + "=true solo si " + //$NON-NLS-1$ //$NON-NLS-2$
                "acepta el riesgo." //$NON-NLS-1$
            );
        }
        if (ciphAlgo.isLegacy() && deciphering) {
            warnLegacyForDecipher(ciphAlgo);
        }

        AOCipherBlockMode ciphMode = mode;
        if (ciphMode == null) {
            if (ciphAlgo.supportsPassword()) {
                // Los algoritmos basados en contrasena llevan su modo en la configuracion JCE
                ciphMode = AOCipherBlockMode.CBC;
            }
            else {
                // Nunca ECB por defecto en la creacion de cifrados nuevos
                ciphMode = AOCipherBlockMode.getDefault();
            }
        }

        AOCipherPadding ciphPadding = padd;
        if (ciphPadding == null) {
            ciphPadding = AOCipherPadding.PKCS5PADDING;
        }
        this.algo = ciphAlgo;
        this.mode = ciphMode;
        this.padding = ciphPadding;
        this.forDeciphering = deciphering;
    }

    /** Crea la configuraci&oacute;n necesaria para <b>descifrar</b> contenido heredado.
     * Admite los algoritmos obsoletos y deja un aviso en el log (sin datos del contenido).
     * @param algo Algoritmo de cifrado leido del mensaje.
     * @param mode Modo de bloque, si se conoce.
     * @param padd Relleno, si se conoce.
     * @return Configuracion de descifrado. */
    public static AOCipherConfig forDeciphering(final AOCipherAlgorithm algo, final AOCipherBlockMode mode,
                                                final AOCipherPadding padd) {
        return new AOCipherConfig(algo, mode, padd, true);
    }

    /** Genera un objeto AOCipherConfig a partir de una cadena que siga uno de
     * los siguientes patrones:
     * <ul>
     * <li>Algoritmo/ModoBloque/Padding</li>
     * <li>Algoritmo</li>
     * </ul>
     * Si s&oacute;lo se especifica el algoritmo de cifrado, se tomara
     * el modo y el padding configurados por defecto para ese algoritmo.
     * @return AOCipherConfig con una configuraci&oacute; espec&iacute;fica de
     *         cifrado
     * @param cipherConfig
     *        Configuraci&oacute;n de cifrado.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo no est&aacute; soportado o es obsoleto y no se ha
     *         activado <code>allowLegacyCiphers</code>. */
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
        if (cipAlgo.isLegacy() && !CipherConstants.isLegacyAllowed()) {
            throw new NoSuchAlgorithmException(
                "Algoritmo de cifrado obsoleto no permitido: " + cipAlgo.getName() //$NON-NLS-1$
            );
        }

        // Establecemos el resto de la configuracion
        if (algoConfig.length == 3) {
            cipBlockMode = AOCipherBlockMode.getValueOf(algoConfig[1]);
            cipPadding = AOCipherPadding.getValueOf(algoConfig[2]);
        }
        return new AOCipherConfig(cipAlgo, cipBlockMode, cipPadding);
    }

    /** Indica si la configuracion usa un algoritmo obsoleto.
     * @return {@code true} si el algoritmo es obsoleto. */
    public boolean isLegacy() {
        return this.algo.isLegacy();
    }

    /** Indica si la configuracion se creo para descifrar contenido heredado.
     * @return {@code true} si es una configuracion de descifrado. */
    public boolean isForDeciphering() {
        return this.forDeciphering;
    }

    /** Olvida los avisos ya emitidos. Solo para pruebas. */
    static void resetLegacyWarningsForTests() {
        WARNED_LEGACY_FOR_DECIPHER.clear();
    }

    private static void warnLegacyForDecipher(final AOCipherAlgorithm algorithm) {
        if (WARNED_LEGACY_FOR_DECIPHER.add(algorithm.getName())) {
            LOGGER.warning(
                "Se descifra contenido heredado con el algoritmo obsoleto '" + algorithm.getName() + //$NON-NLS-1$
                "'. Se admite por compatibilidad; no se usa para cifrar." //$NON-NLS-1$
            );
        }
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
        if (algo != null && algo.isLegacy() && !this.forDeciphering && !CipherConstants.isLegacyAllowed()) {
            throw new IllegalArgumentException(
                "El algoritmo de cifrado '" + algo.getName() + "' esta obsoleto y no se permite crear " + //$NON-NLS-1$ //$NON-NLS-2$
                "cifrados nuevos con el. Use -D" + CipherConstants.ALLOW_LEGACY_CIPHERS_PROPERTY + "=true solo si " + //$NON-NLS-1$ //$NON-NLS-2$
                "acepta el riesgo." //$NON-NLS-1$
            );
        }
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
