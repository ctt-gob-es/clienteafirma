/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.ciphers.jce;

import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyException;
import java.security.spec.AlgorithmParameterSpec;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipher;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherPadding;




/** Cifrador seg&uacute;n las capacidades del proveedor JCE (<i>Java Cryptography
 * Extension</i>). */
public final class AOSunJCECipher implements AOCipher {

    /** Proveedor Java que da soporte a los algoritmos aqui descritos. */
    private static final String PROVIDER = "SunJCE"; //$NON-NLS-1$

    private static final byte[] SALT = {
        (byte) 0xA2, (byte) 0x35, (byte) 0xDC, (byte) 0xA4, (byte) 0x11, (byte) 0x7C, (byte) 0x99, (byte) 0x4B
    };

    private static final int ITERATION_COUNT = 9;

    /** Vector de inicializacion de 8 bytes. Un vector de inicializaci&oacute;n
     * de 8 bytes es necesario para el uso de los algoritmos DES y DESede. */
    private static final byte[] IV_8 = {
        (byte) 0xC6, (byte) 0xBA, (byte) 0xDE, (byte) 0xA4, (byte) 0x76, (byte) 0x43, (byte) 0x32, (byte) 0x6B
    };

    /** Vector de inicializacion de 16 bytes. Un vector de inicializaci&oacute;n
     * de 16 bytes es necesario para el uso de los algoritmos DES y DESede. */
    private static final byte[] IV_16 = {
        (byte) 0xB2, (byte) 0xBA, (byte) 0xDE, (byte) 0xA4, (byte) 0x41, (byte) 0x7F, (byte) 0x97, (byte) 0x4B,
        (byte) 0xAC, (byte) 0x63, (byte) 0xAC, (byte) 0xAA, (byte) 0x76, (byte) 0x73, (byte) 0x12, (byte) 0x6B
    };

    /** Configuraciones de cifrado soportadas. */
    private static final AOCipherConfig[] SUPPORTED_CONFIGS = new AOCipherConfig[] {
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
        new AOCipherConfig(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
        new AOCipherConfig(AOCipherAlgorithm.PBEWITHMD5ANDDES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING)
    };

    /** {@inheritDoc} */
    @Override
	public AOCipherConfig[] getSupportedConfigs() {
        return SUPPORTED_CONFIGS.clone();
    }

    /** {@inheritDoc} */
    @Override
	public byte[] cipher(final byte[] data, final AOCipherConfig algorithmConfig, final Key cipherKey) throws AOException, KeyException {

        if (data == null || algorithmConfig == null || cipherKey == null || data.length == 0) {
            throw new AOException("Los parametros de la funcion de cifrado no pueden ser nulos o vacios"); //$NON-NLS-1$
        }

        // Tomamos el cipher para el algoritmo indicado
        final Cipher cipher;
        try {
            cipher = Cipher.getInstance(algorithmConfig.toString(), PROVIDER);
        }
        catch (final Exception e) {
            throw new AOException("Error al obtener el cifrador: " + e, e); //$NON-NLS-1$
        }

        // Inicializamos el cipher
        try {
            cipher.init(Cipher.ENCRYPT_MODE, cipherKey, AOSunJCECipher.getParams(algorithmConfig));
        }
        catch (final InvalidKeyException e) {
            throw new KeyException("La clave de cifrado introducida no es valida para el algoritmo '" + //$NON-NLS-1$
                    algorithmConfig.getAlgorithm().getName() + "'", e); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar el cifrador", e); //$NON-NLS-1$
        }

        // Realizamos el cifrado
        try {
            return cipher.doFinal(data);
        }
        catch (final Exception e) {
            throw new AOException("Error cifrando los datos", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Override
	public byte[] decipher(final byte[] data, final AOCipherConfig algorithmConfig, final Key decipherKey) throws AOException, InvalidKeyException {

        if (data == null || algorithmConfig == null || decipherKey == null) {
            throw new AOException("Los parametros de la funcion de descifrado no pueden ser nulos"); //$NON-NLS-1$
        }

        // Tomamos el cipher para el algoritmo indicado
        final Cipher cipher;
        try {
            cipher = Cipher.getInstance(algorithmConfig.toString(), PROVIDER);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo el descifrador", e); //$NON-NLS-1$
        }

        // Inicializamos el cipher
        try {
            cipher.init(Cipher.DECRYPT_MODE, decipherKey, AOSunJCECipher.getParams(algorithmConfig));
        }
        catch (final InvalidKeyException e) {
            throw new InvalidKeyException("La clave de descifrado introducida no es valida para el algoritmo '" //$NON-NLS-1$
                    + algorithmConfig.getAlgorithm().getName() + "'", //$NON-NLS-1$
                    e);
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar el descifrador", e); //$NON-NLS-1$
        }

        // Realizamos el descifrado
        try {
            return cipher.doFinal(data);
        }
        catch(final BadPaddingException e) {
        	throw new InvalidKeyException("La clave de descifrado introducida no es correcta", e); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new AOException("Error descifrando los datos", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
	@Override
	public Key decodeKey(final byte[] keyEncoded, final AOCipherConfig algorithmConfig, final Object[] params) throws KeyException {
        if (keyEncoded == null || keyEncoded.length < 1) {
            throw new IllegalArgumentException("La clave a descodificar no puede ser nula ni vacia"); //$NON-NLS-1$
        }
        if (algorithmConfig == null) {
            throw new IllegalArgumentException("La configuracion de cifrado no puede ser nula"); //$NON-NLS-1$
        }

        try {
            return new SecretKeySpec(keyEncoded, algorithmConfig.getAlgorithm().getName());
        }
        catch (final Exception e) {
            throw new KeyException("Error creando la clave secreta", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Override
	public Key decodePassphrase(final char[] passphrase, final AOCipherConfig algorithmConfig, final Object[] params) throws AOException {

        if (passphrase == null || passphrase.length < 1) {
            throw new IllegalArgumentException("La contrasena para la generacion de la clave no puede ser nula ni vacia"); //$NON-NLS-1$
        }
        if (algorithmConfig == null) {
            throw new IllegalArgumentException("La configuracion de cifrado no puede ser nula"); //$NON-NLS-1$
        }

        try {
            return SecretKeyFactory.getInstance(
                    algorithmConfig.getAlgorithm().getName(), PROVIDER).generateSecret(
                            new PBEKeySpec(passphrase, SALT, ITERATION_COUNT));
        }
        catch (final Exception e) {
            throw new AOException("Error generando la clave secreta", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Override
	public Key generateKey(final AOCipherConfig algorithmConfig) throws AOException {
        try {
            return KeyGenerator.getInstance(algorithmConfig.getAlgorithm().getName(), PROVIDER).generateKey();
        }
        catch (final Exception e) {
            throw new AOException("No se pudo generar una clave compatible para la configuracion '" + algorithmConfig + "': " + e , e); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Genera los par&aacute;metros necesarios para poder operar con una
     * configuracion concreta de cifrado. Si no es necesario ning&uacute;n
     * par&aacute;metro especial, devolvemos <code>null</code>.
     * @param algorithmConfig
     *        Configuracion de cifrado que debemos parametrizar.
     * @return Par&aacute;metros para operar. */
    private static AlgorithmParameterSpec getParams(final AOCipherConfig algorithmConfig) {
        if (algorithmConfig.getAlgorithm().supportsPassword()) {
            return new PBEParameterSpec(SALT, ITERATION_COUNT);
        }
        if (!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
            return new IvParameterSpec(algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8);
        }
        return null;
    }

}
