/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 */

package es.gob.afirma.core.ciphers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Test;

import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherPadding;

/** Pruebas de T9: modo por defecto distinto de ECB y algoritmos obsoletos detras de
 * una opcion explicita (<code>-DallowLegacyCiphers=true</code>). */
public class AOCipherConfigTest {

    private static final AOCipherAlgorithm[] LEGACY = {
        AOCipherAlgorithm.DES,
        AOCipherAlgorithm.PBEWITHMD5ANDDES,
        AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE,
        AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40,
        AOCipherAlgorithm.HMACMD5,
        AOCipherAlgorithm.HMACSHA1
    };

    private static final AOCipherAlgorithm[] MODERN = {
        AOCipherAlgorithm.AES,
        AOCipherAlgorithm.BLOWFISH,
        AOCipherAlgorithm.TRIPLEDES,
        AOCipherAlgorithm.HMACSHA256,
        AOCipherAlgorithm.HMACSHA384,
        AOCipherAlgorithm.HMACSHA512
    };

    @After
    public void tearDown() {
        System.clearProperty(CipherConstants.ALLOW_LEGACY_CIPHERS_PROPERTY);
        AOCipherConfig.resetLegacyWarningsForTests();
    }

    @Test
    public void defaultBlockModeIsNotEcb() {
        assertEquals(AOCipherBlockMode.CBC, AOCipherBlockMode.getDefault());

        final AOCipherConfig config = new AOCipherConfig(null, null, null);
        assertEquals(AOCipherAlgorithm.AES, config.getAlgorithm());
        assertEquals(AOCipherBlockMode.CBC, config.getBlockMode());
        assertEquals(AOCipherPadding.PKCS5PADDING, config.getPadding());
        assertEquals("AES/CBC/PKCS5PADDING", config.toString());
    }

    @Test
    public void parsedDefaultModeIsNotEcb() throws NoSuchAlgorithmException {
        final AOCipherConfig config = AOCipherConfig.parse("AES");
        assertEquals(AOCipherBlockMode.CBC, config.getBlockMode());
        assertEquals("AES/CBC/PKCS5PADDING", config.toString());
    }

    @Test
    public void explicitEcbIsStillHonoured() throws NoSuchAlgorithmException {
        // El modo explicito se respeta: solo cambia el valor por defecto
        final AOCipherConfig config = AOCipherConfig.parse("AES/ECB/PKCS5PADDING");
        assertEquals(AOCipherBlockMode.ECB, config.getBlockMode());
    }

    @Test
    public void legacyAlgorithmsAreRejectedByDefault() {
        assertFalse(CipherConstants.isLegacyAllowed());
        for (final AOCipherAlgorithm algorithm : LEGACY) {
            assertTrue(algorithm.getName() + " debe marcarse como obsoleto", algorithm.isLegacy());
            try {
                new AOCipherConfig(algorithm, null, null);
                fail("Se acepto el algoritmo obsoleto " + algorithm.getName() + " sin opcion explicita");
            }
            catch (final IllegalArgumentException e) {
                assertTrue(e.getMessage().contains(algorithm.getName()));
                assertTrue(e.getMessage().contains(CipherConstants.ALLOW_LEGACY_CIPHERS_PROPERTY));
            }
        }
    }

    @Test
    public void legacyAlgorithmsAreRejectedWhenParsedByDefault() {
        for (final AOCipherAlgorithm algorithm : LEGACY) {
            try {
                AOCipherConfig.parse(algorithm.getName());
                fail("Se acepto por parse el algoritmo obsoleto " + algorithm.getName());
            }
            catch (final NoSuchAlgorithmException e) {
                assertNotNull(e.getMessage());
            }
        }
    }

    @Test
    public void legacyAlgorithmsAreAllowedWithExplicitOption() throws NoSuchAlgorithmException {
        System.setProperty(CipherConstants.ALLOW_LEGACY_CIPHERS_PROPERTY, "true");
        assertTrue(CipherConstants.isLegacyAllowed());
        for (final AOCipherAlgorithm algorithm : LEGACY) {
            final AOCipherConfig config = new AOCipherConfig(algorithm, null, null);
            assertEquals(algorithm, config.getAlgorithm());
            assertTrue(config.isLegacy());
            assertEquals(algorithm.getName(), AOCipherConfig.parse(algorithm.getName()).getAlgorithm().getName());
        }
    }

    @Test
    public void setAlgorithmAlsoHonoursTheGate() {
        final AOCipherConfig config = new AOCipherConfig(AOCipherAlgorithm.AES, null, null);
        try {
            config.setAlgorithm(AOCipherAlgorithm.DES);
            fail("setAlgorithm permitio un algoritmo obsoleto sin opcion explicita");
        }
        catch (final IllegalArgumentException e) {
            assertEquals(AOCipherAlgorithm.AES, config.getAlgorithm());
        }
    }

    @Test
    public void decipheringLegacyContentIsAlwaysAllowed() {
        assertFalse(CipherConstants.isLegacyAllowed());
        for (final AOCipherAlgorithm algorithm : LEGACY) {
            final AOCipherConfig config = AOCipherConfig.forDeciphering(algorithm, null, null);
            assertEquals(algorithm, config.getAlgorithm());
            assertTrue(config.isForDeciphering());
            assertTrue(config.isLegacy());
        }
    }

    @Test
    public void decipheringLegacyContentWarnsWithoutData() {
        AOCipherConfig.resetLegacyWarningsForTests();
        final List<String> messages = new ArrayList<>();
        final Logger logger = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
        final Handler handler = new Handler() {
            @Override
            public void publish(final LogRecord record) {
                if (record != null && record.getMessage() != null) {
                    messages.add(record.getMessage());
                }
            }
            @Override
            public void flush() {
                // Sin buffer
            }
            @Override
            public void close() {
                // Sin recursos
            }
        };
        logger.addHandler(handler);
        try {
            AOCipherConfig.forDeciphering(AOCipherAlgorithm.PBEWITHMD5ANDDES, null, null);
            AOCipherConfig.forDeciphering(AOCipherAlgorithm.PBEWITHMD5ANDDES, null, null);
        }
        finally {
            logger.removeHandler(handler);
        }

        assertEquals("El aviso de descifrado heredado se emite una sola vez", 1, messages.size());
        final String warning = messages.get(0);
        assertTrue(warning.contains(AOCipherAlgorithm.PBEWITHMD5ANDDES.getName()));
        assertFalse("El aviso no debe contener datos del contenido", warning.contains("password123")); //$NON-NLS-1$
    }

    @Test
    public void modernAlgorithmsAreUnaffected() {
        for (final AOCipherAlgorithm algorithm : MODERN) {
            assertFalse(algorithm.getName() + " no debe marcarse como obsoleto", algorithm.isLegacy());
            final AOCipherConfig config = new AOCipherConfig(algorithm, null, null);
            assertEquals(algorithm, config.getAlgorithm());
            assertEquals(AOCipherBlockMode.getDefault(), config.getBlockMode());
            assertNotEqualsEcb(config.getBlockMode());
        }
    }

    private static void assertNotEqualsEcb(final AOCipherBlockMode mode) {
        assertFalse("El modo por defecto no puede ser ECB", AOCipherBlockMode.ECB.equals(mode));
    }
}
