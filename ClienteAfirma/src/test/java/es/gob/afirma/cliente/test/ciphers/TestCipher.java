package es.gob.afirma.cliente.test.ciphers;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static es.gob.afirma.cliente.test.TestUtils.CYPHER_ALGOS;
import static es.gob.afirma.cliente.test.TestUtils.CYPHER_ALGOS_PBE;

import java.security.Key;

import org.junit.Test;

/** Pruebas para las funcionalidades de cifrado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCipher {

    private static byte[] DATA = "hfgasddf sdhffgjk sdgfg fgjehwgfjguyrwetrtweu yrgsfjkhgaskjfgallrw rwer wer werwe werwefweefwefwe".getBytes();
    private static char[] PASS = "jdgjasgdjkgfjkasgdkjf".toCharArray();

    /** Pruebas de los cifrados basados en contrase&ntilde;as. */
    @Test
    public void testCypherPBE() {
        for (final String algo : CYPHER_ALGOS_PBE) {
            CipherConfig cypherConfig = null;
            cypherConfig = new CipherConfig(algo);
            assertNotNull("La configuracion de cifrado es nula", cypherConfig);
            Key key = null;
            try {
                key = cypherConfig.getCipher().decodePassphrase(PASS, cypherConfig.getConfig(), null);
            }
            catch (final Exception e) {
                e.printStackTrace();
            }
            assertNotNull("No se ha podido generar una clave");
            byte[] cypheredData = null;
            try {
                cypheredData = cypherConfig.getCipher().cipher(DATA, cypherConfig.getConfig(), key);
            }
            catch (final Exception e) {
                e.printStackTrace();
            }
            assertNotNull("Los datos son nulos", cypheredData);
            if (cypheredData != null) {
                assertTrue(cypheredData.length > 0);
            }
            byte[] plainData = null;
            try {
                plainData = cypherConfig.getCipher().decipher(cypheredData, cypherConfig.getConfig(), key);
            }
            catch (final Exception e) {
                e.printStackTrace();
            }
            assertNotNull("Los datos son nulos", plainData);
            if (plainData != null) {
                assertTrue(plainData.length > 0);
            }
            assertArrayEquals(DATA, plainData);
        }
    }

    /** Pruebas de los cifrados basados en claves. */
    @Test
    public void testCypher() {
        for (final String algo : CYPHER_ALGOS) {
            CipherConfig cypherConfig = null;
            cypherConfig = new CipherConfig(algo);
            assertNotNull("La configuracion de cifrado es nula", cypherConfig);
            Key key = null;
            try {
                key = cypherConfig.getCipher().generateKey(cypherConfig.getConfig());
            }
            catch (final Exception e) {
                e.printStackTrace();
            }
            assertNotNull("No se ha podido autogenerar una clave");
            byte[] cypheredData = null;
            try {
                cypheredData = cypherConfig.getCipher().cipher(DATA, cypherConfig.getConfig(), key);
            }
            catch (final Exception e) {
                e.printStackTrace();
            }
            assertNotNull("Los datos son nulos", cypheredData);
            if (cypheredData != null) {
                assertTrue(cypheredData.length > 0);
            }
            byte[] plainData = null;
            try {
                plainData = cypherConfig.getCipher().decipher(cypheredData, cypherConfig.getConfig(), key);
            }
            catch (final Exception e) {
                e.printStackTrace();
            }
            assertNotNull("Los datos son nulos", plainData);
            if (plainData != null) {
                assertTrue(plainData.length > 0);
            }
            assertArrayEquals(DATA, plainData);

        }
    }
}
