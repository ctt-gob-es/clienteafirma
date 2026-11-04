package es.gob.afirma.core.ciphers;

import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;

/**
 * Reproduccion minima de los hallazgos V6.2 de T9: modo por defecto y aceptacion de
 * algoritmos obsoletos al crear un cifrado. Se compila igual contra la base y contra la
 * rama de T9; solo usa APIs presentes en ambas.
 */
public final class LegacyCiphersRepro {

    private LegacyCiphersRepro() { }

    private static final AOCipherAlgorithm[] LEGACY = {
        AOCipherAlgorithm.DES,
        AOCipherAlgorithm.PBEWITHMD5ANDDES,
        AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE,
        AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40,
        AOCipherAlgorithm.HMACMD5,
        AOCipherAlgorithm.HMACSHA1
    };

    public static void main(final String[] args) {
        final AOCipherConfig defaults = new AOCipherConfig(null, null, null);
        final boolean ecb = AOCipherBlockMode.ECB.equals(defaults.getBlockMode());
        System.out.println("A) Configuracion por defecto: " + defaults + " -> " + (ecb ? "ECB (brecha)" : "no ECB"));

        for (final AOCipherAlgorithm algorithm : LEGACY) {
            try {
                final AOCipherConfig config = new AOCipherConfig(algorithm, null, null);
                System.out.println("B) " + algorithm.getName() + " sin opcion explicita -> ACEPTADO: " + config);
            }
            catch (final RuntimeException e) {
                System.out.println("B) " + algorithm.getName() + " sin opcion explicita -> RECHAZADO");
            }
        }
    }
}
