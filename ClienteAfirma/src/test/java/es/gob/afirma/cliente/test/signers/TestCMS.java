package es.gob.afirma.cliente.test.signers;

import static org.junit.Assert.assertNotNull;
import static es.gob.afirma.cliente.test.TestUtils.ALGOS;
import static es.gob.afirma.cliente.test.TestUtils.BIN_CONTENT;
import static es.gob.afirma.cliente.test.TestUtils.CMS_MODES;
import static es.gob.afirma.cliente.test.TestUtils.CS_TARGETS;
import static es.gob.afirma.cliente.test.TestUtils.KEYSTORES;
import static es.gob.afirma.cliente.test.TestUtils.getValidKeyEntryFromKeyStore;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;
import org.junit.Ignore;

import es.gob.afirma.cliente.test.TestUtils;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOCMSSigner;
import es.gob.afirma.signers.AOSigner;

/** Pruebas de firmas CMS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestCMS {

    /** Prueba de firma CMS. */
    @Test
    public void testSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOCMSSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;
        for (final AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (final String algo : ALGOS) {
                for (final Properties extraParams : CMS_MODES) {
                    System.out.println();
                    System.out.println();
                    System.out.println();
                    System.out.println("Prueba con '" + algo + "', '" + kstore + "' y formato '" + extraParams.getProperty("format"));
                    if (AOConstants.AOKeyStore.WINDOWS.equals(kstore) && AOConstants.SIGN_ALGORITHM_SHA512WITHRSA.equals(algo)) {
                        System.out.println("Omitimos las pruebas en CAPI con SHA2 por errores conocidos de Java");
                        continue;
                    }
                    if (AOKeyStore.APPLE.equals(kstore) && (!Platform.OS.MACOSX.equals(Platform.getOS()))) {
                        System.out.println("Omitimos las pruebas de Llavero Apple si no estamos en Mac OS X");
                        continue;
                    }
                    if (AOKeyStore.WINDOWS.equals(kstore) && (!Platform.OS.WINDOWS.equals(Platform.getOS()))) {
                        System.out.println("Omitimos las pruebas de CAPI si no estamos en Windows");
                        continue;
                    }
                    try {
                        pke = getValidKeyEntryFromKeyStore(kstore);
                        assertNotNull("No hemos encontrado una clave de prueba en " + kstore, pke);
                        result = null;
                        result = signer.sign(BIN_CONTENT, algo, pke, extraParams);
                    }
                    catch (final Exception e) {
                        e.printStackTrace();
                        result = null;
                    }
                    assertNotNull("El resultado de la firma es nulo", result);
                    if (!Platform.JREVER.J5.equals(Platform.getJavaVersion())) {
                        try {
                            System.out.println("Resultado de la validacion: " + TestUtils.verifyBinSignature(result));
                        }
                        catch (final Exception e) {
                            org.junit.Assert.fail("Error validando la firma generada: " + e);
                        }
                    }
                }
            }
        }
    }

    /** Prueba de contrafirma CMS. */
    @Test
    @Ignore
    public void testCounterSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOCMSSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] p7s = null;
        // Primero cargamos el XML a contrafirmar
        try {
            p7s = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Contrafirma3.p7s"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            p7s = null;
        }
        if (p7s != null && p7s.length == 0) {
            p7s = null;
        }
        assertNotNull("No se ha podido cargar el fichero de firmas CMS a contrafirmar", p7s);

        for (final AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (final String algo : ALGOS) {
                for (final Properties extraParams : CMS_MODES) {
                    for (final CounterSignTarget targetType : CS_TARGETS) {
                        System.out.println();
                        System.out.println();
                        System.out.println();
                        System.out.println("Prueba CONTRAFIRMA con '" + algo
                                           + "', '"
                                           + kstore
                                           + "', formato '"
                                           + extraParams.getProperty("format")
                                           + "', objetivo '"
                                           + targetType
                                           + "' y modo '"
                                           + extraParams.getProperty("mode")
                                           + "'");
                        if (AOConstants.AOKeyStore.WINDOWS.equals(kstore) && AOConstants.SIGN_ALGORITHM_SHA512WITHRSA.equals(algo)) {
                            System.out.println("Omitimos las pruebas en CAPI con SHA2 por errores conocidos de Java");
                            continue;
                        }
                        if (AOKeyStore.APPLE.equals(kstore) && (!Platform.OS.MACOSX.equals(Platform.getOS()))) {
                            System.out.println("Omitimos las pruebas de Llavero Apple si no estamos en Mac OS X");
                            continue;
                        }
                        if (AOKeyStore.WINDOWS.equals(kstore) && (!Platform.OS.WINDOWS.equals(Platform.getOS()))) {
                            System.out.println("Omitimos las pruebas de CAPI si no estamos en Windows");
                            continue;
                        }
                        try {
                            pke = getValidKeyEntryFromKeyStore(kstore);
                            assertNotNull("No hemos encontrado una clave de prueba en " + kstore, pke);
                            result = null;
                            result = signer.countersign(p7s, algo, targetType, new Object[0], pke, extraParams);
                        }
                        catch (final Exception e) {
                            e.printStackTrace();
                            result = null;
                        }
                        assertNotNull("El resultado de la contrafirma es nulo", result);
                        if (!Platform.JREVER.J5.equals(Platform.getJavaVersion())) {
                            try {
                                System.out.println("Resultado de la validacion: " + TestUtils.verifyBinSignature(result));
                            }
                            catch (final Exception e) {
                                org.junit.Assert.fail("Error validando la contrafirma generada: " + e);
                            }
                        }
                    }
                }
            }
        }
    }

    /** Prueba de cofirma CMS. */
    @Test
    @Ignore
    public void testCoSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOCMSSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] p7s = null;
        // Primero cargamos el XML a contrafirmar
        try {
            p7s = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Contrafirma3.p7s"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            p7s = null;
        }
        if (p7s != null && p7s.length == 0) {
            p7s = null;
        }
        assertNotNull("No se ha podido cargar el fichero de firmas CMS a cofirmar", p7s);

        for (final AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (final String algo : ALGOS) {
                for (final Properties extraParams : CMS_MODES) {

                    System.out.println();
                    System.out.println();
                    System.out.println();
                    System.out.println("Prueba COFIRMA con '" + algo
                                       + "', '"
                                       + kstore
                                       + "', formato '"
                                       + extraParams.getProperty("format")
                                       + "' y modo '"
                                       + extraParams.getProperty("mode")
                                       + "'");
                    if (AOConstants.AOKeyStore.WINDOWS.equals(kstore) && AOConstants.SIGN_ALGORITHM_SHA512WITHRSA.equals(algo)) {
                        System.out.println("Omitimos las pruebas en CAPI con SHA2 por errores conocidos de Java");
                        continue;
                    }
                    if (AOKeyStore.APPLE.equals(kstore) && (!Platform.OS.MACOSX.equals(Platform.getOS()))) {
                        System.out.println("Omitimos las pruebas de Llavero Apple si no estamos en Mac OS X");
                        continue;
                    }
                    if (AOKeyStore.WINDOWS.equals(kstore) && (!Platform.OS.WINDOWS.equals(Platform.getOS()))) {
                        System.out.println("Omitimos las pruebas de CAPI si no estamos en Windows");
                        continue;
                    }
                    try {
                        pke = getValidKeyEntryFromKeyStore(kstore);
                        assertNotNull("No hemos encontrado una clave de prueba en " + kstore, pke);
                        result = null;
                        result = signer.cosign(p7s, algo, pke, extraParams);
                    }
                    catch (final Exception e) {
                        e.printStackTrace();
                        result = null;
                    }
                    assertNotNull("El resultado de la cofirma es nulo", result);
                    if (!Platform.JREVER.J5.equals(Platform.getJavaVersion())) {
                        try {
                            System.out.println("Resultado de la validacion: " + TestUtils.verifyBinSignature(result));
                        }
                        catch (final Exception e) {
                            org.junit.Assert.fail("Error validando la cofirma generada: " + e);
                        }
                    }
                }
            }
        }
    }
}
