package es.gob.afirma.cliente.test.signers;

import static org.junit.Assert.assertNotNull;
import static es.gob.afirma.cliente.test.TestUtils.ALGOS;
import static es.gob.afirma.cliente.test.TestUtils.BIN_CONTENT;
import static es.gob.afirma.cliente.test.TestUtils.CADES_MODES;
import static es.gob.afirma.cliente.test.TestUtils.CS_TARGETS;
import static es.gob.afirma.cliente.test.TestUtils.KEYSTORES;
import static es.gob.afirma.cliente.test.TestUtils.getValidKeyEntryFromKeyStore;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.cliente.test.TestUtils;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOCAdESSigner;
import es.gob.afirma.signers.AOSigner;

/** Pruebas de las firmas CAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCAdES {

    /** Prueba de firma CAdES. */
    @Test
    public void testSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOCAdESSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;
        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (String algo : ALGOS) {
                for (Properties extraParams : CADES_MODES) {
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

    /** Prueba de contrafirmas CAdES. */
    @Test
    public void testCounterSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOCAdESSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] csig = null;
        // Primero cargamos el XML a contrafirmar
        try {
            csig = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Contrafirma3.csig"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            csig = null;
        }
        if (csig != null && csig.length == 0) csig = null;
        assertNotNull("No se ha podido cargar el fichero de firmas CAdES a contrafirmar", csig);

        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (String algo : ALGOS) {
                for (Properties extraParams : CADES_MODES) {
                    for (CounterSignTarget targetType : CS_TARGETS) {
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
                            result = signer.countersign(csig, algo, targetType, new Object[0], pke, extraParams);
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

    /** Pruebas de cofirmas CAdES. */
    @Test
    public void testCoSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOCAdESSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] csig = null;
        // Primero cargamos el XML a contrafirmar
        try {
            csig = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Contrafirma3.csig"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            csig = null;
        }
        if (csig != null && csig.length == 0) csig = null;
        assertNotNull("No se ha podido cargar el fichero de firmas CAdES a cofirmar", csig);

        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (String algo : ALGOS) {
                for (Properties extraParams : CADES_MODES) {

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
                        result = signer.cosign(csig, algo, pke, extraParams);
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
