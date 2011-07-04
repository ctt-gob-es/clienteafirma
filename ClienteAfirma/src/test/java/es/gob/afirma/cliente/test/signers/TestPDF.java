package es.gob.afirma.cliente.test.signers;

import static es.gob.afirma.cliente.test.TestUtils.ALGOS;
import static es.gob.afirma.cliente.test.TestUtils.KEYSTORES;
import static es.gob.afirma.cliente.test.TestUtils.getValidKeyEntryFromKeyStore;
import static org.junit.Assert.assertNotNull;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.cliente.test.TestUtils;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOPDFSigner;
import es.gob.afirma.signers.AOSigner;

/** Pruebas de firmas PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestPDF {

    /** Prueba de firma PDF. */
    @Test
    public void testSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOPDFSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] pdf = null;
        // Primero cargamos el PDF a firmar
        try {
            pdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Prueba_firma_PDF.pdf"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            pdf = null;
        }
        if (pdf != null && pdf.length == 0) pdf = null;
        assertNotNull("No se ha podido cargar el PDF a firmar", pdf);

        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (String algo : ALGOS) {
                System.out.println();
                System.out.println();
                System.out.println();
                System.out.println("Prueba FIRMA PDF con '" + algo + "' y '" + kstore + "'");
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
                    result = signer.sign(pdf, algo, pke, null);
                }
                catch (final Exception e) {
                    e.printStackTrace();
                    result = null;
                }
                assertNotNull("El resultado de la firma es nulo", result);

                try {
                    System.out.println("Resultado de la validacion: " + TestUtils.verifyBinSignature(result));
                }
                catch (final Exception e) {
                    org.junit.Assert.fail("Error validando la firma generada: " + e);
                }
                
            }
        }
    }

    /** Prueba de cofirma PDF. */
    @Test
    public void testCoSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOPDFSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] pdf = null;
        // Primero cargamos el PDF a firmar
        try {
            pdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Prueba_cofirma_PDF.pdf"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            pdf = null;
        }
        if (pdf != null && pdf.length == 0) pdf = null;
        assertNotNull("No se ha podido cargar el PDF a cofirmar", pdf);

        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            for (String algo : ALGOS) {
                System.out.println();
                System.out.println();
                System.out.println();
                System.out.println("Prueba COFIRMA PDF con '" + algo + "' y '" + kstore + "'");
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
                    result = signer.sign(pdf, algo, pke, null);
                }
                catch (final Exception e) {
                    e.printStackTrace();
                    result = null;
                }
                assertNotNull("El resultado de la cofirma es nulo", result);

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
