package es.gob.afirma.cliente.test.signers;

import static org.junit.Assert.assertNotNull;
import static es.gob.afirma.cliente.test.TestUtils.KEYSTORES;
import static es.gob.afirma.cliente.test.TestUtils.getValidKeyEntryFromKeyStore;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOOOXMLSigner;
import es.gob.afirma.signers.AOSigner;

/** Pruebas de firmas OOXML.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestOOXML {

    /** Prueba de firma OOXML. */
    @Test
    public void testSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOOOXMLSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] ooxml = null;
        // Primero cargamos el OOXML a firmar
        try {
            ooxml = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Prueba_firma_OOXML.docx"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            ooxml = null;
        }
        if (ooxml != null && ooxml.length == 0) ooxml = null;
        assertNotNull("No se ha podido cargar el OOXML a firmar", ooxml);

        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println("Prueba FIRMA OOXML con '" + kstore + "'");
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
                result = signer.sign(ooxml, AOConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke, null);
            }
            catch (final Exception e) {
                e.printStackTrace();
                result = null;
            }
            assertNotNull("El resultado de la firma es nulo", result);
        }
    }

    /** Prueba de cofirma OOXML. */
    @Test
    public void testCoSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOOOXMLSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] ooxml = null;
        // Primero cargamos el OOXML a firmar
        try {
            ooxml = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Prueba_cofirma_OOXML.docx"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            ooxml = null;
        }
        if (ooxml != null && ooxml.length == 0) ooxml = null;
        assertNotNull("No se ha podido cargar el OOXML a cofirmar", ooxml);

        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println("Prueba COFIRMA OOXML con '" + kstore + "'");
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
                result = signer.sign(ooxml, AOConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke, null);
            }
            catch (final Exception e) {
                e.printStackTrace();
                result = null;
            }
            assertNotNull("El resultado de la cofirma es nulo", result);
        }

    }

}
