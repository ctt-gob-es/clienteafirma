package es.gob.afirma.cliente.test.signers;

import static org.junit.Assert.assertNotNull;
import static es.gob.afirma.cliente.test.TestUtils.KEYSTORES;
import static es.gob.afirma.cliente.test.TestUtils.getValidKeyEntryFromKeyStore;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;
import org.junit.Ignore;

import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOODFSigner;
import es.gob.afirma.signers.AOSigner;

/** Prueba de firmas ODF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestODF {

    /** Prueba de firma ODF. */
    @Test
    @Ignore
    public void testSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOODFSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] odf = null;
        // Primero cargamos el ODF a firmar
        try {
            odf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Prueba_firma_ODF.odt"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            odf = null;
        }
        if (odf != null && odf.length == 0) odf = null;
        assertNotNull("No se ha podido cargar el ODF a firmar", odf);

        for (AOConstants.AOKeyStore kstore : KEYSTORES) {
            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println("Prueba FIRMA ODF con '" + kstore + "'");
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
                result = signer.sign(odf, AOConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke, null);
            }
            catch (final Exception e) {
                e.printStackTrace();
                result = null;
            }
            assertNotNull("El resultado de la firma es nulo", result);
        }
    }

    /** Prueba de cofirma ODF. */
    @Test
    @Ignore
    public void testCoSignature() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final AOSigner signer = new AOODFSigner();
        assertNotNull("No se ha podido instanciar el Signer", signer);
        byte[] result = null;
        PrivateKeyEntry pke = null;

        byte[] odf = null;
        // Primero cargamos el ODF a firmar
        try {
            odf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Prueba_cofirma_ODF.odt"));
        }
        catch (final Exception e) {
            e.printStackTrace();
            odf = null;
        }
        if (odf != null && odf.length == 0) odf = null;
        assertNotNull("No se ha podido cargar el ODF a cofirmar", odf);

        for (final AOConstants.AOKeyStore kstore : KEYSTORES) {
            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println("Prueba COFIRMA ODF con '" + kstore + "'");
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
                result = signer.sign(odf, AOConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke, null);
            }
            catch (final Exception e) {
                e.printStackTrace();
                result = null;
            }
            assertNotNull("El resultado de la cofirma es nulo", result);
        }

    }
}
