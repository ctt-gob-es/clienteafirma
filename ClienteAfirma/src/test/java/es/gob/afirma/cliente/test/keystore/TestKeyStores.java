package es.gob.afirma.cliente.test.keystore;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;

/** Pruebas de almacenes de claves y certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@SuppressWarnings("null")
public final class TestKeyStores {

    /** Prueba el almac&eacute;n MY de MS-CAPI en Windows. */
    @Test
    public void testCapiKeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) return;
        System.out.println("Probando almacen CAPI...");
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.WINDOWS, null, null, null, null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }

    /** Prueba el almac&eacute;n de usuario de NSS. */
    @Test
    public void testNssKeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        System.out.println("Probando almacen NSS...");
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.MOZ_UNI, null, null, null, null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }

    /** Prueba un almac&eacute;n PKCS#12 gen&eacute;rico. */
    @Test
    public void testP12KeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        System.out.println("Probando almacen PKCS#12...");
        // Primero copiamos nuestro P12 a un temporal
        String libName = null;
        try {
            final File tmpP12 = File.createTempFile("p12", ".pfx");
            final OutputStream os = new FileOutputStream(tmpP12);
            os.write(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("ANF_PF_Activo.pfx")));
            try {
                os.flush();
            }
            catch (final Exception e) {}
            try {
                os.close();
            }
            catch (final Exception e) {}
            libName = tmpP12.getAbsolutePath();
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.PKCS12, libName, null, new CachePasswordCallback(new char[] {
                    '1', '2', '3', '4', '1', '2', '3', '4'
            }), null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }

    /** Prueba el almac&eacute;n ADDRESSBOOK de MS-CAPI. */
    @Test
    public void testCapiAddressBookKeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) return;
        System.out.println("Probando almacen CAPI AddressBook...");
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.WINADDRESSBOOK, null, null, null, null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }

    /** Prueba el almac&eacute;n CA de MS-CAPI. */
    @Test
    public void testCapiCAKeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) return;
        System.out.println("Probando almacen CAPI CA...");
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.WINCA, null, null, null, null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }

    /** Prueba el almac&eacute;n ROOT de MS-CAPI. */
    @Test
    public void testCapiRootKeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) return;
        System.out.println("Probando almacen CAPI Root...");
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.WINROOT, null, null, null, null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }

    /** Prueba un almac&eacute;n JavaKeyStore gen&eacute;rico. */
    @Test
    public void testJksKeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        System.out.println("Probando almacen Java KeyStore...");
        // Primero copiamos nuestro JKS a un temporal
        String libName = null;
        try {
            final File tmpP12 = File.createTempFile("jks", ".jks");
            final OutputStream os = new FileOutputStream(tmpP12);
            os.write(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("keystore.jks")));
            try {
                os.flush();
            }
            catch (final Exception e) {}
            try {
                os.close();
            }
            catch (final Exception e) {}
            libName = tmpP12.getAbsolutePath();
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.JAVA, libName, null, new CachePasswordCallback(new char[] {
                    'p', 's', 's', '1', '1', '1', '1'
            }), null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }

    /** Prueba el almac&eacute;n Llavero de Mac OS X. */
    @Test
    public void testMacKeyStore() {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) return;
        System.out.println("Probando almacen Llavero de Mac OS X...");
        AOKeyStoreManager ksm = null;
        try {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOConstants.AOKeyStore.APPLE, null, null, null, null);
        }
        catch (final Exception e) {
            e.printStackTrace();
        }
        assertNotNull(ksm);
        assertTrue(ksm.getKeyStores().size() > 0);
        assertNotNull(ksm.getKeyStores().get(0));
        System.out.println();
    }
}
