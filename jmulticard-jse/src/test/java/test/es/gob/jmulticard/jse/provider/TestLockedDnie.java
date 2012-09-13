package test.es.gob.jmulticard.jse.provider;

import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateException;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import org.junit.Assert;
import org.junit.Test;

import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.card.AuthenticationModeLockedException;
import es.gob.jmulticard.jse.provider.DnieProvider;

/** Pruebas de DNI bloqueado. */
public class TestLockedDnie {
	
	private static final Logger LOGGER = Logger.getLogger(TestLockedDnie.class.getName());

	/** Prueba la deteccion de un DNIe con el PIN bloqueado.
	 * @throws IOException 
	 * @throws CertificateException 
	 * @throws NoSuchAlgorithmException 
	 * @throws KeyStoreException 
	 * @throws UnrecoverableEntryException */
	@SuppressWarnings("static-method")
	@Test(expected=AuthenticationModeLockedException.class)
	public void TestLockedDnieDetection() throws NoSuchAlgorithmException, CertificateException, IOException, KeyStoreException, UnrecoverableEntryException {
        System.setProperty("es.gob.jmulticard.fastmode", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		JOptionPane.showMessageDialog(null, "Inserte un DNI con el PIN bloqueado y pulse OK"); //$NON-NLS-1$
        final Provider p = new DnieProvider();
        Security.addProvider(p);
        final KeyStore ks;
        try {
            ks = KeyStore.getInstance("DNI"); //$NON-NLS-1$
        }
        catch (final KeyStoreException e) {
            LOGGER.severe(e.getMessage());
            Assert.fail("No se pudo obtener una instancia del proveedor: " + e.getMessage()); //$NON-NLS-1$
            return;
        }
        try {
            ks.load(null, null);
            ks.getEntry("CertFirmaDigital", null); //$NON-NLS-1$
        }
        catch(final ApduConnectionException e) {
            LOGGER.warning(e.toString());
            return;
        }
	}
}
