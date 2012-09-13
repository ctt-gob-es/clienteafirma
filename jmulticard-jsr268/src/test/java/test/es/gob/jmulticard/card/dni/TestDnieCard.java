package test.es.gob.jmulticard.card.dni;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JOptionPane;

import org.junit.Test;

import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.card.InvalidCardException;
import es.gob.jmulticard.card.dnie.BurnedDnieCardException;
import es.gob.jmulticard.card.dnie.Dnie;

/** Pruebas de DNIe */
public class TestDnieCard {

    /** Prueba la detecci&oacute;n de un DNIe con la memoria vol&aacute;til borrada.
     * @throws InvalidCardException
     * @throws BurnedDnieCardException
     * @throws ApduConnectionException */
    @SuppressWarnings("static-method")
    @Test(expected = BurnedDnieCardException.class)
    public void TestBurnedDnieDetection() throws InvalidCardException, BurnedDnieCardException, ApduConnectionException {
        JOptionPane.showMessageDialog(null, "Inserte un DNI con la memoria volatil borrada y pulse OK"); //$NON-NLS-1$
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            return;
        }
        final Dnie dnie = new Dnie(conn, new PasswordCallback("PIN", true), null); //$NON-NLS-1$
    }

}
