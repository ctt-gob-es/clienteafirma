package test.es.gob.jmulticard.card.dnie;

import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import org.junit.Assert;
import org.junit.Test;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.CardNotPresentException;
import es.gob.jmulticard.card.InvalidCardException;
import es.gob.jmulticard.card.dnie.Dnie;
import es.gob.jmulticard.jse.provider.JseCryptoHelper;

/** Pruebas de DNIe a nivel de tarjeta
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@SuppressWarnings("static-method")
public final class TestDnieCard {

    private static final Logger LOGGER = Logger.getLogger(TestDnieCard.class.getName());

    /** Prueba para obtener el n&uacute;mero de serie de un DNIe */
    @Test
    public void testGetSerialNumberFromADnie() {
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        Dnie card = null;
        byte[] response = null;
        try {
            card = new Dnie(conn, new PasswordCallback("Introduzca su password: ", false), new JseCryptoHelper()); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        try {
            response = card.getSerialNumber();
        }
        catch (final Exception e) {
            Assert.fail(e.getMessage());
        }

        LOGGER.info(HexUtils.hexify(response, true));
    }

    /** Prueba para obtener el n&uacute;mero de serie de una tarjeta que no es un DNIe. */
    @Test
    public void testGetSerialNumberFromANonDnieCard() {
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        Dnie card = null;
        try {
            card = new Dnie(conn, new PasswordCallback("Introduzca su password: ", false), new JseCryptoHelper()); //$NON-NLS-1$
            card.getSerialNumber();
        }
        catch(CardNotPresentException e) {
            LOGGER.info("No hay ninguna tarjeta insertada en el lector"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            if (!(e instanceof InvalidCardException)) {
                Assert.fail(e.toString());
                return;
            }
            LOGGER.info(e.getMessage());
        }
    }
}
