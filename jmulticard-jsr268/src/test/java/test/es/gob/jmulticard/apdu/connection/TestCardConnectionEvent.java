package test.es.gob.jmulticard.apdu.connection;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.apdu.connection.CardConnectionEvent;
import es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection;

/** Pruebas unitarias para la clase {@linkplain es.gob.jmulticard.apdu.connection.CardConnectionEvent}
 * @author Alberto Mart&iacute;nez */
public class TestCardConnectionEvent extends TestCase {

    /** Prueba del m&eacute;todo
     * {@linkplain es.gob.jmulticard.apdu.connection.CardConnectionEvent#CardConnectionEvent(es.gob.jmulticard.apdu.connection.ApduConnection)} */
    public final static void testCardConnectionEvent() {
        Assert.assertNotNull(new CardConnectionEvent(new SmartcardIoConnection()));
    }
}