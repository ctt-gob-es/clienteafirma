package test.es.gob.jmulticard.apdu;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.apdu.ResponseApdu;

/** Prueba de m&eacute;todos de la clase {@linkplain es.gob.jmulticard.apdu.ResponseApdu}
 * @author Alberto Mart&iacute;nez */
public class TestResponseApdu extends TestCase {

    /** Test method for {@link es.gob.jmulticard.apdu.ResponseApdu#isOk()}. */
    public final static void testIsOk() {
        Assert.assertFalse(new ResponseApdu(new byte[] { (byte) 0x90 }).isOk());
        Assert.assertTrue(new ResponseApdu(new byte[] { (byte) 0x00, (byte) 0x00, (byte) 0x90, (byte) 0x00 }).isOk());
    }
}