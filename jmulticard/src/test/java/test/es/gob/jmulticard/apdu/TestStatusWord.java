package test.es.gob.jmulticard.apdu;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.StatusWord;

/** Prueba de m&eacute;todos de la clase {@linkplain es.gob.jmulticard.apdu.StatusWord}
 * @author Alberto Mart&iacute;nez */
public class TestStatusWord extends TestCase {

    /** Prueba el funcionamiento de hashCode */
    public final static void testHashCode() {
        final StatusWord sw = new StatusWord((byte) 0x90, (byte) 0x00);
        Assert.assertEquals(HexUtils.getShort(new byte[] {
                (byte) 0x90, (byte) 0x00
        }, 0), sw.hashCode());
    }

    /** Prueba el funcionamiento de getBytes */
    public final static void testGetBytes() {
        final StatusWord sw = new StatusWord((byte) 0x90, (byte) 0x00);
        final byte[] respuestaEsperada = new byte[] {
                (byte) 0x90, (byte) 0x00
        };

        for (int i = 0; i < sw.getBytes().length; i++) {
            Assert.assertEquals(respuestaEsperada[i], sw.getBytes()[i]);

        }
    }

    /** Prueba el funcionamiento de equals */
    public final static void testEqualsObject() {
        final StatusWord sw1 = new StatusWord((byte) 0x90, (byte) 0x00);
        final StatusWord sw2 = new StatusWord((byte) 0x90, (byte) 0x00);
        Assert.assertTrue(sw1.equals(sw2));
        Assert.assertFalse(sw1.equals(new StatusWord((byte) 0x60, (byte) 0x84)));
        Assert.assertFalse(sw1.equals(String.valueOf(true)));
    }
}