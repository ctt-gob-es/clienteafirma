package test.es.gob.jmulticard.asn1.der;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.asn1.Asn1Exception;
import es.gob.jmulticard.asn1.TlvException;
import es.gob.jmulticard.asn1.der.DerBoolean;

/** @author Alberto Mart&iacute;nez */
public class TestDerBoolean extends TestCase {

    /** Test method for {@link es.gob.jmulticard.asn1.DecoderObject#setDerValue(byte[])}. */
    public final static void testSetDerValueWithNullArgumentMustGenerateIllegalArgumentException() {
        final DerBoolean db = new DerBoolean();
        try {
            db.setDerValue(null);
        }
        catch (final Exception e) {
            if (!(e instanceof IllegalArgumentException)) {
                Assert.fail("Se esperaba " + IllegalArgumentException.class.getName() + " pero se obtuvo " + e.getClass().getName()); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
    }

    /** Test method for {@link es.gob.jmulticard.asn1.DecoderObject#getBytes()}.
     * @throws TlvException
     * @throws Asn1Exception */
    public final static void testGetBytes() throws Asn1Exception, TlvException {
        final DerBoolean db = new DerBoolean();
        db.setDerValue(new byte[] {
                (byte) 0x01, (byte) 0x01, (byte) 0x00
        });
        Assert.assertEquals("010100", HexUtils.hexify(db.getBytes(), false)); //$NON-NLS-1$
    }

    /** Test method for {@link es.gob.jmulticard.asn1.DecoderObject#checkTag(byte)}. */
    public final static void testCheckTagWithWrongTagMustThrowException() {
        try {
            final DerBoolean db = new DerBoolean();
            db.checkTag((byte) 0x02);
        }
        catch (final Exception e) {
            if (!(e instanceof Asn1Exception)) {
                Assert.fail("Se esperaba " + Asn1Exception.class.getName() + " pero se obtuvo " + e.getClass().getName()); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
    }
}