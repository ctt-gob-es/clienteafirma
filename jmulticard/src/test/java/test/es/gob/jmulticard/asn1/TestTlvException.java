package test.es.gob.jmulticard.asn1;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.asn1.TlvException;

/** Pruebas unitarias para la clase {@linkplain es.gob.jmulticard.asn1.TlvException}
 * @author Alberto Mart&iacute;nez */
public class TestTlvException extends TestCase {

    /** Test method for {@link es.gob.jmulticard.asn1.TlvException#TlvException(java.lang.String)} and
     * {@link es.gob.jmulticard.asn1.TlvException#TlvException(String, Throwable)}. */
    public final static void testCreationTlvException() {
        Assert.assertNotNull(new TlvException("")); //$NON-NLS-1$
        Assert.assertNotNull(new TlvException("", new Exception(""))); //$NON-NLS-1$ //$NON-NLS-2$
    }
}