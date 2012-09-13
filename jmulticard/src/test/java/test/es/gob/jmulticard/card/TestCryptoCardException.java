package test.es.gob.jmulticard.card;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.card.CryptoCardException;

/** Pruebas unitarias para la clase {@linkplain es.gob.jmulticard.card.CryptoCardException}
 * @author Alberto Mart&iacute;nez */
public class TestCryptoCardException extends TestCase {

    /** Test method for {@link es.gob.jmulticard.card.CryptoCardException#CryptoCardException()}. */
    public final static void testCryptoCardException() {
        Assert.assertNotNull(new CryptoCardException());
    }

    /** Test method for {@link es.gob.jmulticard.card.CryptoCardException#CryptoCardException(java.lang.String)}. */
    public final static void testCryptoCardExceptionString() {
        Assert.assertNotNull(new CryptoCardException("")); //$NON-NLS-1$
    }

    /** Test method for {@link es.gob.jmulticard.card.CryptoCardException#CryptoCardException(java.lang.String, java.lang.Throwable)}. */
    public final static void testCryptoCardExceptionStringThrowable() {
        Assert.assertNotNull(new CryptoCardException("", new Exception())); //$NON-NLS-1$
    }
}