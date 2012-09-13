/**
 * 
 */
package test.es.gob.jmulticard.asn1;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.asn1.Asn1Exception;

/** Pruebas unitarias para la clase {@linkplain es.gob.jmulticard.asn1.Asn1Exception}
 * @author Alberto Mart&iacute;nez */
public class TestAsn1Exception extends TestCase {

    /** Test method for {@link es.gob.jmulticard.asn1.Asn1Exception#Asn1Exception(java.lang.String)}. */
    public final static void testAsn1ExceptionString() {
        Assert.assertNotNull(new Asn1Exception("")); //$NON-NLS-1$
    }

    /** Test method for {@link es.gob.jmulticard.asn1.Asn1Exception#Asn1Exception(java.lang.Throwable)}. */
    public final static void testAsn1ExceptionThrowable() {
        Assert.assertNotNull(new Asn1Exception(new Exception()));
    }

    /** Test method for {@link es.gob.jmulticard.asn1.Asn1Exception#Asn1Exception(java.lang.String, java.lang.Throwable)}. */
    public final static void testAsn1ExceptionStringThrowable() {
        Assert.assertNotNull(new Asn1Exception("", new Exception())); //$NON-NLS-1$
    }
}