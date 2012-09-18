package test.es.gob.jmulticard.ui.passwordcallback;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.ui.passwordcallback.CancelledOperationException;

/** @author Alberto Mart&iacute;nez */
public class TestCancelledOperationException extends TestCase {

    /** Test method for {@link es.gob.jmulticard.ui.passwordcallback.CancelledOperationException#CancelledOperationException()}. */
    public final static void testCancelledOperationException() {
        Assert.assertNotNull(new CancelledOperationException());
    }

    /** Test method for {@link es.gob.jmulticard.ui.passwordcallback.CancelledOperationException#CancelledOperationException(java.lang.String)}. */
    public final static void testCancelledOperationExceptionString() {
        Assert.assertNotNull(new CancelledOperationException("Operacion cancelada")); //$NON-NLS-1$
    }
}