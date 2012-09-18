package test.es.gob.jmulticard.ui.passwordcallback;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Test;

import es.gob.jmulticard.ui.passwordcallback.Messages;

/** @author Alberto Mart&iacute;nez */
public class TestMessages extends TestCase {

    /** Test method for {@link es.gob.jmulticard.ui.passwordcallback.Messages#getString(java.lang.String)}. */
	@Test
	public final static void testGetString() {
        Assert.assertEquals("##ERROR## Cadena no disponible: Cadena que no existe", Messages.getString("Cadena que no existe")); //$NON-NLS-1$ //$NON-NLS-2$
    }
}