package test.es.gob.jmulticard.asn1;

import junit.framework.TestCase;

import org.junit.Assert;

import es.gob.jmulticard.asn1.Asn1Exception;
import es.gob.jmulticard.asn1.TlvException;
import es.gob.jmulticard.asn1.der.BitString;
import es.gob.jmulticard.asn1.der.Utf8String;
import es.gob.jmulticard.asn1.der.pkcs15.AccessFlags;

/** prueba del tipos simples ASN.1. */
public class TestAsn1SimpleTypes extends TestCase {
	
	/** Prueba la creaci&oacute; de un tipo UTF8String con datos incorrectos.
	 * @throws Asn1Exception
	 * @throws TlvException */
	public void testUtf8StringCreationWithBadData() throws Asn1Exception, TlvException {
		Utf8String u = new Utf8String();
		try {
			u.setDerValue(new byte[] { (byte)0x00, (byte) 0x01, (byte) 0xff});
		}
		catch(final Asn1Exception e) {
			return;
		}
		Assert.fail("Tendria que haber saltado un TlvException"); //$NON-NLS-1$
	}
	
	/** Prueba la creaci&oacute; de un tipo BitString con datos incorrectos.
	 * @throws Asn1Exception
	 * @throws TlvException */
	public void testBitStringCreationWithBadData() throws Asn1Exception, TlvException {
		BitString u = new AccessFlags();
		try {
			u.setDerValue(new byte[] { (byte)0x00, (byte) 0x01, (byte) 0xff});
		}
		catch(final Asn1Exception e) {
			return;
		}
		Assert.fail("Tendria que haber saltado un TlvException"); //$NON-NLS-1$
	}

}
