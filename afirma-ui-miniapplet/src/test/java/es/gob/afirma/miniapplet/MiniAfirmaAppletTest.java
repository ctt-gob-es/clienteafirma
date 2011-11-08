package es.gob.afirma.miniapplet;

import org.junit.Test;

import es.gob.afirma.core.misc.Base64;

public class MiniAfirmaAppletTest {

	@Test
	public void signWithDNIe() {
		
		MiniAfirmaApplet applet = new MiniAfirmaApplet();
		
		try {
			applet.sign(
					Base64.encodeBytes("Hola Mundo!!".getBytes()), //$NON-NLS-1$
					"SHA1withRSA", //$NON-NLS-1$
					"CAdES", //$NON-NLS-1$
						"mode=implicit\n" + //$NON-NLS-1$
						"Filter=DNIe:" //$NON-NLS-1$
			);
		} catch (Exception e) {
			System.out.println("Error: " + e); //$NON-NLS-1$
			return;
		}
	}
}
