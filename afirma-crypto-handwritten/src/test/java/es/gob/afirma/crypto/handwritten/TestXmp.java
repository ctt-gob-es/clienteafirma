package es.gob.afirma.crypto.handwritten;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import es.gob.afirma.crypto.handwritten.pdf.PdfXmpHelper;
import es.gob.afirma.crypto.handwritten.pdf.XmpSignStructure;

/** Pruebas de creaci&oacute;n de XMP
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestXmp {

	/** Prueba simple de creaci&oacute;n de esquema XMP.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testXmpCreation() throws Exception {
		final XmpSignStructure s = new XmpSignStructure(
			new SignerInfoBean("Tomas", "Garcia", null, "12345678Z"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			new byte[] { 0x00, 0x01 },
			null
		);
		final List<XmpSignStructure> l = new ArrayList<XmpSignStructure>(1);
		l.add(s);

		final byte[] res = PdfXmpHelper.buildXmp(l);

		System.out.println(new String(res));

	}

}
