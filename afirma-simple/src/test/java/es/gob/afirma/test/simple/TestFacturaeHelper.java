package es.gob.afirma.test.simple;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.DataAnalizerUtil;

/** Pruebas de la gesti&oacute;n de facturas electr&oacute;nicas.
 * IMPORTANTE: Los ficheros de pruebas que sean facturas electr&oacute;nicas deben contener
 * la cadena "factura" en su nombre
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestFacturaeHelper {

    private static final String[] TEST_FILES = new String[] {
        "ANF_PF_Activo.pfx", //$NON-NLS-1$
        "base64.b64", //$NON-NLS-1$
        "facturae_32v1.xml", //$NON-NLS-1$
        "sample-class-attributes.xml", //$NON-NLS-1$
        "sample-embedded-style.xml", //$NON-NLS-1$
        "sample-encoding-UTF-8.xml", //$NON-NLS-1$
        "sample-factura-firmada-30.xml", //$NON-NLS-1$
        "sample-factura-firmada-31.xml", //$NON-NLS-1$
        "sample-factura-firmada-32v1.xsig.xml", //$NON-NLS-1$
        "sample-facturae-firmada.xsig.xml", //$NON-NLS-1$
        "sample-facturae.xml", //$NON-NLS-1$
        "sample-internal-dtd.xml", //$NON-NLS-1$
        "sample-namespace-encoding-us-ascii.xml", //$NON-NLS-1$
        "TEST_PDF_Certified.pdf" //$NON-NLS-1$
    };

    /** Pruebas de la detecci&oacute;n de facturas electr&oacute;nicas.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testFacturaeHelper() throws Exception {
        for (final String f : TEST_FILES) {
            final byte[] file = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(f));
            Assert.assertEquals(Boolean.valueOf(DataAnalizerUtil.isFacturae(file)), Boolean.valueOf(f.contains("factura"))); //$NON-NLS-1$
        }

    }

}
