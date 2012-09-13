package test.es.gob.jmulticard.asn1;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.asn1.der.pkcs15.PrKdf;

/** Prueba de creaci&oacute;n de PrKDK PKCS#15.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestPrKdfCreation extends TestCase {

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(TestPrKdfCreation.class.getName());

    private static final int BUFFER_SIZE = 4096;

    private static final String[] TEST_FILES = new String[] {
        "PRKDF_TGM.BER" //$NON-NLS-1$
    };

    /** Prueba la creaci&oacute;n de PrKDK con volcados en disco.
     * @throws Exception en caso de cualquier tipo de error */
    public static void testPrKdf() throws Exception {
        byte[] prkdfdata;
        for (int i = 0; i < TEST_FILES.length; i++) {
            prkdfdata = getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[i]));
            LOGGER.info("\n\nPrKDF completo (" + Integer.toString(prkdfdata.length) + "):"); //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info(HexUtils.hexify(prkdfdata, true));
            final PrKdf prkdf = new PrKdf();
            Assert.assertNotNull(prkdf);
            prkdf.setDerValue(prkdfdata);
            LOGGER.info("\n" + prkdf.toString()); //$NON-NLS-1$
        }
    }

    /** Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
     * @param input
     *        Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException
     *         Cuando ocurre un problema durante la lectura */
    private static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }
}