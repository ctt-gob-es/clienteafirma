package test.es.gob.jmulticard.asn1;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import junit.framework.Assert;
import junit.framework.TestCase;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.asn1.Tlv;

/** Prueba de creaci&oacute;n de TLV.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestTlvCreation extends TestCase {

    private static final int BUFFER_SIZE = 4096;

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(TestTlvCreation.class.getName());

    private static final String[] TEST_FILES = new String[] {
            "CDF_EEE.BER", //$NON-NLS-1$
            "CDF_GSD.BER", //$NON-NLS-1$
            "CDF_GVA.BER", //$NON-NLS-1$
            "CDF_JBM.BER", //$NON-NLS-1$
            "CDF_JMA.BER", //$NON-NLS-1$
            "CDF_TGM.BER" //$NON-NLS-1$
    };

    /** Prueba la creaci&oacute;n de TLV con volcados de CDF.
     * @throws Exception en caso de cualquier tipo de error */
    public static void testTlv() throws Exception {
        byte[] cdfdata;
        for (int i = 0; i < TEST_FILES.length; i++) {
            cdfdata = getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[i]));

            final Tlv tlv = new Tlv(cdfdata);
            Assert.assertNotNull(tlv);
            LOGGER.info(tlv.toString());
            LOGGER.info("\n\nProbando " + TEST_FILES[i]); //$NON-NLS-1$
            LOGGER.info("\nTLV completo (" + Integer.toString(tlv.getBytes().length) + "):"); //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info(HexUtils.hexify(tlv.getBytes(), true));
            LOGGER.info("\nTipo TLV:"); //$NON-NLS-1$
            LOGGER.info(HexUtils.hexify(new byte[] {
                tlv.getTag()
            }, true));
            LOGGER.info("\nLongitud TLV:"); //$NON-NLS-1$
            LOGGER.info(Integer.toString(tlv.getLength()));
            LOGGER.info("\nValor TLV (" + Integer.toString(tlv.getValue().length) + "):"); //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info(HexUtils.hexify(tlv.getValue(), true));
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