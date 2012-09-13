package test.es.gob.jmulticard.apdu.dnie;

import org.junit.Assert;
import org.junit.Test;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.StatusWord;
import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.apdu.dnie.GetChipInfoApduCommand;

/** Pruebas de APDU
 * @author Alberto Mart&iacute;nez */
@SuppressWarnings("static-method")
public class TestGetChipInfoApdu {

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(TestGetChipInfoApdu.class.getName());

    /** Trata de obtener el n&uacute;mero de serie de una tarjeta criptogr&aacute;fica
     * @throws ApduConnectionException Si ocurri&oacute; un error al tratar de obtener el n&uacute;mero de serie de la tarjeta */
    @Test
    public void testGetChipInfo() throws ApduConnectionException {
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
        }
        catch (final Exception e) {
            // Ocurrio un error tratando de crear una conexion
            LOGGER.warning(e.getMessage());
            return;
        }
        if (conn != null) {
            try {
                conn.open();
            }
            catch (final ApduConnectionException e) {
                // No se pudo abrir conexion
                LOGGER.warning(e.getMessage());
                return;
            }
            if (conn.isOpen()) {
                long[] terminals = null;
                try {
                    terminals = conn.getTerminals(true);
                }
                catch (final ApduConnectionException e) {
                    // No se pudo abrir conexion con el lector
                    LOGGER.warning(e.getMessage());
                }
                if (terminals != null) {
                    try {
                        final ResponseApdu res = conn.transmit(new GetChipInfoApduCommand());
                        if (res.getStatusWord().equals(new StatusWord((byte) 0x90, (byte) 0x00))) {
                            LOGGER.info(HexUtils.hexify(res.getData(), true));
                        }
                        else {
                            LOGGER.warning("Se obtuvo el error " + HexUtils.hexify(res.getStatusWord().getBytes(), true)); //$NON-NLS-1$
                        }
                    }
                    catch (final ApduConnectionException e) {
                        // Error durante la transmision del comando
                        Assert.fail(e.toString());
                    }
                }
                conn.close();
            }
        }
    }
}