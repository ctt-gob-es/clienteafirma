package test.es.gob.jmulticard.apdu.connection.cwa14890;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

import javax.security.auth.callback.PasswordCallback;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.jmulticard.CryptoHelper;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.apdu.connection.cwa14890.Cwa14890OneConnection;
import es.gob.jmulticard.apdu.iso7816four.ReadBinaryApduCommand;
import es.gob.jmulticard.apdu.iso7816four.SelectDfByNameApduCommand;
import es.gob.jmulticard.apdu.iso7816four.SelectFileByIdApduCommand;
import es.gob.jmulticard.apdu.iso7816four.VerifyApduCommand;
import es.gob.jmulticard.card.dnie.Dnie;
import es.gob.jmulticard.card.iso7816four.Iso7816FourCardException;
import es.gob.jmulticard.jse.provider.JseCryptoHelper;
import es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection;
import es.gob.jmulticard.ui.passwordcallback.gui.UIPasswordCallbackAccessibility;

/** Pruebas para comprobar la conexi&oacute;n de canal seguro. */
@SuppressWarnings("static-method")
public class TestCwa14890Connection {

    private static final PasswordCallback PSSCALLBACK = new UIPasswordCallbackAccessibility("Introduzca el pin del DNIe", null, "Introduzca el pin del DNIe", 'I', "PIN del DNIe"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(TestCwa14890Connection.class.getName());

    private static final byte[] MASTER_FILE_NAME = new byte[] {
            (byte) 0x4d, (byte) 0x61, (byte) 0x73, (byte) 0x74, (byte) 0x65, (byte) 0x72, (byte) 0x2E, (byte) 0x46, (byte) 0x69, (byte) 0x6C,
            (byte) 0x65
    };

    /** Prueba el establecimiento de una conexi&oacute;n
     * @throws ApduConnectionException Si ocurre un error en la comunicaci&oacute;n con la tarjeta. */
    @Test
    public void testOpenCwa14890SecureChannel() throws ApduConnectionException {

        final CryptoHelper cryptoHelper = new JseCryptoHelper();
        final ApduConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        Dnie dnie = null;
        try {
            dnie = new Dnie(conn, PSSCALLBACK, cryptoHelper);
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
            Assert.fail("No se pudo componer el objeto DNIe: " + e); //$NON-NLS-1$
        }
        if (dnie != null) {
            final Cwa14890OneConnection cipherConn = new Cwa14890OneConnection(dnie, conn, cryptoHelper);
            cipherConn.open();

            Assert.assertTrue("No se pudo abrir el canal seguro con la tarjeta: ", cipherConn.isOpen()); //$NON-NLS-1$

            cipherConn.close();
        }
    }

    /** Abre el canal seguro del DNIe y transmite una APDU Verify.
     * @throws ApduConnectionException Si ocurre un error en la comunicaci&oacute;n con la tarjeta. */
    @Test
    public void testTransmitVerifyApduBySecureChannel() throws ApduConnectionException {

        final CryptoHelper cryptoHelper = new JseCryptoHelper();
        final ApduConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        Dnie dnie = null;
        try {
            dnie = new Dnie(conn, PSSCALLBACK, cryptoHelper);
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
        }
        if (dnie != null) {
            final Cwa14890OneConnection cipherConn = new Cwa14890OneConnection(dnie, conn, cryptoHelper);
            cipherConn.open();

            Assert.assertTrue("No se pudo abrir el canal seguro con la tarjeta: ", cipherConn.isOpen()); //$NON-NLS-1$
            CommandApdu apdu = null;

            apdu = new VerifyApduCommand((byte) 0x00, PSSCALLBACK);
            final ResponseApdu res = cipherConn.transmit(apdu);

            LOGGER.info("Resultado de enviar la APDU cifrada: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

            cipherConn.close();
        }
    }

    /** Abre el canal seguro del DNIe y leer un fichero indicando un Le incorrecto.
     * @throws IOException Si ocurre un error durante la comunicaci&oacute;n con la tarjeta */
    @Test
    public void testOpenSecureChannelAndReadFileWithIncorrectLe() throws IOException {
        final CryptoHelper cryptoHelper = new JseCryptoHelper();
        final ApduConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        Dnie dnie = null;
        try {
            dnie = new Dnie(conn, PSSCALLBACK, cryptoHelper);
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        final Cwa14890OneConnection cipherConn = new Cwa14890OneConnection(dnie, conn, cryptoHelper);
        cipherConn.open();

        Assert.assertTrue("No se pudo abrir el canal seguro con la tarjeta: ", cipherConn.isOpen()); //$NON-NLS-1$
        CommandApdu apdu = null;
        ResponseApdu res = null;

        apdu = new VerifyApduCommand((byte) 0x00, PSSCALLBACK);
        res = cipherConn.transmit(apdu);

        LOGGER.info("Resultado de la verificacion: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        // Seleccion del Directorio 0006
        apdu = new SelectFileByIdApduCommand((byte) 0x00, new byte[] {
                (byte) 0x00, (byte) 0x06
        });
        res = cipherConn.transmit(apdu);

        LOGGER.info("Resultado de la seleccion del fichero 0006: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        // Leemos el fichero
        apdu = new ReadBinaryApduCommand((byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xEF);
        res = cipherConn.transmit(apdu);

        LOGGER.info("Resultado de la lectura del fichero: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        conn.close();
    }

    /** Abre el canal seguro del DNIe y lee el certificado de autenticaci&oacute;n.
     * @throws IOException Si ocurre un error durante la comunicaci&oacute;n con la tarjeta */
    @Test
    public void testReadAuthCertificate() throws IOException {

        final CryptoHelper cryptoHelper = new JseCryptoHelper();
        final ApduConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        Dnie dnie = null;
        try {
            dnie = new Dnie(conn, PSSCALLBACK, cryptoHelper);
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        final Cwa14890OneConnection cipherConn = new Cwa14890OneConnection(dnie, conn, cryptoHelper);
        cipherConn.open();

        Assert.assertTrue("No se pudo abrir el canal seguro con la tarjeta: ", cipherConn.isOpen()); //$NON-NLS-1$
        CommandApdu apdu = null;
        ResponseApdu res = null;

        apdu = new VerifyApduCommand((byte) 0x00, PSSCALLBACK);
        res = cipherConn.transmit(apdu);

        LOGGER.info("Resultado de la verificacion: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        // Seleccion del MasterFile
        apdu = new SelectDfByNameApduCommand((byte) 0x00, MASTER_FILE_NAME);
        res = cipherConn.transmit(apdu);

        LOGGER.info("Resultado de la seleccion del directorio raiz (master File): " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        // Seleccion del Directorio 6081
        apdu = new SelectFileByIdApduCommand((byte) 0x00, new byte[] {
                (byte) 0x60, (byte) 0x81
        });
        res = cipherConn.transmit(apdu);

        LOGGER.info("Resultado de la seleccion del fichero 6081: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        // Seleccion del certificado de autenticacion
        apdu = new SelectFileByIdApduCommand((byte) 0x00, new byte[] {
                (byte) 0x70, (byte) 0x04
        });
        res = cipherConn.transmit(apdu);

        LOGGER.info("Resultado de la seleccion del fichero 7004: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        final byte[] data = res.getData();
        final int lastLapSize = ((data[7] << 8) | data[8]) % 0xEF;

        // Lectura del certificado de autenticacion
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();

        apdu = new ReadBinaryApduCommand((byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xEF);
        res = cipherConn.transmit(apdu);
        baos.write(res.getData());

        LOGGER.info("Resultado de la lectura 1: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        apdu = new ReadBinaryApduCommand((byte) 0x00, (byte) 0x00, (byte) 0xEF, (byte) 0xEF);
        res = cipherConn.transmit(apdu);
        baos.write(res.getData());

        LOGGER.info("Resultado de la lectura 2: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        apdu = new ReadBinaryApduCommand((byte) 0x00, (byte) 0x01, (byte) 0xDE, (byte) 0xEF);
        res = cipherConn.transmit(apdu);
        baos.write(res.getData());

        LOGGER.info("Resultado de la lectura 3: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        apdu = new ReadBinaryApduCommand((byte) 0x00, (byte) 0x02, (byte) 0xCD, (byte) 0xEF);
        res = cipherConn.transmit(apdu);
        baos.write(res.getData());

        LOGGER.info("Resultado de la lectura 4: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        apdu = new ReadBinaryApduCommand((byte) 0x00, (byte) 0x03, (byte) 0xBC, (byte) 0xEF);
        res = cipherConn.transmit(apdu);
        baos.write(res.getData());

        LOGGER.info("Resultado de la lectura 5: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        apdu = new ReadBinaryApduCommand((byte) 0x00, (byte) 0x04, (byte) 0xAB, (byte) lastLapSize);
        res = cipherConn.transmit(apdu);
        baos.write(res.getData());

        LOGGER.info("Resultado de la lectura 6: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$
        LOGGER.info("Compressed Cert:\n" + HexUtils.hexify(baos.toByteArray(), true)); //$NON-NLS-1$
        LOGGER.info("\nCertificado: " + new String(deflate(baos.toByteArray()))); //$NON-NLS-1$

        cipherConn.close();
    }

    /** Abre el canal seguro del DNIe y firma con el certificado de firma.
     * @throws IOException Si ocurre un error durante la comunicaci&oacute;n con la tarjeta
     * @throws Iso7816FourCardException */
    @Test
    public void testHighLevelSignature() throws IOException, Iso7816FourCardException {

        final CryptoHelper cryptoHelper = new JseCryptoHelper();
        final ApduConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning("Error en la conexion con la tarjeta: " + e.toString()); //$NON-NLS-1$
            return;
        }

        Dnie dnie = null;
        try {
            dnie = new Dnie(conn, PSSCALLBACK, cryptoHelper);
        }
        catch (final Exception e) {
            e.printStackTrace();
            LOGGER.warning("Error en la configuracion de la tarjeta: " + e.toString()); //$NON-NLS-1$
            return;
        }

        final byte[] signature = dnie.sign("Hola Mundo".getBytes(), "SHA1withRSA", dnie.getPrivateKey("CertFirmaDigital")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        LOGGER.info("Firma:\n" + HexUtils.hexify(signature, true)); //$NON-NLS-1$

        conn.close();
    }

    private static byte[] deflate(final byte[] zlib) throws IOException {
        byte[] buf = null;
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        try {
            final Inflater decompressor = new Inflater();
            decompressor.setInput(zlib, 8, zlib.length - 8);

            // Decompress the data
            buf = new byte[1024];
            while (!decompressor.finished()) {
                final int count = decompressor.inflate(buf);
                if (count == 0) {
                    throw new DataFormatException();
                }
                buffer.write(buf, 0, count);
            }

            // Get the decompressed data
            return buffer.toByteArray();
        }
        catch (final DataFormatException ex) {
            throw new IOException("Error cert compression: " + ex); //$NON-NLS-1$
        }
        finally {
            if (buf != null) {
                Arrays.fill(buf, (byte) 0x00);
            }
            buffer.reset();
        }
    }

    /** Abre el canal seguro del DNIe y selecciona el Master File.
     * @throws IOException Si ocurre un error durante la comunicaci&oacute;n con la tarjeta */
    @Test
    public void testSelectMasterFile() throws IOException {

        final CryptoHelper cryptoHelper = new JseCryptoHelper();
        final ApduConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        try {
            final Dnie dnie = new Dnie(conn, PSSCALLBACK, cryptoHelper);
            LOGGER.info(dnie.getCardName());
        }
        catch (final Exception e) {
            LOGGER.warning(e.getMessage());
            return;
        }

        CommandApdu apdu = null;
        ResponseApdu res = null;

        // Seleccion del Master File
        apdu = new SelectDfByNameApduCommand((byte) 0x00, MASTER_FILE_NAME);
        res = conn.transmit(apdu);

        LOGGER.info("Resultado de la seleccion del Master File: " + HexUtils.hexify(res.getBytes(), true)); //$NON-NLS-1$

        conn.close();
    }

    class GenericApdu extends CommandApdu {
        public GenericApdu(final byte cla, final byte ins, final byte p1, final byte p2, final byte[] data, final Integer ne) {
            super(cla, ins, p1, p2, data, ne);
        }
    }
}