package test.es.gob.jmulticard.card.iso7816four;

import java.io.IOException;
import java.util.Hashtable;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;

import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.card.Location;
import es.gob.jmulticard.card.iso7816four.FileNotFoundException;
import es.gob.jmulticard.card.iso7816four.Iso7816FourCard;
import es.gob.jmulticard.card.iso7816four.Iso7816FourCardException;

/** Pruebas sobre una tarjeta inteligente ISO 7816-4
 * @author Alberto Mart&iacute;nez */
public class TestIso7816FourCard {
	
    /** Nombre del Master File de la estructura PKCS#15. */
    private static final String MASTER_FILE_NAME = "Master.File"; //$NON-NLS-1$

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(TestIso7816FourCard.class.getName());

    private class IsoCard extends Iso7816FourCard {

        IsoCard(final byte c, final ApduConnection conn) throws ApduConnectionException {
            super(c, conn);
        }

        @Override
        public String getCardName() {
            return "Tarjeta Generica de pruebas ISO 7816-4"; //$NON-NLS-1$
        }

		@Override
		protected void selectMasterFile() throws ApduConnectionException,
				FileNotFoundException {
			selectFileByName(MASTER_FILE_NAME);
			
		}
    }

    /** Prueba de lectura de un fichero de una tarjeta ISO 7816-4
     * Test method for {@link es.gob.jmulticard.card.iso7816four.Iso7816FourCard#readBinaryComplete(int)}. */
    @Test
    public void testReadBinary() {
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }

        Iso7816FourCard card;
        try {
            card = new IsoCard((byte) 0x00, conn);
        }
        catch (final ApduConnectionException e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        try {
        	card.selectFileByName(MASTER_FILE_NAME);
            card.readBinaryComplete(card.selectFileById(new byte[] {
                    0x60, (byte) 0xA8
            }));
        }
        catch (final ApduConnectionException e) {
            Assert.fail(e.getMessage());
        }
        catch (final IOException e) {
            Assert.fail(e.getMessage());
        }
        catch (final Iso7816FourCardException e) {
            Assert.fail(e.getMessage());
        }

    }
    

	/** Prueba la selecci&oacute;n de un fichero que no existe.
	 * @throws ApduConnectionException
	 * @throws Iso7816FourCardException */
	@Test(expected=FileNotFoundException.class)
    public void testSelectNonexistantFile() throws ApduConnectionException, Iso7816FourCardException {
    	ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }

        Iso7816FourCard card = null;
        try {
            card = new IsoCard((byte) 0x00, conn);
        }
        catch (final ApduConnectionException e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        card.selectFileById("ho".getBytes()); //$NON-NLS-1$
    }

    /** Prueba para obtener de un fichero de una tarjeta ISO 7816-4
     * Test method for {@link es.gob.jmulticard.card.iso7816four.Iso7816FourCard#selectFileById(byte[])}. */
    @Test
    public void testSelectFileById() {
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }

        Iso7816FourCard card = null;
        try {
            card = new IsoCard((byte) 0x00, conn);
        }
        catch (final ApduConnectionException e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        try {
        	card.selectFileByName(MASTER_FILE_NAME);
            final int longitud = card.selectFileById(new byte[] {
                    0x60, (byte) 0xA8
            });
            TestIso7816FourCard.LOGGER.info("La longitud del fichero es: " + longitud); //$NON-NLS-1$
        }
        catch (final ApduConnectionException e) {
            Assert.fail(e.getMessage());
        }
//        catch (final FileNotFoundException e) {
//        	// El DNIe con Java, en selecciones de fichero muy seguidas
//        	// a veces no selecciona bien los ficheros
//        	TestIso7816FourCard.LOGGER.warning(e.getMessage());
//        	return;
//        }
        catch (final Iso7816FourCardException e) {
            Assert.fail(e.getMessage());
        }

    }

    /** Prueba de lectura de un fichero de una tarjeta ISO 7816-4 y obtenci&oacute;n de su contenido
     * Test method for {@link es.gob.jmulticard.card.iso7816four.Iso7816FourCard#selectFileByIdAndRead(byte[])}. */
    @Test
    public void testSelectFileByIdAndRead() {
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        Iso7816FourCard card;
        try {
            card = new IsoCard((byte) 0x00, conn);
        }
        catch (final ApduConnectionException e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        try {
        	card.selectFileByName(MASTER_FILE_NAME);
            card.selectFileByIdAndRead(new byte[] {
                    0x60, (byte) 0xA8
            });
        }
        catch (final ApduConnectionException e) {
            Assert.fail(e.getMessage());
        }
        catch (final IOException e) {
            Assert.fail(e.getMessage());
        }
//        catch (final FileNotFoundException e) {
//        	// El DNIe con Java, en selecciones de fichero muy seguidas
//        	// a veces no selecciona bien los ficheros
//        	TestIso7816FourCard.LOGGER.warning(e.getMessage());
//        	return;
//        }
        catch (final Iso7816FourCardException e) {
            Assert.fail(e.getMessage());
        }
    }

    /** Prueba de lectura de diversos ficheros de una tarjeta ISO 7816-4 y obtenci&oacute;n de su longitud
     * Test method for {@link es.gob.jmulticard.card.iso7816four.Iso7816FourCard#selectFileByLocation(Location)}. */
    @Test
    public void testSelectFileByLocation() {
        TestIso7816FourCard.LOGGER.info("testSelectFileByLocation()"); //$NON-NLS-1$
        final String[] locations = new String[] {
                "50156001", "50155031", "50156005", "50155032"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        final Hashtable<String, String> locationsWithPathSeparator = new Hashtable<String, String>();
        locationsWithPathSeparator.put("5015/6001", "/"); //$NON-NLS-1$ //$NON-NLS-2$
        locationsWithPathSeparator.put("5015-5031", "-"); //$NON-NLS-1$ //$NON-NLS-2$
        locationsWithPathSeparator.put("5015;6005", ";"); //$NON-NLS-1$ //$NON-NLS-2$
        locationsWithPathSeparator.put("5015:5032", ":"); //$NON-NLS-1$ //$NON-NLS-2$

        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        Iso7816FourCard card;
        try {
            card = new IsoCard((byte) 0x00, conn);
        }
        catch (final ApduConnectionException e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        try {
            for (final String location : locations) {
                TestIso7816FourCard.LOGGER.info("La longitud del fichero " + location + " es " + card.selectFileByLocation(new Location(location)) + "bytes"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            }
            for (final Entry<String, String> location : locationsWithPathSeparator.entrySet()) {
                TestIso7816FourCard.LOGGER.info("La longitud del fichero " + location.getKey() + " es " + card.selectFileByLocation(new Location(location.getKey(), location.getValue().charAt(0))) + "bytes"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            }
        }
        catch (final Exception e) {
            Assert.fail(e.getMessage());
        }
    }
    /** Prueba de lectura de ficheros grandes de una tarjeta ISO 7816-4 y obtenci&oacute;n de su contenido */
    @Test
    public void testSelectAndReadBigFile() {
        final String cdfFile = "50156004"; //$NON-NLS-1$
        TestIso7816FourCard.LOGGER.info("testSelectAndReadBigFile()"); //$NON-NLS-1$
        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }

        Iso7816FourCard card;
        try {
            card = new IsoCard((byte) 0x00, conn);
        }
        catch (final ApduConnectionException e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        try {
            final int fileLength = card.selectFileByLocation(new Location(cdfFile));
            final byte[] fileContent = card.readBinaryComplete(fileLength);
            TestIso7816FourCard.LOGGER.info("La longitud del fichero " + cdfFile + " es " + card.selectFileByLocation(new Location(cdfFile)) + "bytes"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            TestIso7816FourCard.LOGGER.info("El total de bytes escritos para el fichero " + cdfFile + " es " + fileContent.length); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            Assert.fail(e.getMessage());
        }

    }
    
    /** Prueba de lectura de diversos ficheros de una tarjeta ISO 7816-4 y obtenci&oacute;n de su contenido
     * Test method for {@link es.gob.jmulticard.card.iso7816four.Iso7816FourCard#selectFileByLocationAndRead(Location)}. */
    @Test
    public void testSelectFileByLocationAndRead() {
        TestIso7816FourCard.LOGGER.info("testSelectFileByLocationAndRead()"); //$NON-NLS-1$
        final String[] locations = new String[] {
                "50156001", "50155031", "50156005", "50155032"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        final Hashtable<String, String> locationsWithPathSeparator = new Hashtable<String, String>();
        locationsWithPathSeparator.put("5015/6001", "/"); //$NON-NLS-1$ //$NON-NLS-2$
        locationsWithPathSeparator.put("5015-5031", "-"); //$NON-NLS-1$ //$NON-NLS-2$
        locationsWithPathSeparator.put("5015;6005", ";"); //$NON-NLS-1$ //$NON-NLS-2$
        locationsWithPathSeparator.put("5015:5032", ":"); //$NON-NLS-1$ //$NON-NLS-2$

        ApduConnection conn = null;
        try {
            conn = new es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection();
            conn.reset();
        }
        catch (final Exception e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }

        Iso7816FourCard card;
        try {
            card = new IsoCard((byte) 0x00, conn);
        }
        catch (final ApduConnectionException e) {
            TestIso7816FourCard.LOGGER.warning(e.getMessage());
            return;
        }
        try {
            for (final String location : locations) {
                TestIso7816FourCard.LOGGER.info(location);
                card.selectFileByLocationAndRead(new Location(location));
            }
            for (final Entry<String, String> location : locationsWithPathSeparator.entrySet()) {
                TestIso7816FourCard.LOGGER.info(location.getKey());
                card.selectFileByLocationAndRead(new Location(location.getKey(), location.getValue().charAt(0)));
            }
        }
        catch (final ApduConnectionException e) {
            Assert.fail(e.getMessage());
        }
        catch (final Exception e) {
            Assert.fail(e.getMessage());
        }

    }
}
