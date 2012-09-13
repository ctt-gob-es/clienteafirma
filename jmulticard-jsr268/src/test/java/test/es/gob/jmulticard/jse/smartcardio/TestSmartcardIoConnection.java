package test.es.gob.jmulticard.jse.smartcardio;

import org.junit.Assert;
import org.junit.Test;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.apdu.connection.CardConnectionEvent;
import es.gob.jmulticard.apdu.connection.CardConnectionListener;
import es.gob.jmulticard.apdu.dnie.GetChipInfoApduCommand;
import es.gob.jmulticard.apdu.iso7816four.GetChallengeApduCommand;
import es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection;
import es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection.ConnectionProtocol;

/** Pruebas para comprobar el funcionamiento de la conexi&oacute;n con una tarjeta inteligente
 * @author Ricardo Borillo
 * @since 2012-03-26 */
@SuppressWarnings("static-method")
public class TestSmartcardIoConnection {

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(TestSmartcardIoConnection.class.getName());

    /** Prueba el establecimiento de un terminal inexistente en una conexi&oacute;n abierta. */
    @Test
    public void setBadTerminalOnOpenConnection() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        try {
        	conn.open();
        	conn.setTerminal(9);
        	Assert.assertFalse(conn.isOpen());
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
    }
    
    /** Prueba la transmisi&oacute;n de una APDU nula. */
    @Test(expected=IllegalArgumentException.class)
    public void testTransmitNullApdu() {
    	final SmartcardIoConnection conn = new SmartcardIoConnection();
    	try {
        	conn.open();
        	conn.transmit(null);
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
    }
    
    /** Prueba el cambio de exclusividad en una conexi&oacute;n abierta. */
    @Test
    public void setExclusiveOnOpenConnection() {
    	final SmartcardIoConnection conn = new SmartcardIoConnection();
    	try {
    		conn.setExclusiveUse(false);
        	conn.open();
        	conn.setExclusiveUse(true);
        	Assert.assertFalse(conn.isExclusiveUse());
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
    }
    
    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#close()}. */
    @Test
    public void testClose() {
        LOGGER.info("testClose():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        try {
            conn.open();
            if (conn.isOpen()) {
                conn.close();
                Assert.assertFalse(conn.isOpen());
            }
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        finally {
            try {
                conn.close();
            }
            catch (final ApduConnectionException e) {
                LOGGER.warning(e.getMessage());
            }
            catch (final Exception e) {
                Assert.fail(e.getMessage());
            }
        }
    }

    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#getTerminalInfo(int)} . */
    @Test
    public void testGetTerminalInfo() {
        LOGGER.info("testGetTerminalInfo():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        final boolean[] cardsPresent = new boolean[] {
                true, false
        };
        for (final boolean isCardPresent : cardsPresent) {
            long[] terminals = null;
            try {
                terminals = conn.getTerminals(isCardPresent);
            }
            catch (final ApduConnectionException e) {
                LOGGER.warning(e.getMessage());
                return;
            }
            if (terminals != null) {
                for (final long terminal : terminals) {
                    try {
                        final String info = conn.getTerminalInfo((int) terminal);
                        LOGGER.info(info);
                    }
                    catch (final ApduConnectionException e) {
                        LOGGER.warning(e.getMessage());
                    }
                }
            }
            try {
                conn.getTerminalInfo(-1);
            }
            catch (final Exception e) {
                Assert.assertEquals(e.getClass().getName(), ApduConnectionException.class.getName());
            }
        }
    }

    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#getTerminals(boolean)} . */
    @Test
    public void testGetTerminals() {
        LOGGER.info("testGetTerminals():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        long[] terminals = null;
        try {
            terminals = conn.getTerminals(false);
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        if (terminals != null) {
            for (final long terminal : terminals) {
                LOGGER.info(String.valueOf(terminal));
            }
        }
    }

    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#isOpen()} . */
    @Test
    public void testIsOpen() {
        LOGGER.info("testIsOpen():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        try {
            conn.open();
            Assert.assertTrue(conn.isOpen());
            conn.close();
            Assert.assertFalse(conn.isOpen());
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
    }

    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#removeCardConnectionListener(CardConnectionListener)}. */
    @Test
    public void testRemoveCardConnectionListener() {
        try {
            final SmartcardIoConnection conn = new SmartcardIoConnection();
            conn.removeCardConnectionListener(new CardConnectionListener() {
                @Override
                public void cardInserted(final CardConnectionEvent cce) {
                    /**/
                }

                @Override
                public void cardRemoved(final CardConnectionEvent cce) {
                    /**/
                }
            });
        }
        catch (final Exception e) {
            Assert.assertEquals(e.getClass().getName(), UnsupportedOperationException.class.getName());
        }
    }

    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#reset()}. */
    @Test
    public void testReset() {
        LOGGER.info("testReset():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        try {
            final byte[] atr = conn.reset();
            LOGGER.info(HexUtils.hexify(atr, true));
            conn.close();
        }
        catch (final ApduConnectionException e) {
            Assert.fail(e.getMessage());
        }
    }

    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#setExclusiveUse(boolean)} . */
    @Test
    public void testSetExclusiveUse() {
        LOGGER.info("testSetExclusiveUse():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        LOGGER.info("Connection opened? " + conn.isOpen()); //$NON-NLS-1$
        LOGGER.info("Stablishing exclusive use"); //$NON-NLS-1$
        conn.setExclusiveUse(true);
        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        try {
            Assert.assertTrue(conn.isOpen());
            conn.open();
        }
        catch (final Exception e) {
            if (!(e instanceof ApduConnectionException)) {
                Assert.fail(e.getMessage());
            }
        }
        try {
            conn.close();
            Assert.assertFalse(conn.isOpen());
        }
        catch (final ApduConnectionException e) {
            Assert.fail(e.getMessage());
        }
    }

    /** Test method for
     * {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#setProtocol(es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection.ConnectionProtocol)}
     * . */
    @Test
    public void testSetProtocol() {
        LOGGER.info("testSetProtocol():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        conn.setProtocol(null);
        conn.setProtocol(ConnectionProtocol.T0);
        Assert.assertEquals(conn.getProtocol().toString(), ConnectionProtocol.T0.toString());
        conn.setProtocol(ConnectionProtocol.T1);
        Assert.assertEquals(conn.getProtocol().toString(), ConnectionProtocol.T1.toString());
        conn.setProtocol(ConnectionProtocol.TCL);
        Assert.assertEquals(conn.getProtocol().toString(), ConnectionProtocol.TCL.toString());
    }

    /** Test method for {@link es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection#setTerminal(int)}. */
    @Test
    public void testSetTerminal() {
        LOGGER.info("testSetTerminal():"); //$NON-NLS-1$
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        long[] terminals = null;
        try {
            terminals = conn.getTerminals(false);
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
        }
        if (terminals != null) {
            if (terminals.length == 0) {
                LOGGER.warning("No se han detectado lectores activos en el equipo"); //$NON-NLS-1$
            }
            else if (terminals.length == 1) {
                LOGGER.warning("Solo se ha detectado un lector activo en el equipo"); //$NON-NLS-1$
            }
            else if (terminals.length > 1) {
                try {
                    conn.open();
                    for (int i = terminals.length - 1; i >= 0; i--) {
                        conn.setTerminal((int) terminals[i]);
                    }
                }
                catch (final ApduConnectionException e) {
                    LOGGER.warning(e.getMessage());
                }
                finally {
                    try {
                        conn.close();
                    }
                    catch (final ApduConnectionException e) {
                        LOGGER.warning(e.getMessage());
                    }
                }
            }
        }
        else {
            try {
                conn.open();
                conn.setTerminal(0);
                conn.setTerminal(-1);
                conn.close();
            }
            catch (final ApduConnectionException e) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que se obtenga una excepci&oacute;n de tipo {@link UnsupportedOperationException} */
    @Test(expected = UnsupportedOperationException.class)
    public void unsupportedOperationExceptionShouldBeThrownIfCardConnectionListenerIsAdded() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        conn.addCardConnectionListener(new CardConnectionListener() {
            @Override
            public void cardInserted(final CardConnectionEvent cce) {
                /* Metodo del interfaz vacio. No se necesita definir para la prueba. */
            }

            @Override
            public void cardRemoved(final CardConnectionEvent cce) {
                /* Metodo del interfaz vacio. No se necesita definir para la prueba. */
            }
        });
    }

    /** Comprueba que se produce un cambio de estado al abrir y cerrar la conexi&oacute;n con la tarjeta */
    @Test
    public void smartcardConnectionShouldBeOpenedAndCloseChangingTheInternalState() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
            Assert.assertTrue(conn.isOpen());

            if (conn.isOpen()) {
                conn.close();
                Assert.assertFalse(conn.isOpen());
            }
        }
        catch (final ApduConnectionException e) {
            /* No hacemos nada */
        }
    }

    /** Comprueba que los terminales proporcionan informaci&oacute;n */
    @Test
    public void informationShouldBeAvailableOnTerminals() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        for (final boolean isCardPresent : new boolean[] {
                true, false
        }) {
            long[] terminals = null;
            try {
                terminals = conn.getTerminals(isCardPresent);

                if (terminals != null) {
                    for (final long terminal : terminals) {
                        try {
                            final String info = conn.getTerminalInfo((int) terminal);
                            Assert.assertNotNull(info);
                        }
                        catch (final ApduConnectionException e) {
                            /* No hacemos nada */
                        }
                    }
                }
            }
            catch (final ApduConnectionException e) {
                /* No hacemos nada */
            }
        }
    }

    /** Comprueba que se lanza una excepci&oacute;n de tipo {@link ApduConnectionException} cuando se emplea una terminal incorrecta
     * @throws ApduConnectionException */
    @Test(expected = ApduConnectionException.class)
    public void apduConnectionExceptionShouldBeThrownWhenIncorrectTerminalIsRequested() throws ApduConnectionException {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        conn.getTerminalInfo(-1);
    }

    /** Abre una conexi&oacute;n y obtiene los datos de identificaci&oacute;n de una tarjeta inteligente */
    @Test
    public void testOpen() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
            final ResponseApdu response = conn.transmit(new GetChipInfoApduCommand());
            LOGGER.info(HexUtils.hexify(response.getBytes(), true));
            LOGGER.info(HexUtils.hexify(response.getData(), true));
            conn.close();
        }
        catch (final ApduConnectionException e) {
            /* No hacemos nada */
        }
    }

    /** Comprueba que se abre una conexi&oacute;n tras realizar un reset a la tarjeta */
    @Test
    public void connectionShouldBeOpenAfterResetCommand() {
        final SmartcardIoConnection connection = new SmartcardIoConnection();

        try {
            connection.open();
            final byte[] resetCommandResponse = connection.reset();

            Assert.assertNotNull(HexUtils.hexify(resetCommandResponse, true));
            Assert.assertTrue(connection.isOpen());

            connection.close();
        }
        catch (final ApduConnectionException e) {
            /* No hacemos nada */
        }
    }

    /** Comprueba que se peude abrir una conexi&oacute;n en modo exclusivo */
    @Test
    public void openConnectionShouldAllowExclusiveUse() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        conn.setExclusiveUse(true);

        try {
            conn.open();
            Assert.assertTrue(conn.isOpen());
            conn.close();
        }
        catch (final ApduConnectionException e) {
            /* No hacemos nada */
        }
    }

    /** Comprueba que no se puede abrir una conexi&oacute;n cuando ya existe una previamente abierta en modo exclusivo
     * @throws ApduConnectionException */
    @Test(expected = ApduConnectionException.class)
    public void openANewConnectionShouldNotBeAllowedWhenExclusiveUseIsActivated() throws ApduConnectionException {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        conn.setExclusiveUse(true);

        conn.open();
        Assert.assertTrue(conn.isOpen());

        if (conn.isOpen()) {
            conn.open();
        }
    }

    /** Comprueba que se establece T0 como protocolo por defecto cuando no se selecciona ninguno */
    @Test
    public void whenNoProtocolOrNullIsSpecifiedT0IsAssumed() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        Assert.assertEquals(ConnectionProtocol.T0, conn.getProtocol());

        conn.setProtocol(null);
        Assert.assertEquals(ConnectionProtocol.T0, conn.getProtocol());
    }

    /** Comprueba que se puede cambiar de terminal con una conexi&oacute;n abierta */
    @Test
    public void changingTerminalAndTransmitingShouldBePossibleInOpenConnections() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();

        long[] terminals = null;

        try {
            terminals = conn.getTerminals(false);
            conn.open();

            for (final long terminalId : terminals) {
                conn.setTerminal((int) terminalId);

                final ResponseApdu response = conn.transmit(new GetChipInfoApduCommand());
                Assert.assertNotNull(HexUtils.hexify(response.getData(), true));
            }
        }
        catch (final ApduConnectionException e) {
            /* No hacemos nada */
        }
    }

    /** Comprueba que una nueva conexi&oacute;n no es exclusiva si no se ha indicado previamente */
    @Test
    public void connectionShouldNotBeExclusiveByDefault() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();
        Assert.assertFalse(conn.isExclusiveUse());
    }

    /** Comprueba que no se puede enviar comandos nulos */
    public void nullApduCommandsShouldBeAvoidedAndNotTransmited() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
            conn.transmit(null);
        }
        catch (final Exception e) {
            if (conn.isOpen() && !(e instanceof IllegalArgumentException)) {
                Assert.assertTrue(false);
            }
        }
    }

    /** Comprueba el correcto funcionamiento de la transmisi&oacute;n de comandos */
    @Test
    public void testTransmit() {
        final SmartcardIoConnection conn = new SmartcardIoConnection();

        try {
            conn.open();
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.getMessage());
            return;
        }
        try {
            ResponseApdu response = conn.transmit(new GetChallengeApduCommand((byte) 0x00));
            LOGGER.info(HexUtils.hexify(response.getBytes(), true));
            LOGGER.info(HexUtils.hexify(response.getData(), true));
            conn.close();
            try {
                response = conn.transmit(new GetChallengeApduCommand((byte) 0x00));
            }
            catch (final Exception e) {
                if (!(e instanceof ApduConnectionException)) {
                    Assert.fail("Excepción no controlada: " + e.getMessage()); //$NON-NLS-1$
                }
            }
        }
        catch (final ApduConnectionException e) {
            /* No hacemos nada */
        }
    }
}