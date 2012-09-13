/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros 
 * para la realizacion de procesos de autenticacion, firma electronica y validacion 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion 
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha 
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos 
 * e Impulso de la Administracion Electronica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * European Union Public License publicada por la Comision Europea, 
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 * 
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 * 
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.jmulticard.jse.smartcardio;

import java.util.List;
import java.util.logging.Logger;

import javax.smartcardio.Card;
import javax.smartcardio.CardChannel;
import javax.smartcardio.CardException;
import javax.smartcardio.CardTerminal;
import javax.smartcardio.CommandAPDU;
import javax.smartcardio.TerminalFactory;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.apdu.connection.ApduConnectionOpenedInExclusiveModeException;
import es.gob.jmulticard.apdu.connection.CardConnectionListener;
import es.gob.jmulticard.apdu.connection.CardNotPresentException;
import es.gob.jmulticard.apdu.connection.LostChannelException;
import es.gob.jmulticard.apdu.connection.NoReadersFoundException;
import es.gob.jmulticard.apdu.iso7816four.GetResponseApduCommand;

/** Conexi&oacute;n con lector de tarjetas inteligentes implementado sobre
 * JSR-268 SmartCard I/O.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SmartcardIoConnection implements ApduConnection {

    /** Constante para la indicaci&oacute;n de que se ha detectado un reinicio del canal
     * con la tarjeta. */
    private final static String SCARD_W_RESET_CARD = "SCARD_W_RESET_CARD"; //$NON-NLS-1$

    /** Protocolo de conexi&oacute;n con la tarjeta. */
    public enum ConnectionProtocol {
        /** T=0. */
        T0,
        /** T=1. */
        T1,
        /** T=CL. */
        TCL;

        @Override
        public String toString() {
            switch (this) {
                case T0:
                    return "T=0"; //$NON-NLS-1$
                case T1:
                    return "T=1"; //$NON-NLS-1$
                case TCL:
                    return "T=CL"; //$NON-NLS-1$
                default:
                    return ""; //$NON-NLS-1$
            }
        }

    }

    private static final Logger LOGGER = Logger.getLogger("es.gob.jmulticard"); //$NON-NLS-1$

    private int terminalNumber = 0;

    private CardChannel canal = null;

    private Card card = null;

    private boolean exclusive = false;

    private ConnectionProtocol protocol = ConnectionProtocol.T0;

    /** JSR-268 no soporta eventos de inserci&oacute;n o extracci&oacute;n. */
    @Override
    public void addCardConnectionListener(final CardConnectionListener ccl) {
        throw new UnsupportedOperationException("JSR-268 no soporta eventos de insercion o extraccion"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    public void close() throws ApduConnectionException {
    	if (this.card != null) {
	        try {
	            this.card.disconnect(false);
	        }
	        catch (final Exception e) {
	            throw new ApduConnectionException(
	                "Error intentando cerrar el objeto de tarjeta inteligente, la conexion puede quedar abierta pero inutil", e //$NON-NLS-1$
	            );
	        }
	        this.card = null;
    	}
        this.canal = null;
    }

    /** {@inheritDoc} */
    @Override
    public String getTerminalInfo(final int terminal) throws ApduConnectionException {
        try {
            final List<CardTerminal> terminales = TerminalFactory.getDefault().terminals().list();
            if (terminal < terminales.size()) {
                final CardTerminal cardTerminal = terminales.get(terminal);
                if (cardTerminal != null) {
                    return cardTerminal.getName();
                }
            }

            return null;
        }
        catch (final Exception ex) {
            throw new ApduConnectionException("Error recuperando la lista de lectores de tarjetas del sistema", ex); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Override
    public long[] getTerminals(final boolean onlyWithCardPresent) throws ApduConnectionException {
    	final List<CardTerminal> terminales;
    	try {
    		terminales = TerminalFactory.getDefault().terminals().list();
    	}
    	catch(final CardException e) {
    		LOGGER.warning("No se ha podido recuperar la lista de lectores del sistema: " + e); //$NON-NLS-1$
    		return new long[0];
    	}
        try {
            

            if (terminales.size() > 0) {
                // Calculamos el numero de terminales que hay que devolver
                int numTerminales = 0;
                if (onlyWithCardPresent) {
                    for (final CardTerminal terminal : terminales) {
                        if (terminal.isCardPresent()) {
                            numTerminales++;
                        }
                    }
                }
                else {
                    numTerminales = terminales.size();
                }

                final long[] idsTerminales = new long[numTerminales];

                // Creamos una lista con los identificadores de lectores de
                // tarjetas. Los identificadores
                // son los indices dentro de la lista de terminales
                int offset = 0;
                for (int i = 0; i < terminales.size(); i++) {
                    final CardTerminal terminal = terminales.get(i);
                    if (onlyWithCardPresent) {
                        if (terminal.isCardPresent()) {
                            idsTerminales[offset] = i;
                        }
                        else {
                            continue;
                        }
                    }
                    else {
                        idsTerminales[offset] = i;
                    }

                    offset++;
                }

                return idsTerminales;
            }
            return new long[0];
        }
        catch (final Exception ex) {
            throw new ApduConnectionException("Error recuperando la lista de lectores de tarjetas del sistema", ex); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean isOpen() {
        return (this.card != null);
    }

    /** {@inheritDoc} */
    @Override
    public void open() throws ApduConnectionException {

        // Desactivamos las respuestas automaticas para evitar los problemas con el canal seguro
        System.setProperty("sun.security.smartcardio.t0GetResponse", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        System.setProperty("sun.security.smartcardio.t1GetResponse", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        if (isExclusiveUse() && isOpen()) {
            throw new ApduConnectionOpenedInExclusiveModeException();
        }

        try {
            final List<CardTerminal> terminales = TerminalFactory.getDefault().terminals().list();
            if (terminales.size() < 1) {
                throw new NoReadersFoundException();
            }
            if (terminales.size() <= this.terminalNumber) {
                throw new ApduConnectionException("No se detecto el lector de tarjetas numero " + Integer.toString(this.terminalNumber)); //$NON-NLS-1$
            }
            this.card = terminales.get(this.terminalNumber).connect(this.protocol.toString());
        }
        catch(final javax.smartcardio.CardNotPresentException e) {
            throw new CardNotPresentException(e);
        }
        catch (final CardException e) {
            throw new ApduConnectionException(
                "No se ha podido abrir la conexion con el lector de tarjetas numero " + Integer.toString(this.terminalNumber), e);  //$NON-NLS-1$
        }

        if (this.exclusive) {
            try {
                this.card.beginExclusive();
            }
            catch (final CardException e) {
                throw new ApduConnectionException(
                    "No se ha podido abrir la conexion exclusiva con el lector de tarjetas numero " + Integer.toString(this.terminalNumber), e //$NON-NLS-1$
                );
            }
        }
        this.canal = this.card.getBasicChannel();
    }

    /** JSR-268 no soporta eventos de inserci&oacute;n o extracci&oacute;n. */
    @Override
    public void removeCardConnectionListener(final CardConnectionListener ccl) {
        throw new UnsupportedOperationException("JSR-268 no soporta eventos de insercion o extraccion"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    public byte[] reset() throws ApduConnectionException {
        close();
        open();
        if (this.card != null) {
            return this.card.getATR().getBytes();
        }
        throw new ApduConnectionException("Error indefinido reiniciando la conexion con la tarjeta"); //$NON-NLS-1$
    }

    /** Establece si la conexi&oacute;n se debe abrir en modo exclusivo. Solo
     * puede establecerse si la conexi&oacute;n aun no ha sido abierta
     * @param ex
     *        <code>true</code> para abrir la conexi&oacute;n en modo
     *        exclusivo, <code>false</code> para abrirla en modo no
     *        exclusivo */
    public void setExclusiveUse(final boolean ex) {
        if (this.card == null) {
            this.exclusive = ex;
        }
        else {
            SmartcardIoConnection.LOGGER.warning(
                "No se puede cambiar el modo de acceso a la tarjeta con la conexion abierta, se mantendra el modo EXCLUSIVE=" + Boolean.toString(this.exclusive) //$NON-NLS-1$
            );
        }
    }

    /** establece el Protocolo de conexi&oacute;n con la tarjeta.
     * @param p
     *        Protocolo de conexi&oacute;n con la tarjeta */
    public void setProtocol(final ConnectionProtocol p) {
        if (p == null) {
            SmartcardIoConnection.LOGGER.warning(
                "El protocolo de conexion no puede ser nulo, se usara T=0" //$NON-NLS-1$
            );
            this.protocol = ConnectionProtocol.T0;
            return;
        }
        this.protocol = p;
    }

    /** {@inheritDoc} */
    @Override
    public void setTerminal(final int terminalN) {
        if (this.terminalNumber == terminalN) {
            return;
        }

        final boolean wasOpened = isOpen();

        if (wasOpened) {
            try {
                close();
            }
            catch (final Exception e) {
                SmartcardIoConnection.LOGGER.warning(
                    "Error intentando cerrar la conexion con el lector: " + e); //$NON-NLS-1$
            }
            this.terminalNumber = terminalN;
            if (wasOpened) {
                try {
                    open();
                }
                catch (final Exception e) {
                    SmartcardIoConnection.LOGGER.warning("Error intentando abrir la conexion con el lector: " + e); //$NON-NLS-1$
                }
            }
        }
    }

    /** Tag que identifica que es necesario recuperar el resultado del comando anterior. */
    private static final byte TAG_RESPONSE_PENDING = 0x61;

    private static final byte TAG_RESPONSE_INVALID_LENGTH = 0x6C;

    /** {@inheritDoc} */
    @Override
    public ResponseApdu transmit(final CommandApdu command) throws ApduConnectionException {
        if (this.canal == null) {
            throw new ApduConnectionException(
                "No se puede transmitir sobre una conexion cerrada" //$NON-NLS-1$
            );
        }
        if (command == null) {
            throw new IllegalArgumentException("No se puede transmitir una APDU nula" //$NON-NLS-1$
            );
        }
        try {
            final ResponseApdu response = new ResponseApdu(this.canal.transmit(new CommandAPDU(command.getBytes())).getBytes());

            // Solicitamos el resultado de la operacion si es necesario
            if (response.getStatusWord().getMsb() == TAG_RESPONSE_PENDING) {
                // Si ya se ha devuelto parte de los datos, los concatenamos al resultado
                if (response.getData().length > 0) {
                    final byte[] data = response.getData();
                    final byte[] additionalData = transmit(
                            new GetResponseApduCommand((byte) 0x00, response.getStatusWord().getLsb())).getBytes();

                    final byte[] fullResponse = new byte[data.length + additionalData.length];
                    System.arraycopy(data, 0, fullResponse, 0, data.length);
                    System.arraycopy(additionalData, 0, fullResponse, data.length, additionalData.length);

                    return new ResponseApdu(fullResponse);
                }
                return transmit(new GetResponseApduCommand((byte) 0x00, response.getStatusWord().getLsb()));
            }
            // En caso de longitud esperada incorrecta reenviamos la APDU con la longitud esperada.
            // Incluimos la condicion del CLA igual 0x00 para que no afecte a las APDUs cifradas
            // (de eso se encargara la clase de conexion con canal seguro)
            else if (response.getStatusWord().getMsb() == TAG_RESPONSE_INVALID_LENGTH && command.getCla() == (byte) 0x00) {
                command.setLe(response.getStatusWord().getLsb());
                return transmit(command);
            }

            return response;
        }
        catch (final CardException e) {
            Throwable t = e.getCause();
            if ((t != null) && (SCARD_W_RESET_CARD.equals(t.getMessage()))) {
                throw new LostChannelException(t.getMessage());
            }
            throw new ApduConnectionException(
                    "Error de comunicacion con la tarjeta tratando de transmitir la APDU " + //$NON-NLS-1$
                    HexUtils.hexify(command.getBytes(), true) +
                    " al lector " + Integer.toString(this.terminalNumber) + //$NON-NLS-1$
                    " en modo EXCLUSIVE=" + //$NON-NLS-1$
                    Boolean.toString(this.exclusive) +
                    " con el protocolo " + this.protocol.toString(), e //$NON-NLS-1$
            );
        }
        catch (final Exception e) {
            throw new ApduConnectionException(
                    "Error tratando de transmitir la APDU " + HexUtils.hexify(command.getBytes(), true) + //$NON-NLS-1$
                    " al lector " + Integer.toString(this.terminalNumber) + //$NON-NLS-1$
                    " en modo EXCLUSIVE=" + //$NON-NLS-1$
                    Boolean.toString(this.exclusive) +
                    " con el protocolo " + this.protocol.toString(), e //$NON-NLS-1$
            );
        }
    }

    /** Devuelve el protocolo de conexi&oacute;n con la tarjeta usado actualmente
     * @return Un objeto de tipo enumerado <code>ConnectionProtocol</code> */
    public ConnectionProtocol getProtocol() {
        return this.protocol;
    }

    /** Indica si la conexi&oacute;n con la tarjeta se ha establecido en modo exclusivo o no
     * @return <code>true</code> si la conexi&oacute;n est&aacute; establecida en modo exclusivo */
    public boolean isExclusiveUse() {
        return this.exclusive;
    }
}