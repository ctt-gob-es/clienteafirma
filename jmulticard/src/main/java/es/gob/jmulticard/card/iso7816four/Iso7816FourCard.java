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
package es.gob.jmulticard.card.iso7816four;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.security.auth.callback.PasswordCallback;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.StatusWord;
import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.apdu.connection.cwa14890.SecureChannelException;
import es.gob.jmulticard.apdu.iso7816four.GetChallengeApduCommand;
import es.gob.jmulticard.apdu.iso7816four.MseSetVerificationKeyApduCommand;
import es.gob.jmulticard.apdu.iso7816four.ReadBinaryApduCommand;
import es.gob.jmulticard.apdu.iso7816four.SelectDfByNameApduCommand;
import es.gob.jmulticard.apdu.iso7816four.SelectFileApduResponse;
import es.gob.jmulticard.apdu.iso7816four.SelectFileByIdApduCommand;
import es.gob.jmulticard.apdu.iso7816four.VerifyApduCommand;
import es.gob.jmulticard.card.AuthenticationModeLockedException;
import es.gob.jmulticard.card.Location;
import es.gob.jmulticard.card.SmartCard;
import es.gob.jmulticard.ui.passwordcallback.gui.CommonPasswordCallback;

/** Tarjeta compatible ISO-7816-4.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Alberto Mart&iacute;nez */
public abstract class Iso7816FourCard extends SmartCard {

    /** Byte que identifica una verificaci&oacute;n fallida del PIN */
    private final static byte ERROR_PIN_SW1 = (byte) 0x63;

    /** Construye una tarjeta compatible ISO 7816-4.
     * @param c Octeto de clase (CLA) de las APDU
     * @param conn Connexi&oacute;n con la tarjeta
     * @throws ApduConnectionException
     *         Si la conexi&oacute;n con la tarjeta se proporciona cerrada y
     *         no se posible abrirla */
    public Iso7816FourCard(final byte c, final ApduConnection conn) throws ApduConnectionException {
        super(c, conn);
    }

    /** Lee un contenido binario del fichero actualmente seleccionado.
     * @param msbOffset
     *        Octeto m&aacute;s significativo del desplazamiento
     *        (<i>offset</i>) hasta el punto de inicio de la lectura desde
     *        el comienzo del fichero
     * @param lsbOffset
     *        Octeto menos significativo del desplazamiento (<i>offset</i>)
     *        hasta el punto de inicio de la lectura desde el comienzo del
     *        fichero
     * @param readLength Longitud de los datos a leer (en octetos)
     * @return APDU de respuesta
     * @throws ApduConnectionException Si hay problemas en el env&iacute;o de la APDU */
    private ResponseApdu readBinary(final byte msbOffset, final byte lsbOffset, final byte readLength) throws ApduConnectionException {
    	final ResponseApdu res = this.getConnection().transmit(new ReadBinaryApduCommand(this.getCla(), msbOffset, lsbOffset, readLength));
        if (res.isOk()) {
        	return res;
        }
        throw new ApduConnectionException("Respuesta invalida en la lectura de binario con el codigo: " + res.getStatusWord()); //$NON-NLS-1$
    }

    /** Lee por completo el contenido binario del fichero actualmente seleccionado.
     * @param len Longitud del fichero a leer
     * @return APDU de respuesta
     * @throws ApduConnectionException Si hay problemas en el env&iacute;o de la APDU
     * @throws IOException Si hay problemas en el buffer de lectura */
    public byte[] readBinaryComplete(final int len) throws IOException {

        int off = 0;
        ResponseApdu readResponse;
        final ByteArrayOutputStream out = new ByteArrayOutputStream();

        // Leemos en iteraciones de 239 bytes (0xEF) lo maximo que permite el DNIe
        // una vez abierto el canal seguro
        while (off < len) {
            final byte msbOffset = (byte) (off >> 8);
            final byte lsbOffset = (byte) (off & 0x0ff);
            final int left = len - off;
            if (left < 0x0ef) { // Si es menor que el maximo que podemos leer por iteracion
                readResponse = this.readBinary(msbOffset, lsbOffset, (byte) left);
            }
            else {
                readResponse = this.readBinary(msbOffset, lsbOffset, (byte) 0x0ef);
            }

            if (!readResponse.isOk()) {
                return readResponse.getStatusWord().getBytes();
            }

            out.write(readResponse.getData());

            off += 0x0ef;
        }

        return out.toByteArray();
    }

	/** Selecciona un fichero por nombre.
	 * @param name Nombre del fichero
	 * @throws FileNotFoundException Si el ficheo no existe
     * @throws ApduConnectionException Si ocurre alg&uacute;n problema durante la selecci&oacute;n */
    public void selectFileByName(final String name) throws ApduConnectionException, FileNotFoundException {
    	final ResponseApdu response = this.getConnection().transmit(new SelectDfByNameApduCommand(this.getCla(), name.getBytes()));
    	if (response.isOk()) {
    		return;
    	}
    	if (HexUtils.arrayEquals(response.getBytes(), new byte[] { (byte) 0x6a, (byte) 0x82})) {
    		throw new FileNotFoundException(name);
    	}
    }

    /** Selecciona un fichero (DF o EF).
     * @param id Identificador del fichero a seleccionar
     * @return Tama&ntilde;o del fichero seleccionado
     * @throws ApduConnectionException Si hay problemas en el env&iacute;o de la APDU
     * @throws Iso7816FourCardException Si falla la selecci&oacute;n de fichero */
    public int selectFileById(final byte[] id) throws ApduConnectionException, Iso7816FourCardException {
		final ResponseApdu res = this.getConnection().transmit(new SelectFileByIdApduCommand(this.getCla(), id));
    	if (HexUtils.arrayEquals(res.getBytes(), new byte[] { (byte) 0x6a, (byte) 0x82 })) {
    		throw new FileNotFoundException(id);
    	}
        final SelectFileApduResponse response = new SelectFileApduResponse(res);
        if (response.isOk()) {
            return response.getFileLength();
        }
        final StatusWord sw = response.getStatusWord();
        if (sw.equals(new StatusWord((byte) 0x6A, (byte) 0x82))) {
            throw new FileNotFoundException(id);
        }
        throw new Iso7816FourCardException(sw);
    }

    /** Selecciona un fichero y lo lee por completo.
     * @param id Identificador del fichero a leer
     * @return Contenido del fichero apuntado por la direccion id
     * @throws ApduConnectionException Si hay problemas en el env&iacute;o de la APDU
     * @throws Iso7816FourCardException Si falla la selecci&oacute;n de fichero
     * @throws IOException Si hay problemas en el buffer de lectura */
    public byte[] selectFileByIdAndRead(final byte[] id) throws Iso7816FourCardException, IOException {
        final int fileLength = selectFileById(id);
        return readBinaryComplete(fileLength);
    }

    /** Selecciona un fichero (DF o EF).
     * @param location La ruta absoluta donde se encuentra el fichero a leer
     * @return Tama&ntilde;o del fichero seleccionado
     * @throws ApduConnectionException Si hay problemas en el env&iacute;o de la APDU
     * @throws Iso7816FourCardException Si falla la selecci&oacute;n de fichero */
    public int selectFileByLocation(final Location location) throws ApduConnectionException, Iso7816FourCardException {
        int fileLength = 0;
        Location loc = location;
        selectMasterFile();
        while (loc != null) {
            final byte[] id = loc.getFile();
            fileLength = this.selectFileById(id);
            loc = loc.getChild();
        }
        return fileLength;
    }

    /** Selecciona un fichero y lo lee por completo.
     * @param location Ruta absoluta del fichero a leer
     * @return Contenido del fichero apuntado por la ruta location
     * @throws ApduConnectionException Si hay problemas en el env&iacute;o de la APDU
     * @throws Iso7816FourCardException Si falla la selecci&oacute;n de fichero
     * @throws IOException Si hay problemas en el buffer de lectura */
    public byte[] selectFileByLocationAndRead(final Location location) throws IOException, Iso7816FourCardException {
        final int fileLenght = this.selectFileByLocation(location);
        return readBinaryComplete(fileLenght);
    }

    /** Selecciona el fichero maestro.
     * @throws ApduConnectionException Si hay problemas en el env&iacute;o de la APDU
     * @throws FileNotFoundException Si no se encuentra el MF */
    protected abstract void selectMasterFile() throws ApduConnectionException, FileNotFoundException;

    /** Establece una clave p&uacute;blica para la la verificaci&oacute;n posterior de
     * un certificado emitido por otro al que pertenece esta clave.
     * @param refPublicKey Referencia a la clave p&uacute;blica para su carga.
     * @throws SecureChannelException Cuando ocurre un error durante la selecci&oacute;n de la clave.
     * @throws ApduConnectionException Cuando ocurre un error en la comunicaci&oacute;n con la tarjeta. */
    public void setPublicKeyToVerification(final byte[] refPublicKey) throws SecureChannelException, ApduConnectionException {
        final CommandApdu apdu = new MseSetVerificationKeyApduCommand((byte) 0x00, refPublicKey);
        final ResponseApdu res = this.getConnection().transmit(apdu);
        if (!res.isOk()) {
            throw new SecureChannelException(
        		"Error al seleccionar una clave publica para verificacion. Se obtuvo el error: " + //$NON-NLS-1$
                HexUtils.hexify(res.getBytes(), true)
            );
        }
    }

    /** Lanza un desafio a la tarjeta para obtener un array de 8 bytes aleatorios.
     * @return Array de 8 bytes aleatorios.
     * @throws ApduConnectionException Cuando ocurre un error en la comunicaci&oacute;n con la tarjeta. */
    public byte[] getChallenge() throws ApduConnectionException {
        final ResponseApdu res = this.getConnection().transmit(new GetChallengeApduCommand((byte) 0x00));
        if (res.isOk()) {
        	return res.getData();
        }
        throw new ApduConnectionException("Respuesta invalida en la obtencion de desafio con el codigo: " + res.getStatusWord()); //$NON-NLS-1$
    }

    /** Verifica el PIN de la tarjeta. El m&eacute;todo reintenta hasta que se introduce el PIN correctamente,
     * se bloquea la tarjeta por exceso de intentos de introducci&oacute;n de PIN o se recibe una excepci&oacute;n
     * (derivada de <code>RuntimeException</code> o una <code>ApduConnectionException</code>.
     * @param pinPc PIN de la tarjeta
     * @throws ApduConnectionException Cuando ocurre un error en la comunicaci&oacute;n con la tarjeta.
     * @throws es.gob.jmulticard.card.AuthenticationModeLockedException Si est&aacute; bloqueada la verificaci&oacute;n de PIN (por ejemplo, por superar
     *                                  el n&uacute;mero m&aacute;ximo de intentos) */
    public void verifyPin(final PasswordCallback pinPc) throws ApduConnectionException {
    	verifyPin(pinPc, Integer.MAX_VALUE);
    }

    /** Verifica el PIN de la tarjeta. El m&eacute;todo reintenta hasta que se introduce el PIN correctamente,
     * se bloquea la tarjeta por exceso de intentos de introducci&oacute;n de PIN o se recibe una excepci&oacute;n
     * (derivada de <code>RuntimeException</code> o una <code>ApduConnectionException</code>.
     * @param pinPc PIN de la tarjeta
     * @param retriesLeft Intentos restantes que quedan antes de bloquear la tarjeta. Un valor de Integer.MAX_VALUE
     *                    indica un valor desconocido
     * @throws ApduConnectionException Cuando ocurre un error en la comunicaci&oacute;n con la tarjeta.
     * @throws es.gob.jmulticard.card.AuthenticationModeLockedException Cuando el DNI tiene el PIN bloqueado. */
    private void verifyPin(final PasswordCallback pinPc, final int retriesLeft) throws ApduConnectionException {

    	final PasswordCallback psc = (pinPc != null) ? pinPc : ((retriesLeft < Integer.MAX_VALUE) ?
    			CommonPasswordCallback.getDnieBadPinPasswordCallback(retriesLeft) :
    				CommonPasswordCallback.getDniePinForCertificateReadingPasswordCallback(null));

    	VerifyApduCommand verifyCommandApdu = new VerifyApduCommand((byte) 0x00, psc);

    	final ResponseApdu verifyResponse = this.getConnection().transmit(
    			verifyCommandApdu
    	);
    	verifyCommandApdu = null;

        // Comprobamos si ocurrio algun error durante la verificacion del PIN para volverlo
        // a pedir si es necesario
    	psc.clearPassword();
        if (!verifyResponse.isOk()) {
            if (verifyResponse.getStatusWord().getMsb() == ERROR_PIN_SW1) {
            	verifyPin(
            			pinPc,
            			verifyResponse.getStatusWord().getLsb() - (byte) 0xC0
            	);
            }
            else if (verifyResponse.getStatusWord().getMsb() == (byte)0x69 && verifyResponse.getStatusWord().getLsb() == (byte)0x83) {
            	throw new AuthenticationModeLockedException();
            }
        }
    }
}