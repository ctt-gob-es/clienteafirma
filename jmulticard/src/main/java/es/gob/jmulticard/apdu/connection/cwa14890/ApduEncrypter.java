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
package es.gob.jmulticard.apdu.connection.cwa14890;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import es.gob.jmulticard.CryptoHelper;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.asn1.Tlv;
import es.gob.jmulticard.asn1.bertlv.BerTlv;

/** Gestor de canal CWA-14890.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Carlos Gamuci Mill&aacute;n */
final class ApduEncrypter {

    private ApduEncrypter() {
        /* Constructor privado. */
    }

    /** Primer byte a agregar en los padding ISO-7816. */
    private static final byte ISO7816_PADDING_PREFIX = (byte) 0x80;

    /** Byte prefijo de los datos para el c&aacute;lculo de la MAC. */
    private static final byte TLV_VALUE_PREFIX_TO_MAC = (byte) 0x01;

    /** Tag del TLV de datos de una APDU protegida. */
    private static final byte TAG_DATA_TLV = (byte) 0x87;

    /** Tag del TLV del Le de una APDU protegida. */
    private static final byte TAG_LE_TLV = (byte) 0x97;

    /** Tag del TLV de estado de respuesta de una APDU de respuesta. */
    private static final byte TAG_SW_TLV = (byte) 0x99;

    /** Tag del TLV de codigo de autenticacion de mensaje (MAC) de una APDU de respuesta. */
    private static final byte TAG_MAC_TLV = (byte) 0x8E;

    /** CLA que se suma a los CLA de las APDU que se protegen. */
    private static final byte CLA_OF_PROTECTED_APDU = (byte) 0x0C; // Indicate "Secure messaging" (0x08) and "Header is protected" (0x04)

    /** Encapsula una APDU para ser enviada por un canal seguro CWA-14890.
     * El contador SSC se autoincrementa durante la operaci&oacute;n.
     * @param unprotectedAPDU APDU desprotegida (en claro)
     * @param keyCipher Clave sim&eacute;trica de cifrado
     * @param keyMac Clave sim&eacute;trica para el MAC
     * @param sendSequenceCounter Contador de secuencia actual
     * @param cryptoHelper Operador criptogr&aacute;fico
     * @return APDU protegida (cifrada y con MAC)
     * @throws IOException Si ocurren problemas durante los cifrados de la APDU */
    static CipheredApdu protectAPDU(final CommandApdu unprotectedAPDU,
                                    final byte[] keyCipher,
                                    final byte[] keyMac,
                                    final byte[] sendSequenceCounter,
                                    final CryptoHelper cryptoHelper) throws IOException {

        byte cla = unprotectedAPDU.getCla();
        final byte ins = unprotectedAPDU.getIns();
        final byte p1 = unprotectedAPDU.getP1();
        final byte p2 = unprotectedAPDU.getP2();
        final byte[] data = unprotectedAPDU.getData();
        final Integer le = unprotectedAPDU.getLe();

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();

        // Si hay datos calculamos el TLV con estos datos cifrados
        byte[] tlvDataBytes = new byte[0];
        if (data != null && data.length > 0) {
            baos.write(TLV_VALUE_PREFIX_TO_MAC);
            byte[] paddedData = addPadding7816(data);
            baos.write(cryptoHelper.desedeEncrypt(paddedData, keyCipher));

            // Sobrescribimos los datos de la APDU inmediatamente despues de cifrarla, para que este
            // el minimo tiempo en memoria. Como los arrays son mutables con escribir esta copia se
            // sobreescriben todas las referencias.
            for (int i=0; i<paddedData.length; i++){
            	paddedData[i] = (byte) 0x00;
            }
            
            for (int i=0;i<data.length;i++) {
                data[i] = '\0';
            }

            tlvDataBytes = new Tlv(TAG_DATA_TLV, baos.toByteArray()).getBytes();
        }

        // Si hay campo Le calculamos el TLV con ellos
        byte[] tlvLeBytes = new byte[0];
        if (le != null) {
            tlvLeBytes = new Tlv(TAG_LE_TLV, new byte[] {
                le.byteValue()
            }).getBytes();
        }

        // Concatenamos los TLV de datos y Le para obtener el cuerpo de la nueva APDU
        final byte[] completeDataBytes = new byte[tlvDataBytes.length + tlvLeBytes.length];
        System.arraycopy(tlvDataBytes, 0, completeDataBytes, 0, tlvDataBytes.length);
        System.arraycopy(tlvLeBytes, 0, completeDataBytes, tlvDataBytes.length, tlvLeBytes.length);

        // Sumamos la CLA el valor indicativo de APDU cifrada
        cla = (byte) (cla | CLA_OF_PROTECTED_APDU);

        // Componemos los datos necesario para el calculo del MAC del mensaje
        baos.reset();
        baos.write(addPadding7816(new byte[] {
                cla, ins, p1, p2
        }));
        baos.write(completeDataBytes);
        final byte[] encryptedDataPadded = addPadding7816(baos.toByteArray());

        // Calculamos el valor MAC para la autenticacion de los datos
        final byte[] mac = generateMac(encryptedDataPadded, sendSequenceCounter, keyMac, cryptoHelper);

        return new CipheredApdu(cla, ins, p1, p2, completeDataBytes, mac);
    }

    /** Agrega un Padding a un array de bytes conforme las especificaciones de la ISO 7816.
     * Esto es, se agrega un byte 0x80 al array y se completa con bytes 0x00 hasta que el
     * array es m&uacute;ltiplo de 8.
     * @param data Datos a los que agregar el padding.
     * @return Datos con padding. */
    private static byte[] addPadding7816(final byte[] data) {
        final byte[] paddedData = new byte[((data.length / 8) + 1) * 8];
        System.arraycopy(data, 0, paddedData, 0, data.length);
        paddedData[data.length] = ISO7816_PADDING_PREFIX;
        // Machacamos los datos
        for (int i = data.length + 1; i < paddedData.length; i++) {
            paddedData[i] = '\0';
        }
        return paddedData;
    }

    /** Elimina el padding ISO 7816 de los datos.
     * @param paddedData Datos con padding.
     * @return Datos sin padding. */
    private static byte[] removePadding7816(final byte[] paddedData) {
        for (int i = paddedData.length - 1; i >= 0; i--) {
            if (paddedData[i] == ISO7816_PADDING_PREFIX) {
                if (i == 0) {
                    return new byte[0];
                }
                return HexUtils.subArray(paddedData, 0, i);
            }
            else if (paddedData[i] != (byte) 0x00) {
                // Consideramos que no tenia padding
                return paddedData;
            }
        }
        // Esto solo ocurriria si todo fuesen 0x00
        return paddedData;
    }

    /** Aplica el algoritmo para la generaci&oacute;n de la MAC del mensaje.
     * @param dataPadded Datos sobre los que generar la MAC.
     * @param ssc Contador de secuencia de la operaci&oacute;n.
     * @param kMac Clave triple DES necesaria para la operaci&oacute;n.
     * @param cryptoHelper Manejador para la realizaci&oacute;n de las operaciones criptogr&aacute;ficas.
     * @return Clave de autenticaci&oacute;n de los datos.
     * @throws IOException */
    private static byte[] generateMac(final byte[] dataPadded,
                                      final byte[] ssc,
                                      final byte[] kMac,
                                      final CryptoHelper cryptoHelper) throws IOException {

        final byte keyDesBytes[] = new byte[8];
        System.arraycopy(kMac, 0, keyDesBytes, 0, 8);

        byte tmpData[] = cryptoHelper.desEncrypt(ssc, keyDesBytes);

        int i = 0;
        while (i < (dataPadded.length - 8)) {
            tmpData = cryptoHelper.desEncrypt(HexUtils.xor(tmpData, HexUtils.subArray(dataPadded, i, 8)), keyDesBytes);
            i += 8;
        }

        final byte[] keyTdesBytes = new byte[24];
        System.arraycopy(kMac, 0, keyTdesBytes, 0, 16);
        System.arraycopy(kMac, 0, keyTdesBytes, 16, 8);

        final byte[] ret =
                HexUtils.subArray(cryptoHelper.desedeEncrypt(HexUtils.xor(tmpData, HexUtils.subArray(dataPadded, i, 8)), keyTdesBytes), 0, 4);

        return ret;
    }

    /** Desencripta los datos de una APDU de respuesta protegida.
     * @param responseApdu APDU de respuesta cifrada.
     * @param keyCipher Clave para el descifrado de la respuesta.
     * @param ssc C&oacute;digo de secuencia correspondiente a la respuesta.
     * @param kMac Clave para la verificaci&oacute;n de la respuesta.
     * @param cryptoHelper Manejador para el desencriptado.
     * @return APDU con la respuesta descifrada.
     * @throws IOException Cuando ocurre un error durante la desencriptaci&oacute;n de los datos. */
    static ResponseApdu decryptResponseApdu(final ResponseApdu responseApdu,
            								final byte[] keyCipher,
            								final byte[] ssc,
            								final byte[] kMac,
            								final CryptoHelper cryptoHelper) throws IOException {

        // Si el resultado es incorrecto, lo devolvemos para su evaluacion
        if (!responseApdu.isOk()) {
            return new ResponseApdu(responseApdu.getStatusWord().getBytes());
        }

        // Desciframos y validamos el resultado
        final ByteArrayInputStream recordOfTlvs = new ByteArrayInputStream(responseApdu.getData());
        BerTlv dataTlv = null;
        BerTlv swTlv = null;
        BerTlv macTlv = null;
        try {
            BerTlv tlv = BerTlv.getInstance(recordOfTlvs);
            if (tlv.getTag().getTagValue() == TAG_DATA_TLV) {
                dataTlv = tlv;
                tlv = BerTlv.getInstance(recordOfTlvs);
            }
            if (tlv.getTag().getTagValue() == TAG_SW_TLV) {
            	swTlv = tlv;
                tlv = BerTlv.getInstance(recordOfTlvs);
            }
            if (tlv.getTag().getTagValue() == TAG_MAC_TLV) {
                macTlv = tlv;
            }
        }
        catch (final NegativeArraySizeException e) {
            throw new ApduConnectionException("Error en el formato de la respuesta remitida por el canal seguro", e); //$NON-NLS-1$
        }

        if (macTlv == null) {
        	throw new SecureChannelException("No se ha encontrado el TLV del MAC en la APDU"); //$NON-NLS-1$
        }
        if (swTlv == null) {
        	throw new SecureChannelException("No se ha encontrado el TLV del StatusWord en la APDU cifrada"); //$NON-NLS-1$
        }

        // Pasamos el TLV completo de datos y el del StatusWord concatenados
        final int tlvsLenght = (dataTlv != null ? 1 + 1 + (dataTlv.getValue().length / 128) + dataTlv.getValue().length : 0) + // Tag (1 byte) + Lenght (1 byte + 1 por cada 128) + Value (Value.lenght bytes
        		(1 + 1 + swTlv.getValue().length); // Tag (1 byte) + Lenght (1 byte) + Value (Value.lenght bytes)
        verifyMac(HexUtils.subArray(responseApdu.getData(), 0, tlvsLenght), macTlv.getValue(), ssc, kMac, cryptoHelper);

        if (dataTlv == null) {
            return new ResponseApdu(swTlv.getValue());
        }

        // Desencriptamos y eliminamos el padding de los datos, teniendo en cuenta que el primer byte
        // de los datos es fijo (0x01) y no cuenta dentro de los datos
        final byte[] decryptedData =
        		removePadding7816(cryptoHelper.desedeDecrypt(HexUtils.subArray(dataTlv.getValue(), 1, dataTlv.getValue().length - 1), keyCipher));

        final byte[] responseApduBytes = new byte[decryptedData.length + swTlv.getValue().length];
        System.arraycopy(decryptedData, 0, responseApduBytes, 0, decryptedData.length);
        System.arraycopy(swTlv.getValue(), 0, responseApduBytes, decryptedData.length, swTlv.getValue().length);

        return new ResponseApdu(responseApduBytes);
    }

    /** Comprueba que un codigo de verificacion sea correcto con respecto a unos datos y el
     * c&oacute;digo de respuesta de una petici&oacute;n.
     * @param verificableData Datos.
     * @param macTlvBytes C&oacute;digo de verificaci&oacute;n.
     * @param ssc C&oacute;digo de secuencia.
     * @param kMac Clave para la generaci&oacute;n del MAC.
     * @param cryptoHelper Manejador de operaciones criptogr&aacute;ficas.
     */
    private static void verifyMac(final byte[] verificableData, final byte[] macTlvBytes, final byte[] ssc, final byte[] kMac, final CryptoHelper cryptoHelper) {

    	final byte[] calculatedMac;
    	try {
    		calculatedMac = generateMac(addPadding7816(verificableData), ssc, kMac, cryptoHelper);
    	}
    	catch (final IOException e) {
    		throw new SecurityException("No se pudo calcular el MAC teorico de la respuesta del DNIe para su verificacion"); //$NON-NLS-1$
		}

    	// Comparamos que el MAC recibido sea igual que el MAC que debimos recibir
        if (!HexUtils.arrayEquals(macTlvBytes, calculatedMac)) {
            throw new InvalidCryptographicChecksum();
        }
	}
}