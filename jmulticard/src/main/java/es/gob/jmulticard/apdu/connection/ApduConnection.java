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
package es.gob.jmulticard.apdu.connection;

import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.apdu.ResponseApdu;

/** Define los requerimientos de clases que representen una conexi&oacute;n con una tarjeta inteligente insertada en un lector.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s Capote, Gonzalo Henr&iacute;quez Manzano
 * @version 1.0 */
public interface ApduConnection {

    /** Abre la conexi&oacute;n con la tarjeta inteligente actualmente insertada en el lector.
     * @throws ApduConnectionException Cuando ocurre cualquier problema abriendo la conexi&oacute;n
     * @see #close() */
    void open() throws ApduConnectionException;

    /** Cierra la conexi&oacute;n con la tarjeta inteligente actualmente insertada en el lector.
     * @throws ApduConnectionException Cuando ocurre cualquier problema cerrando la conexi&oacute;n
     * @see #open() */
    void close() throws ApduConnectionException;

    /** Env&iacute;a un comando APDU a la tarjeta inteligente.
     * @param command APDU que se desea enviar a la tarjeta
     * @return APDU de respuesta de la tarjeta al env&iacute;o
     * @throws ApduConnectionException Cuando ocurre cualquier problema con la conexi&oacute;n transmitiendo la APDU */
    ResponseApdu transmit(CommandApdu command) throws ApduConnectionException;

    /** Reinicializa la conexi&oacute;n con la tarjeta inteligente.
     * @return Respuesta al reset (ATR) de la tarjeta
     * @throws ApduConnectionException Cuando ocurre cualquier problema reinicializando la conexi&oacute;n */
    byte[] reset() throws ApduConnectionException;

    /** A&ntilde;ade un objeto al que se notificar&aacute; cuando existan eventos en la conexi&oacute;n.
     * Solo se notificar&aacute;n las inserciones y las extracciones cuando la implementaci&oacute;n subyacente lo soporte.
     * @param ccl Objeto al que se desea notificar los eventos de la conexi&oacute;n */
    void addCardConnectionListener(CardConnectionListener ccl);

    /** Indica que ya no se desea notificar a un objeto cuando existan eventos en la conexi&oacute;n.
     * @param ccl Objeto al que ya no se desea notificar los eventos de la conexi&oacute;n */
    void removeCardConnectionListener(CardConnectionListener ccl);

    /** Devuelve todos los lectores de tarjetas presentes en el sistema
     * @param onlyWithCardPresent Para indicar que s&oacute;lo devuelva lectores que tengan una tarjeta insertada
     * @return Una lista con los identificadores de lectores de tarjetas conectados
     * @throws ApduConnectionException Cuando ocurran problemas en la conexi&oacute;n con los lectores */
    long[] getTerminals(boolean onlyWithCardPresent) throws ApduConnectionException;

    /** Devuelve informaci&oacute;n sobre un terminal
     * @param terminal N&uacute;mero de terminal que se desea obtener informaci&oacute;n
     * @return Una descripci&oacute;n del terminal especificado
     * @throws ApduConnectionException Cuando ocurran problemas en la conexi&oacute;n con los lectores */
    String getTerminalInfo(int terminal) throws ApduConnectionException;

    /** Establece el lector de tarjetas que se usar&aacute; para la conexi&oacute;n. Si se cambia el terminal
     * estando la conexi&oacute;n ya abierta, se intentar&aacute; reabrirla con el nuevo terminal.
     * @param t N&uacute;mero de terminal que se desea pase a ser el actual
     * @throws NullPointerException Cuando se especifica un n&uacute;mero de terminal no v&aacute;lido */
    void setTerminal(int t);

    /** Indica si la conexi&oacute;n est&aacute; abierta o no.
     * @return <code>true</code> si la conexi&oacute;n esta abierta, <code>false</code> si est&aacute; cerrada */
    boolean isOpen();

}
