/*
 * Controlador Java de la Secretaría de Estado de Administraciones Públicas
 * para el DNI electrónico.
 *
 * El Controlador Java para el DNI electrónico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electrónico en aplicaciones Java de terceros 
 * para la realización de procesos de autenticación, firma electrónica y validación 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electrónico, así como la realización 
 * de operaciones criptográficas de firma con el DNI electrónico. El Controlador ha 
 * sido diseñado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Dirección General de Modernización Administrativa, Procedimientos 
 * e Impulso de la Administración Electrónica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podrán elegir bajo cual de las
 * licencias desean utilizar el código fuente. Su elección deberá reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinará
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la versión 2.1 de la Licencia, o en una versión posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * European Union Public License publicada por la Comisión Europea, 
 * tanto en la versión 1.1 de la Licencia, o en una versión posterior.
 * 
 * Debería recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://www.gnu.org/licenses/>.
 * 
 * Debería recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea útil, pero
 * SIN NINGUNA GARANTÍA; incluso sin la garantía implícita de comercialización
 * o idoneidad para un propósito particular.
 */
package es.gob.jmulticard.apdu;

/** APDU respuesta para comunicaci&oacute;n con tarjeta inteligente.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class ResponseApdu extends Apdu {

    /** Construye una APDU de respuesta a partir de su representaci&oacute;n
     * binaria directa.
     * @param fullBytes Representaci&oacute;n binaria directa de la APDU */
    public ResponseApdu(final byte[] fullBytes) {
        super();
        this.setBytes(fullBytes);
    }

    /** Obtiene el campo de datos de la APDU.
     * @return Campo de datos de la APDU */
    public byte[] getData() {
        final byte[] dat = new byte[getBytes().length-2];
        System.arraycopy(getBytes(), 0, dat, 0, getBytes().length-2);
        return dat;
    }

    /** Obtiene la palabra de estado (<i>Status Word</i>) de la APDU.
     * @return Palabra de estado (<i>Status Word</i>) de la APDU */
    public StatusWord getStatusWord() {
        return new StatusWord(getBytes()[getBytes().length - 2], getBytes()[getBytes().length - 1]);
    }

    /** Indica si la APDU es una respuesta correcta o no a un comando.
     * @return <code>true</code> si el comando termin&oacute; con &eacute;xito
     *         (termina en 9000), <code>false</code> en caso contrario */
    public boolean isOk() {
        if (getBytes() == null || getBytes().length < 2) {
            return false;
        }
        return (getBytes()[getBytes().length - 1] == (byte) 0x00) && (getBytes()[getBytes().length - 2] == (byte) 0x90);
    }

}