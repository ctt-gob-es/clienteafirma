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
package es.gob.jmulticard.apdu.iso7816four;

import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.asn1.Tlv;

/** APDU ISO 7816-4 de gesti&oacute;n de entorno de seguridad orientada a
 * establecer una clave p&uacute;blica para verificaci&oacute;n.
 * @author Carlos Gamuci Mill&aacute;n */
public final class MseSetVerificationKeyApduCommand extends CommandApdu {

    /** Byte de instrucci&oacute;n de la APDU. */
    private static final byte INS_MANAGE_ENVIROMENT = (byte) 0x22;

    /** Establece el fichero identificado para verificaci&oacute;n. */
    private static final byte SET_FOR_VERIFICATION = (byte) 0x81;

    /** Control Reference Template for Digital Signature (DST) */
    private static final byte DST = (byte) 0xb6;

    /** Tag para identificar un identificador de fichero. */
    private static final byte TAG_FILE_ID = (byte) 0x83;

    /** Crea un objeto para el establecimiento de una clave publica para verificacion.
     * @param cla Clase (CLA) de la APDU
     * @param keyFileId Identificador de campo con la clave p&uacute;blica. Puede ser
     *        una referencia a la direcci&oacute;n en donde se almacena o un CHR para su carga
     *        desde memoria */
    public MseSetVerificationKeyApduCommand(final byte cla, final byte[] keyFileId) {
        super(cla,                                        // CLA
              INS_MANAGE_ENVIROMENT,                      // INS
              SET_FOR_VERIFICATION,                       // P1
              DST,                                        // P2
              new Tlv(TAG_FILE_ID, keyFileId).getBytes(), // Data
              null                                        // Le
        );
    }
}