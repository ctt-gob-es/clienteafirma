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

/** APDU ISO 7816-4 de gesti&oacute;n de entorno de seguridad orientada a
 * establecer una clave privada para firma.
 * @author Carlos Gamuci Mill&aacute;n */
public final class MseSetSignatureKeyApduCommand extends CommandApdu {

    /** Byte de instrucci&oacute;n de la APDU. */
    private static final byte INS_MANAGE_ENVIROMENT = (byte) 0x22;

    /** Establece la clave indicada para firma. */
    private static final byte SET_FOR_SIGN = (byte) 0x41;

    /** Plantilla de referencia de control para firma */
    private static final byte SIGN_TEMPLATE = (byte) 0xb6;

    /** Etiqueta para hacer referencia a un fichero dedicado. */
    private static final byte TAG_DF_NAME = (byte) 0x84;

    /** Crea un objeto para el establecimiento de la clave privada de firma.
     * Se requiere el ultimo segmento del path de la clave.
     * @param cla Clase (CLA) de la APDU
     * @param privateKeyPath Path de la clave privada. */
    public MseSetSignatureKeyApduCommand(final byte cla, final byte[] privateKeyPath) {
        super(cla,                                      // CLA
              INS_MANAGE_ENVIROMENT,                    // INS
              SET_FOR_SIGN,								// P1
              SIGN_TEMPLATE,                            // P2
              buidData(privateKeyPath),					// Data
              null                                      // Le
        );
    }

    private static byte[] buidData(final byte[] privateKeyPath) {

        final byte[] ret = new byte[privateKeyPath.length + 2];
        ret[0] = TAG_DF_NAME;
        ret[1] = (byte) privateKeyPath.length;
        System.arraycopy(privateKeyPath, 0, ret, 2, privateKeyPath.length);

        return ret;
    }
}