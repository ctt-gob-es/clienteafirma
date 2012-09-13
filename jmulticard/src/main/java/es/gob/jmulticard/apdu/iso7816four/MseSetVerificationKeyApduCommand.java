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