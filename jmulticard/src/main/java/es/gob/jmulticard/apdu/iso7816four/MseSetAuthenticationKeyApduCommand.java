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

/** APDU ISO 7816-4 de gesti&oacute;n de entorno de seguridad orientada a
 * establecer una clave p&uacute;blica para autenticaci&oacute;n interna y
 * externa.
 * @author Carlos Gamuci Mill&aacute;n */
public final class MseSetAuthenticationKeyApduCommand extends CommandApdu {

    /** Byte de instrucci&oacute;n de la APDU. */
    private static final byte INS_MANAGE_ENVIROMENT = (byte) 0x22;

    /** Establece el fichero identificado para autenticaci&oacute;n. */
    private static final byte SET_FOR_AUTHENTICATION = (byte) 0xc1;

    /** Control Reference Template for Authentication (AT) */
    private static final byte AT = (byte) 0xa4;

    /** Tag para identificar un identificador de fichero. */
    private static final byte TAG_FILE_ID = (byte) 0x83;

    /** Etiqueta para hacer referencia a un fichero dedicado. */
    private static final byte TAG_DF_NAME = (byte) 0x84;

    /** Crea un objeto para la gesti&oacute;n del entorno de seguridad.
     * @param cla Clase (CLA) de la APDU
     * @param publicKeyFileId Identificador de campo de clave publica (CHR). Se utilizan los 12 bytes,
     *        rellenando con ceros por la izquierda desde los 8 m&iacute;nimos si es necesario
     * @param privateKeyRef Referencia a clave privada */
    public MseSetAuthenticationKeyApduCommand(final byte cla, final byte[] publicKeyFileId, final byte[] privateKeyRef) {
        super(cla,                                      // CLA
              INS_MANAGE_ENVIROMENT,                    // INS
              SET_FOR_AUTHENTICATION,                   // P1
              AT,                                       // P2
              buidData(publicKeyFileId, privateKeyRef), // Data
              null                                      // Le
        );
    }

    private static byte[] buidData(final byte[] publicKeyFileId, final byte[] privateKeyRef) {
        // Rellenamos el id de la clave hasta completar completar los 12 bytes
        final byte[] publicKeyFileIdCompleted = new byte[12];
        System.arraycopy(publicKeyFileId,
                         0,
                         publicKeyFileIdCompleted,
                         publicKeyFileIdCompleted.length - publicKeyFileId.length,
                         publicKeyFileId.length);
        for (int i = 0; i < publicKeyFileIdCompleted.length - publicKeyFileId.length; i++) {
            publicKeyFileIdCompleted[i] = (byte) 0x00;
        }

        final byte[] ret = new byte[publicKeyFileIdCompleted.length + privateKeyRef.length + 4];
        ret[0] = TAG_FILE_ID;
        ret[1] = (byte) publicKeyFileIdCompleted.length;
        System.arraycopy(publicKeyFileIdCompleted, 0, ret, 2, publicKeyFileIdCompleted.length);

        int idx = 1 + publicKeyFileIdCompleted.length;
        ret[++idx] = TAG_DF_NAME;
        ret[++idx] = (byte) privateKeyRef.length;
        System.arraycopy(privateKeyRef, 0, ret, ++idx, privateKeyRef.length);

        return ret;
    }
}