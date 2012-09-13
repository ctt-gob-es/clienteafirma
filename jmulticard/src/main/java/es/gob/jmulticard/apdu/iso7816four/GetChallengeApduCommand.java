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

/** APDU ISO 7816-4 para la obtenci&oacute;n de una cadena de bytes aleatoria de desaf&iacute;o.
 * @author Alberto Mart&iacute;nez Cerquero */
public final class GetChallengeApduCommand extends CommandApdu {

    private static final byte INS_GET_CHALLENGE = (byte) 0x84;

    /** Crea una APDU ISO 7816-4 para la obtenci&oacute;n de un n&uacute;mero aleatorio de 8 bytes a
     * trav&eacute;s de la tarjeta.
     * @param cla Clase (CLA) de la APDU */
    public GetChallengeApduCommand(final byte cla) {
        super(cla,                                  // CLA
              INS_GET_CHALLENGE,                    // INS
              (byte) 0x00,                          // P1
              (byte) 0x00,                          // P2
              null,                                 // Data
              Integer.valueOf(String.valueOf(8))    // Le
        );
    }
    
    /** Crea una APDU ISO 7816-4 para la obtenci&oacute;n de un n&uacute;mero aleatorio de n bytes a
     * trav&eacute;s de la tarjeta.
     * @param cla Clase (CLA) de la APDU 
     * @param numBytes N&uacute;mero de bytes del desafio generado */
    public GetChallengeApduCommand(final byte cla, final int numBytes) {
        super(cla,                                  // CLA
              INS_GET_CHALLENGE,                    // INS
              (byte) 0x00,                          // P1
              (byte) 0x00,                          // P2
              null,                                 // Data
              Integer.valueOf(String.valueOf(numBytes))    // Le
        );
    }
}