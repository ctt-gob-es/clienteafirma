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

/** APDU ISO 7816-4 mediante la que se env&iacute;a un token para la autenticaci&oacute;n
 * externa ante la tarjeta.
 * @author Carlos Gamuci Mill&aacute;n */
public final class ExternalAuthenticateApduCommand extends CommandApdu {

    /** Byte de instrucci&oacute;n de la APDU. */
    private static final byte INS_EXTERNAL_AUTHENTICATE = (byte) 0x82;

    /** Valor para indicar que no se dispone de informaci&oacute;n al respecto. */
    private static final byte NO_INFORMATION_GIVEN = (byte) 0x00;

    /** Crea una APDU de autenticaci&oacute;n externa.
     * @param cla Clase (CLA) de la APDU
     * @param authenticationToken Token de autenticaci&oacute;n. */
    public ExternalAuthenticateApduCommand(final byte cla, final byte[] authenticationToken) {
        super(cla,                       // CLA
              INS_EXTERNAL_AUTHENTICATE, // INS
              NO_INFORMATION_GIVEN,      // P1
              NO_INFORMATION_GIVEN,      // P2
              authenticationToken,       // Data
              null                       // Le
        );
    }
}