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
package es.gob.jmulticard.apdu.dnie;

import es.gob.jmulticard.apdu.CommandApdu;

/** APDU de DNIe de obtenci&oacute;n del n&uacute;mero de serie.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class GetChipInfoApduCommand extends CommandApdu {

    private static final byte INS_GET_CHIP_INFO = (byte) 0xb8;

    private static final byte INSTRUCTION_PARAMETER_P1 = 0x00;
    private static final byte INSTRUCTION_PARAMETER_P2 = 0x00;
    private static final byte MAXIMUM_LENGTH_EXPECTED_LE = 0x07;

    /** Crea una APDU de DNIe de obtenci&oacute;n del n&uacute;mero de serie. */
    public GetChipInfoApduCommand() {
        super(
    		(byte) 0x90,								// CLA
    		INS_GET_CHIP_INFO, 							// INS
    		INSTRUCTION_PARAMETER_P1, 					// P1
    		INSTRUCTION_PARAMETER_P2,					// P2
    		null,										// Data
    		Integer.valueOf(String.valueOf(MAXIMUM_LENGTH_EXPECTED_LE)) // Le
		);
    }
}