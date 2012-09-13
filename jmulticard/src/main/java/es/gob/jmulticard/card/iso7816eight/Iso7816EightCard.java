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
package es.gob.jmulticard.card.iso7816eight;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.apdu.connection.cwa14890.SecureChannelException;
import es.gob.jmulticard.apdu.iso7816eight.PsoVerifyCertificateApduCommand;
import es.gob.jmulticard.card.iso7816four.Iso7816FourCard;

/** Tarjeta compatible ISO-7816-8. */
public abstract class Iso7816EightCard extends Iso7816FourCard {

    /** Construye una tarjeta compatible ISO 7816-8.
     * @param c Clase (CLA) de la APDU
     * @param conn Connexi&oacute;n con la tarjeta
     * @throws ApduConnectionException Si la conexi&oacute;n con la tarjeta se proporciona cerrada y no se posible abrirla */
    public Iso7816EightCard(final byte c, final ApduConnection conn) throws ApduConnectionException {
        super(c, conn);
    }

    /** Verifica un certificado en base a una clave p&uacute;blica cargada anteriormente
     * y que deber&aacute; ser la del certificado a partir del cual se gener&oacute; el
     * certificado que ahora se valida.
     * @param cert Certificado que se desea comprobar
     * @throws SecureChannelException Cuando el certificado no es correcto u ocurre alg&uacute;n error en la validaci&oacute;n
     * @throws ApduConnectionException Cuando ocurre un error en la comunicaci&oacute;n con la tarjeta */
    public void verifyCertificate(final byte[] cert) throws ApduConnectionException {
        final CommandApdu apdu = new PsoVerifyCertificateApduCommand((byte) 0x00, cert);
        final ResponseApdu res = this.getConnection().transmit(apdu);
        if (!res.isOk()) {
            throw new SecureChannelException(
        		"Error en la verificacion del certificado. Se obtuvo el error: " + //$NON-NLS-1$
                HexUtils.hexify(res.getBytes(), true)
            );
        }
    }
}