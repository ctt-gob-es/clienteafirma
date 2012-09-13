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
package es.gob.jmulticard.card.dnie;

import es.gob.jmulticard.apdu.StatusWord;
import es.gob.jmulticard.card.CryptoCardException;

/** Excepci&oacute;n gen&eacute;rica en tarjetas ISO 7816-4.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DnieCardException extends CryptoCardException {

	private static final long serialVersionUID = 5935577997660561619L;

    private final StatusWord returnCode;

    /** Construye la excepci&oacute;n.
     * @param desc Descripci&oacute;n del problema.
     * @param retCode C&oacute;digo devuelto por la tarjeta. */
    DnieCardException(final String desc, final StatusWord retCode) {
        super(desc);
        this.returnCode = retCode;
    }

    /** Construye la excepci&oacute;n.
     * @param desc Descripci&oacute;n del problema.
     * @param t Problema que origino la excepci&oacute;n. */
    DnieCardException(final String desc, final Throwable t) {
        super(desc, t);
        this.returnCode = null;
    }

    /** Construye la excepci&oacute;n.
     * @param retCode C&oacute;digo devuelto por la tarjeta. */
    DnieCardException(final StatusWord retCode) {
        super();
        this.returnCode = retCode;
    }

    /** Obtiene el c&oacute;digo de finalizaci&oacute;n (en modo de palabra de estado) que caus&oacute; la
     * excepci&oacute;n.
     * @return C&oacute;digo de finalizaci&oacute;n que caus&oacute; la excepci&oacute;n */
    public StatusWord getStatusWord() {
        return this.returnCode;
    }
}