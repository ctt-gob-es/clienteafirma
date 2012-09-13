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