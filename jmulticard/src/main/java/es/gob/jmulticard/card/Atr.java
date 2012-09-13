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
package es.gob.jmulticard.card;

import java.io.Serializable;

/** Respuesta al reset (ATR, <i>Answer To Reset</i>) de una tarjeta.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Atr implements Serializable {

    private static final long serialVersionUID = 1L;
    private final byte[] atrBytes;
    private final byte[] mask;

    /** Construye una respuesta al reset.
     * @param a ATR de la tarjeta
     * @param m M&aacute;scara de comparaci&oacute;n del ATR para determinar modelo espec&iacite;fico
     *        de tarjeta */
    public Atr(final byte[] a, final byte[] m) {
        if (a == null || m == null) {
            throw new IllegalArgumentException("El ATR y su mascara no pueden ser nulos"); //$NON-NLS-1$
        }
        this.atrBytes = new byte[a.length];
        System.arraycopy(a, 0, this.atrBytes, 0, a.length);
        this.mask = new byte[m.length];
        System.arraycopy(m, 0, this.mask, 0, m.length);
    }

    /** Obtiene los octetos binarios de la respuesta al reset.
     * @return Representaci&oacute;n binaria de la respuesta al reset */
    public byte[] getBytes() {
        final byte[] tmp = new byte[this.atrBytes.length];
        System.arraycopy(this.atrBytes, 0, tmp, 0, this.atrBytes.length);
        return tmp;
    }

    /** {@inheritDoc} */
    public boolean equals(final Object o) {
        if (!(o instanceof Atr)) {
            return false;
        }
        final Atr tmpAtr = (Atr) o;
        if (tmpAtr.getBytes().length != this.atrBytes.length) {
            return false;
        }
        for (int i = 0; i < this.atrBytes.length; i++) {
            if ((this.atrBytes[i] & this.mask[i]) != (tmpAtr.getBytes()[i] & this.mask[i])) {
                return false;
            }
        }
        return true;
    }

    /** {@inheritDoc} */
    public int hashCode() {
        return hashCode(this.atrBytes) + hashCode(this.mask);
    }

    private static int hashCode(final byte[] a) {
        if (a == null) {
            return 0;
        }
        int result = 1;
        for (int i = 0; i < a.length; i++) {
            result = 31 * result + a[i];
        }
        return result;
    }
}
