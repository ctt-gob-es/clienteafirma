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
package es.gob.jmulticard.card;

import es.gob.jmulticard.HexUtils;

/** Indica que se esperaba un tipo de tarjeta pero se ha encontrado otro no compatible.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidCardException extends CardException {

    private static final long serialVersionUID = 4888120866657775782L;

    private final Atr atr;
    private final String name;
    private final byte[] badAtr;

    /** Construye una excepci&oacute;n de tarjeta inv&aacute;lida.
     * @param expectedCardName Nombre de la tarjeta esperada
     * @param expectedAtr ATR de la tarjeta esperada
     * @param foundAtr ATR (binario) de la tarjeta encontrada */
    public InvalidCardException(final String expectedCardName, final Atr expectedAtr, final byte[] foundAtr) {
        super("Se esperaba una tarjeta de tipo '" + //$NON-NLS-1$
              expectedCardName
              + "' pero se encontro otra con ATR=" + //$NON-NLS-1$
              ((foundAtr == null) ? "NULO" : HexUtils.hexify(foundAtr, true)) //$NON-NLS-1$
        );
        this.atr = expectedAtr;
        this.name = expectedCardName;
        if (foundAtr != null) {
            this.badAtr = new byte[foundAtr.length];
            System.arraycopy(foundAtr, 0, this.badAtr, 0, this.badAtr.length);
        }
        else {
            this.badAtr = null;
        }
    }

    /** Obtiene el ATR de la tarjeta que se esperaba.
     * @return ATR de la tarjeta que se esperaba */
    public Atr getExpectedAtr() {
        return this.atr;
    }

    /** Obtiene el nombre de la tarjeta que se esperaba.
     * @return nombre de la tarjeta que se esperaba */
    public String getExpectedCardName() {
        return this.name;
    }

    /** Obtiene el ATR (binario) de la tarjeta encontrada.
     * @return ATR (binario) de la tarjeta encontrada */
    public byte[] getFoundAtr() {
        if (this.badAtr == null) {
            return null;
        }
        final byte[] tmpAtr = new byte[this.badAtr.length];
        System.arraycopy(this.badAtr, 0, tmpAtr, 0, this.badAtr.length);
        return tmpAtr;
    }
}
