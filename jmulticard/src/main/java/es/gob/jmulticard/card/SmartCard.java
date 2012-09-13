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

import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;

/** Tarjeta inteligente gen&eacute;rica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class SmartCard implements Card {

    private final byte cla;
    private ApduConnection connection;

    protected ApduConnection getConnection() {
        return this.connection;
    }

    /**
     * Establece una nueva conexi&oacute;n con la tarjeta cerrando la anterior.
     * @param conn Nueva conexi&oacute;n con la tarjeta.
     * @throws ApduConnectionException Cuando no se puede sustituir la conexi&oacute;n actual por la nueva.
     */
    protected void setConnection(final ApduConnection conn) throws ApduConnectionException {
    	this.connection.close();
        if (!conn.isOpen()) {
            conn.open();
        }
        this.connection = conn;
    }
    
    protected byte getCla() {
        return this.cla;
    }

    /** Construye una tarjeta inteligente gen&eacute;rica.
     * @param c Octeto de clase (CLA) de las APDU
     * @param conn Connexi&oacute;n con la tarjeta
     * @throws ApduConnectionException Si la conexi&oacute;n con la tarjeta se proporciona cerrada y no es posible abrirla */
    public SmartCard(final byte c, final ApduConnection conn) throws ApduConnectionException {
        if (conn == null) {
            throw new IllegalArgumentException("La conexion con la tarjeta no puede ser nula"); //$NON-NLS-1$
        }
        this.cla = c;
        this.connection = conn;
    }

    /** Obtiene el nombre de la tarjeta.
     * @return Nombre de la tarjeta */
    public abstract String getCardName();

}
