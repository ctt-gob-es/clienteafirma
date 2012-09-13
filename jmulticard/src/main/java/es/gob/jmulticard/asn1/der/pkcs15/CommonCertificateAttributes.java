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
package es.gob.jmulticard.asn1.der.pkcs15;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.asn1.der.OctectString;
import es.gob.jmulticard.asn1.der.Sequence;

/** Tipo ASN.1 <i>CommonCertificateAttributes</i>.
 * <pre>
 *  CommonCertificateAttributes ::= SEQUENCE {
 *   iD Identifier
 *  }
 *  Identifier ::= OCTET STRING (SIZE (0..pkcs15-ub-identifier))
 * </pre>
 * @author Gonzalo Henr&iacute;quez Manzano. */
public final class CommonCertificateAttributes extends Sequence {

    /** Construye un objeto ASN.1 <i>CommonCertificateAttributes</i>. */
    public CommonCertificateAttributes() {
        super(new Class[] { Identifier.class });
    }

    /** Obtiene el identificador de este <i>CommonCertificateAttributes</i>.
     * @return Identificador del <i>CommonCertificateAttributes</i> */
    byte[] getId() {
        if (this.getElementAt(0) == null) {
        	throw new IllegalStateException("No existe el identificador dentro del objeto"); //$NON-NLS-1$
        }
        return ((OctectString) this.getElementAt(0)).getOctectStringByteValue();
    }

    /** {@inheritDoc} */
    public String toString() {
    	return HexUtils.hexify(getId(), false);
    }

}
