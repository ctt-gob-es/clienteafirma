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

import es.gob.jmulticard.asn1.DecoderObject;
import es.gob.jmulticard.asn1.der.Sequence;

/** Tipo PKCS#15 ASN.1 <i>PKCS15Object</i> (<i>CIO</i> de ISO 7816-15).
 * <pre>
 *  PKCS15Object ::= SEQUENCE {
 *      commonObjectAttributes CommonObjectAttributes,
 *      classAttributes ClassAttributes,
 *      typeAttributes [1] TypeAttributes
 *  }
 * </pre>
 * @author Gonzalo Henr&iacute;quez Manzano */
abstract class Pkcs15Object extends Sequence {

    /** Construye un tipo PKCS#15 ASN.1 <i>PKCS15Object</i> (<i>CIO</i> de ISO 7816-15).
     * @param classAttributes Tipo de los Atributos espec&iacute;ficos de la clase general del objeto
     * @param typeAttributes Tipo de los Atributos espec&iacute;ficos del tipo concreto del objeto */
    Pkcs15Object(final Class classAttributes, final Class typeAttributes) {
        super(new Class[] {
            CommonObjectAttributes.class,
            classAttributes,
            typeAttributes
        });
    }

    /** Obtiene los atributos comunes (<i>CommonObjectAttributes</i>).
     * @return Atributos comunes */
    CommonObjectAttributes getCommonObjectAttributes() {
        return (CommonObjectAttributes) this.getElementAt(0);
    }

    /** Obtiene los atributos espec&iacute;ficos de clase.
     * @return Atributos espec&iacute;ficos de clase */
    DecoderObject getClassAttributes() {
        return this.getElementAt(1);
    }

    /** Obtiene los atributos espec&iacute;ficos del tipo.
     * @return Atributos espec&iacute;ficos del tipo */
    DecoderObject getTypeAttributes() {
        return this.getElementAt(2);
    }

}
