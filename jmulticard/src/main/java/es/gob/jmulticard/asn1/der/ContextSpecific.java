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
package es.gob.jmulticard.asn1.der;

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.asn1.Asn1Exception;
import es.gob.jmulticard.asn1.DecoderObject;
import es.gob.jmulticard.asn1.Tlv;
import es.gob.jmulticard.asn1.TlvException;

/** Tipo ASN.1 espec&iacute;fico del contexto.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class ContextSpecific extends DecoderObject {

    private DecoderObject object = null;

    protected DecoderObject getObject() {
        if (this.object == null) {
            throw new IllegalStateException();
        }
        return this.object;
    }

    protected void decodeValue() throws Asn1Exception, TlvException {
        final Tlv tlv = new Tlv(this.getRawDerValue());
        final DecoderObject tmpDo;
        try {
            tmpDo = (DecoderObject) this.elementType.newInstance();
        }
        catch (final Exception e) {
            throw new Asn1Exception("No se ha podido instanciar un " + this.elementType.getName() + " en el contexto especifico: " + e, e //$NON-NLS-1$ //$NON-NLS-2$
            );
        }
        tmpDo.setDerValue(tlv.getValue());
        this.object = tmpDo;
    }

    private final Class elementType;

    /** Construye un tipo ASN.1 espec&iacute;fico del contexto.
     * @param type Tipo de elemento contenido dentro de este objeto */
    public ContextSpecific(final Class type) {
        super();
        if (type == null) {
            throw new IllegalArgumentException("El tipo contenido dentro de ContextSpecific no puede ser nulo" //$NON-NLS-1$
            );
        }
        this.elementType = type;
    }

    protected byte getDefaultTag() {
        throw new UnsupportedOperationException("No hay tipo por defecto"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public void checkTag(final byte tag) throws Asn1Exception {
        if ((tag & 0x0c0) != 0x080) {
            throw new Asn1Exception("La etiqueta " + HexUtils.hexify(new byte[] { tag}, false) + //$NON-NLS-1$
                                    " no es valida para un objeto especifico del contexto" //$NON-NLS-1$
            );
        }
    }
}