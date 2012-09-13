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

import es.gob.jmulticard.asn1.Asn1Exception;
import es.gob.jmulticard.asn1.DecoderObject;
import es.gob.jmulticard.asn1.Tlv;
import es.gob.jmulticard.asn1.TlvException;

/** Tipo ASN.1 <i>Sequence</i>. */
public abstract class Sequence extends DecoderObject {

    /** Tipo ASN.1 "SEQUENCE". */
    private static final byte TAG_SEQUENCE = (byte) 0x30;

    private final DecoderObject[] elements;

    private final Class[] elementsTypes;

    /** Construye un tipo ASN.1 <i>Sequence</i>.
     * Un <i>Sequence</i> contiene una secuencia de tipos ASN.1 (que pueden ser distintos)
     * @param types Tipos (etiquetas) de objetos ASN.1 (cero a n elementos) que va a contener la secuencia. El orden es relevante */
    protected Sequence(final Class[] types) {
        super();
        if (types == null) {
            throw new IllegalArgumentException();
        }
        this.elementsTypes = new Class[types.length];
        System.arraycopy(types, 0, this.elementsTypes, 0, types.length);
        this.elements = new DecoderObject[types.length];
    }

    protected void decodeValue() throws Asn1Exception, TlvException {
        final Tlv mainTlv = new Tlv(this.getRawDerValue());
        checkTag(mainTlv.getTag());
        int offset = 0;
        Tlv tlv;
        byte[] remainingBytes;
        DecoderObject tmpDo;
        final byte[] rawValue = mainTlv.getValue();
        for (int i = 0; i < this.elementsTypes.length; i++) {
            remainingBytes = new byte[rawValue.length - offset];
            System.arraycopy(rawValue, offset, remainingBytes, 0, remainingBytes.length);
            tlv = new Tlv(remainingBytes);
            try {
                tmpDo = (DecoderObject) this.elementsTypes[i].newInstance();
            }
            catch (final Exception e) {
                throw new Asn1Exception("No se ha podido instanciar un " + this.elementsTypes[i].getName() + //$NON-NLS-1$
                                        " en la posicion " + Integer.toString(i) + " de la secuencia: " + e, e //$NON-NLS-1$ //$NON-NLS-2$
                );
            }
            tmpDo.checkTag(tlv.getTag());
            tmpDo.setDerValue(tlv.getBytes());
            offset = offset + tlv.getBytes().length;
            this.elements[i] = tmpDo;
        }
    }

    /** {@inheritDoc} */
    protected byte getDefaultTag() {
        return TAG_SEQUENCE;
    }

    /** Devuelve el elemento situado en la posici&oacute;n indicada.
     * @param index Posici&oacute;n del elemento a recuperar
     * @return Un objeto de tipo <code>DecoderObject</code> que contiene el TLV deseado.
     * @throws IndexOutOfBoundsException Si el indice indicado no pertenece al rango de la secuencia */
    protected DecoderObject getElementAt(final int index) {
        return this.elements[index];
    }
}