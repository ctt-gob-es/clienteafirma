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

import java.util.Vector;

import es.gob.jmulticard.asn1.Asn1Exception;
import es.gob.jmulticard.asn1.DecoderObject;
import es.gob.jmulticard.asn1.Tlv;
import es.gob.jmulticard.asn1.TlvException;

/** Tipo ASN.1 <i>SequenceOf</i>. */
public abstract class SequenceOf extends DecoderObject {

	/** Tipo ASN.1 "SEQUENCE". */
    private static final byte TAG_SEQUENCE = (byte) 0x30;

    private final Class elementsType;

    private Vector sequenceObjects = null;

	protected void decodeValue() throws Asn1Exception, TlvException {
		Tlv tlv = new Tlv(this.getRawDerValue());
		checkTag(tlv.getTag());
		int offset = 0;
		byte[] remainingBytes;
        DecoderObject tmpDo;
        final byte[] valueBytes = tlv.getValue();
        this.sequenceObjects = new Vector();
        while (offset < valueBytes.length) {
        	remainingBytes = new byte[valueBytes.length - offset];
        	System.arraycopy(valueBytes, offset, remainingBytes, 0, remainingBytes.length);
    		tlv = new Tlv(remainingBytes);
        	try {
        		tmpDo = (DecoderObject)this.elementsType.newInstance();
        	}
        	catch (final Exception e) {
        		throw new Asn1Exception(
    				"No se ha podido instanciar un " + this.elementsType.getName() + //$NON-NLS-1$
    				" en la secuencia: " + e, e  //$NON-NLS-1$
        		);
        	}
        	offset = offset + tlv.getBytes().length;
        	tmpDo.checkTag(tlv.getTag());
        	tmpDo.setDerValue(tlv.getBytes());
        	this.sequenceObjects.addElement(tmpDo);
        }
	}

	/** Construye un tipo ASN.1 <i>SequenceOf</i>.
     * Un <i>SequenceOf</i> contiene una secuencia de tipos ASN.1 (que deben ser iguales)
     * @param type Tipos (etiquetas) de objetos ASN.1 (1 a n elementos) que va a contener la secuencia. El orden es irrelevante */
	protected SequenceOf(final Class type) {
	    super();
		if (type == null) {
			throw new IllegalArgumentException();
		}
		this.elementsType = type;
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
        return (DecoderObject) this.sequenceObjects.elementAt(index);
    }

    protected int getElementCount() {
    	return this.sequenceObjects.size();
    }

}
