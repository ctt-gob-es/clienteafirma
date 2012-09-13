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

/** Tipo ASN.1 <i>OctectString</i>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class OctectString extends DecoderObject {

	/** Tipo ASN.1 "OCTET STRING". */
    private static final byte TAG_OCTECTSTRING = (byte) 0x04;

    private byte[] value = null;

    protected void decodeValue() throws Asn1Exception, TlvException {
    	final Tlv tlv = new Tlv(this.getRawDerValue());
    	if (TAG_OCTECTSTRING != tlv.getTag()) {
    		throw new TlvException("Se esperaba un TLV de tipo OctectString pero se ha encontrado uno de tipo " + //$NON-NLS-1$
                HexUtils.hexify(new byte[] {
                    tlv.getTag()
                }, false)
            );
    	}
    	this.value = tlv.getValue();
    }

    protected byte getDefaultTag() {
        return TAG_OCTECTSTRING;
    }

    /** Proporciona valor binario del <i>OctectString</i>.
     * @return Array de octetos con el valor binario del <i>OctectString</i> */
    public byte[] getOctectStringByteValue() {
        final byte[] out = new byte[this.value.length];
        System.arraycopy(this.value, 0, out, 0, this.value.length);
        return out;
    }
}
