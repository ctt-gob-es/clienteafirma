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
package es.gob.jmulticard.asn1;

import es.gob.jmulticard.HexUtils;

/** Objeto ASN.1 gen&eacute;rico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Alberto Mart&iacute;nez */
public abstract class DecoderObject {

    private byte[] rawDerValue = null;

    protected byte[] getRawDerValue() {
        final byte[] out = new byte[this.rawDerValue.length];
        System.arraycopy(this.rawDerValue, 0, out, 0, this.rawDerValue.length);
        return out;
    }

    /** Establece el valor (en codificaci&oacute;n DER) del objeto ASN.1.
     * @param value Valor (TLC con codificaci&oacute;n DER) del objeto ASN.1
     * @throws Asn1Exception Si no se puede decodificar adecuadamente el valor establecido
     * @throws TlvException Si hay errores relativos a los TLV DER al decodificar los datos de entrada */
    public void setDerValue(final byte[] value) throws Asn1Exception, TlvException {
        if (value == null) {
            throw new IllegalArgumentException("El valor del objeto ASN.1 no puede ser nulo"); //$NON-NLS-1$
        }
        this.rawDerValue = new byte[value.length];
        System.arraycopy(value, 0, this.rawDerValue, 0, value.length);
        decodeValue();
    }

    /** Obtiene el valor binario del objeto ASN.1.
     * @return Valor binario del objeto ASN.1 */
    public byte[] getBytes() {
        final byte[] out = new byte[this.rawDerValue.length];
        System.arraycopy(this.rawDerValue, 0, out, 0, this.rawDerValue.length);
        return out;
    }

    /** Decodifica el valor DER establecido comprobando que corresponde al esperado y formando las
     * estructuras internas.
     * @throws Asn1Exception Si hay errores correspondientes a las estructuras ASN.1 DER
     * @throws TlvException Si hay errores relativos a los TLV DER */
    protected abstract void decodeValue() throws Asn1Exception, TlvException;

    /** Obtiene la etiqueta de tipo ASN.1 del objeto.
     * @return Etiqueta de tipo ASN.1 del objeto */
    protected abstract byte getDefaultTag();

    /** Comprueba que el tipo proporcionado sea compatible con el del objeto ASN.1.
     * @param tag Etiqueta de tipo a comprobar
     * @throws Asn1Exception Si las etiquetas de tipo no son compatibles */
    public void checkTag(final byte tag) throws Asn1Exception {
        if (getDefaultTag() != tag) {
            throw new Asn1Exception("Se esperaba un tipo " + HexUtils.hexify(new byte[] { getDefaultTag()}, false) + //$NON-NLS-1$
                                    " (" + this.getClass().getName() + ") " + //$NON-NLS-1$ //$NON-NLS-2$ 
                                    "pero se encontro un tipo " + HexUtils.hexify(new byte[] { tag }, false)  //$NON-NLS-1$
            );
        }
    }
}