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
package es.gob.jmulticard.asn1;

import java.io.ByteArrayInputStream;
import java.io.IOException;

/** Representaci&oacute;n de un TLV (Top-Longitud-Valor) binario en representaci&oacute;n ASN.1 DER. */
public final class Tlv {

    /** Octeto de tipo (equiqueta). */
    private final byte tag;

    /** Longitud del valor. */
    private final int length;

    /** Estructura binaria completa del TLV. */
    private final byte[] bytes;

    private final int valueOffset;

    /** Construye un TLV simple con etiqueta y longitud de un solo octeto cada uno.
     * @param t Etiqueta (tipo) del TLV
     * @param value Valor del TLV */
    public Tlv(final byte t, final byte[] value) {
        if (value == null) {
            throw new IllegalArgumentException("El valor del TLV no puede ser nulo"); //$NON-NLS-1$
        }
        this.valueOffset = 2;
        this.tag = t;
        this.length = value.length;
        this.bytes = new byte[value.length + 2];
        this.bytes[0] = t;
        this.bytes[1] = (byte) value.length;
        System.arraycopy(value, 0, this.bytes, 2, value.length);
    }

    /** Construye un TLV simple a partir de su representaci&oacute;n binaria directa.
     * @param buffer Representaci&oacute;n binaria del TLV
     * @throws TlvException en caso de error analizando el TLV */
    public Tlv(final byte[] buffer) throws TlvException {
        if (buffer == null || buffer.length < 3) {
            throw new IllegalArgumentException("El TLV no puede ser nulo ni medir menos de tres octetos"); //$NON-NLS-1$
        }

        int offset = 0;

        // Copiamos el TLV completo
        final byte[] tempBytes = new byte[buffer.length];
        System.arraycopy(buffer, 0, tempBytes, 0, buffer.length);

        this.tag = tempBytes[offset++];

        // Comprobamos que el Tipo sea valido (tipos multi-octeto)
        if ((this.tag & 0x1f) == 0x1f) {
            throw new TlvException("El tipo del TLV es invalido"); //$NON-NLS-1$
        }

        // Copiamos la longitud
        int size = tempBytes[offset++] & 0xff;
        final boolean indefinite = (size == 128);
        if (indefinite) {
            if ((this.tag & 0x20) == 0) {
                throw new TlvException("Longitud del TLV invalida"); //$NON-NLS-1$
            }
        }
        else if (size >= 128) {
            int sizeLen = size - 128;
            // NOTA: El tamano debe caber en tres octetos
            if (sizeLen > 3) {
                throw new TlvException("TLV demasiado largo"); //$NON-NLS-1$
            }
            size = 0;
            while (sizeLen > 0) {
                size = (size << 8) + (tempBytes[offset++] & 0xff);
                sizeLen--;
            }
        }

        this.length = size;
        this.valueOffset = offset;

        this.bytes = new byte[this.valueOffset + this.length];
        System.arraycopy(tempBytes, 0, this.bytes, 0, this.valueOffset + this.length);

    }

    /** Devuelve el TLV directamente en binario.
     * @return Valor finario completo del TLV */
    public byte[] getBytes() {
        final byte[] out = new byte[this.bytes.length];
        System.arraycopy(this.bytes, 0, out, 0, this.bytes.length);
        return out;
    }

    /** Devuelve la longitud del valor del TLV.
     * @return Longitud del valor del TLV */
    public int getLength() {
        return this.length;
    }

    /** Devuelve el tipo (etiqueta) del TLV.
     * @return Tipo (etiqueta) del TLV */
    public byte getTag() {
        return this.tag;
    }

    /** Devuelve el valor del TLV.
     * @return Valor del del TLV */
    public byte[] getValue() {
        final byte[] out = new byte[this.length];
        System.arraycopy(this.bytes, this.valueOffset, out, 0, this.length);
        return out;
    }

    /** Obtiene un TLV de un flujo de entrada, leyendo del mismo s&oacute;lo los bytes
     * correspondientes al TLV en cuesti&oacute;n.
     * @param recordOfTlv Flujo de entrada.
     * @return TLV recuperado del flujo.
     * @throws IOException Si ocurre un error durante la lectura del TLV. */
    public static Tlv decode(final ByteArrayInputStream recordOfTlv) throws IOException {

        final byte tag = (byte) recordOfTlv.read();

        // Comprobamos que el Tipo sea valido (tipos multi-octeto)
        if ((tag & 0x1f) == 0x1f) {
            throw new IOException("El tipo del TLV es invalido"); //$NON-NLS-1$
        }

        // Copiamos la longitud
        int size = recordOfTlv.read() & 0xff;

        final boolean indefinite = (size == 128);
        if (indefinite) {
            if ((tag & 0x20) == 0) {
                throw new IOException("Longitud del TLV invalida"); //$NON-NLS-1$
            }
        }
        else if (size >= 128) {
            int sizeLen = size - 128;
            // NOTA: El tamano debe caber en tres octetos
            if (sizeLen > 3) {
                throw new IOException("TLV demasiado largo"); //$NON-NLS-1$
            }
            size = 0;
            while (sizeLen > 0) {
                size = (size << 8) + (recordOfTlv.read() & 0xff);
                sizeLen--;
            }
        }

        final byte[] value = new byte[size];
        if (value.length != recordOfTlv.read(value)) {
            throw new IndexOutOfBoundsException("La longitud de los datos leidos no coincide con el parametro indicado"); //$NON-NLS-1$
        }

        return new Tlv(tag, value);
    }
}