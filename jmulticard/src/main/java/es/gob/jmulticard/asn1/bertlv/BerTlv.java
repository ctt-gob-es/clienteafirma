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

/* 
 * Se ha modificado el archivo para adaptar la clase a las necesidades de la aplicacion.
 */

/*
   Copyright Isaac Levin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */
package es.gob.jmulticard.asn1.bertlv;

import java.io.ByteArrayInputStream;

/** TLV seg&uacute;n ASN.1 BER.
 * @author Isaac Levin */
public final class BerTlv {
    private BerTlvIdentifier tag;
    private int length;
    private byte[] value;

    /** Obtiene la etiqueta (tipo) del TLV.
     * @return Etiqueta (tipo) del TLV */
    public BerTlvIdentifier getTag() {
        return this.tag;
    }

    /** Obtiene el valor del TLV.
     * @return Valor del TLV */
    public byte[] getValue() {
        if (this.value == null) {
            return null;
        }
        final byte[] out = new byte[this.value.length];
        System.arraycopy(this.value, 0, out, 0, this.value.length);
        return out;
    }

    /** Obtiene una instancia del TLV.
     * @param stream Representaci&oacute;n binaria del TLV
     * @return Instancia del TLV */
    public static BerTlv getInstance(final ByteArrayInputStream stream) {
        final BerTlv tlv = new BerTlv();
        tlv.decode(stream);
        return tlv;
    }

    private void decode(final ByteArrayInputStream stream) throws IndexOutOfBoundsException {
        // Decode Tag
        this.tag = new BerTlvIdentifier();
        this.tag.decode(stream);

        // Decode length
        int tmpLength = stream.read();
        if (tmpLength <= 127) { // 0111 1111
            // Es un short
            this.length = tmpLength;
        }
        else if (tmpLength == 128) { // 1000 0000
            // Es un tipo indefinido, lo establecemos despues
            this.length = tmpLength;
        }
        else {
            // Es un long
            final int numberOfLengthOctets = tmpLength & 127; // turn off 8th bit
            tmpLength = 0;
            for (int i = 0; i < numberOfLengthOctets; i++) {
                final int nextLengthOctet = stream.read();
                tmpLength <<= 8;
                tmpLength |= nextLengthOctet;
            }
            this.length = tmpLength;
        }

        // Decodificamos el valor
        if (this.length == 128) { // 1000 0000
            // Formato indefinido
            stream.mark(0);
            int prevOctet = 1;
            int curOctet = 0;
            int len = 0;
            while (true) {
                len++;
                curOctet = stream.read();
                if (prevOctet == 0 && curOctet == 0) {
                    break;
                }
                prevOctet = curOctet;
            }
            len -= 2;
            this.value = new byte[len];
            stream.reset();
            if (len != stream.read(this.value, 0, len)) {
                throw new IndexOutOfBoundsException("La longitud de los datos leidos no coincide con el parametro indicado"); //$NON-NLS-1$
            }
            this.length = len;
        }
        else {
            // Formato definido
            this.value = new byte[this.length];
            if (this.length != stream.read(this.value, 0, this.length)) {
                throw new IndexOutOfBoundsException("La longitud de los datos leidos no coincide con el parametro indicado"); //$NON-NLS-1$
            }
        }
    }
    
    /** {@inheritDoc} */
    public String toString() {
        return "[TLV: T=" + this.tag + ";L=" + this.length + ";V=" + ((this.value == null) ? "null" : this.value.length + " bytes") + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    }
}