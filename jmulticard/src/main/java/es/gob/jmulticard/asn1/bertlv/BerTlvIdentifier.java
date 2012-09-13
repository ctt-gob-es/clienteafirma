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

/* 
 * Se ha modificado el archivo para adaptar la clase a las necesidades de la aplicación.
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
import java.math.BigInteger;

/** Identificador de TLV ASN.1 BER
 * @author Isaac Levin */
public final class BerTlvIdentifier {

    private byte[] value;

    /** Obtiene el valor de la etiqueta (tipo) del TLV
     * @return Valor de la etiqueta (tipo) del TLV */
    public int getTagValue() {
        if (this.value == null) {
            return 0;
        }
        if (this.value.length == 1) {
            return this.value[0];
        }
        final byte[] tagBytes = new byte[this.value.length - 1];
        System.arraycopy(this.value, 1, tagBytes, 0, this.value.length - 1);
        for (int i = 0; i < tagBytes.length - 1; i++) {
            // Establecemos el octavo bit indicador a false
            tagBytes[i] = (byte) BitManipulationHelper.setBitValue(tagBytes[i], 8, false);
        }
        return new BigInteger(tagBytes).intValue();
    }

    void decode(final ByteArrayInputStream stream) {
        final int tlvIdFirstOctet = stream.read();

        this.value = new byte[] {
            (byte) tlvIdFirstOctet
        };
        // Comprobamos si el id es multi-octeto (los bits del 5 al 1 deben codificarse como 11111)
        final int mask = 31;
        if ((tlvIdFirstOctet & mask) == mask) {
            // Multi-octeto
            do {
                final int tlvIdNextOctet = stream.read();
                boolean lastOctet = false;
                if (!BitManipulationHelper.getBitValue(tlvIdNextOctet, 8)) {
                    lastOctet = true;
                }

                this.value = BitManipulationHelper.mergeArrays(this.value, new byte[] {
                    (byte) tlvIdNextOctet
                });

                if (lastOctet) {
                    break;
                }
            } while (true);
        }
    }

    /** {@inheritDoc} */
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof BerTlvIdentifier) {
            final BerTlvIdentifier bti = (BerTlvIdentifier) obj;
            if (this.value.length != bti.value.length) {
                return false;
            }
            for (int i = 0; i < this.value.length; i++) {
                try {
                    if (this.value[i] != bti.value[i]) {
                        return false;
                    }
                }
                catch (final ArrayIndexOutOfBoundsException e) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /** {@inheritDoc} */
    public int hashCode() {
        return new BigInteger(this.value).intValue();
    }

    /** {@inheritDoc} */
    public String toString() {
        if (this.value == null) {
            return "NULL"; //$NON-NLS-1$
        }
        final StringBuffer buf = new StringBuffer("["); //$NON-NLS-1$
        for (int i = 0; i < this.value.length; i++) {
            buf.append("0x").append(Integer.toHexString(this.value[i])).append(' '); //$NON-NLS-1$
        }
        buf.append(']');
        return buf.toString();
    }
}
