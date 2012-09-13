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
