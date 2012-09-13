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
package es.gob.jmulticard;

import java.math.BigInteger;

/** Utilidades varias de tratamiento de datos binarios y hexadecimales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Alberto Mart&iacute;nez
 * @author Carlos Gamuci */
public final class HexUtils {

    /** Equivalencias de hexadecimal a texto por la posici&oacute;n del vector. Para
     * ser usado en <code>hexify()</code> */
    private static final char[] HEX_CHARS = {
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
    };

    private HexUtils() {
        /* Constructor privado. No se permite instanciacion en una clase de utilidades. */
    }

    /** Comprueba si dos arrays de octetos son iguales.
     * @param v Primer array de octetos
     * @param w Segundo array de octetos
     * @return <code>true</code> si los arrays son iguales, <code>false</code> en caso contrario */
    public static boolean arrayEquals(final byte[] v, final byte[] w) {
        return HexUtils.arrayEquals(v, 0, v.length, w, 0, w.length);
    }

    /** Comprueba si dos arrays de octetos son iguales.
     * @param v Primer array de octetos
     * @param vOffset Desplazamiento (<i>offset</i>) de inicio para el primer array
     * @param vLen Longitud de los datos en el primer array
     * @param w Segundo array de octetos
     * @param wOffset Desplazamiento (<i>offset</i>) de inicio para el segundo array
     * @param wLen Longitud de los datos en el segundo array
     * @return <code>true</code> si los arrays son iguales en longitudes y valores comparados desde
     *         los respectivos desplazamientos, <code>false</code> en caso contrario */
    public static boolean arrayEquals(final byte[] v, final int vOffset, final int vLen, final byte[] w, final int wOffset, final int wLen) {
        if ((vLen != wLen) || (v.length < vOffset + vLen) || (w.length < wOffset + wLen)) {
            return false;
        }

        for (int i = 0; i < vLen; i++) {
            if (v[i + vOffset] != w[i + wOffset]) {
                return false;
            }
        }
        return true;
    }

    /** Obtiene un <code>short</code> a partir de un array de octetos.
     * @param data Array de octetos
     * @param offset Desplazamiento (<i>offset</i>) hasta el inicio de los datos a tratar
     * @return Valor <code>short</code> */
    public static short getShort(final byte[] data, final int offset) {
        return (short) HexUtils.getUnsignedInt(data, offset);
    }

    /** Obtiene un entero sin signo (doble octeto) a partir de un array de octetos.
     * @param data Array de octetos
     * @param offset Desplazamiento (<i>offset</i>) hasta el inicio de los datos a tratar
     * @return Valor entero sin signo (<i>2-byte unsigned int</i>) */
    public static int getUnsignedInt(final byte[] data, final int offset) {
        return ((data[offset] & 0xff) << 8) | (data[offset + 1] & 0xff);
    }


    /** Convierte un vector de octetos en una cadena de caracteres que contiene la
     * representaci&oacute;n hexadecimal. Copiado directamente de <a href="http://www.openscdp.org/ocf/api/opencard/core/util/HexString.html">
     * <code>opencard.core.util.HexString</code></a>.
     * @param abyte Array de octetos que deseamos representar textualmente
     * @param separator Indica si han de separarse o no los octetos con un gui&oacute;n y en
     *        l&iacute;neas de 16
     * @return Representaci&oacute;n textual del vector de octetos de entrada */
    public static String hexify(final byte abyte[], final boolean separator) {
        if (abyte == null) {
            return "null"; //$NON-NLS-1$
        }
        final StringBuffer stringbuffer = new StringBuffer(256);
        int i = 0;
        for (int j = 0; j < abyte.length; j++) {
            if (separator && i > 0) {
                stringbuffer.append('-');
            }
            stringbuffer.append(HexUtils.HEX_CHARS[abyte[j] >> 4 & 0xf]);
            stringbuffer.append(HexUtils.HEX_CHARS[abyte[j] & 0xf]);
            if (++i == 16) {
                if (separator) {
                    stringbuffer.append('\n');
                }
                i = 0;
            }
        }
        return stringbuffer.toString();
    }

    /** Devuelve una porci&oacute;n del array especificado.
     * @param src Array de octetos original
     * @param srcPos Posici&oacute;n de origen de la porci&oacute;n del array de octetos a obtener
     * @param length N&uacute;mero de octetos de la porci&oacute;n a obtener
     * @return Una porci&oacute;n del array especificado */
    public static byte[] subArray(final byte[] src, final int srcPos, final int length) {
        if (length == 0) {
            return null;
        }
        if (src.length < (srcPos + length)) {
            return null;
        }
        final byte[] temp = new byte[length];
        System.arraycopy(src, srcPos, temp, 0, length);
        return temp;
    }

    /** Realiza la operacion XOR entre dos array de bytes. El resultado se recortar&aacute; para
     * ser del tama&ntilde;o del primer array recibido tomando los bytes menos significativos
     * del resultado.
     * @param v Primer array de bytes
     * @param w Segundo array de bytes
     * @return Resultado del XOR de los arrays de entrada */
    public static byte[] xor(final byte[] v, final byte[] w) {

        byte[] xored = null;
        byte[] trimmedXor = null;
        xored = new BigInteger(1, v).xor(new BigInteger(1, w)).toByteArray();
        trimmedXor = new byte[v.length];
        if (xored.length >= trimmedXor.length) {
            System.arraycopy(xored, xored.length - trimmedXor.length, trimmedXor, 0, trimmedXor.length);
        }
        else {
            System.arraycopy(xored, 0, trimmedXor, trimmedXor.length - xored.length, xored.length);
        }
        return trimmedXor;
    }
}