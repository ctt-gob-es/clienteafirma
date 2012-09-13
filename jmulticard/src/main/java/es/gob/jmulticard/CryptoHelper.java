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
package es.gob.jmulticard;

import java.io.IOException;
import java.security.Key;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;

/** Funcionalidades criptogr&aacute;ficas de utilidad que pueden variar entre
 * JSE/JME/Dalvik.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface CryptoHelper {

    /** Realiza una huella digital de los datos proporcionados.
     * @param algorithm Algoritmo de huella digital que debe utilizarse
     * @param data Datos de entrada
     * @return Huella digital de los datos
     * @throws IOException Si ocurre alg&uacute;n problema generando la huella
     *         digital */
    byte[] digest(final String algorithm, final byte[] data) throws IOException;

    /** Encripta datos mediante Triple DES (modo CBC sin relleno) y con un
     * salto de (IV) de 8 bytes a cero. Si se le indica una clave de 24 bytes,
     * la utilizar&aacute;a tal cual. Si se le indica una clave de 16 bytes,
     * duplicar&aacute; los 8 primeros y los agregar&aacute; al final para
     * obtener una de 24.
     * @param data Datos a encriptar
     * @param key Clave 3DES de cifrado
     * @return Datos cifrados
     * @throws IOException Si ocurre alg&uacute;n problema durante el
     *         encriptado */
    byte[] desedeEncrypt(final byte[] data, final byte[] key) throws IOException;

    /** Desencripta datos mediante Triple DES (modo CBC sin relleno) y con un
     * salto de (IV) de 8 bytes a cero. Si se le indica una clave de 24 bytes,
     * la utilizar&aacute;a tal cual. Si se le indica una clave de 16 bytes,
     * duplicar&aacute; los 8 primeros y los
     * agregar&aacute; al final para obtener una de 24.
     * @param data Datos a desencriptar
     * @param key Clave 3DES de descifrado
     * @return Datos descifrados
     * @throws IOException Si ocurre alg&uacute;n problema durante el
     *         desencriptado */
    byte[] desedeDecrypt(final byte[] data, final byte[] key) throws IOException;

    /** Encripta datos mediante DES (modo ECB sin relleno).
     * @param data Datos a encriptar
     * @param key Clave DES de cifrado
     * @return Datos cifrados
     * @throws IOException Si ocurre alg&uacute;n problema durante el
     *         encriptado */
    byte[] desEncrypt(final byte[] data, final byte[] key) throws IOException;

    /** Desencripta datos mediante DES (modo ECB sin relleno).
     * @param data Datos a desencriptar
     * @param key Clave DES de descifrado
     * @return Datos descifrados
     * @throws IOException Si ocurre alg&uacute;n problema durante el
     *         desencriptado */
    byte[] desDecrypt(final byte[] data, final byte[] key) throws IOException;

    /** Desencripta datos mediante RSA.
     * @param cipheredData Datos a desencriptar
     * @param key Clava RSA de descifrado
     * @return Datos descifrados
     * @throws IOException Si ocurre alg&uacute;n problema durante el
     *         desencriptado */
    byte[] rsaDecrypt(final byte[] cipheredData, final Key key) throws IOException;

    /** Encripta datos mediante RSA.
     * @param data Datos a encriptar
     * @param key Clava RSA de cifrado
     * @return Datos encriptados
     * @throws IOException Si ocurre alg&uacute;n problema durante el
     *         encriptado */
    byte[] rsaEncrypt(final byte[] data, final Key key) throws IOException;

    /** Genera un certificado del tipo indicado a partir de su
     * codificaci&oacute;n.
     * @param encode Codificaci&oacute;n del certificado
     * @return Certificado generado
     * @throws CertificateException Si ocurre alg&uacute;n problema durante la
     *         generaci&oacute;n */
    Certificate generateCertificate(byte[] encode) throws CertificateException;

    /** Genera un aleatorio contenido en un array de bytes.
     * @param numBytes N&uacute;mero de bytes aleatorios que generar
     * @return Array de bytes aleatorios
     * @throws IOException Si ocurre alg&uacute;n problema durante la
     *         generaci&oacute;n
     *         del aleatorio */
    byte[] generateRandomBytes(int numBytes) throws IOException;
}
