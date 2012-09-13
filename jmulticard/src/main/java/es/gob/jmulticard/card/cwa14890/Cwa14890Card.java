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
package es.gob.jmulticard.card.cwa14890;

import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.interfaces.RSAPrivateKey;

import es.gob.jmulticard.apdu.connection.ApduConnectionException;

/** Interfaz con los m&eacute;todos necesarios para la operaci&oacute;n de las tarjetas
 * acordes a la especificaci&oacute;n CWA-14890.
 * @author Carlos Gamuci */
public interface Cwa14890Card {

    /** Verifica la CA intermedia del certificado de componente de la tarjeta.
     * @throws CertificateException Cuando ocurre alg&uacute;n problema en la
     *         validaci&oacute;n del certificado
     * @throws IOException Cuando ocurre alg&uacute;n problema en la selecci&oacute;n
     *         y lectura del certificado
     * @throws SecurityException Si falla la validaci&oacute;n de la CA */
    void verifyCaIntermediateIcc() throws CertificateException, IOException;

    /** Verifica el certificado de componente de la tarjeta.
     * @throws CertificateException Cuando ocurre alg&uacute;n problema en la
     *         validaci&oacute;n del certificado
     * @throws IOException Cuando ocurre alg&uacute;n problema en la selecci&oacute;n
     *         y lectura del certificado
     * @throws SecurityException Si falla la validaci&oacute;n del certificado */
    void verifyIcc() throws CertificateException, IOException;

    /** Recupera el certificado de componente codificado.
     * @return Certificado codificado
     * @throws IOException Cuando ocurre alg&uacute;n problema en la selecci&oacute;n
     *         y lectura del certificado */
    byte[] getIccCertEncoded() throws IOException;

    /** Verifica que los certificados declarados por el controlador (certificados de
     * Terminal) sean v&aacute;lidos para el uso de la tarjeta.
     * @throws ApduConnectionException Cuando ocurre alg&iacute;n error en la
     *         comunicaci&oacute;n con la tarjeta
     * @throws es.gob.jmulticard.apdu.connection.cwa14890.SecureChannelException Cuando ocurre alg&uacute;n error en la
     *         verificaci&oacute;n de los certificados */
    void verifyIfdCertificateChain() throws ApduConnectionException;

    /** Obtiene el mensaje de autenticaci&oacute;n interna de la tarjeta.
     * @param randomIfd Bytes aleatorios generados
     * @param chrCCvIfd CHR de la clave p&uacute;blica del certificado de Terminal
     * @return Mensaje cifrado con la clave privada de componente de la tarjeta
     * @throws ApduConnectionException Cuando ocurre un error de comunicaci&oacute;n con la tarjeta */
    byte[] getInternalAuthenticateMessage(final byte[] randomIfd, final byte[] chrCCvIfd) throws ApduConnectionException;

    /** Envia el mensaje de autenticaci&oacute;n externa.
     * @param extAuthenticationData Mensaje de autenticaci&oacute;n externa
     * @return {@code true} si la autenticaci&oacute;n finaliz&oacute; correctamente, {@code false} en caso contrario
     * @throws ApduConnectionException Cuando ocurre un error en la comunicaci&oacute;n con
     *         la tarjeta */
    boolean externalAuthentication(final byte[] extAuthenticationData) throws ApduConnectionException;

    /** Establece una clave p&uacute;blica y otra privada para la autenticaci&oacute;n
     * interna y externa de la tarjeta.
     * @param refPublicKey Referencia a la clave publica
     * @param refPrivateKey Referencia a la clave privada
     * @throws es.gob.jmulticard.apdu.connection.cwa14890.SecureChannelException Cuando ocurre un error durante el proceso de autenticaci&oacute;n
     * @throws ApduConnectionException Cuando ocurre un error de comunicaci&oacute;n con la tarjeta */
    void setKeysToAuthentication(final byte[] refPublicKey, final byte[] refPrivateKey) throws ApduConnectionException;

    /** Solicita un desaf&iacute;o de 8 bytes a la tarjeta.
     * @return Array de 8 bytes aleatorios
     * @throws ApduConnectionException Cuando ocurre un error de comunicaci&oacute;n con la tarjeta */
    byte[] getChallenge() throws ApduConnectionException;

    /** Recupera el numero de serie de la tarjeta.
     * @return N&uacute;mero de serie
     * @throws ApduConnectionException Cuando ocurre un error en la comunicaci&oacute;n con
     *         la tarjeta */
    byte[] getSerialNumber() throws ApduConnectionException;

    /** Recupera la referencia a la clave privada del certificado de Componente.
     * @return Referencia a clave privada */
    byte[] getRefIccPrivateKey();

    /** Recupera el CHR de la clave publica del certificado de Terminal.
     * @return Referencia a clave p&uacute;blica */
    byte[] getChrCCvIfd();

    /** Recupera la clave privada del certificado de componente de la tarjeta.
     * @return Clave privada */
    RSAPrivateKey getIfdPrivateKey();
}