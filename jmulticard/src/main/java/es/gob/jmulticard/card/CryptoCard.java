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
package es.gob.jmulticard.card;

import java.security.cert.X509Certificate;

/** Operaciones comunes a todas las tarjetas criptogr&aacute;ficas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface CryptoCard {

    /** Obtiene los alias de los certificados de la tarjeta.
     * @return Array con los alias de los certificados de la tarjeta
     * @throws CryptoCardException Si ocurre alg&uacute;n problema al recuperar los alias. */
    String[] getAliases() throws CryptoCardException;

    /** Obtiene el certificado correspondiente al alias proporcionado.
     * @param alias Alias del certificado
     * @return Certificado correspondiente al alias proporcionado o <code>null</code> si no
     *         existe ning&uacute;n certificado con ese alias
     * @throws CryptoCardException Si ocurre alg&uacute;n problema al recuperar el certificado.
     * @throws es.gob.jmulticard.card.AuthenticationModeLockedException Cuando el DNIe est&aacute; bloqueado.
     * @throws es.gob.jmulticard.ui.passwordcallback.CancelledOperationException Cuando el usuario
     * cancela la operaci&oacute;n o el di&aacute;logo de inserci&oacute;n de contrase&ntilde;a. */
    X509Certificate getCertificate(final String alias) throws CryptoCardException;

    /** Obtiene una referencia a la clave privada correspondiente al alias proporcionado.
     * @param alias Alias del certificado
     * @return Referencia a la clave privada correspondiente al alias proporcionado o
     * 		   <code>null</code> si no existe ninguna clave privada con ese alias
     * @throws CryptoCardException Si ocurre alg&uacute;n problema al recuperar la clave privada.
     */
    PrivateKeyReference getPrivateKey(final String alias) throws CryptoCardException;

    /** Realiza una firma electr&oacute;nica.
     * @param data Datos a firmar
     * @param algorithm Algoritmo de firma
     * @param keyRef Referencia a la clave privada de firma
     * @return Datos firmados (PKCS#1 v1.5)
     * @throws es.gob.jmulticard.card.AuthenticationModeLockedException Cuando el DNIe est&aacute; bloqueado.
     * @throws CryptoCardException Si ocurre alg&uacute;n problema durante la firma.
     * @throws es.gob.jmulticard.ui.passwordcallback.CancelledOperationException Cuando el usuario
     * cancela la operaci&oacute;n o el di&aacute;logo de inserci&oacute;n de contrase&ntilde;a. */
    byte[] sign(byte[] data, String algorithm, PrivateKeyReference keyRef) throws CryptoCardException;
}