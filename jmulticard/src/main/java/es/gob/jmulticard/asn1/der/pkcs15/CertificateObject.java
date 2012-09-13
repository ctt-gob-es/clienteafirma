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
package es.gob.jmulticard.asn1.der.pkcs15;

import java.math.BigInteger;


/** Tipo PKCS#15 ASN.1 <i>CertificateObject</i> (<i>CertificateInfoObject</i> en ISO 7816-15).
 *  <pre>
 *    CertificateObject {CertAttributes} ::= PKCS15Object {
 *      CommonObjectAttributes,
 *      CommonCertificateAttributes,
 *      X509CertificateAttributes
 *    }
 *  </pre>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateObject extends Pkcs15Object {

	/** Crea un objeto <i>CertificateObject</i>. */
	public CertificateObject() {
		super(
			CommonCertificateAttributes.class,
			X509CertificateAttributesContextSpecific.class
		);
	}

	/** Proporciona el nombre X.500 del emisor del certificado
     * @return Nombre X.500 del emisor del certificado */
    String getIssuer() {
    	return ((X509CertificateAttributesContextSpecific)getTypeAttributes()).getIssuer();
    }

    /** Proporciona el nombre X.500 del titular del certificado
     * @return Nombre X.500 del emisor del certificado */
    String getSubject() {
    	return ((X509CertificateAttributesContextSpecific)getTypeAttributes()).getSubject();
    }

    /** Devuelve la ruta del certificado.
     * @return Ruta (<i>path</i>) del certificado */
    String getPath() {
    	return ((X509CertificateAttributesContextSpecific)getTypeAttributes()).getPath();
    }

    /** Obtiene el n&uacute;mero de serie del Certificado.
     * @return N&uacute;mero de serie del Certificado */
    BigInteger getSerialNumber() {
    	return ((X509CertificateAttributesContextSpecific)getTypeAttributes()).getSerialNumber();
    }

	/** Obtiene el identificador binario del certificado.
	 * @return Identificador del certificado */
	byte[] getIdentifier() {
		return ((CommonCertificateAttributes) getClassAttributes()).getId();
	}

	/** Obtiene el alias del certificado.
	 * @return Alias del certificado */
	String getAlias() {
		return getCommonObjectAttributes().getLabel();
	}

    /** {@inheritDoc} */
    public String toString() {
    	return getTypeAttributes().toString() +
			"\nAlias del certificado: " + getCommonObjectAttributes().getLabel() + //$NON-NLS-1$
			"\nIdentificador del certificado: " + getClassAttributes().toString(); //$NON-NLS-1$
    }

}
