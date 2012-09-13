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

import es.gob.jmulticard.asn1.der.DerInteger;
import es.gob.jmulticard.asn1.der.Sequence;
import es.gob.jmulticard.asn1.der.x509.RdnSequence;

/** Tipo ASN.1 PKCS#15 <i>X509CertificateAttributes</i>.
 * <pre>
 *  X509CertificateAttributes ::= SEQUENCE {
 *    value ObjectValue { Certificate },
 *    subject Name,
 *    issuer [0] Name,
 *    serialNumber INTEGER
 *  }
 *  Name ::= CHOICE {
 *    rdnSequence RDNSequence
 *  }
 * </pre>
 * @author Gonzalo Henr&iacute;quez Manzano */
public final class X509CertificateAttributes extends Sequence {

	/** Crea un objeto ASN.1 PKCS#15 <i>X509CertificateAttributes</i>. */
	public X509CertificateAttributes() {
		super(new Class[] {
			Path.class,
			RdnSequence.class,
			CertificateIssuerContextSpecific.class,
			DerInteger.class
		});
	}

    /** Proporciona el nombre X.500 del emisor del certificado
     * @return Nombre X.500 del emisor del certificado */
    String getIssuer() {
        return getElementAt(2).toString();
    }

    /** Proporciona el nombre X.500 del titular del certificado
     * @return Nombre X.500 del emisor del certificado */
    String getSubject() {
        return getElementAt(1).toString();
    }

    /** Devuelve la ruta del certificado.
     * @return Ruta (<i>path</i>) del certificado */
    String getPath() {
        return ((Path)getElementAt(0)).getPathString();
    }

    /** Obtiene el n&uacute;mero de serie del Certificado.
     * @return N&uacute;mero de serie del Certificado */
    BigInteger getSerialNumber() {
    	return ((DerInteger)getElementAt(3)).getIntegerValue();
    }

    /** {@inheritDoc} */
    public String toString() {
    	return "Atributos del certificado\n" + //$NON-NLS-1$
			" Ruta: " + getPath() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
			" Titular: " + getSubject() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
			" Emisor: " + getIssuer() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
			" Numero de serie: " + getSerialNumber().toString(); //$NON-NLS-1$
    }

}
