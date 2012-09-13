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
