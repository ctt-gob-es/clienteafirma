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

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.asn1.der.Record;

/** Objeto PKCS#15 CDF (<i>Certificate Description File</i>) ASN.1 (<i>EF.CD</i> en ISO 7816-15).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Cdf extends Record {

    private static final int BUFFER_SIZE = 150;

    /** Construye un objeto PKCS#15 CDF (<i>Certificate Description File</i>) ASN.1. */
    public Cdf() {
        super(new Class[] {
                CertificateObject.class, CertificateObject.class, CertificateObject.class
        });
    }

    /** Obtiene el n&uacute;mero de certificados del CDF.
     * @return N&uacute;mero de certificados del CDF */
    public int getCertificateCount() {
        return getElementCount();
    }

    /** Obtiene el nombre X.500 del emisor del certificado indicado.
     * @param index &Iacute;ndice del certificado
     * @return Nombre X.500 del emisor del certificado indicado */
    public String getCertificateIssuerPrincipal(final int index) {
        return ((CertificateObject) getElementAt(index)).getIssuer();
    }

    /** Obtiene el nombre X.500 del titular del certificado indicado.
     * @param index &Iacute;ndice del certificado
     * @return Nombre X.500 del titular del certificado indicado */
    public String getCertificateSubjectPrincipal(final int index) {
        return ((CertificateObject) getElementAt(index)).getSubject();
    }

    /** Obtiene el n&uacute;mero de serie del certificado indicado.
     * @param index &Iacute;ndice del certificado
     * @return N&uacute;mero de serie del certificado indicado */
    public BigInteger getCertificateSerialNumber(final int index) {
        return ((CertificateObject) getElementAt(index)).getSerialNumber();
    }

    /** Obtiene el identificador binario del certificado indicado.
     * @param index &Iacute;ndice del certificado
     * @return Identificador binario del certificado indicado */
    public byte[] getCertificateIdentifier(final int index) {
        return ((CertificateObject) getElementAt(index)).getIdentifier();
    }

    /** Obtiene la ruta PKCS#15 hacia el certificado indicado.
     * @param index &Iacute;ndice del certificado
     * @return Ruta PKCS#15 hacia el certificado indicado */
    public String getCertificatePath(final int index) {
        return ((CertificateObject) getElementAt(index)).getPath();
    }

    /** Obtiene el alias del certificado indicado.
     * @param index &Iacute;ndice del certificado
     * @return Alias del certificado indicado */
    public String getCertificateAlias(final int index) {
        return ((CertificateObject) getElementAt(index)).getAlias();
    }

    /** {@inheritDoc} */
    public String toString() {
        final StringBuffer sb = new StringBuffer(BUFFER_SIZE);
        sb.append("Fichero de Descripcion de Certificados:\n"); //$NON-NLS-1$
        for (int index = 0; index < getCertificateCount(); index++) {
            sb.append(" Certificado "); //$NON-NLS-1$
            sb.append(Integer.toString(index));
            sb.append("\n  Alias: "); //$NON-NLS-1$
            sb.append(getCertificateAlias(index));
            sb.append("\n  Titular: "); //$NON-NLS-1$
            sb.append(getCertificateSubjectPrincipal(index));
            sb.append("\n  Emisor: "); //$NON-NLS-1$
            sb.append(getCertificateIssuerPrincipal(index));
            sb.append("\n  Numero de serie: "); //$NON-NLS-1$
            sb.append(getCertificateSerialNumber(index).toString());
            sb.append("\n  Identificador: "); //$NON-NLS-1$
            sb.append(HexUtils.hexify(getCertificateIdentifier(index), true));
            sb.append("\n  Ruta PKCS#15: "); //$NON-NLS-1$
            sb.append(getCertificatePath(index));
            sb.append('\n');
        }
        return sb.toString();
    }
}
