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
