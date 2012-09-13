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

import es.gob.jmulticard.asn1.der.Record;

/** Objeto PKCS#15 PrKDF (<i>Private Key Description File</i>) ASN.1.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PrKdf extends Record {

	private static final int BUFFER_SIZE = 150;

    /** Construye un objeto PKCS#15 PrKDF (<i>Private Key Description File</i>) ASN.1. */
	public PrKdf() {
		super(new Class[] {
			PrivateKeyObject.class,
			PrivateKeyObject.class
		});
	}

	/** Obtiene el n&uacute;mero de claves del PrKDF.
	 * @return N&uacute;mero de claves del PrKDF */
	public int getKeyCount() {
		return getElementCount();
	}

	/** Obtiene el identificador de la clave indicada.
	 * @param index &Iacute;ndice de la clave
	 * @return Identificador de la clave */
	public String getKeyIdentifier(final int index) {
		return ((PrivateKeyObject) getElementAt(index)).getKeyIdentifier();
	}
	
	/** Obtiene el nombre de la clave indicada
	 * @param index &Iacute;ndice de la clave
	 * @return Nombre de la clave */
	public String getKeyName(final int index) {
		return ((PrivateKeyObject) getElementAt(index)).getKeyName();
	}

	/** Obtiene la ruta PKCS#15 hacia la clave indicada.
	 * @param index &Iacute;ndice de la clave
	 * @return Ruta PKCS#15 hacia la clave indicada */
	public String getKeyPath(final int index) {
		return ((PrivateKeyObject) getElementAt(index)).getKeyPath();
	}

    /** {@inheritDoc} */
	public String toString() {
		final StringBuffer sb = new StringBuffer(PrKdf.BUFFER_SIZE); 
		sb.append("Fichero de Descripcion de Claves Privadas:\n"); //$NON-NLS-1$
		for (int index=0;index<getKeyCount();index++) {
			sb.append(" Clave privada "); //$NON-NLS-1$
			sb.append(Integer.toString(index));
			sb.append("\n  Nombre de la clave: "); //$NON-NLS-1$
			sb.append(getKeyName(index));
			sb.append("\n  Ruta hacia la clave: "); //$NON-NLS-1$
			sb.append(getKeyPath(index));
			sb.append('\n');
		}
		return sb.toString();
	}

}