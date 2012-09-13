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