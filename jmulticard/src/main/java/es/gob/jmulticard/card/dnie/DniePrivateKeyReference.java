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
package es.gob.jmulticard.card.dnie;

import es.gob.jmulticard.card.Location;
import es.gob.jmulticard.card.PrivateKeyReference;

/** Clave privada de un DNIe. La clase no contiene la clave privada en si, sino una referencia a ella
 * y una referencia al propio DNIe, con el canal seguro establecido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DniePrivateKeyReference implements PrivateKeyReference {

	private final Dnie dnieCard;

	private final String identifier;

	private final Location keyPath;

	private final String label;

	/**
	 * Crear una referencia a una clave privada del DNIe.
	 * @param dnieCard Tarjeta en la que se almacena la clave privada.
	 * @param identifier Identificador de la clave.
	 * @param keyPath Ruta interna de la clave.
	 * @param label Etiqueta de la clave.
	 */
	public DniePrivateKeyReference(final Dnie dnieCard, final String identifier, final Location keyPath, final String label) {
		this.dnieCard = dnieCard;
		this.identifier = identifier;
		this.keyPath = keyPath;
		this.label = label;
	}

	/**
	 * Recupera el manejador de la tarjeta en la que se almacena la clave.
	 * @return Manejador de la tarjeta.
	 */
	public Dnie getDnieCard() {
		return this.dnieCard;
	}

	/**
	 * Recupera el identificador de la clave.
	 * @return Identificador de la clave.
	 */
	public String getIdentifier() {
		return this.identifier;
	}

	/**
	 * Recupera la ruta de la clave.
	 * @return Ruta de la clave.
	 */
	public Location getKeyPath() {
		return this.keyPath;
	}

	/**
	 * Recupera la etiqueta de la clave.
	 * @return Etiqueta de la clave.
	 */
	public String getLabel() {
		return this.label;
	}
}
