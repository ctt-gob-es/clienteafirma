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
