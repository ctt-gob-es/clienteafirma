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

/* 
 * Se ha modificado el archivo para adaptar la clase a las necesidades de la aplicación.
 */

/*
   Copyright Isaac Levin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */
/*
   Copyright Isaac Levin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */	
package es.gob.jmulticard.asn1.bertlv;


/** Clase de utilidad para la manipulaci&oacute;n de bits y octetos.
 * @author Isaac Levin */
final class BitManipulationHelper {

	private BitManipulationHelper() {
		// No permitimos la instancacion
	}

	/** Obtiene el valor del bit de la posici&oacute;n indicada.
	 * @param position Posici&oacute;n del bit, empezando desde 1 */
	static boolean getBitValue(final int value, final int position) {

		if (position > 32) {
			throw new BerParsingException("No se puede obtener el valor del bit de la posicion  " //$NON-NLS-1$
					+ position + ", un entero en Java tiene solo 32 bits"); //$NON-NLS-1$
		}
		int bitPosition = position;
		bitPosition--; // Lo pasamos a contador desde 0
		final int mask = 1 << bitPosition;
		return (value & mask) == 0 ? false : true;
	}

	/** Establece el valor del bit de la posici&oacute;n indicada.
	 * @param position Posici&oacute;n del bit, empezando desde 1 */
	static int setBitValue(final int value, final int position, final boolean bitValue) {
		if (position > 32) {
			throw new BerParsingException("No se puede establecer el valor del bit de la posicion  " //$NON-NLS-1$
					+ position + ", un entero en Java tiene solo 32 bits"); //$NON-NLS-1$
		}
		int bitPosition = position;
		bitPosition--; // Lo pasamos a contador desde 0
		final int mask = 1 << bitPosition;
		if (bitValue) {
			// Lo establecemos a 1
			return (value | mask);
		}
		// Lo establecemos a 0
		return (value & ~mask);
	}

	static byte[] mergeArrays(final byte[] buf1, final byte[] buf2) {
		final byte[] resBuf = new byte[buf1.length + buf2.length];
		System.arraycopy(buf1, 0, resBuf, 0, buf1.length);
		System.arraycopy(buf2, 0, resBuf, buf1.length, buf2.length);
		return resBuf;
	}
}
