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
package es.gob.jmulticard.apdu.iso7816four;

import es.gob.jmulticard.apdu.CommandApdu;

/** APDU ISO 7816-4 mediante la que se pide a la tarjeta que demuestre que posee la
 * clave privada de su certificado de componente. *
 * @author Carlos Gamuci Mill&aacute;n */
public final class InternalAuthenticateApduCommand extends CommandApdu {

	/** Byte de instrucci&oacute;n de la APDU. */
	private static final byte INS_INTERNAL_AUTHENTICATE = (byte) 0x88;

	/** Valor para indicar que no se dispone de informaci&oacute;n al respecto. */
	private static final byte NO_INFORMATION_GIVEN = (byte) 0x00;

	/** Crea un objeto para la auitenticaci&oacute;n interna.
	 * @param cla Clase (CLA) de la APDU
	 * @param randomBytes Array de 8 bytes aleatorios para la autenticaci&oacute;n
	 * @param privateKeyRef Referencia a la clave privada del certificado a autenticar
	 */
	public InternalAuthenticateApduCommand(final byte cla, final byte[] randomBytes, final byte[] privateKeyRef) {
		super(
			cla,									// CLA
			INS_INTERNAL_AUTHENTICATE,				// INS
			NO_INFORMATION_GIVEN,					// P1
			NO_INFORMATION_GIVEN,					// P2
			buildData(randomBytes, privateKeyRef),	// Data
			null									// Le
		);
	}

	private static byte[] buildData(final byte[] randomBytes, final byte[] privateKeyRef) {
		final byte[] ret = new byte[randomBytes.length + privateKeyRef.length];
		// Numero aleatorio (8 bytes)
		System.arraycopy(randomBytes, 0, ret, 0, randomBytes.length);
		// Referencia a la clave privada
		System.arraycopy(privateKeyRef, 0, ret, 0 + randomBytes.length, privateKeyRef.length);
		return ret;
	}
}