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
package es.gob.jmulticard.apdu.iso7816eight;

import es.gob.jmulticard.apdu.CommandApdu;

/** APDU ISO 7816-8 para la realizaci&oacute;n de una operaci&oacute;n de seguridad, m&aacute;s
 * concretamente la verificaci&oacute;n de un certificado.
 * @author Carlos Gamuci Mill&aacute;n */
public final class PsoVerifyCertificateApduCommand extends CommandApdu {

	/** Byte de instrucci&oacute;n de la APDU. */
	private static final byte INS_PERFORM_SECURITY_OPERATION = (byte) 0x2a;

	/** Byte para indicar que la respuesta no tiene datos. */
	private static final byte DATA_FIELD_RESPONSE_EMPTY = (byte) 0x00;

	/** Byte para indicar que se proporciona un certificado para su verificaci&oacute;n. */
	private static final byte DATA_FIELD_COMMAND_VERIFY_CERTIFICATE = (byte) 0xae;

	/** Crea una APDU de verificaci&oacute;n de certificado.
	 * @param cla Clase (CLA) de la APDU
	 * @param certEncoding Certificado a verificar */
	public PsoVerifyCertificateApduCommand(final byte cla, final byte[] certEncoding) {
		super(
			cla,									// CLA
			INS_PERFORM_SECURITY_OPERATION, 		// INS
			DATA_FIELD_RESPONSE_EMPTY, 				// P1
			DATA_FIELD_COMMAND_VERIFY_CERTIFICATE,	// P2
			certEncoding,							// Data
			null									// Le
		);
	}
}