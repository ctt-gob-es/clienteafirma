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
package es.gob.jmulticard.apdu;

import java.io.ByteArrayOutputStream;

/**
 * Comando APDU para comunicaci&oacute;n con tarjeta inteligente.
 * 
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public abstract class CommandApdu extends Apdu {

	private final byte cla;
	private final byte ins;
	private final byte p1;
	private final byte p2;
	private Integer le;
	private final byte[] body;

	protected CommandApdu(final byte cla, final byte ins, final byte param1,
			final byte param2, final byte[] data, final Integer ne) {
		super();
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();

		this.cla = cla;
		baos.write(cla);

		this.ins = ins;
		baos.write(ins);

		this.p1 = param1;
		baos.write(param1);

		this.p2 = param2;
		baos.write(param2);

		if (data == null) {
			this.body = null;
		} else {
			this.body = new byte[data.length];
			System.arraycopy(data, 0, this.body, 0, data.length);

			baos.write(Integer.valueOf(String.valueOf(this.body.length))
					.byteValue());

			if (this.body.length > 0) {
				try {
					baos.write(this.body);
				} catch (final Exception e) {
					throw new IllegalArgumentException(
							"No se pueden tratar los datos de la APDU: " + e); //$NON-NLS-1$
				}
			}
		}
		this.le = ne;
		if (ne != null) {
			baos.write(ne.byteValue());
		}
		setBytes(baos.toByteArray());
	}

	/**
	 * Devuelve la clase (CLA) de APDU.
	 * 
	 * @return Clase (CLA) de APDU
	 */
	public byte getCla() {
		return this.cla;
	}

	/**
	 * Obtiene el cuerpo de la APDU.
	 * 
	 * @return Cuerpo de la APDU, o <code>null</code> si no est&aacute;
	 *         establecido
	 */
	public byte[] getData() {
		if (this.body == null) {
			return null;
		}
		final byte[] out = new byte[this.body.length];
		System.arraycopy(this.body, 0, out, 0, this.body.length);
		return out;
	}

	/**
	 * Devuelve el octeto identificador de la instrucci&oacute;n (INS) que esta
	 * APDU representa.
	 * 
	 * @return Identificador de instrucci&oacute;n
	 */
	public byte getIns() {
		return this.ins;
	}

	/**
	 * Obtiene el n&uacute;mero m&aacute;ximo de octetos esperados en la APDU de
	 * respuesta.
	 * 
	 * @return N&uacute;mero m&aacute;ximo de octetos esperados en la APDU de
	 *         respuesta, o <code>null</code> si no est&aacute; establecido
	 */
	public Integer getLe() {
		return this.le;
	}

	/**
	 * Devuelve el primer par&aacute;metro (P1) de la APDU.
	 * 
	 * @return Primer par&aacute;metro (P1) de la APDU
	 */
	public byte getP1() {
		return this.p1;
	}

	/**
	 * Devuelve el segundo par&aacute;metro (P2) de la APDU.
	 * 
	 * @return Segundo par&aacute;metro (P2) de la APDU
	 */
	public byte getP2() {
		return this.p2;
	}

	/**
	 * Establece el n&uacute;mero de octetos esperados en la APDU de respuesta.
	 * 
	 * @param le
	 *            N&uacute;mero esperado de octetos.
	 */
	public void setLe(final int le) {
		this.le = Integer.valueOf(String.valueOf(le));
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		baos.write(this.cla);
		baos.write(this.ins);
		baos.write(this.p1);
		baos.write(this.p2);
		if (this.body != null && this.body.length > 0) {
			try {
				baos.write(this.body);
			} catch (final Exception e) {
				throw new IllegalArgumentException(
						"No se pueden tratar los datos de la APDU: " + e); //$NON-NLS-1$
			}
		}
		baos.write(le);
		setBytes(baos.toByteArray());
	}
}