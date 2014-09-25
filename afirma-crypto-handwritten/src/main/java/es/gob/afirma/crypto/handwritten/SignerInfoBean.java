package es.gob.afirma.crypto.handwritten;

import javax.xml.bind.annotation.XmlElement;

/** Datos del firmante.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignerInfoBean {

	@XmlElement(name = "signerName")
	private final String name;

	@XmlElement(name = "signerSurname1")
	private final String surname1;

	@XmlElement(name = "signerSurname2")
	private final String surname2;

	@XmlElement(name = "signerId")
	private final String id;

	@Override
	public String toString() {
		return this.name + " " + this.surname1 + " " + (this.surname2 != null ? this.surname2 : "") + " (NIF: " + this.id + ")";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
	}

	/** Crea los datos del firmante.
	 * @param n Nombre.
	 * @param s1 Primer apellido.
	 * @param s2 Segundo apellido.
	 * @param nif NIF. */
	public SignerInfoBean(final String n, final String s1, final String s2, final String nif) {
		this.name = n;
		this.surname1 = s1;
		this.surname2 = s2;
		this.id = nif;
	}

	/** Obtiene el nombre del firmante.
	 * @return Nombre del firmante. */
	public String getName() {
		return this.name;
	}

	/** Obtiene el primer apellido del firmante.
	 * @return Primer apellido del firmante. */
	public String getSurname1() {
		return this.surname1;
	}

	/** Obtiene el segundo apellido del firmante.
	 * @return Segundo apellido del firmante. */
	public String getSurname2() {
		return this.surname2;
	}

	/** Obtiene el NIF del firmante.
	 * @return NIF del firmante. */
	public String getId() {
		return this.id;
	}

}
