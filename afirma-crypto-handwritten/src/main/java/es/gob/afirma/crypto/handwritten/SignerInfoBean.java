package es.gob.afirma.crypto.handwritten;

/** Datos del firmante.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignerInfoBean {

	private final String name;
	private final String surname1;
	private final String surname2;
	private final String id;

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
