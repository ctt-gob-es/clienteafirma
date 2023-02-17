package es.gob.afirma.standalone.plugins;

/**
 * Permisos disponibles para las aplicaciones.
 */
public enum Permission {
	/** Permiso de instalaci&oacute;n. */
	INSTALL(1),
	/** Permiso para reiniciar la aplicaci&oacute;n. */
	RESET(3),
	/** Permiso para procesar los datos antes de la firma. */
	PRESIGN(4),
	/** Permiso para procesar los datos despu&eacute;s de la firma. */
	POSTSIGN(5),
	/** Permiso para agregar botones a la interfaz. */
	BUTTONS(6),
	/** Permiso para agregar un menu a la interfaz. */
	MENU(7),
	/** Permiso para agregar nuevos comandos a la l&iacute;nea de comandos. */
	COMMANDS(8),
	/** Permiso para manipular el proceso de firma cuando se invoca desde una fuente externa. */
	INLINE_PROCESS(9);

	private int order;

	/**
	 * Construye el permiso.
	 * @param order N&uacute;mero de orden.
	 */
	private Permission(final int order) {
		this.order = order;
	}

	/**
	 * Obtiene el n&uacute;mero de orden del permiso.
	 * @return N&uacute;mero de orden.
	 */
	public int getOrder() {
		return this.order;
	}

	/**
	 * Identifica un permiso a partir de su nombre.
	 * @param name Nombre del permiso.
	 * @return Permiso referenciado.
	 */
	public static Permission forName(final String name) {
		for (final Permission permission : values()) {
			if (permission.toString().equalsIgnoreCase(name)) {
				return permission;
			}
		}
		throw new IllegalArgumentException("Permiso no reconocido: " + name); //$NON-NLS-1$
	}
}
