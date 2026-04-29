package es.gob.afirma.core.misc.protocol;

/**
 * Identificadores de las versiones de protocolo en las que se introducen cambios
 * que rompen compatibilidad. Estos identificadores no coindicen con la versi&oacute;n
 * de la aplicaci&oacute;n, sino con la versi&oacute;n del protocolo.
 * Dos versiones de protocolo son compatibles si tienen la misma version mayor. Las
 * versiones menores se utilizan para definir comportamiento que aportan cambios
 * pero que no necesariamente rompen compatibilidad.
 * <ul>
 * <li><b>Versi&oacute;n 0:</b> versi&oacute;n inicial.</li>
 * <li><b>Versi&oacute;n 1:</b> introduce cambios en el cifrado de los datos para el servidor intermedio.</li>
 * <li><b>Versi&oacute;n 2:</b> realiza cambios en la codificaci&oacute;n del par&aacute;metro keystore.</li>
 * <li><b>Versi&oacute;n 3:</b> devuelve el nombre del fichero seleccionado en las operaciones de firma.</li>
 * <li><b>Versi&oacute;n 4:</b> seguridad adicional en la comunicacion por WebSockets.</li>
 * <li><b>Versi&oacute;n 4.1:</b> evoluci&oacute;n de los c&oacute;digos de error devueltos por la
 * 		aplicaci&oacute;n y ejecuci&oacute;n as&iacute;ncrona de las tareas por websocket.</li>
 * </ul>
 */
public class ProtocolVersion {

	/** Versi&oacute;n inicial. */
	public static final String VERSION_0 = "0"; //$NON-NLS-1$
	/** Introduce cambios en el cifrado de los datos para el servidor intermedio. */
	public static final String VERSION_1 = "1"; //$NON-NLS-1$
	/** Realiza cambios en la codificaci&oacute;n del par&aacute;metro keystore. */
	public static final String VERSION_2 = "2"; //$NON-NLS-1$
	/** Devuelve el nombre del fichero seleccionado en las operaciones de firma. */
	public static final String VERSION_3 = "3"; //$NON-NLS-1$
	/** Seguridad adicional en la comunicacion por WebSockets. */
	public static final String VERSION_4 = "4"; //$NON-NLS-1$
	/** Nuevos c&oacute;digos de error y ejecuci&oacute;n as&iacute;ncrona de las tareas por websocket. */
	public static final String VERSION_4_1 = "4.1"; //$NON-NLS-1$

	private final int majorVersion;
	private final int minorVersion;

	private ProtocolVersion(final int majorVersion) {
		this(majorVersion, 0);
	}

	private ProtocolVersion(final int majorVersion, final int minorVersion) {
		this.majorVersion = majorVersion;
		this.minorVersion = minorVersion;
	}

	/**
	 * Devuelve el identificador de versi&oacute;n principal asociado.
	 * @return Identificador de versi&oacute;n.
	 */
	public int getMajorVersion() {
		return this.majorVersion;
	}

	/**
	 * Devuelve el identificador de versi&oacute;n secundario asociado.
	 * @return Identificador de versi&oacute;n.
	 */
	public int getMinorVersion() {
		return this.minorVersion;
	}

	/**
	 * Construye una versi&oacute;n de protocolo. Debe tener la forma "X" o "X.Y", en donde
	 * X es uno o m&aacute;s digitos que indican la versi&oacute;n principal e Y uno o m&aacute;s
	 * d&iacute;gitos que indican la subversi&oacute;n.
	 * @param protocolVersion Identificador de la versi&oacute;n.
	 * @return La versi&oacute;n.
	 */
	public static ProtocolVersion getInstance(final String protocolVersion) {
		int v1, v2;
		try {
			final int sepIdx = protocolVersion.indexOf("."); //$NON-NLS-1$
			if (sepIdx > 0) {
				v1 = Integer.parseUnsignedInt(protocolVersion.substring(0, sepIdx));
				v2 = Integer.parseUnsignedInt(protocolVersion.substring(sepIdx + 1));
			}
			else {
				v1 = Integer.parseUnsignedInt(protocolVersion);
				v2 = 0;
			}
		}
		catch (final Exception e) {
			throw new IllegalArgumentException("El identificador de la version de protocolo indicada no es valido", e); //$NON-NLS-1$
		}
		return new ProtocolVersion(v1, v2);
	}

	/**
	 * Comprueba si esta versi&oacute;n es compatible con otra. Dos versiones de protocolo son
	 * compatibles cuando tienen la misma versi&oacute;n mayor. Este m&eacute;todo se utiliza
	 * para saber si esta versi&oacute;n de la aplicaci&oacute;n es compatible con la
	 * aplicaci&oacute;n que solicita la operaci&oacute;n. Si la aplicaci&oacute;n no soporta esta
	 * versi&oacute;n, no podr&aacute; realizar la operaci&oacute;n.
	 * @param version Versi&oacute;n de protocolo con la que comparar.
	 * @return {@code true} si son compatibles, {@code false} en caso contrario.
	 */
	public boolean isCompatibleWith(final ProtocolVersion version) {
		return this.majorVersion == version.getMajorVersion();
	}

	/**
	 * Comprueba si se soporta la funcionalidad concreta solicitada. Este m&eacute;todo comprueba
	 * que tanto la versi&oacute;n mayor sea igual como que la menor sea igual o mayor que la
	 * solicitada. Se utiliza para saber si la aplicaci&oacute;n que invoca la operaci&oacute;n
	 * est&aacute; preparada para que apliquemos un formato, configuraci&oacute;n o comportamiento
	 * concreto en la operaci&oacute;n o su resultado. Si la aplicaci&oacute;n que invoca soporta
	 * el comportamiento, lo aplicamos. Si no, se ejecutar&aacute; en "modo compatibilidad" para
	 * que la aplicaci&oacute;n que nos invoca pueda tratar el resultado.
	 * @param version Versi&oacute;n de la aplicaci&oacute;n.
	 * @return {@code true} si esta version de protocolo es mayor o igual que la indicad,
	 * {@code false} en caso contrario.
	 */
	public boolean hasSupportTo(final ProtocolVersion version) {
		return this.majorVersion > version.getMajorVersion()
				? true
				: this.majorVersion == version.getMajorVersion()
					&& this.minorVersion >= version.getMinorVersion();
	}

	@Override
	public String toString() {
		String v = Integer.toString(this.majorVersion);
		if (this.minorVersion != 0) {
			v += "." + Integer.toString(this.minorVersion); //$NON-NLS-1$
		}
		return v;
	}
}
