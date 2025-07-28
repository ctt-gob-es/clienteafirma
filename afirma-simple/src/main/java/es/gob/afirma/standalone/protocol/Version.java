package es.gob.afirma.standalone.protocol;

import java.util.ArrayList;
import java.util.List;

/**
 * C&oacute;digo de vers&oacute;n de Autofirma con el que comparar versiones.
 */
public class Version {

	private final List<Integer> versionParts;

	private final String aditionalText;

	/**
	 * Construye la informaci&oacute;n de version a partir de la cadena de texto de versi&oacute;n.
	 * @param legibleVersion Cadena de texto legible con la versi&oacute;n.
	 * @throws IllegalArgumentException Cuando la cadena de texto no ten&iacute;a un formato
	 * v&aacute;lido.
	 */
	public Version(final String legibleVersion) throws IllegalArgumentException {

		this.versionParts = new ArrayList<>();

		if (legibleVersion == null) {
			this.versionParts.add(Integer.valueOf(0));
			this.aditionalText = ""; //$NON-NLS-1$
			return;
		}

		// La version estara formada por varias partes separadas por puntos
		final String[] parts = legibleVersion.split("\\."); //$NON-NLS-1$

		// Todas las partes salvo la ultima seran un numero entero
		for (int i = 0; i < parts.length - 1; i++) {
			final String part = parts[i];
			this.versionParts.add(Integer.valueOf(part));
		}

		// La ultima parte puede ser un numero entero seguido de un texto
		int limit = -1;
		final String lastPart = parts[parts.length - 1];
		for (int i = 0; i < lastPart.length() && limit < 0; i++) {
			if (!Character.isDigit(lastPart.charAt(i))) {
				limit = i;
			}
		}

		if (limit > 0) {
			this.versionParts.add(Integer.valueOf(lastPart.substring(0, limit)));
			this.aditionalText = lastPart.substring(limit);
		} else {
			this.versionParts.add(Integer.valueOf(lastPart));
			this.aditionalText = ""; //$NON-NLS-1$
		}
	}

	/**
	 * Compara la versi&oacute;n con la indicada por par&aacute;metro.
	 * La l&oacute;gica a seguida durante la comparacion es la siguiente:
	 * <ol>
	 *  <li>Se compara de izquierda a derecha los numeros de cada parte separada por puntos
	 *  de las versiones. Si alguna de las partes, antes de la &uacute;ltima, es mayor que la
	 *  de la otra cadena, esa ser&aacute; la versi&oacute;n mayor.
	 *  Por ejemplo, "1.6.5" &lt; "1.7.0"</li>
	 *  <li>Si todas las partes son iguales, pero una tiene mas partes que la otra, la que
	 *  tiene mas partes que la otra, esa sera la mayor.
	 *  Por ejemplo, "1.7.0.1" &gt; "1.7.0" y "1.7.0.0" &gt; "1.7.0"</li>
	 *  <li>Si las cadenas de texto de la &uacute;ltima parte de la versi&oacute;n son iguales,
	 *  (sin atender a may&uacute;sculas/min&uacute;sculas) las versiones son iguales.</li>
	 *  <li>Si una cadena de texto empieza por espacio y la otra esta vacia o empieza por
	 *	cualquier otro caracter, se considerar que el que la version que empieza por espacio
	 *  es menor.
	 *  Por ejemplo, "1.7a" &gt; "1.7 RC1" y "1.7" &gt; "1.7 RC1".
	 *  <li>Si una cadena esta vacia y la otra tiene contenido (que no empiece por espacio)
	 *	la que tiene contenido es mayor.
	 *	Por ejemplo, "1.7a" &gt; "1.7".
	 *  <li>En cualquier otro caso, las cadenas se comparan a nivel binario (sin atender a
	 *  may&uacute;sculas/min&uacute;sculas) para determinar la mayor.
	 *  Por ejemplo, "1.7a" &lt; "1.7b", "1.7a1" &lt; "1.7a2" y "1.7A" es igual a "1.7a".</li>
	 * </ol>
	 * @param version Cadena de texto legible con la versi&oacute;n con la que comparar.
	 * @return {@code true} si esta versi&oacute;n es mayor que la indicada por
	 * par&aacute;metro. {@code false} si es igual o menor.
	 * @throws IllegalArgumentException Cuando la cadena de texto no ten&iacute;a un formato
	 * v&aacute;lido.
	 */
	public boolean greaterThan(final String version) throws IllegalArgumentException {
		return greaterThan(new Version(version));
	}

	/**
	 * Compara la versi&oacute;n con la indicada por par&aacute;metro.
	 * La l&oacute;gica a seguida durante la comparacion es la siguiente:
	 * <ol>
	 *  <li>Se compara de izquierda a derecha los numeros de cada parte separada por puntos
	 *  de las versiones. Si alguna de las partes, antes de la &uacute;ltima, es mayor que la
	 *  de la otra cadena, esa ser&aacute; la versi&oacute;n mayor.
	 *  Por ejemplo, "1.6.5" &lt; "1.7.0"</li>
	 *  <li>Si todas las partes son iguales, pero una tiene mas partes que la otra, la que
	 *  tiene mas partes que la otra, esa sera la mayor.
	 *  Por ejemplo, "1.7.0.1" &gt; "1.7.0" y "1.7.0.0" &gt; "1.7.0"</li>
	 *  <li>Si las cadenas de texto de la &uacute;ltima parte de la versi&oacute;n son iguales,
	 *  (sin atender a may&uacute;sculas/min&uacute;sculas) las versiones son iguales.</li>
	 *  <li>Si una cadena de texto empieza por espacio y la otra esta vacia o empieza por
	 *	cualquier otro caracter, se considerar que el que la version que empieza por espacio
	 *  es menor.
	 *  Por ejemplo, "1.7a" &gt; "1.7 RC1" y "1.7" &gt; "1.7 RC1".
	 *  <li>Si una cadena esta vacia y la otra tiene contenido (que no empiece por espacio)
	 *	la que tiene contenido es mayor.
	 *	Por ejemplo, "1.7a" &gt; "1.7".
	 *  <li>En cualquier otro caso, las cadenas se comparan a nivel binario (sin atender a
	 *  may&uacute;sculas/min&uacute;sculas) para determinar la mayor.
	 *  Por ejemplo, "1.7a" &lt; "1.7b", "1.7a1" &lt; "1.7a2" y "1.7A" es igual a "1.7a".</li>
	 * </ol>
	 * @param version Versi&oacute;n con la que comparar.
	 * @return {@code true} si esta versi&oacute;n es mayor que la indicada por
	 * par&aacute;metro. {@code false} si es igual o menor.
	 */
	public boolean greaterThan(final Version version) {

		int i = 0;
		// Comparamos las distintas partes de digitos hasta el numero de partes de cada version
		while (this.versionParts.size() > i && version.getVersionParts().size() > i) {
			final int thisPart = this.versionParts.get(i).intValue();
			final int anotherPart = version.getVersionParts().get(i).intValue();
			if (thisPart > anotherPart) {
				return true;
			} else if (thisPart < anotherPart) {
				return false;
			}
			i++;
		}

		// Comprobamos si faltan digitos por valorar en alguna de las versiones.
		// Si es asi, esa version es superior a la otra
		if (this.versionParts.size() > version.getVersionParts().size()) {
			return true;
		} else if (this.versionParts.size() < version.getVersionParts().size()) {
			return false;
		}

		// Si ambas versiones tienen la misma longitud y los mismos numeros de version
		// se consideran iguales y hay que comparar las cadenas de texto adicional siguiendo
		// las directrices declaradas en el javadoc del metodo
		if (this.aditionalText.equalsIgnoreCase(version.getAditionalText())) {
			return false;
		}
		else if ((this.aditionalText.isEmpty() || !Character.isSpaceChar(this.aditionalText.charAt(0)))
				&& version.getAditionalText().length() > 0 && Character.isSpaceChar(version.getAditionalText().charAt(0))) {
			return true;
		}
		else if ((version.getAditionalText().isEmpty() || !Character.isSpaceChar(version.getAditionalText().charAt(0)))
				&& this.aditionalText.length() > 0 && Character.isSpaceChar(this.aditionalText.charAt(0))) {
			return false;
		}
		else if (this.aditionalText.isEmpty()
				&& !Character.isSpaceChar(version.getAditionalText().charAt(0))) {
			return false;
		}
		else if (version.getAditionalText().isEmpty()
				&& !Character.isSpaceChar(this.aditionalText.charAt(0))) {
			return true;
		}
		else {
			return this.aditionalText.toLowerCase().compareTo(
					version.getAditionalText().toLowerCase()) > 0;
		}
	}

	/**
	 * Distintas partes del n&uacute;mero de versi&oacute;n.
	 * @return Listado de partes del n&uacute;mero de versi&oacute;n.
	 */
	public List<Integer> getVersionParts() {
		return this.versionParts != null ? new ArrayList<>(this.versionParts) : null;
	}

	/**
	 * Obtiene el texto adicional asociado al numero de versi&oacute;n.
	 * @return Texto adicional asociado a la versi&oacute;n o cadena vac&iacute;a si no
	 * hay un texto asociado.
	 */
	public String getAditionalText() {
		return this.aditionalText;
	}

	/**
	 * Obtiene el texto legible con el n&uacute;mero de versi&oacute;n.
	 * @return Texto legible con el n&uacute;mero de versi&oacute;n.
	 */
	@Override
	public String toString() {

		final StringBuilder buffer = new StringBuilder();
		for (int i = 0; i < this.versionParts.size(); i++) {
			if (i > 0) {
				buffer.append('.');
			}
			buffer.append(this.versionParts.get(i).intValue());
		}
		buffer.append(this.aditionalText);

		return buffer.toString();
	}
}
