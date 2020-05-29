package es.gob.afirma.signers.pades;

/**
 * M&acute;scara para la ofuscaci&oacute;n de textos de las firmas visibles PDF.
 */
public class PdfTextMask {

	/** Caracter sustitutivo a utilizar por defecto. */
	private static final char DEFAULT_OBFUSCATED_CHAR = '*';

	/** Longitud m&iacute;nima de digitos consecutivos para considerar que un
	 * texto es un identificador a ser ofuscado */
	private static final int DEFAULT_MIN_NUMBER_LENGTH = 3;

	/** Caracter sustitutivo. */
	private char obfuscatedChar;

	/** Longitud minima de digitos para identificar una cadena como objetivo a
	 * ofuscar. */
	private int minLength;

	/** M&aacute;scara con las posiciones que no deben mostrarse ({@code false})
	 * y las que s&iacute; ({@code true}).
	 */
	private boolean[] positions;

	/** Indica si la mascara debe aplicarse de forma fiel o si deber&iacute;an
	 * desplazarse adelante los caracteres que deben ser visibles. */
	private boolean shiftSupported;

	/** Genera la m&aacute;scara por defecto. */
	PdfTextMask() {
		this.obfuscatedChar = DEFAULT_OBFUSCATED_CHAR;
		this.minLength = DEFAULT_MIN_NUMBER_LENGTH;
		this.positions = new boolean[] { false, false, false, true, true, true, true};
		this.shiftSupported = true;
	}

	/** Establece el caracter sustitutivo.
	 * @param ofuscatedChar Caracter sustitutivo.
	 */
	public void setObfuscatedChar(final char ofuscatedChar) {
		this.obfuscatedChar = ofuscatedChar;
	}

	/** Recupera el caracter sustitutivo.
	 * @return Caracter sustitutivo.
	 */
	public char getObfuscatedChar() {
		return this.obfuscatedChar;
	}

	/** Establece la l&oacute;ngitud m&iacute;nima de digitos que debe tener
	 * una cadena para considerarla objetivo de ofuscaci&oacute;n.
	 * @param minLength L&oacute;ngitud m&iacute;nima de d&iacute;gitos.
	 */
	public void setMinLength(final int minLength) {
		this.minLength = minLength;
	}

	/** Recupera la l&oacute;ngitud m&iacute;nima de digitos que debe tener
	 * una cadena para considerarla objetivo de ofuscaci&oacute;n.
	 * @return L&oacute;ngitud m&iacute;nima de d&iacute;gitos.
	 */
	public int getMinLength() {
		return this.minLength;
	}

	/** Establece las posiciones de la m&aacute;scara que deben no mostrarse
	 * ({@code false}) y las que si ({@code true}).
	 * @param positions Posiciones que deben ocultarse o mostrarse.
	 */
	public void setPositions(final boolean[] positions) {
		this.positions = positions != null ? positions.clone() : null;
	}

	/** Recupera las posiciones de la m&aacute;scara que deben no mostrarse
	 * ({@code false}) y las que si ({@code true}).
	 * @return Posiciones que deben ocultarse o mostrarse.
	 */
	public boolean[] getPositions() {
		return this.positions != null ? this.positions.clone() : null;
	}

	/** Establece si la m&aacute;scara debe aplicarse fielmente para la
	 * ocultaci&oacute;n o si debe respetarse el n&uacute;mero de caracters a
	 * mostrar desplazando sus posiciones.
	 * @param shiftSupported Si deben desplazarse las posiciones de la
	 * m&aacute;scara ({@code true}) o no ({@code false}).
	 */
	public void setShiftSupported(final boolean shiftSupported) {
		this.shiftSupported = shiftSupported;
	}

	/** Indica si la m&aacute;scara debe aplicarse fielmente para la
	 * ocultaci&oacute;n o si debe respetarse el n&uacute;mero de caracters a
	 * mostrar desplazando sus posiciones.
	 * @return Si deben desplazarse las posiciones de la m&aacute;scara
	 * ({@code true}) o no ({@code false}).
	 */
	public boolean isShiftSupported() {
		return this.shiftSupported;
	}

	/**
	 * Genera una m&aacute;scara a partir de una cadena en la que se
	 * especifican sus valores con la forma:<br>
	 * {@code caracterSustitutivo;longitudDigitos;posiciones;desplazamiento}<br>
	 * Aqu&iacute;:
	 * <ul>
	 * <li>{@code caracterSustitutivo}: Es el car&aacute;cter que debemos usar para ofuscar caracteres.</li>
	 * <li>{@code longitudDigitos}: Es el n&uacute;mero m&iacute;nimo de d&iacute;gitos que debe tener
	 * una cadena de texto para que se considere que debe ofuscarse.</li>
	 * <li>{@code posiciones}: Es el listado de posiciones que indica qu&eacute; caracteres deben mostrarse.
	 * El listado se expresar&aacute; por una sucesi&oacute;n {@code true}/{@code false} separados por comas
	 * (','), en donde {@code true} indica que el car&aacute;cter debe mostrarse y {@code false} que no. Las
	 * posiciones a ofucar al final del patr&oacute;n se sobreentender&aacute;n.</li>
	 * <li>{@code desplazamiento}: Indica si se admite el desplazamiento de posiciones de la m&aacute;scara para
	 * mostrar todos los caracteres indicados ({@code true}) o si esta debe respetarse ({@code false}).</li>
	 * </ul>
	 * @param param Cadena con el patr&oacute;n definido.
	 * @return La m&aacute;scara con el patr&oacute;n definido o la por defecto si el patr&oacute;n no
	 * est&aacute; bien formado.
	 * @throws IllegalArgumentException Si la m&aacute;scara no esta bien definida.
	 */
	public static PdfTextMask parseParam(final String param) throws IllegalArgumentException {

		final PdfTextMask mask = new PdfTextMask();

		char character = '\0';
		int length = -1;
		boolean[] positions = null;
		final boolean shiftment;

		final String[] elements = param.split(";"); //$NON-NLS-1$
		if (elements.length == 4) {

			// Caracter de ofuscacion
			if (elements[0].length() != 1) {
				throw new IllegalArgumentException("No se indico un caracter de ofuscacion unico"); //$NON-NLS-1$
			}
			character = elements[0].charAt(0);

			// Numero minimo de digitos para identificar elementos
			boolean isNumber = true;
			for (final char c : elements[1].toCharArray()) {
				if (!Character.isDigit(c)) {
					isNumber = false;
				}
			}
			if (!isNumber) {
				throw new IllegalArgumentException("No se indico un numero valido de digitos minimos"); //$NON-NLS-1$
			}
			length = Integer.parseInt(elements[1]);

			// Posiciones de la mascara
			final String[] positionLists = elements[2].split(","); //$NON-NLS-1$
			if (positionLists.length <= 1) {
				throw new IllegalArgumentException("No se indico un listado de posiciones valido"); //$NON-NLS-1$
			}
			int pos = positionLists.length - 1;
			while (pos >= 0 && !Boolean.parseBoolean(positionLists[pos])) {
				pos--;
			}
			if (pos > 0) {
				positions = new boolean[pos + 1];
				for (int i = 0; i < positions.length; i++) {
					positions[i] = Boolean.parseBoolean(positionLists[i]);
				}
			}

			shiftment = Boolean.parseBoolean(elements[3]);

			if (character != '\0' && length > -1 && positions != null) {
				mask.setObfuscatedChar(character);
				mask.setMinLength(length);
				mask.setPositions(positions);
				mask.setShiftSupported(shiftment);
			}
		}
		else {
			throw new IllegalArgumentException("El numero de elementos de la mascara no es valido"); //$NON-NLS-1$
		}

		return mask;
	}
}
