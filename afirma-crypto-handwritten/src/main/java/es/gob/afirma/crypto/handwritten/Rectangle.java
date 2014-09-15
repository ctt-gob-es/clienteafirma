package es.gob.afirma.crypto.handwritten;

/** Rect&aacute;ngulo.
 * No se reutilizan las clases de AWT para compatibilidad con Android.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Rectangle {

	/** Coordenada horizontal de a esquina superior izquierda del rect&aacute;ngulo. */
	public final int x;

	/** Coordenada vertical de a esquina superior izquierda del rect&aacute;ngulo. */
	public final int y;

	/** Ancho del rect&aacute;ngulo. */
	public final int width;

	/** Alto del rect&aacute;ngulo. */
	public final int height;

	/** Crea un rect&aacute;ngulo.
	 * @param x Coordenada horizontal de a esquina superior izquierda del rect&aacute;ngulo
	 * @param y Coordenada vertical de a esquina superior izquierda del rect&aacute;ngulo
	 * @param h Alto del rect&aacute;ngulo
	 * @param w Ancho del rect&aacute;ngulo */
	public Rectangle(final int x, final int y, final int w, final int h) {
		this.x = x;
		this.y = y;
		this.height = h;
		this.width = w;
	}

}
