package es.gob.afirma.crypto.handwritten.pdf;

import java.io.IOException;

import javax.xml.bind.annotation.XmlElement;

import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.TextField;

/** C&oacute;digo Seguro de Validaci&oacute;n (CSV) en un PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Csv {

	/** &Uacute;ltima p&aacute;gina. */
	public static int LAST_PAGE = 0;

	/** Todas las p&aacute;ginas. */
	public static int ALL_PAGES = -1;

	@XmlElement(name = "page")
	private final int page;

	@XmlElement(name = "text")
	private final String text;

	@XmlElement(name = "rotation")
	private final int rotation;

	@XmlElement(name = "positionOnPageLowerLeftX")
	private final int positionOnPageLowerLeftX;

	@XmlElement(name = "positionOnPageLowerLeftY")
	private final int positionOnPageLowerLeftY;

	@XmlElement(name = "positionOnPageUpperRightX")
	private final int positionOnPageUpperRightX;

	@XmlElement(name = "positionOnPageUpperRightY")
	private final int positionOnPageUpperRightY;

	@XmlElement(name = "fontSize")
	private final float fontSize;

	/** Construye un CSV vac&iacute;o. */
	public Csv() {
		// Para la serializacion JAXB
		this.page = 1;
		this.text = "CSV"; //$NON-NLS-1$
		this.rotation = 0;
		this.positionOnPageLowerLeftX = 0;
		this.positionOnPageLowerLeftY = 0;
		this.positionOnPageUpperRightX = 100;
		this.positionOnPageUpperRightY = 100;
		this.fontSize = 25;
	}

	/** Crea un CSV para inrsertarlo en un PDF.
	 * @param pg P&aacute;gina de inserci&oacute;n.
	 * @param txt Texto del CSV.
	 * @param fontSz Tama&ntilde;o del texto del CSV.
	 * @param r Rotaci&oacute;n en grados.
	 * @param llx Coordenada horizontal inferior izquierda del recuadro del CSV.
	 * @param lly Coordenada vertical inferior izquierda del recuadro del CSV.
	 * @param urx Coordenada horizontal superior derecha del recuadro del CSV.
	 * @param ury Coordenada vertical superior derecha del recuadro del CSV. */
	public Csv(final int pg,
			   final String txt,
			   final float fontSz,
			   final int r,
			   final int llx,
			   final int lly,
			   final int urx,
			   final int ury) {
		this.page = pg;
		this.text = txt;
		this.rotation = r;
		this.positionOnPageLowerLeftX = llx;
		this.positionOnPageLowerLeftY = lly;
		this.positionOnPageUpperRightX = urx;
		this.positionOnPageUpperRightY = ury;
		this.fontSize = fontSz;
	}

	void applyCsv(final PdfStamper stamper) throws IOException {
		final TextField tf = new TextField(
			stamper.getWriter(),
			new com.lowagie.text.Rectangle(
				this.positionOnPageLowerLeftX,
				this.positionOnPageLowerLeftY,
				this.positionOnPageUpperRightX,
				this.positionOnPageUpperRightY
			),
			"CSV" //$NON-NLS-1$
		);
		tf.setRotation(this.rotation);
		tf.setText(this.text);
		tf.setFontSize(this.fontSize);
	    try {
			stamper.addAnnotation(tf.getTextField(), this.page);
		}
	    catch (final Exception e) {
			throw new IOException(e);
		}

	}

}
