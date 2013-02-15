package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfBoolean;
import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;

/**
 * @author blowagie
 *
 */
class PdfCollectionField extends PdfDictionary {
	/** A possible type of collection field. */
	private static final int TEXT = 0;
	/** A possible type of collection field. */
	public static final int DATE = 1;
	/** A possible type of collection field. */
	public static final int NUMBER = 2;






	/**
	 * The type of the PDF collection field.
	 * @since 2.1.2 (was called <code>type</code> previously)
	 */
	protected int fieldType;



	/**
	 * The relative order of the field name. Fields are sorted in ascending order.
	 * @param i	a number indicating the order of the field
	 */
	public void setOrder(final int i) {
		put(PdfName.O, new PdfNumber(i));
	}

	/**
	 * Sets the initial visibility of the field.
	 * @param visible	the default is true (visible)
	 */
	public void setVisible(final boolean visible) {
		put(PdfName.V, new PdfBoolean(visible));
	}

	/**
	 * Indication if the field value should be editable in the viewer.
	 * @param editable	the default is false (not editable)
	 */
	public void setEditable(final boolean editable) {
		put(PdfName.E, new PdfBoolean(editable));
	}

	/**
	 * Checks if the type of the field is suitable for a Collection Item.
	 */
	public boolean isCollectionItem() {
		switch(this.fieldType) {
		case TEXT:
		case DATE:
		case NUMBER:
			return true;
		default:
			return false;
		}
	}

	/**
	 * Returns a PdfObject that can be used as the value of a Collection Item.
	 * @param v	value	the value that has to be changed into a PdfObject (PdfString, PdfDate or PdfNumber)
	 */
	public PdfObject getValue(final String v) {
		switch(this.fieldType) {
		case TEXT:
			return new PdfString(v, PdfObject.TEXT_UNICODE);
		case DATE:
			return new PdfDate(PdfDate.decode(v));
		case NUMBER:
			return new PdfNumber(v);
		}
		throw new IllegalArgumentException(v + " is not an acceptable value for the field " + get(PdfName.N).toString());
	}
}