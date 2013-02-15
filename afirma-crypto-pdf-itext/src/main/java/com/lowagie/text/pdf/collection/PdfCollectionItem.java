package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;

public class PdfCollectionItem extends PdfDictionary {

	/** The PdfCollectionSchema with the names and types of the items. */
	private final PdfCollectionSchema schema;

	/**
	 * Constructs a Collection Item that can be added to a PdfFileSpecification.
	 */
	public PdfCollectionItem(final PdfCollectionSchema schema) {
		super(PdfName.COLLECTIONITEM);
		this.schema = schema;
	}

	/**
	 * Sets the value of the collection item.
	 * @param value
	 */
	public void addItem(final String key, final String value) {
		final PdfName fieldname = new PdfName(key);
		final PdfCollectionField field = (PdfCollectionField)this.schema.get(fieldname);
		put(fieldname, field.getValue(value));
	}



	/**
	 * Sets the value of the collection item.
	 * @param d
	 */
	private void addItem(final String key, final PdfDate d) {
		final PdfName fieldname = new PdfName(key);
		final PdfCollectionField field = (PdfCollectionField)this.schema.get(fieldname);
		if (field.fieldType == PdfCollectionField.DATE) {
			put(fieldname, d);
		}
	}

	/**
	 * Sets the value of the collection item.
	 * @param n
	 */
	private void addItem(final String key, final PdfNumber n) {
		final PdfName fieldname = new PdfName(key);
		final PdfCollectionField field = (PdfCollectionField)this.schema.get(fieldname);
		if (field.fieldType == PdfCollectionField.NUMBER) {
			put(fieldname, n);
		}
	}










}