package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfString;

public class PdfCollection extends PdfDictionary {


	/** A type of PDF Collection */
	private static final int TILE = 1;
	/** A type of PDF Collection */
	private static final int HIDDEN = 2;



	/**
	 * Identifies the document that will be initially presented
	 * in the user interface.
	 * @param description	the description that was used when attaching the file to the document
	 */
	public void setInitialDocument(final String description) {
		put(PdfName.D, new PdfString(description, null));
	}

	/**
	 * Sets the Collection schema dictionary.
	 * @param schema	an overview of the collection fields
	 */
	public void setSchema(final PdfCollectionSchema schema) {
		put(PdfName.SCHEMA, schema);
	}

	/**
	 * Gets the Collection schema dictionary.
	 * @return schema	an overview of the collection fields
	 */
	public PdfCollectionSchema getSchema() {
		return (PdfCollectionSchema)get(PdfName.SCHEMA);
	}

	/**
	 * Sets the Collection sort dictionary.
	 * @param sort	a collection sort dictionary
	 */
	public void setSort(final PdfCollectionSort sort) {
		put(PdfName.SORT, sort);
	}
}