package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfBoolean;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;

class PdfCollectionSort extends PdfDictionary {





	/**
	 * Defines the sort order of the field (ascending or descending).
	 * @param ascending	true is the default, use false for descending order
	 */
	public void setSortOrder(final boolean ascending) {
		final PdfObject o = get(PdfName.S);
		if (o instanceof PdfName) {
			put(PdfName.A, new PdfBoolean(ascending));
		}
		else {
			throw new IllegalArgumentException("You have to define a boolean array for this collection sort dictionary.");
		}
	}

	/**
	 * Defines the sort order of the field (ascending or descending).
	 * @param ascending	an array with every element corresponding with a name of a field.
	 */
	public void setSortOrder(final boolean[] ascending) {
		final PdfObject o = get(PdfName.S);
		if (o instanceof PdfArray) {
			if (((PdfArray)o).size() != ascending.length) {
				throw new IllegalArgumentException("The number of booleans in this array doesn't correspond with the number of fields.");
			}
			final PdfArray array = new PdfArray();
			for (final boolean element : ascending) {
				array.add(new PdfBoolean(element));
			}
			put(PdfName.A, array);
		}
		else {
			throw new IllegalArgumentException("You need a single boolean for this collection sort dictionary.");
		}
	}


}
