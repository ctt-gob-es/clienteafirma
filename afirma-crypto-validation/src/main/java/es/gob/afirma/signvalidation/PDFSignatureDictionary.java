package es.gob.afirma.signvalidation;

import java.io.Serializable;

import com.aowagie.text.pdf.PdfDictionary;

/**
 * <p>Class that contains information related to a signature dictionary of a PDF document.</p>
 * <b>Project:</b><p>Library for the integration with the services of @Firma, eVisor and TS@.</p>
 * @version 1.0, 13/11/2014.
 */
public class PDFSignatureDictionary implements Serializable, Comparable<PDFSignatureDictionary> {

    /**
     * Class serial version.
     */
    private static final long serialVersionUID = -1252456113292585014L;

    /**
     * Attribute that represents the number of the revision on the PDF document when the signature dictionary was added.
     */
    private Integer revision;

    /**
     * Attribute that represents the signature dictionary.
     */
    private PdfDictionary dictionary;

    /**
     * Attribute that represents the name of the signature dictionary.
     */
    private String name;

    /**
     * Constructor method for the class PDFSignatureDictionary.java.
     * @param revisionParam Parameter that represents the number of the revision on the PDF document when the signature dictionary was added.
     * @param dictionaryParam Parameter that represents the signature dictionary.
     * @param nameParam Parameter that represents the name of the signature dictionary.
     */
    public PDFSignatureDictionary(final int revisionParam, final PdfDictionary dictionaryParam, final String nameParam) {
	this.revision = revisionParam;
	this.dictionary = dictionaryParam;
	this.name = nameParam;
    }

    /**
     * Gets the value of the attribute {@link #revision}.
     * @return the value of the attribute {@link #revision}.
     */
    public final Integer getRevision() {
	return this.revision;
    }

    /**
     * Sets the value of the attribute {@link #revision}.
     * @param revisionParam The value for the attribute {@link #revision}.
     */
    public final void setRevision(final Integer revisionParam) {
	this.revision = revisionParam;
    }

    /**
     * Gets the value of the attribute {@link #dictionary}.
     * @return the value of the attribute {@link #dictionary}.
     */
    public final PdfDictionary getDictionary() {
	return this.dictionary;
    }

    /**
     * Sets the value of the attribute {@link #dictionary}.
     * @param dictionaryParam The value for the attribute {@link #dictionary}.
     */
    public final void setDictionary(final PdfDictionary dictionaryParam) {
	this.dictionary = dictionaryParam;
    }

    /**
     * Gets the value of the attribute {@link #name}.
     * @return the value of the attribute {@link #name}.
     */
    public final String getName() {
	return this.name;
    }

    /**
     * Sets the value of the attribute {@link #name}.
     * @param nameParam The value for the attribute {@link #name}.
     */
    public final void setName(final String nameParam) {
	this.name = nameParam;
    }

    /**
     * {@inheritDoc}
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public final int compareTo(final PDFSignatureDictionary o) {
	return this.revision.compareTo(o.getRevision());
    }
}

