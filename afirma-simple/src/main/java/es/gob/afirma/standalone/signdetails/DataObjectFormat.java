package es.gob.afirma.standalone.signdetails;

public class DataObjectFormat {

	private String identifier;
    private String description;
    private String mimeType;
    private String encoding;

    public DataObjectFormat() {
    	//Constructor vacio
	}

    public DataObjectFormat(final String mimeType) {
    	this.mimeType = mimeType;
	}

	public String getIdentifier() {
		return this.identifier;
	}
	public void setIdentifier(final String identifier) {
		this.identifier = identifier;
	}
	public String getDescription() {
		return this.description;
	}
	public void setDescription(final String description) {
		this.description = description;
	}
	public String getMimeType() {
		return this.mimeType;
	}
	public void setMimeType(final String mimeType) {
		this.mimeType = mimeType;
	}
	public String getEncoding() {
		return this.encoding;
	}
	public void setEncoding(final String encoding) {
		this.encoding = encoding;
	}

}
