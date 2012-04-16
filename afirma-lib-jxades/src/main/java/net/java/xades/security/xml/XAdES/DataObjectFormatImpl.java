package net.java.xades.security.xml.XAdES;

public class DataObjectFormatImpl implements DataObjectFormat 
{
	private String description;
	private ObjectIdentifier objectIdentifier;
	private String mimeType;
	private String encoding;
	private String objectReference;
	
	public DataObjectFormatImpl()
	{		
	}

	public DataObjectFormatImpl(String description, ObjectIdentifier objectIdentifier, String mimeType, String encoding, String objectReference)
	{		
		this.description = description;
		this.objectIdentifier = objectIdentifier;
		this.mimeType = mimeType;
		this.encoding = encoding;
		this.objectReference = objectReference;
	}

	public String getDescription() 
	{
		return description;
	}
	
	public void setDescription(String description) 
	{
		this.description = description;
	}
	
	public ObjectIdentifier getObjectIdentifier() 
	{
		return objectIdentifier;
	}
	
	public void setObjectIdentifier(ObjectIdentifier objectIdentifier) 
	{
		this.objectIdentifier = objectIdentifier;
	}
	
	public String getMimeType() 
	{
		return mimeType;
	}
	
	public void setMimeType(String mimeType) 
	{
		this.mimeType = mimeType;
	}
	
	public String getEncoding() 
	{
		return encoding;
	}
	
	public void setEncoding(String encoding) 
	{
		this.encoding = encoding;
	}

	public String getObjectReference() {
		return objectReference;
	}

	public void setObjectReference(String objectReference) {
		this.objectReference = objectReference;
	}
	
	
}
