package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

public interface ObjectIdentifier 
{
	public String getQualifier();
	public void setQualifier(String qualifier);
	public String getIdentifier();
	public void setIdentifier(String identifier);
	public String getDescription();
	public void setDescription(String description);
	public ArrayList<String> getDocumentationReferences();
	public void setDocumentationReferences(ArrayList<String> documentationReferences); 
}
