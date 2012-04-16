package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

public interface CommitmentTypeId 
{
	public String getIdentifier();
	public String getQualifier();
	public String getDescription();
	public ArrayList<String> getDocumentationReferences();
}
