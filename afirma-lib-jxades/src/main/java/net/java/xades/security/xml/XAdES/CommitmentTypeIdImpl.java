package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

public class CommitmentTypeIdImpl implements CommitmentTypeId
{
    private String qualifier;
    private String identifier;
    private String description;
    private ArrayList<String> documentationReferences;

	public CommitmentTypeIdImpl(String qualifier, String identifier, String description, ArrayList<String> documentationReferences) 
	{
		this.qualifier = qualifier;
		this.identifier = identifier;
		this.description = description;
		this.documentationReferences = documentationReferences;
	}

	public String getQualifier() 
	{
		return qualifier;
	}

	public void setQualifier(String qualifier) 
	{
		this.qualifier = qualifier;
	}

	public String getIdentifier() 
	{
		return identifier;
	}

	public void setIdentifier(String identifier) 
	{
		this.identifier = identifier;
	}

	public String getDescription() 
	{
		return description;
	}

	public void setDescription(String description) 
	{
		this.description = description;
	}

	public ArrayList<String> getDocumentationReferences() 
	{
		return documentationReferences;
	}

	public void setDocumentationReferences(ArrayList<String> documentationReferences) 
	{
		this.documentationReferences = documentationReferences;
	}
}
