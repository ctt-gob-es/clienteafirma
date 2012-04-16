package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

public class CommitmentTypeIndicationImpl implements CommitmentTypeIndication 
{
	private CommitmentTypeId commitmentTypeId;
	private String objectReference;
	private ArrayList<String> commitmentTypeQualifiers;	   
	
	public CommitmentTypeIndicationImpl()
	{		
	}

	public CommitmentTypeIndicationImpl(CommitmentTypeId commitmentTypeId, String objectReference, ArrayList<String> commitmentTypeQualifiers)
	{		
		this.commitmentTypeId = commitmentTypeId;
		this.objectReference = objectReference;
		this.commitmentTypeQualifiers = commitmentTypeQualifiers;
	}

	public CommitmentTypeId getCommitmentTypeId() 
	{
		return commitmentTypeId;
	}

	public void setCommitmentTypeId(CommitmentTypeId commitmentTypeId) 
	{
		this.commitmentTypeId = commitmentTypeId;
	}

	public String getObjectReference() 
	{
		return objectReference;
	}

	public void setObjectReference(String objectReference) 
	{
		this.objectReference = objectReference;
	}

	public ArrayList<String> getCommitmentTypeQualifiers() 
	{
		return commitmentTypeQualifiers;
	}

	public void setCommitmentTypeQualifiers(ArrayList<String> commitmentTypeQualifiers) 
	{
		this.commitmentTypeQualifiers = commitmentTypeQualifiers;
	}
}
