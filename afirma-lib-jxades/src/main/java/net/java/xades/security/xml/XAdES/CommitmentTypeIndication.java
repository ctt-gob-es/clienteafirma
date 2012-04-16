package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

public interface CommitmentTypeIndication 
{   
   public void setCommitmentTypeId(CommitmentTypeId commitmentTypeId);
   public void setObjectReference(String objectReference);
   public void setCommitmentTypeQualifiers(ArrayList<String> commitmentTypeQualifiers);
   
   public CommitmentTypeId getCommitmentTypeId();
   public String getObjectReference();
   public ArrayList<String> getCommitmentTypeQualifiers();
}
