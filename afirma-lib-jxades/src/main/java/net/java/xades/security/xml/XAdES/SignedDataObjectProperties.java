package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

public class SignedDataObjectProperties extends XAdESStructure
{
    public SignedDataObjectProperties(SignedProperties signedProperties, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        super(signedProperties, "SignedDataObjectProperties", xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);
    }

    public void setDataObjectFormat(ArrayList<DataObjectFormat> dataObjectFormat)
    {
        for (DataObjectFormat dof : dataObjectFormat)
        {
            new DataObjectFormatDetails(this, dof, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
        }
    }

    public void setCommitmentTypeIndication(CommitmentTypeIndication commitmentTypeIndication)
    {
        new CommitmentTypeIndicationDetails(this, commitmentTypeIndication, xadesPrefix,
                xadesNamespace, xmlSignaturePrefix);
    }

    public void setAllDataObjectsTimeStamp(
            ArrayList<AllDataObjectsTimeStamp> allDataObjectsTimeStamp, String tsaURL)
    {
        for (AllDataObjectsTimeStamp adots : allDataObjectsTimeStamp)
        {
            new AllDataObjectsTimeStampDetails(this, adots, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, tsaURL);
        }
    }

    public void setIndividualDataObjectsTimeStamp(
            ArrayList<IndividualDataObjectsTimeStamp> individualDataObjectsTimeStamp, String tsaURL)
    {
        for (IndividualDataObjectsTimeStamp idots : individualDataObjectsTimeStamp)
        {
            new IndividualDataObjectsTimeStampDetails(this, idots, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, tsaURL);
        }
    }
}