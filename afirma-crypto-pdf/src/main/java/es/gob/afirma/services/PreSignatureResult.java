package es.gob.afirma.services;

import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class PreSignatureResult
{
    private byte[] preSignData;
    private String fileID;

    public PreSignatureResult()
    {
    }
    
    public PreSignatureResult(byte[] preSignData, String fileID)
    {
        this.fileID = fileID;
        this.preSignData = preSignData;
    }

    public byte[] getPreSignData()
    {
        return preSignData;
    }

    public void setPreSignData(byte[] preSignData)
    {
        this.preSignData = preSignData;
    }

    public String getFileID()
    {
        return fileID;
    }

    public void setFileID(String fileID)
    {
        this.fileID = fileID;
    }
}
