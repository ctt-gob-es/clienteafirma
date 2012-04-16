package net.java.xades.security.timestamp;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;

import sun.security.timestamp.HttpTimestamper;
import sun.security.timestamp.TSRequest;
import sun.security.timestamp.TSResponse;

public class TimeStampFactory
{
    public static TSResponse getTimeStampResponse(String strUrl, byte[] data, boolean calculateDigest) throws NoSuchAlgorithmException, IOException, SignatureException
    {
        HttpTimestamper httpTimestamper = new HttpTimestamper(strUrl);
        
        byte[] digest = data;
        
        if (calculateDigest)
        {
            MessageDigest messageDigest = MessageDigest.getInstance("SHA-1");
            digest = messageDigest.digest(data);
        }

        TSRequest request = new TSRequest(digest, "SHA-1");
        request.requestCertificate(false);
        
        TSResponse response = httpTimestamper.generateTimestamp(request); 

        return response;
    }
    
    public static byte[] getTimeStamp(String tsaURL, byte[] data, boolean calculateDigest) throws NoSuchAlgorithmException, IOException, SignatureException
    {
        TSResponse response = getTimeStampResponse(tsaURL, data, calculateDigest); 

        return response.getEncodedToken();
    }
}
