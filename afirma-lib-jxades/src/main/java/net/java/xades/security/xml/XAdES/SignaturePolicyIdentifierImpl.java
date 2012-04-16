package net.java.xades.security.xml.XAdES;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.xml.crypto.dsig.DigestMethod;

import net.java.xades.util.Base64;

public class SignaturePolicyIdentifierImpl implements SignaturePolicyIdentifier
{    
    private boolean implied;
    private String sigPolicyId;
    private String description;
    private String sigPolicyQualifierSPURI;
    private String sigPolicyHashBase64;
    private String sigPolicyHashHashAlgorithm;
    
    public SignaturePolicyIdentifierImpl(final boolean implied)
    {
        this.implied = implied;
    }

    private byte[] inputStreamToByteArray(InputStream in) throws IOException
    {
        final byte[] buffer = new byte[2048];
        int length = 0;
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((length = in.read(buffer)) >= 0)
        {
            baos.write(buffer, 0, length);
        }
        return baos.toByteArray();
    }
    
    public void setIdentifier(final String identifier) throws IOException, NoSuchAlgorithmException
    {
        setIdentifier(identifier, null, null);
    }
    
    public void setIdentifier(final String identifier, final String hashBase64, final String hashAlgorithm) throws IOException, NoSuchAlgorithmException
    {
        if (hashBase64 == null || "".equals(hashBase64) || hashAlgorithm == null || "".equals(hashAlgorithm)) //$NON-NLS-1$ //$NON-NLS-2$
        {
            final URLConnection conn = new URL(identifier).openConnection();
            final byte[] data = inputStreamToByteArray(conn.getInputStream());
            final MessageDigest md = MessageDigest.getInstance("SHA1"); //$NON-NLS-1$
            md.update(data);
            this.sigPolicyHashBase64 = Base64.encodeBytes(md.digest()); 
            this.sigPolicyHashHashAlgorithm = DigestMethod.SHA1;
        }
        else
        {
            this.sigPolicyHashBase64 = hashBase64; 
            this.sigPolicyHashHashAlgorithm = hashAlgorithm;
        }
        this.sigPolicyId = identifier;
    }
    
    public boolean isImplied()
    {
        return this.implied;
    }

    public void setImplied(boolean implied)
    {
        this.implied = implied;
    }

    public String getIdentifier()
    {
        return this.sigPolicyId;
    }

    public String getHashBase64()
    {
        return this.sigPolicyHashBase64;
    }

    public String getDescription()
    {
        return this.description;
    }

    public void setDescription(String description)
    {
        this.description = description;        
    }
    
    public String getQualifier()
    {
        return this.sigPolicyQualifierSPURI;
    }

    public void setQualifier(String qualifier)
    {
        this.sigPolicyQualifierSPURI = qualifier;        
    }

    public String getHashAlgorithm() {
        return this.sigPolicyHashHashAlgorithm;
    }    
}
