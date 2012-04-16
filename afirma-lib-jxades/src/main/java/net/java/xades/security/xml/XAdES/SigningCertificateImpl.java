package net.java.xades.security.xml.XAdES;

import java.math.BigInteger;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.cert.X509Certificate;

import javax.xml.crypto.dsig.DigestMethod;

import net.java.xades.util.Base64;

public class SigningCertificateImpl implements SigningCertificate
{
	private X509Certificate certificate;
	private String digestMethod;
	
	public SigningCertificateImpl(X509Certificate certificate, String digestMethod) 
	{
		this.certificate = certificate;
		this.digestMethod = digestMethod;
	}

	public String getDigestMethodAlgorithm() 
	{
		return digestMethod;
	}

	public String getDigestValue() throws GeneralSecurityException
	{
		String result;
		
		try
		{
		    String algorithm = "SHA-1";
		    
		    if (DigestMethod.SHA256.equals(digestMethod))
		    {
		        algorithm = "SHA-256";
		    }
		    else if (DigestMethod.SHA512.equals(digestMethod))
		    {
                algorithm = "SHA-512";
		    }
		    
			MessageDigest md = MessageDigest.getInstance(algorithm);	
			md.update(certificate.getEncoded());
			result = Base64.encodeBytes(md.digest());
		}
		catch (Exception e)
		{
			throw new GeneralSecurityException(e);
		}
		
		return result;
	}

	public String getIssuerName() 
	{
		return certificate.getIssuerDN().getName();
	}

	public BigInteger getX509SerialNumber() 
	{
		return certificate.getSerialNumber();
	}
}
