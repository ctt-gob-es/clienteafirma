package net.java.xades.security.xml.XAdES;

import java.math.BigInteger;
import java.security.GeneralSecurityException;

public interface SigningCertificate 
{
	public String getDigestMethodAlgorithm();
	public String getDigestValue() throws GeneralSecurityException;
	public String getIssuerName();
	public BigInteger getX509SerialNumber();
}
