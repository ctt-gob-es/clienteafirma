package net.java.xades.security;

import java.security.GeneralSecurityException;

/**
 *
 * @author miro
 */
public class CertificateValidatorException
    extends GeneralSecurityException
{
    
    /** Creates a new instance of CertificateValidatorException */
    public CertificateValidatorException(String message, Throwable cause)
    {
        super(message, cause);
    }
    
    public CertificateValidatorException(String message)
    {
        super(message);
    }

    public CertificateValidatorException(Throwable cause)
    {
        super(cause);
    }
}
