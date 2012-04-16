package net.java.xades.security;

/**
 *
 * @author miro
 */
public class ImpossibleCertificateValidationException
    extends CertificateValidatorException
{
    public ImpossibleCertificateValidationException()
    {
        super("Missing Authority Certificate in Trust CA Certificates collection.");
    }
}
