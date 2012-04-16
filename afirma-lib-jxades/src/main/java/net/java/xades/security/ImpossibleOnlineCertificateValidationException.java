package net.java.xades.security;

/**
 *
 * @author miro
 */
public class ImpossibleOnlineCertificateValidationException
    extends CertificateValidatorException
{
    public ImpossibleOnlineCertificateValidationException()
    {
        super("Missing OCSP data and CRL distribution points for online certificate validation.");
    }
}
