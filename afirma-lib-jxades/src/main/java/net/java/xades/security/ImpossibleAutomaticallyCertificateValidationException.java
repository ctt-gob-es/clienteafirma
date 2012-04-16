package net.java.xades.security;

/**
 *
 * @author miro
 */
public class ImpossibleAutomaticallyCertificateValidationException
    extends CertificateValidatorException
{
    public ImpossibleAutomaticallyCertificateValidationException()
    {
        super("Not enough or missing information of OCSP request or CRL distribution points for automatically certificate validation.");
    }
}
