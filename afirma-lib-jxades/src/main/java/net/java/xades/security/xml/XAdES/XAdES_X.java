package net.java.xades.security.xml.XAdES;

import java.security.cert.X509Certificate;
import java.util.Collection;

/**
 *
 * @author miro
 */
public interface XAdES_X
    extends XAdES_T
{
    public CompleteCertificateRefs getCompleteCertificateRefs();
    public void setCompleteCertificateRefs(Collection<X509Certificate> caCertificates);
    
    public CompleteRevocationRefs getCompleteRevocationRefs();
//    public void setCompleteRevocationRefs(CertValidationInfo certValidationInfo);    
}
