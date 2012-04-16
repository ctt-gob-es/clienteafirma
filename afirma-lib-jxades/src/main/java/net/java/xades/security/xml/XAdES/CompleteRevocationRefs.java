package net.java.xades.security.xml.XAdES;

import java.util.List;

/**
 *
 * @author miro
 */
public interface CompleteRevocationRefs
{
    public List<OCSPRef> getOCSPRefs();
    public List<CRLRef> getCRLRefs();
    public ValidationResult getValidationResult();
}
