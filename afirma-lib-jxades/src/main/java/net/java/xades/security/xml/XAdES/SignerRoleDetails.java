package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * 
 * <p:SignerRole>
 * <p:ClaimedRoles>
 * <p:ClaimedRole>
 * ANYTYPE
 * </p:ClaimedRole>
 * </p:ClaimedRoles>
 * <p:CertifiedRoles>
 * <p:CertifiedRole Encoding="http://tempuri.org" Id="id">
 * 0
 * </p:CertifiedRole>
 * </p:CertifiedRoles> </p:SignerRole>
 * 
 */

public class SignerRoleDetails extends XAdESStructure
{
    public SignerRoleDetails(SignedSignatureProperties ssp, SignerRole signerRole,
            String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(ssp, "SignerRole", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        Element claimedRoles = createElement("ClaimedRoles");
        Element certifiedRoles = createElement("ClaimedRoles");

        for (String sr : signerRole.getClaimedRole())
        {
            Element claimedRole = createElement("ClaimedRole");
            claimedRole.setTextContent(sr);
            claimedRoles.appendChild(claimedRole);
        }

        // TODO: Implement support for certified role and attribute certificates management
        for (String sr : signerRole.getCertifiedRole())
        {
            Element certifiedRole = createElement("CertifiedRole");
            certifiedRole.setTextContent(sr);
            certifiedRoles.appendChild(certifiedRole);
        }

        if (signerRole.getClaimedRole().size() > 0)
        {
            getNode().appendChild(claimedRoles);
        }

        if (signerRole.getCertifiedRole().size() > 0)
        {
            getNode().appendChild(certifiedRoles);
        }
    }

    public SignerRoleDetails(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }
}
