package net.java.xades.security.xml.XAdES;

import javax.xml.crypto.dsig.XMLSignature;
import org.w3c.dom.Node;

/*
 * 
 * <SignedProperties>
 *   <SignedSignatureProperties>
 *     (SigningTime)?
 *     (SigningCertificate)?
 *     (SignatureProductionPlace)?
 *     (SignerRole)?
 *   </SignedSignatureProperties>
 *   <SignedDataObjectProperties>
 *     (DataObjectFormat)*
 *     (CommitmentTypeIndication)*
 *     (AllDataObjectsTimeStamp)*
 *     (IndividualDataObjectsTimeStamp)*
 *   </SignedDataObjectProperties>
 *   </SignedProperties>
 *   
 */

/**
 * 
 * @author miro
 */
public class SignedProperties extends XAdESStructure
{
    private SignedSignatureProperties signedSignatureProperties;
    private SignedDataObjectProperties signedDataObjectProperties;

    public SignedProperties(QualifyingProperties qp, String signatureIdPrefix, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        super(qp, "SignedProperties", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        setAttributeNS(xadesNamespace, ID_ATTRIBUTE, signatureIdPrefix + "-SignedProperties");
    }

    public SignedProperties(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public SignedSignatureProperties getSignedSignatureProperties()
    {
        if (signedSignatureProperties == null)
        {
            signedSignatureProperties = new SignedSignatureProperties(this, xadesPrefix,
                    xadesNamespace, xmlSignaturePrefix);
        }

        return signedSignatureProperties;
    }

    public SignedDataObjectProperties getSignedDataObjectProperties()
    {
        if (signedDataObjectProperties == null)
        {
            signedDataObjectProperties = new SignedDataObjectProperties(this, xadesPrefix,
                    xadesNamespace, xmlSignaturePrefix);
        }

        return signedDataObjectProperties;
    }
}
