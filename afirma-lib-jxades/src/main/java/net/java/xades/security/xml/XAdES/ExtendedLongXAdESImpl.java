package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;

/*
    <ds:Signature ID?>
        ...
        <ds:Object>
            <QualifyingProperties>
                ...
                <UnsignedProperties>
                    <UnsignedSignatureProperties>
                        (CertificatesValues)
                        (RevocationValues)
                        (AttrAuthoritiesCertValues)?
                        (AttributeRevocationValues)?
                    </UnsignedSignatureProperties>
                </UnsignedProperties>
            </QualifyingProperties>
        </ds:Object>
    </ds:Signature>-
 */

/**
 * 
 * @author miro
 */
public class ExtendedLongXAdESImpl extends ExtendedXAdESImpl implements XAdES_X_L
{

    /*
     * public ExtendedLongXAdESImpl(Element baseElement, boolean useExplicitPolicy) {
     * super(baseElement, useExplicitPolicy); }
     */

    public ExtendedLongXAdESImpl(Element baseElement, boolean readOnlyMode, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix, String digestMethod)
    {
        super(baseElement, readOnlyMode, xadesPrefix, xadesNamespace, xmlSignaturePrefix, digestMethod);
    }
}
