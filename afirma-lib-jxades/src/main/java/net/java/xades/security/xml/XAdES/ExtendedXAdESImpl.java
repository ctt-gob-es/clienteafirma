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
 ((SigAndRefsTimeStamp)*
 (RefsOnlyTimeStamp)*)
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
public class ExtendedXAdESImpl extends CompleteValidationXAdESImpl implements XAdES_X
{

    /*
     * public ExtendedXAdES(Element baseElement, boolean useExplicitPolicy) { super(baseElement,
     * useExplicitPolicy); }
     */

    public ExtendedXAdESImpl(Element baseElement, boolean readOnlyMode, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix, String digestMethod)
    {
        super(baseElement, readOnlyMode, xadesPrefix, xadesNamespace, xmlSignaturePrefix,
                digestMethod);
    }
}
