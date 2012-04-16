package net.java.xades.security.xml.XAdES;

import net.java.xades.util.ObjectId;
import net.java.xades.util.OccursRequirement;

/**
 *
 * @author miro
 */
public interface XadesElement
{
    public XAdES getXAdES();
    public ObjectId getObjectId();
    public String getElementName();
    public OccursRequirement getOccursRequirement();
    public XadesElement getParent();
}
