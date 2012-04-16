package net.java.xades.security.xml.XAdES;

import java.io.IOException;
import java.security.AccessController;
import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Security;
import java.security.SignatureException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignedInfo;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.keyinfo.KeyValue;
import javax.xml.crypto.dsig.keyinfo.X509Data;
import javax.xml.crypto.dsig.keyinfo.X509IssuerSerial;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.parsers.ParserConfigurationException;

import net.java.xades.security.timestamp.TimeStampFactory;
import net.java.xades.security.xml.DOMCanonicalizationFactory;
import net.java.xades.security.xml.SignatureStatus;
import net.java.xades.security.xml.WrappedKeyStorePlace;
import net.java.xades.security.xml.XMLSignatureElement;
import net.java.xades.security.xml.XmlWrappedKeyInfo;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.c14n.CanonicalizationException;
import com.sun.org.apache.xml.internal.security.c14n.InvalidCanonicalizerException;
import com.sun.org.apache.xml.internal.security.utils.Base64;

/**
 * 
 * @author miro
 */
public class XMLAdvancedSignature
{
    public static final String XADES_v132 = "http://uri.etsi.org/01903/v1.3.2#";
    public static final String XADES_v141 = "http://uri.etsi.org/01903/v1.4.1#";
    
    public String signedPropertiesTypeUrl = "http://uri.etsi.org/01903#SignedProperties";

    public static final String ELEMENT_SIGNATURE = "Signature";
    public static final String ELEMENT_SIGNATURE_VALUE = "SignatureValue";

    protected BasicXAdESImpl xades;
    protected Element baseElement;
    protected XMLSignatureFactory xmlSignatureFactory;
    protected DigestMethod digestMethod;
    protected String xadesNamespace;
    protected XmlWrappedKeyInfo wrappedKeyInfo = XmlWrappedKeyInfo.CERTIFICATE;

    protected List<XMLObject> xmlObjects = new ArrayList<XMLObject>();

    protected List<XMLStructure> defaultXMLObjectItems = new ArrayList<XMLStructure>();
    protected String defaultXMLObjectId;
    protected String defaultXMLObjectMimeType;
    protected String defaultXMLObjectEncoding;

    protected XMLSignature signature;
    protected DOMSignContext signContext;

    static
    {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>()
        {
            public Void run()
            {
                if (System.getProperty("java.version").startsWith("1.5"))
                {
                    try
                    {
                        Security.addProvider(new org.jcp.xml.dsig.internal.dom.XMLDSigRI());
                    }
                    catch (Throwable e)
                    {
                        e.printStackTrace();
                    }
                }
                return null;
            }
        });
    }

    protected XMLAdvancedSignature(XAdES_BES xades)
    {
        if (xades == null)
        {
            throw new IllegalArgumentException("XAdES parameter can not be NULL.");
        }

        this.baseElement = xades.getBaseElement();

        if (this.baseElement == null)
        {
            throw new IllegalArgumentException("Root/Base XML Element can not be NULL.");
        }

        this.xades = (BasicXAdESImpl) xades;
    }

    public static XMLAdvancedSignature newInstance(XAdES_BES xades) throws GeneralSecurityException
    {
        XMLAdvancedSignature result = new XMLAdvancedSignature(xades);
        result.setDigestMethod(xades.getDigestMethod());
        result.setXadesNamespace(xades.getXadesNamespace());

        return result;
    }

    public static XMLAdvancedSignature getInstance(XAdES_BES xades) throws GeneralSecurityException
    {
        return newInstance(xades);
    }

    public Element getBaseElement()
    {
        return this.baseElement;
    }

    public void setXadesNamespace(String xadesNamespace)
    {
        this.xadesNamespace = xadesNamespace;
    }
    
    public void setSignedPropertiesTypeUrl(String signedPropertiesTypeUrl) 
    {
    	this.signedPropertiesTypeUrl = signedPropertiesTypeUrl;
    }

    public void sign(X509Certificate certificate, PrivateKey privateKey, String signatureMethod,
            List refsIdList, String signatureIdPrefix, String tsaURL) throws MarshalException,
            XMLSignatureException, GeneralSecurityException, TransformException,
            InvalidCanonicalizerException, CanonicalizationException, IOException,
            ParserConfigurationException, SAXException
    {
        List referencesIdList = new ArrayList(refsIdList);

        if (WrappedKeyStorePlace.SIGNING_CERTIFICATE_PROPERTY.equals(getWrappedKeyStorePlace()))
        {
            this.xades.setSigningCertificate(certificate);
        }
        else
        {
            /*
             * @ToDo The ds:KeyInfo element also MAY contain other certificates forming a chain that
             * MAY reach the point of trust;
             */
        }

        XMLObject xadesObject = marshalXMLSignature(this.xadesNamespace, 
        		this.signedPropertiesTypeUrl, signatureIdPrefix,
                referencesIdList, tsaURL);
        addXMLObject(xadesObject);

        String signatureId = getSignatureId(signatureIdPrefix);
        String signatureValueId = getSignatureValueId(signatureIdPrefix);

        XMLSignatureFactory fac = getXMLSignatureFactory();
        CanonicalizationMethod cm = fac.newCanonicalizationMethod(
                CanonicalizationMethod.INCLUSIVE, (C14NMethodParameterSpec) null);

        List<Reference> documentReferences = getReferences(referencesIdList);
        String keyInfoId = getKeyInfoId(signatureIdPrefix);
        documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod()));

        SignatureMethod sm = fac.newSignatureMethod(signatureMethod, null);
        SignedInfo si = fac.newSignedInfo(cm, sm, documentReferences);

        this.signature = fac.newXMLSignature(si, newKeyInfo(certificate, keyInfoId),
                getXMLObjects(), signatureId, signatureValueId);

        this.signContext = new DOMSignContext(privateKey, this.baseElement);
        this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
        this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());

        this.signature.sign(this.signContext);

        enrichUnsignedProperties(tsaURL);
    }

    public void enrichUnsignedProperties(String tsaURL) throws TransformException,
            MarshalException, NoSuchAlgorithmException, SignatureException, IOException,
            InvalidCanonicalizerException, CanonicalizationException, ParserConfigurationException,
            SAXException
    {
        if (this.signature == null)
        {
            throw new IllegalStateException(
                    "Can not find Signature. You must call sign method firs to generate it");
        }

        if (this.xades instanceof TimestampXAdESImpl)
        {
            //
            // SignatureTimeStamp
            //            

            NodeList unsignedProperties = this.baseElement.getElementsByTagNameNS(this.xadesNamespace,
                    "UnsignedSignatureProperties");
            NodeList signatureValue = this.baseElement.getElementsByTagNameNS(XMLSignature.XMLNS,
                    "SignatureValue");
            NodeList canonicalizationMethod = this.baseElement.getElementsByTagNameNS(
                    XMLSignature.XMLNS, "CanonicalizationMethod");

            if (unsignedProperties != null && unsignedProperties.getLength() == 1
                    && signatureValue != null && signatureValue.getLength() == 1
                    && canonicalizationMethod != null && canonicalizationMethod.getLength() == 1)
            {
                // Determine c14n algorithm
                String c14nAlgorithm = canonicalizationMethod.item(0).getAttributes().getNamedItem(
                        "Algorithm").getTextContent();

                // c14n signatureValue node
                byte[] c10nSignatureValue = DOMCanonicalizationFactory.c14n(c14nAlgorithm,
                        signatureValue.item(0));

                // Generate timestamp with the c14n result
                byte[] timestampData = TimeStampFactory.getTimeStamp(tsaURL, c10nSignatureValue,
                        true);

                // Append new SignatureTimeStamp node to UnsignedSignatureProperties
                Element encapsulatedTimeStamp = this.baseElement.getOwnerDocument()
                        .createElementNS(this.xadesNamespace, "EncapsulatedTimeStamp");
                encapsulatedTimeStamp.setPrefix(this.xades.getXadesPrefix());
                encapsulatedTimeStamp.setTextContent(Base64.encode(timestampData));

                Element signatureTimestamp = this.baseElement.getOwnerDocument().createElementNS(
                        this.xadesNamespace, "SignatureTimeStamp");
                signatureTimestamp.setPrefix(this.xades.getXadesPrefix());
                signatureTimestamp.appendChild(encapsulatedTimeStamp);
                signatureTimestamp.setAttributeNS(this.xadesNamespace, "Id", "TS1-SignatureTimeStamp");

                unsignedProperties.item(0).appendChild(signatureTimestamp);
            }
            else
            {
                throw new MarshalException(
                        "UnsignedProperties section not found in signature. Unable to generate SignatureTimeStamp element.");
            }
        }
    }

    public List<SignatureStatus> validate()
    {
        ArrayList<SignatureStatus> validateResult;
        List<XMLSignatureElement> signatureElements = getXMLSignatureElements();
        validateResult = new ArrayList<SignatureStatus>(signatureElements.size());
        for (XMLSignatureElement signatureElement : signatureElements)
        {
            validateResult.add(signatureElement.validate());
        }

        return validateResult;
    }

    public WrappedKeyStorePlace getWrappedKeyStorePlace()
    {
        return WrappedKeyStorePlace.KEY_INFO;
    }

    public void setWrappedKeyStorePlace(WrappedKeyStorePlace wrappedKeyStorePlace)
    {
    }

    public XmlWrappedKeyInfo getXmlWrappedKeyInfo()
    {
        return this.wrappedKeyInfo;
    }

    public List<XMLObject> getXMLObjects()
    {
        return this.xmlObjects;
    }

    public void setXmlWrappedKeyInfo(XmlWrappedKeyInfo wrappedKeyInfo)
    {
        this.wrappedKeyInfo = wrappedKeyInfo;
    }

    protected List<XMLSignatureElement> getXMLSignatureElements()
    {
        NodeList nl = this.baseElement.getElementsByTagNameNS(XMLSignature.XMLNS, ELEMENT_SIGNATURE);
        int size = nl.getLength();
        ArrayList<XMLSignatureElement> signatureElements = new ArrayList<XMLSignatureElement>(size);
        for (int i = 0; i < size; i++)
        {
            signatureElements.add(new XMLSignatureElement((Element) nl.item(i)));
        }

        return signatureElements;
    }

    protected String getSignatureId(String idPrefix)
    {
        return idPrefix + "-Signature";
    }

    protected String getSignatureValueId(String idPrefix)
    {
        return idPrefix + "-SignatureValue";
    }

    protected String getKeyInfoId(String idPrefix)
    {
        return idPrefix + "-KeyInfo";
    }

    protected XMLSignatureFactory getXMLSignatureFactory()
    {
        if (this.xmlSignatureFactory == null)
        {
            this.xmlSignatureFactory = XMLSignatureFactory.getInstance("DOM");
        }
        return this.xmlSignatureFactory;
    }

    protected Reference getReference(String uri) throws GeneralSecurityException
    {
        return getReference(uri, null);
    }

    protected Reference getReference(String uri, String type) throws GeneralSecurityException
    {
        return getReference(uri, null, type, null);
    }

    protected Reference getReference(String uri, List<Transform> transforms, String type)
            throws GeneralSecurityException
    {
        return getReference(uri, transforms, type, null);
    }

    protected Reference getReference(String uri, List<Transform> transforms, String type,
            String referenceId) throws GeneralSecurityException
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        DigestMethod dm = getDigestMethod();
        uri = uri.trim();

        if (uri.equals(""))
        {
            Transform envelopedTransform = fac.newTransform(Transform.ENVELOPED,
                    (TransformParameterSpec) null);

            if (transforms != null)
            {
                transforms.add(envelopedTransform);
            }
            else
            {
                transforms = Collections.singletonList(envelopedTransform);
            }
        }
        else if (!uri.startsWith("#"))
        {
            uri = "#" + uri;
        }

        return fac.newReference(uri, dm, transforms, type, referenceId);
    }

    protected List<Reference> getReferences(List<?> idList) throws GeneralSecurityException
    {
        ArrayList<Reference> references = new ArrayList<Reference>(idList.size());

        for (Object id : idList)
        {
            if (id instanceof Reference)
            {
                references.add((Reference) id);
            }
            else
            {
                references.add(getReference((String) id));
            }
        }

        return references;
    }

    protected DigestMethod getDigestMethod() throws GeneralSecurityException
    {
        if (this.digestMethod == null)
        {
            this.digestMethod = getXMLSignatureFactory().newDigestMethod(DigestMethod.SHA1, null);
        }
        return this.digestMethod;
    }

    public void setDigestMethod(String method) throws GeneralSecurityException
    {
        this.digestMethod = getXMLSignatureFactory().newDigestMethod(method, null);
    }

    protected KeyInfo newKeyInfo(X509Certificate certificate, String keyInfoId) throws KeyException
    {
        KeyInfoFactory keyInfoFactory = getXMLSignatureFactory().getKeyInfoFactory();
        KeyValue keyValue = keyInfoFactory.newKeyValue(certificate.getPublicKey());

        List<Object> x509DataList = new ArrayList<Object>();

        if (!XmlWrappedKeyInfo.PUBLIC_KEY.equals(getXmlWrappedKeyInfo()))
        {
            x509DataList.add(certificate);
        }

        X509IssuerSerial x509IssuerSerial = keyInfoFactory.newX509IssuerSerial(certificate
                .getIssuerDN().getName(), certificate.getSerialNumber());

        x509DataList.add(certificate.getSubjectX500Principal().getName("RFC1779"));
        x509DataList.add(x509IssuerSerial);

        X509Data x509Data = keyInfoFactory.newX509Data(x509DataList);

        List<Object> newList = new ArrayList<Object>();
        newList.add(keyValue);
        newList.add(x509Data);

        return keyInfoFactory.newKeyInfo(newList, keyInfoId);
    }

    protected XMLObject newXMLObject(List<XMLStructure> xmlObjects)
    {
        return newXMLObject(xmlObjects, getDefaultXMLObjectId());
    }

    protected XMLObject newXMLObject(List<XMLStructure> xmlObjects, String id)
    {
        return newXMLObject(xmlObjects, id, getDefaultXMLObjectMimeType());
    }

    protected XMLObject newXMLObject(List<XMLStructure> xmlObjects, String id, String mimeType)
    {
        return newXMLObject(xmlObjects, id, mimeType, getDefaultXMLObjectEncoding());
    }

    protected XMLObject newXMLObject(List<XMLStructure> xmlObjects, String id, String mimeType,
            String encoding)
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        return fac.newXMLObject(xmlObjects, id, mimeType, encoding);
    }

    protected String getDefaultXMLObjectId()
    {
        return this.defaultXMLObjectId;
    }

    protected String getDefaultXMLObjectMimeType()
    {
        return this.defaultXMLObjectMimeType;
    }

    protected String getDefaultXMLObjectEncoding()
    {
        return this.defaultXMLObjectEncoding;
    }

    public XMLObject addXMLObject(XMLObject xmlObject)
    {
        this.xmlObjects.add(xmlObject);
        return xmlObject;
    }

    private List<QualifyingPropertiesReference> qualifyingPropertiesReferences;
//    private WrappedKeyStorePlace wrappedKeyStorePlace = WrappedKeyStorePlace.KEY_INFO;

    protected QualifyingProperties marshalQualifyingProperties(String xmlNamespace,
            String signedPropertiesTypeUrl, String signatureIdPrefix, List referencesIdList, 
            String tsaURL)
            throws GeneralSecurityException, MarshalException
    {
        QualifyingProperties qp;
        qp = new QualifyingProperties(getBaseElement(), signatureIdPrefix, this.xades.getXadesPrefix(),
                xmlNamespace, this.xades.getXmlSignaturePrefix());

        this.xades.marshalQualifyingProperties(qp, signatureIdPrefix, referencesIdList, tsaURL);

        SignedProperties sp = qp.getSignedProperties();
//        UnsignedProperties up = qp.getUnsignedProperties();
//        UnsignedSignatureProperties usp = qp.getUnsignedProperties()
//                .getUnsignedSignatureProperties();

        List transforms = null;
        String spId = sp.getId();
        Reference spReference = getReference(spId, transforms, this.signedPropertiesTypeUrl);
        referencesIdList.add(spReference);

        return qp;
    }

    protected XMLObject marshalXMLSignature(String xadesNamespace, 
    		String signedPropertiesTypeUrl, String signatureIdPrefix,
            List referencesIdList, String tsaURL) throws GeneralSecurityException, MarshalException
    {
        QualifyingProperties qp;
        qp = marshalQualifyingProperties(xadesNamespace, signedPropertiesTypeUrl,
        		signatureIdPrefix, referencesIdList, tsaURL);

        List<QualifyingPropertiesReference> qpr = getQualifyingPropertiesReferences();
        ArrayList<XMLStructure> content = new ArrayList<XMLStructure>(qpr.size() + 1);
        content.add(qp);
        content.addAll(qpr);

        return newXMLObject(content);
    }

    public List<QualifyingPropertiesReference> getQualifyingPropertiesReferences()
    {
        if (this.qualifyingPropertiesReferences == null)
        {
            this.qualifyingPropertiesReferences = new ArrayList<QualifyingPropertiesReference>();
        }

        return this.qualifyingPropertiesReferences;
    }

    public void setQualifyingPropertiesReferences(List<QualifyingPropertiesReference> refs)
    {
        this.qualifyingPropertiesReferences = refs;
    }
}
