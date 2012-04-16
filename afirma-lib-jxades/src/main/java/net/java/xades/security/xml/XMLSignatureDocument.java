package net.java.xades.security.xml;

import java.io.File;
import java.io.OutputStream;
import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignatureProperties;
import javax.xml.crypto.dsig.SignatureProperty;
import javax.xml.crypto.dsig.SignedInfo;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.keyinfo.KeyValue;
import javax.xml.crypto.dsig.keyinfo.X509Data;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author miro
 */
public class XMLSignatureDocument
{
    private static final Logger logger = Logger.getLogger(XMLSignatureDocument.class.getName());

    private Element baseElement;
    private XMLSignatureFactory xmlSignatureFactory;
    private DigestMethod digestMethod;

    private XmlWrappedKeyInfo wrappedKeyInfo = XmlWrappedKeyInfo.CERTIFICATE;

    private List<SignatureProperties> listOfSignatureProperties = new ArrayList<SignatureProperties>();
    private List<SignatureProperty> defaultSignatureProperties = new ArrayList<SignatureProperty>();
    private String defaultSignaturePropertiesId;

    private List<XMLObject> xmlObjects = new ArrayList<XMLObject>();
    private List<XMLStructure> defaultXMLObjectItems = new ArrayList<XMLStructure>();
    private String defaultXMLObjectId;
    private String defaultXMLObjectMimeType;
    private String defaultXMLObjectEncoding;

    public XMLSignatureDocument(Element baseElement)
    {
        if(baseElement == null)
        {
            throw new IllegalArgumentException("Root/Base XML Element can not be NULL");
        }

        this.baseElement = baseElement;
    }

    public Element getBaseElement()
    {
        return baseElement;
    }

    protected XMLSignatureFactory getXMLSignatureFactory()
    {
        if(xmlSignatureFactory == null)
        {
            xmlSignatureFactory = XMLSignatureFactory.getInstance("DOM");
        }
        return xmlSignatureFactory;
    }

    public DigestMethod getDigestMethod()
        throws GeneralSecurityException
    {
        if(digestMethod == null)
        {
            digestMethod = getXMLSignatureFactory().newDigestMethod(DigestMethod.SHA1, null);
        }
        return digestMethod;
    }

    public void setDigestMethod(DigestMethod digestMethod)
    {
        this.digestMethod = digestMethod;
    }

    public List<XMLSignatureElement> getXMLSignatureElements()
    {
        NodeList nl = baseElement.getElementsByTagNameNS(XMLSignature.XMLNS,
                                                         XMLAdvancedSignature.ELEMENT_SIGNATURE);
        int size = nl.getLength();
        ArrayList<XMLSignatureElement> signatureElements = new ArrayList<XMLSignatureElement>(size);
        for(int i = 0; i < size; i++)
        {
            signatureElements.add(new XMLSignatureElement((Element)nl.item(i)));
        }

        return signatureElements;
    }

    public XmlWrappedKeyInfo getXmlWrappedKeyInfo()
    {
        return wrappedKeyInfo;
    }

    public void setXmlWrappedKeyInfo(XmlWrappedKeyInfo wrappedKeyInfo)
    {
        this.wrappedKeyInfo = wrappedKeyInfo;
    }

    public WrappedKeyStorePlace getWrappedKeyStorePlace()
    {
        return WrappedKeyStorePlace.KEY_INFO;
    }

    public void setWrappedKeyStorePlace(WrappedKeyStorePlace wrappedKeyStorePlace)
    {
    }

    protected Reference getReference(String uri)
        throws GeneralSecurityException
    {
        return getReference(uri, null);
    }

    protected Reference getReference(String uri, String type)
        throws GeneralSecurityException
    {
        return getReference(uri, null, type, null);
    }

    protected Reference getReference(String uri,
                                     List transforms,
                                     String type)
        throws GeneralSecurityException
    {
        return getReference(uri, transforms, type, null);
    }

    protected Reference getReference(String uri,
                                     List transforms,
                                     String type,
                                     String referenceId)
        throws GeneralSecurityException
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        DigestMethod dm = getDigestMethod();
        uri = uri.trim();
        if(!uri.startsWith("#"))
            uri = "#" + uri;
        return fac.newReference(uri, dm, transforms, type, referenceId);
    }

    protected List<Reference> getReferences(List idList)
        throws GeneralSecurityException
    {
        ArrayList<Reference> references = new ArrayList<Reference>(idList.size());
        for(Object id : idList)
        {
            if(id instanceof Reference)
                references.add((Reference)id);
            else
            {
                references.add(getReference((String)id));
            }
        }

        return references;
    }

    public List<SignatureStatus> validate()
    {
        ArrayList<SignatureStatus> validateResult;
        List<XMLSignatureElement> signatureElements = getXMLSignatureElements();
        validateResult = new ArrayList<SignatureStatus>(signatureElements.size());
        for(XMLSignatureElement signatureElement : signatureElements)
        {
            validateResult.add(signatureElement.validate());
        }

        return validateResult;
    }

    public void sign(X509Certificate certificate,
                     PrivateKey privateKey,
                     String signatureMethod,
                     List referencesIdList,
                     String signatureIdPrefix,
                     String xadesPrefix,
                     String xadesNamespace,
                     String xmlSignaturePrefix)
        throws MarshalException,
               XMLSignatureException,
               GeneralSecurityException
    {
        String signatureId = getSignatureId(signatureIdPrefix);
        String signatureValueId = getSignatureValueId(signatureIdPrefix);

        XMLSignatureFactory fac = getXMLSignatureFactory();
        CanonicalizationMethod cm = fac.newCanonicalizationMethod(
            CanonicalizationMethod.INCLUSIVE_WITH_COMMENTS,
            (C14NMethodParameterSpec)null);

        List<Reference> documentReferences = getReferences(referencesIdList);
        String keyInfoId = getKeyInfoId(signatureIdPrefix);
        documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod()));

        SignatureMethod sm = fac.newSignatureMethod(signatureMethod, null);
        SignedInfo si = fac.newSignedInfo(cm, sm, documentReferences);

        XMLSignature signature = fac.newXMLSignature(si,
                                                     newKeyInfo(certificate, keyInfoId),
                                                     getXMLObjects(),
                                                     signatureId,
                                                     signatureValueId);

        DOMSignContext signContext = new DOMSignContext(privateKey, baseElement);
        signContext.putNamespacePrefix(XMLSignature.XMLNS, xmlSignaturePrefix);
        signContext.putNamespacePrefix(xadesNamespace, xadesPrefix);

        signature.sign(signContext);
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

    public KeyInfo newKeyInfo(X509Certificate certificate, String keyInfoId)
        throws KeyException
    {
        KeyInfoFactory kif = getXMLSignatureFactory().getKeyInfoFactory();
        if(XmlWrappedKeyInfo.PUBLIC_KEY.equals(getXmlWrappedKeyInfo()))
        {
            KeyValue kv = kif.newKeyValue(certificate.getPublicKey());
            return kif.newKeyInfo(Collections.singletonList(kv), keyInfoId);
        }
        else
        {
            X509Data certData = kif.newX509Data(Collections.singletonList(certificate));
            return kif.newKeyInfo(Collections.singletonList(certData), keyInfoId);
        }
    }

    public List<SignatureProperties> getListOfSignatureProperties()
    {
        listOfSignatureProperties.add(getDefaultSignatureProperties());
        return listOfSignatureProperties;
    }

    public void setListOfSignatureProperties(List<SignatureProperties> listOfSignatureProperties)
    {
        this.listOfSignatureProperties = listOfSignatureProperties;
    }

    public void addSignatureProperty(SignatureProperty signatureProperty)
    {
        defaultSignatureProperties.add(signatureProperty);
    }

    public void addSignatureProperty(List<DOMStructure> content,
                                     String target,
                                     String id)
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        addSignatureProperty(fac.newSignatureProperty(content, target, id));
    }

    public SignatureProperties getDefaultSignatureProperties()
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        return fac.newSignatureProperties(defaultSignatureProperties, defaultSignaturePropertiesId);
    }

    public void setDefaultSignaturePropertiesId(String id)
    {
        defaultSignaturePropertiesId = id;
    }

    public String getDefaultSignaturePropertiesId()
    {
        return defaultSignaturePropertiesId;
    }

    public XMLObject newXMLObject(List<XMLStructure> xmlObjects)
    {
        return newXMLObject(xmlObjects, getDefaultXMLObjectId());
    }

    public XMLObject newXMLObject(List<XMLStructure> xmlObjects, String id)
    {
        return newXMLObject(xmlObjects, id, getDefaultXMLObjectMimeType());
    }

    public XMLObject newXMLObject(List<XMLStructure> xmlObjects, String id, String mimeType)
    {
        return newXMLObject(xmlObjects, id, mimeType, getDefaultXMLObjectEncoding());
    }

    public XMLObject newXMLObject(List<XMLStructure> xmlObjects,
                                  String id,
                                  String mimeType,
                                  String encoding)
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        return fac.newXMLObject(xmlObjects, id, mimeType, encoding);
    }

    public XMLObject addXMLObject(List<XMLStructure> xmlObjects)
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        return addXMLObject(fac.newXMLObject(xmlObjects,
                            getDefaultXMLObjectId(),
                            getDefaultXMLObjectMimeType(),
                            getDefaultXMLObjectEncoding()));
    }

    public XMLObject addXMLObject(XMLObject xmlObject)
    {
        xmlObjects.add(xmlObject);
        return xmlObject;
    }

    public List<XMLObject> getXMLObjects()
    {
        return xmlObjects;
    }

    public void addXMLObjectItem(XMLStructure xmlObjectItem)
    {
        defaultXMLObjectItems.add(xmlObjectItem);
    }

    public XMLObject getDefaultXMLObject()
    {
        XMLSignatureFactory fac = getXMLSignatureFactory();
        return fac.newXMLObject(getXMLObjectItems(),
                                getDefaultXMLObjectId(),
                                getDefaultXMLObjectMimeType(),
                                getDefaultXMLObjectEncoding());
    }

    public List<XMLStructure> getXMLObjectItems()
    {
        return defaultXMLObjectItems;
    }

    public void setDefaultXMLObjectId(String defaultXMLObjectId)
    {
        this.defaultXMLObjectId = defaultXMLObjectId;
    }

    public String getDefaultXMLObjectId()
    {
        return defaultXMLObjectId;
    }

    public void setDefaultXMLObjectMimeType(String defaultXMLObjectMimeType)
    {
        this.defaultXMLObjectMimeType = defaultXMLObjectMimeType;
    }

    public String getDefaultXMLObjectMimeType()
    {
        return defaultXMLObjectMimeType;
    }

    public void setDefaultXMLObjectEncoding(String defaultXMLObjectEncoding)
    {
        this.defaultXMLObjectEncoding = defaultXMLObjectEncoding;
    }

    public String getDefaultXMLObjectEncoding()
    {
        return defaultXMLObjectEncoding;
    }

    private static Document loadEncryptionDocument()
        throws Exception
    {
        String fileName = "BluesRecorderPro.log.wse";
        File encryptionFile = new File(fileName);
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document document = db.parse(encryptionFile);
        System.out.println("Encryption document loaded from " +
            encryptionFile.toString());
        return document;
    }

    public static void printDocument(Node node)
        throws TransformerException
    {
        OutputStream os = System.out;
        TransformerFactory tf = TransformerFactory.newInstance();
        Transformer trans = tf.newTransformer();
        trans.transform(new DOMSource(node), new StreamResult(os)); 
    }
}
