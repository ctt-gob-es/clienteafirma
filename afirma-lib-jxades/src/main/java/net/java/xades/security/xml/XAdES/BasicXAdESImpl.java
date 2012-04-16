package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.TreeMap;

import javax.xml.crypto.MarshalException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * 
 * @author miro
 */
public class BasicXAdESImpl implements XAdES_BES
{
    protected boolean readOnlyMode = true;
    protected TreeMap<XAdES.Element, Object> data;
    private Element baseElement;
    private QualifyingProperties qualifyingProperties;

    public String xadesPrefix;
    public String xadesNamespace;
    public String xmlSignaturePrefix;
    public String digestMethod;

    public BasicXAdESImpl(Element baseElement, boolean readOnlyMode, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix, String digestMethod)
    {
        if (baseElement == null)
        {
            throw new IllegalArgumentException("Root/Base XML Element can not be NULL.");
        }
        this.baseElement = baseElement;
        this.readOnlyMode = readOnlyMode;

        data = new TreeMap<XAdES.Element, Object>();

        this.xadesPrefix = xadesPrefix;
        this.xadesNamespace = xadesNamespace;
        this.xmlSignaturePrefix = xmlSignaturePrefix;
        this.digestMethod = digestMethod;
    }

    public Element getBaseElement()
    {
        return baseElement;
    }

    public String getDigestMethod()
    {
        return digestMethod;
    }

    public Date getSigningTime()
    {
        return (Date) data.get(XAdES.Element.SIGNING_TIME);
    }

    public X509Certificate getSigningCertificate()
    {
        return (X509Certificate) data.get(XAdES.Element.SIGNING_CERTIFICATE);
    }

    public SignatureProductionPlace getSignatureProductionPlace()
    {
        return (SignatureProductionPlace) data.get(XAdES.Element.SIGNATURE_PRODUCTION_PLACE);
    }

    public SignerRole getSignerRole()
    {
        return (SignerRole) data.get(XAdES.Element.SIGNER_ROLE);
    }

    public Signer getSigner()
    {
        return (Signer) data.get(XAdES.Element.SIGNER);
    }

    @SuppressWarnings("unchecked")
    public List<DataObjectFormat> getDataObjectFormats()
    {
        return (List<DataObjectFormat>) data.get(XAdES.Element.DATA_OBJECT_FORMATS);
    }

    @SuppressWarnings("unchecked")
    public List<CommitmentTypeIndication> getCommitmentTypeIndications()
    {
        return (List<CommitmentTypeIndication>) data.get(XAdES.Element.COMMITMENT_TYPE_INDICATIONS);
    }

    @SuppressWarnings("unchecked")
    public List<AllDataObjectsTimeStamp> getAllDataObjectsTimeStamps()
    {
        return (List<AllDataObjectsTimeStamp>) data.get(XAdES.Element.ALL_DATA_OBJECTS_TIMESTAMPS);
    }

    @SuppressWarnings("unchecked")
    public List<XAdESTimeStamp> getIndividualDataObjectsTimeStamps()
    {
        return (List<XAdESTimeStamp>) data.get(XAdES.Element.INDIVIDUAL_DATA_OBJECTS_TIMESTAMPS);
    }

    @SuppressWarnings("unchecked")
    public List<CounterSignature> getCounterSignatures()
    {
        return (List<CounterSignature>) data.get(XAdES.Element.COUNTER_SIGNATURES);
    }

    @SuppressWarnings("unchecked")
    public List<SignatureTimeStamp> getSignatureTimeStamps()
    {
        return (List<SignatureTimeStamp>) data.get(XAdES.Element.SIGNATURE_TIME_STAMP);
    }

    public void setSigningTime(Date signingTime)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (signingTime != null)
            data.put(XAdES.Element.SIGNING_TIME, signingTime);
        else
            data.remove(XAdES.Element.SIGNING_TIME);
    }

    public void setSigningCertificate(X509Certificate certificate)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        SigningCertificateImpl sci = new SigningCertificateImpl(certificate, digestMethod);

        if (certificate != null)
            data.put(XAdES.Element.SIGNING_CERTIFICATE, sci);
        else
            data.remove(XAdES.Element.SIGNING_CERTIFICATE);
    }

    public void setSignatureProductionPlace(SignatureProductionPlace productionPlace)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (productionPlace != null)
            data.put(XAdES.Element.SIGNATURE_PRODUCTION_PLACE, productionPlace);
        else
            data.remove(XAdES.Element.SIGNATURE_PRODUCTION_PLACE);
    }

    public void setSignerRole(SignerRole signerRole)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (signerRole != null)
            data.put(XAdES.Element.SIGNER_ROLE, signerRole);
        else
            data.remove(XAdES.Element.SIGNER_ROLE);
    }

    public void setSigner(Signer signer)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (signer != null)
            data.put(XAdES.Element.SIGNER, signer);
        else
            data.remove(XAdES.Element.SIGNER);
    }

    public void setDataObjectFormats(List<DataObjectFormat> dataObjectFormats)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (dataObjectFormats != null && dataObjectFormats.size() > 0)
            data.put(XAdES.Element.DATA_OBJECT_FORMATS, dataObjectFormats);
        else
            data.remove(XAdES.Element.DATA_OBJECT_FORMATS);
    }

    public void setCommitmentTypeIndications(
            List<CommitmentTypeIndication> commitmentTypeIndications)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (commitmentTypeIndications != null && commitmentTypeIndications.size() > 0)
            data.put(XAdES.Element.COMMITMENT_TYPE_INDICATIONS, commitmentTypeIndications);
        else
            data.remove(XAdES.Element.COMMITMENT_TYPE_INDICATIONS);
    }

    public void setAllDataObjectsTimeStamps(List<AllDataObjectsTimeStamp> allDataObjectsTimeStamps)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (allDataObjectsTimeStamps != null && allDataObjectsTimeStamps.size() > 0)
            data.put(XAdES.Element.ALL_DATA_OBJECTS_TIMESTAMPS, allDataObjectsTimeStamps);
        else
            data.remove(XAdES.Element.ALL_DATA_OBJECTS_TIMESTAMPS);
    }

    public void setIndividualDataObjectsTimeStamps(
            List<IndividualDataObjectsTimeStamp> individualDataObjectsTimeStamps)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (individualDataObjectsTimeStamps != null && individualDataObjectsTimeStamps.size() > 0)
            data.put(XAdES.Element.INDIVIDUAL_DATA_OBJECTS_TIMESTAMPS,
                    individualDataObjectsTimeStamps);
        else
            data.remove(XAdES.Element.INDIVIDUAL_DATA_OBJECTS_TIMESTAMPS);
    }

    public void setCounterSignatures(List<CounterSignature> counterSignatures)
    {
        if (readOnlyMode)
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");

        if (counterSignatures != null && counterSignatures.size() > 0)
            data.put(XAdES.Element.COUNTER_SIGNATURES, counterSignatures);
        else
            data.remove(XAdES.Element.COUNTER_SIGNATURES);
    }

    public void setSignatureTimeStamps(List<SignatureTimeStamp> signatureTimeStamps)
    {
        if (readOnlyMode)
        {
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");
        }

        if (signatureTimeStamps != null && signatureTimeStamps.size() > 0)
        {
            data.put(XAdES.Element.SIGNATURE_TIME_STAMP, signatureTimeStamps);
        }
        else
        {
            data.remove(XAdES.Element.SIGNATURE_TIME_STAMP);
        }
    }

    // public void setCompleteCertificateRefs(Collection<X509Certificate> caCertificates)
    // {
    // if (readOnlyMode)
    // {
    // throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");
    // }
    //
    // if (caCertificates != null && caCertificates.size() > 0)
    // {
    // data.put(XAdES.Element.COMPLETE_CERTIFICATE_REFS, caCertificates);
    // }
    // else
    // {
    // data.remove(XAdES.Element.COMPLETE_CERTIFICATE_REFS);
    // }
    // }

    // Each implementation have to inherit this method
    // and to return the appropriate XAdES type.
    // This is important for checking cases in new XML Advanced Signature
    protected XAdES getXAdESType()
    {
        return XAdES.BES;
    }

    protected QualifyingProperties getQualifyingProperties()
    {
        if (qualifyingProperties == null)
        {
            NodeList nl = baseElement.getElementsByTagNameNS(xadesNamespace,
                    XAdES.Element.QUALIFYING_PROPERTIES.getElementName());
            if (nl != null && nl.getLength() > 0)
            {
                qualifyingProperties = new QualifyingProperties(nl.item(0), xadesPrefix,
                        xadesNamespace, xmlSignaturePrefix);
            }
        }

        return qualifyingProperties;
    }

    protected SignedSignatureProperties getSignedSignatureProperties(QualifyingProperties qp)
    {
        return qp.getSignedProperties().getSignedSignatureProperties();
    }

    protected SignedDataObjectProperties getSignedDataObjectProperties(QualifyingProperties qp)
    {
        return qp.getSignedProperties().getSignedDataObjectProperties();
    }

    protected UnsignedSignatureProperties getUnsignedSignatureProperties(QualifyingProperties qp)
    {
        return qp.getUnsignedProperties().getUnsignedSignatureProperties();
    }

    @SuppressWarnings("unchecked")
    protected void marshalQualifyingProperties(QualifyingProperties qp, String signatureIdPrefix,
            List referencesIdList, String tsaURL) throws MarshalException
    {
        SignedSignatureProperties ssp;
        SignedDataObjectProperties sdop;
        UnsignedSignatureProperties usp;

        try
        {
            for (XAdES.Element key : XAdES.Element.values())
            {
                if (XAdES.Element.SIGNING_TIME.equals(key))
                {
                    ssp = getSignedSignatureProperties(qp);
                    ssp.setSigningTime();
                }
                else
                {
                    Object value = data.get(key);

                    if (value != null)
                    {
                        if (XAdES.Element.SIGNER.equals(key))
                        {
                            ssp = getSignedSignatureProperties(qp);
                            ssp.setSigner((Signer) value);
                        }
                        else if (XAdES.Element.SIGNING_CERTIFICATE.equals(key))
                        {
                            ssp = getSignedSignatureProperties(qp);
                            ssp.setSigningCertificate((SigningCertificate) value);
                        }
                        else if (XAdES.Element.SIGNATURE_POLICY_IDENTIFIER.equals(key))
                        {
                            ssp = getSignedSignatureProperties(qp);
                            ssp.setSignaturePolicyIdentifier((SignaturePolicyIdentifier) value);
                        }
                        else if (XAdES.Element.SIGNATURE_PRODUCTION_PLACE.equals(key))
                        {
                            ssp = getSignedSignatureProperties(qp);
                            ssp.setSignatureProductionPlace((SignatureProductionPlace) value);
                        }
                        else if (XAdES.Element.SIGNER_ROLE.equals(key))
                        {
                            ssp = getSignedSignatureProperties(qp);
                            ssp.setSignerRole((SignerRole) value);
                        }
                        else if (XAdES.Element.DATA_OBJECT_FORMATS.equals(key))
                        {
                            sdop = getSignedDataObjectProperties(qp);
                            sdop.setDataObjectFormat((ArrayList<DataObjectFormat>) value);
                        }
                        else if (XAdES.Element.COMMITMENT_TYPE_INDICATIONS.equals(key))
                        {
                            // TODO: Manage CommitmentTypeIndication as a ArrayList
                            sdop = getSignedDataObjectProperties(qp);
                            sdop
                                    .setCommitmentTypeIndication(((ArrayList<CommitmentTypeIndication>) value)
                                            .get(0));
                        }
                        else if (XAdES.Element.ALL_DATA_OBJECTS_TIMESTAMPS.equals(key))
                        {
                            sdop = getSignedDataObjectProperties(qp);
                            sdop.setAllDataObjectsTimeStamp(
                                    (ArrayList<AllDataObjectsTimeStamp>) value, tsaURL);
                        }
                        else if (XAdES.Element.INDIVIDUAL_DATA_OBJECTS_TIMESTAMPS.equals(key))
                        {
                            sdop = getSignedDataObjectProperties(qp);
                            sdop.setIndividualDataObjectsTimeStamp(
                                    (ArrayList<IndividualDataObjectsTimeStamp>) value, tsaURL);
                        }
                        else if (XAdES.Element.COMPLETE_CERTIFICATE_REFS.equals(key))
                        {
                            usp = getUnsignedSignatureProperties(qp);
                            usp.setCompleteCertificateRefs((Collection<X509Certificate>) value,
                                    signatureIdPrefix);
                        }
                        // else if(XAdES.Element.COMPLETE_REVOCATION_REFS.equals(key))
                        // {
                        // usp = getUnsignedSignatureProperties(qp);
                        // usp.setCompleteRevocationRefs((CertValidationInfo)value,
                        // signatureIdPrefix);
                        // }
                        else if (XAdES.Element.SIGNATURE_TIME_STAMP.equals(key))
                        {
                            usp = getUnsignedSignatureProperties(qp);
                            usp
                                    .setSignatureTimeStamp((ArrayList<SignatureTimeStamp>) value,
                                            tsaURL);
                        }
                    }
                }
            }
        }
        catch (GeneralSecurityException ex)
        {
            throw new MarshalException(ex);
        }
    }

    public String getXadesPrefix()
    {
        return this.xadesPrefix;
    }

    public String getXmlSignaturePrefix()
    {
        return this.xmlSignaturePrefix;
    }

    public String getXadesNamespace()
    {
        return this.xadesNamespace;
    }
}
