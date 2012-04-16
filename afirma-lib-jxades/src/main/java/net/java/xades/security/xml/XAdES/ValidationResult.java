package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Node;


/*
    <ValidationResult
            Status              =  "VALID,
                                    REVOKED,
                                    INVALID,
                                    EXPIRED,
                                    NOT_YET_VALID,
                                    IMPOSSIBLE_VALIDATION,
                                    UNKNOWN"

            OCSPResponseStatus  =  "SUCCESSFUL,
                                    MALFORMED_REQUEST,
                                    INTERNAL_ERROR,
                                    TRY_AGAIN_LATER,
                                    UNUSED_STATUS_CODE,
                                    REQUEST_MUST_BE_SIGNED,
                                    REQUEST_IS_UNAUTHORIZED,
                                    UNKNOWN_STATUS_CODE"

            OCSPCertStatus      =  "GOOD,
                                    REVOKED,
                                    UNKNOWN,
                                    UNKNOWN_STATUS_CODE"

            CRLReasonStatus     =  "UNSPECIFIED,
                                    KEY_COMPROMISE,
                                    CA_COMPROMISE,
                                    AFFILIATION_CHANGED,
                                    SUPERSEDED,
                                    CESSATION_OF_OPERATION,
                                    CERTIFICATE_HOLD,
                                    MISSING_IN_SYNTAX,
                                    REMOVE_FROM_CRL,
                                    PRIVILEGE_WITHDRAWN,
                                    AA_COMPROMISE,
                                    UNREVOKED,
                                    UNKNOWN,
                                    UNRECOGNIZED_REASON_CODE">
        <ValidationTime />
        <Exception />
    </ValidationResult>
*/

/**
 *
 * @author miro
 */
public class ValidationResult
    extends XAdESStructure
{

    public ValidationResult(Node node, String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

//    public ValidationResult(XAdESStructure parent, CertValidationInfo certValidationInfo)
//    {
//        this(parent);
//        Element thisElement = getElement();
//
//        thisElement.setAttribute("Status", certValidationInfo.getStatus().name());
//
//        Date validationTime = certValidationInfo.getValidationTime();
//        if(validationTime != null)
//        {
//            Element element = createElement("ValidationTime");
//            element.setTextContent(SystemUtils.formatDate(validationTime));
//            thisElement.appendChild(element);
//        }
//
//        Exception ex = certValidationInfo.getUnrecognizedException();
//        if(ex != null)
//        {
//            Element element = createElement("Exception");
//            thisElement.appendChild(element);
//            ByteArrayOutputStream outStream = new ByteArrayOutputStream();
//            ex.printStackTrace(new PrintStream(outStream));
//            element.setTextContent(outStream.toString());
//        }
//    }
//
//    public ValidationResult(XAdESStructure parent, XAdESRevocationStatus revocationStatus)
//    {
//        this(parent);
//        Element thisElement = getElement();
//
//        OCSPResponse ocspResponse = revocationStatus.getOCSPResponse();
//        if(ocspResponse != null)
//        {
//            thisElement.setAttribute("OCSPResponseStatus", ocspResponse.getOCSPResponseStatus().name());
//            SingleResponse singleResponse = ocspResponse.getSingleResponse();
//            if(singleResponse != null)
//            {
//                thisElement.setAttribute("OCSPCertStatus", singleResponse.getCertStatus().name());
//            }
//        }
//
//        if(revocationStatus != null)
//        {
//            CertPathRevocationStatus status = revocationStatus.getStatus();
//            if(status != null && status instanceof CRLReasonStatus)
//            {
//                thisElement.setAttribute("CRLReasonStatus", ((Enum)status).name());
//            }
//        }
//
//        Exception ex = revocationStatus.getException();
//        if(ex != null)
//        {
//            Element element = createElement("Exception");
//            thisElement.appendChild(element);
//            StringBuilder sb = new StringBuilder();
//            Throwable cause = ex;
//            while(cause != null)
//            {
//                if(cause != ex)
//                    sb.append("\n");
//                sb.append(cause.toString());
//
//                cause = cause.getCause();
//            }
//            element.setTextContent(sb.toString());
//        }
//    }
//
//    protected ValidationResult(XAdESStructure parent)
//    {
//        super(parent, "ValidationResult");
//    }
//
//    public ValidationResult(Node node)
//    {
//        super(node);
//    }
//
//    public CertValidationInfo.Status getStatus()
//    {
//        String status = getAttribute("Status");
//        if(status != null)
//            return CertValidationInfo.Status.valueOf(status);
//
//        return null;
//    }
//
//    public OCSPResponseStatus getOCSPResponseStatus()
//    {
//        String status = getAttribute("OCSPResponseStatus");
//        if(status != null)
//            return OCSPResponseStatus.valueOf(status);
//
//        return null;
//    }
//
//    public OCSPCertStatus getOCSPCertStatus()
//    {
//        String status = getAttribute("OCSPCertStatus");
//        if(status != null)
//            return OCSPCertStatus.valueOf(status);
//
//        return null;
//    }
//
//    public CRLReasonStatus getCRLReasonStatus()
//    {
//        String status = getAttribute("CRLReasonStatus");
//        if(status != null)
//            return CRLReasonStatus.valueOf(status);
//
//        return null;
//    }
//
//    public Date getValidationTime()
//        throws ParseException
//    {
//        String value = getChildElementTextContent("ValidationTime");
//        if(value != null)
//            return SystemUtils.parseDate(value);
//
//        return null;
//    }
//
//    public String getException()
//    {
//        return getChildElementTextContent("Exception");
//    }
}
