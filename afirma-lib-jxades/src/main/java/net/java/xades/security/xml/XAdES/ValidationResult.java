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

    public ValidationResult(final Node node, final String xadesPrefix, final String xadesNamespace, final String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }
}
