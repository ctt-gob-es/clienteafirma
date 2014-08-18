
package es.gob.afirma.signfolder.client;

import javax.activation.DataHandler;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlMimeType;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the es.gob.afirma.signfolder.client package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _MobileRequestImportanceLevel_QNAME = new QName("", "importanceLevel");
    private final static QName _MobileRequestRef_QNAME = new QName("", "ref");
    private final static QName _MobileRequestApplication_QNAME = new QName("", "application");
    private final static QName _MobileRequestText_QNAME = new QName("", "text");
    private final static QName _MobileRequestForward_QNAME = new QName("", "forward");
    private final static QName _MobileRequestSubject_QNAME = new QName("", "subject");
    private final static QName _MobileRequestFentry_QNAME = new QName("", "fentry");
    private final static QName _MobileRequestWorkflow_QNAME = new QName("", "workflow");
    private final static QName _MobileRequestIdentifier_QNAME = new QName("", "identifier");
    private final static QName _MobileSignLineMobileSignerList_QNAME = new QName("", "mobileSignerList");
    private final static QName _MobileSignLineType_QNAME = new QName("", "type");
    private final static QName _MobileDocumentSignatureType_QNAME = new QName("", "signatureType");
    private final static QName _MobileDocumentData_QNAME = new QName("", "data");
    private final static QName _MobileDocumentSignAlgorithm_QNAME = new QName("", "signAlgorithm");
    private final static QName _MobileDocumentSize_QNAME = new QName("", "size");
    private final static QName _MobileDocumentSignatureParameters_QNAME = new QName("", "signatureParameters");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: es.gob.afirma.signfolder.client
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link QueryApplicationsMobileResponse }
     * 
     */
    public QueryApplicationsMobileResponse createQueryApplicationsMobileResponse() {
        return new QueryApplicationsMobileResponse();
    }

    /**
     * Create an instance of {@link ReportPreview }
     * 
     */
    public ReportPreview createReportPreview() {
        return new ReportPreview();
    }

    /**
     * Create an instance of {@link MobileError }
     * 
     */
    public MobileError createMobileError() {
        return new MobileError();
    }

    /**
     * Create an instance of {@link MobileRequestList }
     * 
     */
    public MobileRequestList createMobileRequestList() {
        return new MobileRequestList();
    }

    /**
     * Create an instance of {@link SignPreview }
     * 
     */
    public SignPreview createSignPreview() {
        return new SignPreview();
    }

    /**
     * Create an instance of {@link SaveSign }
     * 
     */
    public SaveSign createSaveSign() {
        return new SaveSign();
    }

    /**
     * Create an instance of {@link MobileRequestFilter }
     * 
     */
    public MobileRequestFilter createMobileRequestFilter() {
        return new MobileRequestFilter();
    }

    /**
     * Create an instance of {@link DocumentPreview }
     * 
     */
    public DocumentPreview createDocumentPreview() {
        return new DocumentPreview();
    }

    /**
     * Create an instance of {@link QueryRequest }
     * 
     */
    public QueryRequest createQueryRequest() {
        return new QueryRequest();
    }

    /**
     * Create an instance of {@link MobileRequestFilterList }
     * 
     */
    public MobileRequestFilterList createMobileRequestFilterList() {
        return new MobileRequestFilterList();
    }

    /**
     * Create an instance of {@link MobileSignLine }
     * 
     */
    public MobileSignLine createMobileSignLine() {
        return new MobileSignLine();
    }

    /**
     * Create an instance of {@link GetDocumentsToSign }
     * 
     */
    public GetDocumentsToSign createGetDocumentsToSign() {
        return new GetDocumentsToSign();
    }

    /**
     * Create an instance of {@link ApproveRequest }
     * 
     */
    public ApproveRequest createApproveRequest() {
        return new ApproveRequest();
    }

    /**
     * Create an instance of {@link SaveSignResponse }
     * 
     */
    public SaveSignResponse createSaveSignResponse() {
        return new SaveSignResponse();
    }

    /**
     * Create an instance of {@link ReportPreviewResponse }
     * 
     */
    public ReportPreviewResponse createReportPreviewResponse() {
        return new ReportPreviewResponse();
    }

    /**
     * Create an instance of {@link MobileSignLineList }
     * 
     */
    public MobileSignLineList createMobileSignLineList() {
        return new MobileSignLineList();
    }

    /**
     * Create an instance of {@link MobileApplicationList }
     * 
     */
    public MobileApplicationList createMobileApplicationList() {
        return new MobileApplicationList();
    }

    /**
     * Create an instance of {@link SignPreviewResponse }
     * 
     */
    public SignPreviewResponse createSignPreviewResponse() {
        return new SignPreviewResponse();
    }

    /**
     * Create an instance of {@link MobileRequest }
     * 
     */
    public MobileRequest createMobileRequest() {
        return new MobileRequest();
    }

    /**
     * Create an instance of {@link MobileDocument }
     * 
     */
    public MobileDocument createMobileDocument() {
        return new MobileDocument();
    }

    /**
     * Create an instance of {@link RejectRequestResponse }
     * 
     */
    public RejectRequestResponse createRejectRequestResponse() {
        return new RejectRequestResponse();
    }

    /**
     * Create an instance of {@link MobileStringList }
     * 
     */
    public MobileStringList createMobileStringList() {
        return new MobileStringList();
    }

    /**
     * Create an instance of {@link MobileApplication }
     * 
     */
    public MobileApplication createMobileApplication() {
        return new MobileApplication();
    }

    /**
     * Create an instance of {@link QueryRequestList }
     * 
     */
    public QueryRequestList createQueryRequestList() {
        return new QueryRequestList();
    }

    /**
     * Create an instance of {@link ApproveRequestResponse }
     * 
     */
    public ApproveRequestResponse createApproveRequestResponse() {
        return new ApproveRequestResponse();
    }

    /**
     * Create an instance of {@link GetDocumentsToSignResponse }
     * 
     */
    public GetDocumentsToSignResponse createGetDocumentsToSignResponse() {
        return new GetDocumentsToSignResponse();
    }

    /**
     * Create an instance of {@link QueryRequestResponse }
     * 
     */
    public QueryRequestResponse createQueryRequestResponse() {
        return new QueryRequestResponse();
    }

    /**
     * Create an instance of {@link QueryRequestListResponse }
     * 
     */
    public QueryRequestListResponse createQueryRequestListResponse() {
        return new QueryRequestListResponse();
    }

    /**
     * Create an instance of {@link MobileDocumentList }
     * 
     */
    public MobileDocumentList createMobileDocumentList() {
        return new MobileDocumentList();
    }

    /**
     * Create an instance of {@link MobileDocSignInfo }
     * 
     */
    public MobileDocSignInfo createMobileDocSignInfo() {
        return new MobileDocSignInfo();
    }

    /**
     * Create an instance of {@link DocumentPreviewResponse }
     * 
     */
    public DocumentPreviewResponse createDocumentPreviewResponse() {
        return new DocumentPreviewResponse();
    }

    /**
     * Create an instance of {@link MobileDocSignInfoList }
     * 
     */
    public MobileDocSignInfoList createMobileDocSignInfoList() {
        return new MobileDocSignInfoList();
    }

    /**
     * Create an instance of {@link RejectRequest }
     * 
     */
    public RejectRequest createRejectRequest() {
        return new RejectRequest();
    }

    /**
     * Create an instance of {@link QueryApplicationsMobile }
     * 
     */
    public QueryApplicationsMobile createQueryApplicationsMobile() {
        return new QueryApplicationsMobile();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "importanceLevel", scope = MobileRequest.class)
    public JAXBElement<String> createMobileRequestImportanceLevel(String value) {
        return new JAXBElement<String>(_MobileRequestImportanceLevel_QNAME, String.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "ref", scope = MobileRequest.class)
    public JAXBElement<String> createMobileRequestRef(String value) {
        return new JAXBElement<String>(_MobileRequestRef_QNAME, String.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "application", scope = MobileRequest.class)
    public JAXBElement<String> createMobileRequestApplication(String value) {
        return new JAXBElement<String>(_MobileRequestApplication_QNAME, String.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "text", scope = MobileRequest.class)
    public JAXBElement<String> createMobileRequestText(String value) {
        return new JAXBElement<String>(_MobileRequestText_QNAME, String.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Boolean }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "forward", scope = MobileRequest.class)
    public JAXBElement<Boolean> createMobileRequestForward(Boolean value) {
        return new JAXBElement<Boolean>(_MobileRequestForward_QNAME, Boolean.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "subject", scope = MobileRequest.class)
    public JAXBElement<String> createMobileRequestSubject(String value) {
        return new JAXBElement<String>(_MobileRequestSubject_QNAME, String.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link XMLGregorianCalendar }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "fentry", scope = MobileRequest.class)
    public JAXBElement<XMLGregorianCalendar> createMobileRequestFentry(XMLGregorianCalendar value) {
        return new JAXBElement<XMLGregorianCalendar>(_MobileRequestFentry_QNAME, XMLGregorianCalendar.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Boolean }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "workflow", scope = MobileRequest.class)
    public JAXBElement<Boolean> createMobileRequestWorkflow(Boolean value) {
        return new JAXBElement<Boolean>(_MobileRequestWorkflow_QNAME, Boolean.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "identifier", scope = MobileRequest.class)
    public JAXBElement<String> createMobileRequestIdentifier(String value) {
        return new JAXBElement<String>(_MobileRequestIdentifier_QNAME, String.class, MobileRequest.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MobileStringList }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "mobileSignerList", scope = MobileSignLine.class)
    public JAXBElement<MobileStringList> createMobileSignLineMobileSignerList(MobileStringList value) {
        return new JAXBElement<MobileStringList>(_MobileSignLineMobileSignerList_QNAME, MobileStringList.class, MobileSignLine.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "type", scope = MobileSignLine.class)
    public JAXBElement<String> createMobileSignLineType(String value) {
        return new JAXBElement<String>(_MobileSignLineType_QNAME, String.class, MobileSignLine.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MobileSignFormat }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "signatureType", scope = MobileDocument.class)
    public JAXBElement<MobileSignFormat> createMobileDocumentSignatureType(MobileSignFormat value) {
        return new JAXBElement<MobileSignFormat>(_MobileDocumentSignatureType_QNAME, MobileSignFormat.class, MobileDocument.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link DataHandler }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "data", scope = MobileDocument.class)
    @XmlMimeType("application/octet-stream")
    public JAXBElement<DataHandler> createMobileDocumentData(DataHandler value) {
        return new JAXBElement<DataHandler>(_MobileDocumentData_QNAME, DataHandler.class, MobileDocument.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "signAlgorithm", scope = MobileDocument.class)
    public JAXBElement<String> createMobileDocumentSignAlgorithm(String value) {
        return new JAXBElement<String>(_MobileDocumentSignAlgorithm_QNAME, String.class, MobileDocument.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Integer }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "size", scope = MobileDocument.class)
    public JAXBElement<Integer> createMobileDocumentSize(Integer value) {
        return new JAXBElement<Integer>(_MobileDocumentSize_QNAME, Integer.class, MobileDocument.class, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "signatureParameters", scope = MobileDocument.class)
    public JAXBElement<String> createMobileDocumentSignatureParameters(String value) {
        return new JAXBElement<String>(_MobileDocumentSignatureParameters_QNAME, String.class, MobileDocument.class, value);
    }

}
