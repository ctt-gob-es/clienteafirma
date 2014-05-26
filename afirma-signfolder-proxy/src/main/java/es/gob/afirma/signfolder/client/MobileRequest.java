
package es.gob.afirma.signfolder.client;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * <p>Java class for mobileRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="mobileRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="identifier" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="subject" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="view" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="text" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="ref" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="fentry" type="{http://www.w3.org/2001/XMLSchema}dateTime" minOccurs="0"/>
 *         &lt;element name="importanceLevel" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="application" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="workflow" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="forward" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="senders" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileStringList" minOccurs="0"/>
 *         &lt;element name="signLineList" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileSignLineList" minOccurs="0"/>
 *         &lt;element name="documentList" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileDocumentList" minOccurs="0"/>
 *         &lt;element name="requestType" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="requestTagId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "mobileRequest", namespace = "urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0", propOrder = {
    "identifier",
    "subject",
    "view",
    "text",
    "ref",
    "fentry",
    "importanceLevel",
    "application",
    "workflow",
    "forward",
    "senders",
    "signLineList",
    "documentList",
    "requestType",
    "requestTagId"
})
public class MobileRequest {

    @XmlElementRef(name = "identifier", type = JAXBElement.class)
    protected JAXBElement<String> identifier;
    @XmlElementRef(name = "subject", type = JAXBElement.class)
    protected JAXBElement<String> subject;
    protected String view;
    @XmlElementRef(name = "text", type = JAXBElement.class)
    protected JAXBElement<String> text;
    @XmlElementRef(name = "ref", type = JAXBElement.class)
    protected JAXBElement<String> ref;
    @XmlElementRef(name = "fentry", type = JAXBElement.class)
    protected JAXBElement<XMLGregorianCalendar> fentry;
    @XmlElementRef(name = "importanceLevel", type = JAXBElement.class)
    protected JAXBElement<String> importanceLevel;
    @XmlElementRef(name = "application", type = JAXBElement.class)
    protected JAXBElement<String> application;
    @XmlElementRef(name = "workflow", type = JAXBElement.class)
    protected JAXBElement<Boolean> workflow;
    @XmlElementRef(name = "forward", type = JAXBElement.class)
    protected JAXBElement<Boolean> forward;
    protected MobileStringList senders;
    protected MobileSignLineList signLineList;
    protected MobileDocumentList documentList;
    protected String requestType;
    protected String requestTagId;

    /**
     * Gets the value of the identifier property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public JAXBElement<String> getIdentifier() {
        return identifier;
    }

    /**
     * Sets the value of the identifier property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public void setIdentifier(JAXBElement<String> value) {
        this.identifier = ((JAXBElement<String> ) value);
    }

    /**
     * Gets the value of the subject property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public JAXBElement<String> getSubject() {
        return subject;
    }

    /**
     * Sets the value of the subject property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public void setSubject(JAXBElement<String> value) {
        this.subject = ((JAXBElement<String> ) value);
    }

    /**
     * Gets the value of the view property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getView() {
        return view;
    }

    /**
     * Sets the value of the view property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setView(String value) {
        this.view = value;
    }

    /**
     * Gets the value of the text property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public JAXBElement<String> getText() {
        return text;
    }

    /**
     * Sets the value of the text property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public void setText(JAXBElement<String> value) {
        this.text = ((JAXBElement<String> ) value);
    }

    /**
     * Gets the value of the ref property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public JAXBElement<String> getRef() {
        return ref;
    }

    /**
     * Sets the value of the ref property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public void setRef(JAXBElement<String> value) {
        this.ref = ((JAXBElement<String> ) value);
    }

    /**
     * Gets the value of the fentry property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link XMLGregorianCalendar }{@code >}
     *     
     */
    public JAXBElement<XMLGregorianCalendar> getFentry() {
        return fentry;
    }

    /**
     * Sets the value of the fentry property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link XMLGregorianCalendar }{@code >}
     *     
     */
    public void setFentry(JAXBElement<XMLGregorianCalendar> value) {
        this.fentry = ((JAXBElement<XMLGregorianCalendar> ) value);
    }

    /**
     * Gets the value of the importanceLevel property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public JAXBElement<String> getImportanceLevel() {
        return importanceLevel;
    }

    /**
     * Sets the value of the importanceLevel property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public void setImportanceLevel(JAXBElement<String> value) {
        this.importanceLevel = ((JAXBElement<String> ) value);
    }

    /**
     * Gets the value of the application property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public JAXBElement<String> getApplication() {
        return application;
    }

    /**
     * Sets the value of the application property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public void setApplication(JAXBElement<String> value) {
        this.application = ((JAXBElement<String> ) value);
    }

    /**
     * Gets the value of the workflow property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link Boolean }{@code >}
     *     
     */
    public JAXBElement<Boolean> getWorkflow() {
        return workflow;
    }

    /**
     * Sets the value of the workflow property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link Boolean }{@code >}
     *     
     */
    public void setWorkflow(JAXBElement<Boolean> value) {
        this.workflow = ((JAXBElement<Boolean> ) value);
    }

    /**
     * Gets the value of the forward property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link Boolean }{@code >}
     *     
     */
    public JAXBElement<Boolean> getForward() {
        return forward;
    }

    /**
     * Sets the value of the forward property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link Boolean }{@code >}
     *     
     */
    public void setForward(JAXBElement<Boolean> value) {
        this.forward = ((JAXBElement<Boolean> ) value);
    }

    /**
     * Gets the value of the senders property.
     * 
     * @return
     *     possible object is
     *     {@link MobileStringList }
     *     
     */
    public MobileStringList getSenders() {
        return senders;
    }

    /**
     * Sets the value of the senders property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileStringList }
     *     
     */
    public void setSenders(MobileStringList value) {
        this.senders = value;
    }

    /**
     * Gets the value of the signLineList property.
     * 
     * @return
     *     possible object is
     *     {@link MobileSignLineList }
     *     
     */
    public MobileSignLineList getSignLineList() {
        return signLineList;
    }

    /**
     * Sets the value of the signLineList property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileSignLineList }
     *     
     */
    public void setSignLineList(MobileSignLineList value) {
        this.signLineList = value;
    }

    /**
     * Gets the value of the documentList property.
     * 
     * @return
     *     possible object is
     *     {@link MobileDocumentList }
     *     
     */
    public MobileDocumentList getDocumentList() {
        return documentList;
    }

    /**
     * Sets the value of the documentList property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileDocumentList }
     *     
     */
    public void setDocumentList(MobileDocumentList value) {
        this.documentList = value;
    }

    /**
     * Gets the value of the requestType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRequestType() {
        return requestType;
    }

    /**
     * Sets the value of the requestType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRequestType(String value) {
        this.requestType = value;
    }

    /**
     * Gets the value of the requestTagId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRequestTagId() {
        return requestTagId;
    }

    /**
     * Sets the value of the requestTagId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRequestTagId(String value) {
        this.requestTagId = value;
    }

}
