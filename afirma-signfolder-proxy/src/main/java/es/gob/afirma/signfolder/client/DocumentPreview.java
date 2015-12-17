
package es.gob.afirma.signfolder.client;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for anonymous complex type.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="certificate" type="{http://www.w3.org/2001/XMLSchema}base64Binary"/&gt;
 *         &lt;element name="documentId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "certificate",
    "documentId"
})
@XmlRootElement(name = "documentPreview")
public class DocumentPreview {

    @XmlElement(required = true)
    protected byte[] certificate;
    @XmlElement(required = true)
    protected String documentId;

    /**
     * Gets the value of the certificate property.
     *
     * @return
     *     possible object is
     *     byte[]
     */
    public byte[] getCertificate() {
        return this.certificate;
    }

    /**
     * Sets the value of the certificate property.
     *
     * @param value
     *     allowed object is
     *     byte[]
     */
    public void setCertificate(final byte[] value) {
        this.certificate = value;
    }

    /**
     * Gets the value of the documentId property.
     *
     * @return
     *     possible object is
     *     {@link String }
     *
     */
    public String getDocumentId() {
        return this.documentId;
    }

    /**
     * Sets the value of the documentId property.
     *
     * @param value
     *     allowed object is
     *     {@link String }
     *
     */
    public void setDocumentId(final String value) {
        this.documentId = value;
    }

}
