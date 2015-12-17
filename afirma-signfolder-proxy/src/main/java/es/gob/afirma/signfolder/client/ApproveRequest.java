
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
 *         &lt;element name="requestTagId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
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
    "requestTagId"
})
@XmlRootElement(name = "approveRequest")
public class ApproveRequest {

    @XmlElement(required = true)
    protected byte[] certificate;
    @XmlElement(required = true)
    protected String requestTagId;

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
     * Gets the value of the requestTagId property.
     *
     * @return
     *     possible object is
     *     {@link String }
     *
     */
    public String getRequestTagId() {
        return this.requestTagId;
    }

    /**
     * Sets the value of the requestTagId property.
     *
     * @param value
     *     allowed object is
     *     {@link String }
     *
     */
    public void setRequestTagId(final String value) {
        this.requestTagId = value;
    }

}
