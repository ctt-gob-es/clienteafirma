
package juntadeandalucia.cice.pfirma.mobile.request.v2;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileDocSignInfoList;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="certificate" type="{http://www.w3.org/2001/XMLSchema}base64Binary"/>
 *         &lt;element name="requestTagId" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="docSignInfoList" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileDocSignInfoList"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "certificate",
    "requestTagId",
    "docSignInfoList"
})
@XmlRootElement(name = "saveSign")
public class SaveSign {

    @XmlElement(required = true)
    protected byte[] certificate;
    @XmlElement(required = true)
    protected String requestTagId;
    @XmlElement(required = true)
    protected MobileDocSignInfoList docSignInfoList;

    /**
     * Gets the value of the certificate property.
     * 
     * @return
     *     possible object is
     *     byte[]
     */
    public byte[] getCertificate() {
        return certificate;
    }

    /**
     * Sets the value of the certificate property.
     * 
     * @param value
     *     allowed object is
     *     byte[]
     */
    public void setCertificate(byte[] value) {
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

    /**
     * Gets the value of the docSignInfoList property.
     * 
     * @return
     *     possible object is
     *     {@link MobileDocSignInfoList }
     *     
     */
    public MobileDocSignInfoList getDocSignInfoList() {
        return docSignInfoList;
    }

    /**
     * Sets the value of the docSignInfoList property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileDocSignInfoList }
     *     
     */
    public void setDocSignInfoList(MobileDocSignInfoList value) {
        this.docSignInfoList = value;
    }

}
