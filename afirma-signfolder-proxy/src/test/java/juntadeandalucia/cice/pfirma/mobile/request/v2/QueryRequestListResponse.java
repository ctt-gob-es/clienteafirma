
package juntadeandalucia.cice.pfirma.mobile.request.v2;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileRequestList;


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
 *         &lt;element name="requestList" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileRequestList"/>
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
    "requestList"
})
@XmlRootElement(name = "queryRequestListResponse")
public class QueryRequestListResponse {

    @XmlElement(required = true)
    protected MobileRequestList requestList;

    /**
     * Gets the value of the requestList property.
     * 
     * @return
     *     possible object is
     *     {@link MobileRequestList }
     *     
     */
    public MobileRequestList getRequestList() {
        return requestList;
    }

    /**
     * Sets the value of the requestList property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileRequestList }
     *     
     */
    public void setRequestList(MobileRequestList value) {
        this.requestList = value;
    }

}
