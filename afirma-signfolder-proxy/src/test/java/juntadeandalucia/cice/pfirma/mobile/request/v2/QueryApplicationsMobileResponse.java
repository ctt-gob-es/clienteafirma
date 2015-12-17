
package juntadeandalucia.cice.pfirma.mobile.request.v2;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileApplicationList;


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
 *         &lt;element name="applicationList" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileApplicationList"/>
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
    "applicationList"
})
@XmlRootElement(name = "queryApplicationsMobileResponse")
public class QueryApplicationsMobileResponse {

    @XmlElement(required = true)
    protected MobileApplicationList applicationList;

    /**
     * Gets the value of the applicationList property.
     * 
     * @return
     *     possible object is
     *     {@link MobileApplicationList }
     *     
     */
    public MobileApplicationList getApplicationList() {
        return applicationList;
    }

    /**
     * Sets the value of the applicationList property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileApplicationList }
     *     
     */
    public void setApplicationList(MobileApplicationList value) {
        this.applicationList = value;
    }

}
