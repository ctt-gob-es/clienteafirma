
package juntadeandalucia.cice.pfirma.mobile.request.v2;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileDocumentList;


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
 *         &lt;element name="documentList" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileDocumentList"/>
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
    "documentList"
})
@XmlRootElement(name = "getDocumentsToSignResponse")
public class GetDocumentsToSignResponse {

    @XmlElement(required = true)
    protected MobileDocumentList documentList;

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

}
