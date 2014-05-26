
package es.gob.afirma.signfolder.client;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for mobileSignLine complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="mobileSignLine">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="mobileSignerList" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileStringList" minOccurs="0"/>
 *         &lt;element name="type" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "mobileSignLine", namespace = "urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0", propOrder = {
    "mobileSignerList",
    "type"
})
public class MobileSignLine {

    @XmlElementRef(name = "mobileSignerList", type = JAXBElement.class)
    protected JAXBElement<MobileStringList> mobileSignerList;
    @XmlElementRef(name = "type", type = JAXBElement.class)
    protected JAXBElement<String> type;

    /**
     * Gets the value of the mobileSignerList property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link MobileStringList }{@code >}
     *     
     */
    public JAXBElement<MobileStringList> getMobileSignerList() {
        return mobileSignerList;
    }

    /**
     * Sets the value of the mobileSignerList property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link MobileStringList }{@code >}
     *     
     */
    public void setMobileSignerList(JAXBElement<MobileStringList> value) {
        this.mobileSignerList = ((JAXBElement<MobileStringList> ) value);
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public JAXBElement<String> getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link String }{@code >}
     *     
     */
    public void setType(JAXBElement<String> value) {
        this.type = ((JAXBElement<String> ) value);
    }

}
