
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
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="certificate" type="{http://www.w3.org/2001/XMLSchema}base64Binary"/>
 *         &lt;element name="state" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="initPage" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="pageSize" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="signFormats" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileStringList"/>
 *         &lt;element name="filters" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileRequestFilterList"/>
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
    "state",
    "initPage",
    "pageSize",
    "signFormats",
    "filters"
})
@XmlRootElement(name = "queryRequestList")
public class QueryRequestList {

    @XmlElement(required = true)
    protected byte[] certificate;
    @XmlElement(required = true)
    protected String state;
    @XmlElement(required = true)
    protected String initPage;
    @XmlElement(required = true)
    protected String pageSize;
    @XmlElement(required = true)
    protected MobileStringList signFormats;
    @XmlElement(required = true)
    protected MobileRequestFilterList filters;

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
        this.certificate = ((byte[]) value);
    }

    /**
     * Gets the value of the state property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getState() {
        return state;
    }

    /**
     * Sets the value of the state property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setState(String value) {
        this.state = value;
    }

    /**
     * Gets the value of the initPage property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInitPage() {
        return initPage;
    }

    /**
     * Sets the value of the initPage property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInitPage(String value) {
        this.initPage = value;
    }

    /**
     * Gets the value of the pageSize property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPageSize() {
        return pageSize;
    }

    /**
     * Sets the value of the pageSize property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPageSize(String value) {
        this.pageSize = value;
    }

    /**
     * Gets the value of the signFormats property.
     * 
     * @return
     *     possible object is
     *     {@link MobileStringList }
     *     
     */
    public MobileStringList getSignFormats() {
        return signFormats;
    }

    /**
     * Sets the value of the signFormats property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileStringList }
     *     
     */
    public void setSignFormats(MobileStringList value) {
        this.signFormats = value;
    }

    /**
     * Gets the value of the filters property.
     * 
     * @return
     *     possible object is
     *     {@link MobileRequestFilterList }
     *     
     */
    public MobileRequestFilterList getFilters() {
        return filters;
    }

    /**
     * Sets the value of the filters property.
     * 
     * @param value
     *     allowed object is
     *     {@link MobileRequestFilterList }
     *     
     */
    public void setFilters(MobileRequestFilterList value) {
        this.filters = value;
    }

}
