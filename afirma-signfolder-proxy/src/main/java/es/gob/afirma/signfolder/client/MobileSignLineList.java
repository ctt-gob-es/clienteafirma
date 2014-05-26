
package es.gob.afirma.signfolder.client;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for mobileSignLineList complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="mobileSignLineList">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="mobileSignLine" type="{urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0}mobileSignLine" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "mobileSignLineList", namespace = "urn:juntadeandalucia:cice:pfirma:mobile:type:v2.0", propOrder = {
    "mobileSignLine"
})
public class MobileSignLineList {

    protected List<MobileSignLine> mobileSignLine;

    /**
     * Gets the value of the mobileSignLine property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the mobileSignLine property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getMobileSignLine().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MobileSignLine }
     * 
     * 
     */
    public List<MobileSignLine> getMobileSignLine() {
        if (mobileSignLine == null) {
            mobileSignLine = new ArrayList<MobileSignLine>();
        }
        return this.mobileSignLine;
    }

}
