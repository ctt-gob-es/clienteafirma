
package juntadeandalucia.cice.pfirma.mobile.type.v2;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for mobileSignFormat.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="mobileSignFormat">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="PKCS7"/>
 *     &lt;enumeration value="CMS"/>
 *     &lt;enumeration value="CADES"/>
 *     &lt;enumeration value="XADES"/>
 *     &lt;enumeration value="XADES IMPLICITO"/>
 *     &lt;enumeration value="XADES EXPLICITO"/>
 *     &lt;enumeration value="XADES ENVELOPING"/>
 *     &lt;enumeration value="PDF"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "mobileSignFormat")
@XmlEnum
public enum MobileSignFormat {

    @XmlEnumValue("PKCS7")
    PKCS_7("PKCS7"),
    CMS("CMS"),
    CADES("CADES"),
    XADES("XADES"),
    @XmlEnumValue("XADES IMPLICITO")
    XADES_IMPLICITO("XADES IMPLICITO"),
    @XmlEnumValue("XADES EXPLICITO")
    XADES_EXPLICITO("XADES EXPLICITO"),
    @XmlEnumValue("XADES ENVELOPING")
    XADES_ENVELOPING("XADES ENVELOPING"),
    PDF("PDF");
    private final String value;

    MobileSignFormat(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static MobileSignFormat fromValue(String v) {
        for (MobileSignFormat c: MobileSignFormat.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
