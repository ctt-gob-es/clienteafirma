package net.java.xades.test.old;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.List;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.java.xades.security.xml.SignatureStatus;
import net.java.xades.security.xml.ValidateResult;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifier;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifierImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;
import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;
import net.java.xades.util.XMLUtils;

import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.c14n.CanonicalizationException;
import com.sun.org.apache.xml.internal.security.c14n.InvalidCanonicalizerException;

public class SimpleTest
{
    public static void main(String[] args) throws FileNotFoundException, IOException,
            ParserConfigurationException, SAXException, MarshalException, XMLSignatureException,
            GeneralSecurityException, TransformException, InvalidCanonicalizerException, CanonicalizationException
    {
        // PKCS12
        KeyStore keystore = KeyStore.getInstance(KeyStore.getDefaultType());
        keystore.load(new FileInputStream("src/main/resources/uji.keystore"), "cryptoapplet"
                .toCharArray());

        // Alias del certificado de firma
        String alias = (String) keystore.aliases().nextElement();

        // Certificado de firma
        X509Certificate certificate = (X509Certificate) keystore.getCertificate(alias);

        // Clave privada para firmar
        PrivateKey privateKey = (PrivateKey) keystore.getKey(alias, "cryptoapplet".toCharArray());

        // Datos XML
        byte[] data = "<root/>".getBytes();

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Element element = db.parse(new ByteArrayInputStream(data)).getDocumentElement();

        // XAdES-EPES
        XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(XAdES.EPES,
                XMLAdvancedSignature.XADES_v141, "xades", "ds", DigestMethod.SHA256, element);
        xades.setSigningCertificate(certificate);

        SignaturePolicyIdentifier spi = new SignaturePolicyIdentifierImpl(true);
        xades.setSignaturePolicyIdentifier(spi);

        // Firma enveloped
        XMLAdvancedSignature xmlSignature = XMLAdvancedSignature.newInstance(xades);
        // xmlSignature.setDigestMethod(DigestMethod.SHA256);
        xmlSignature.sign(certificate, privateKey, SignatureMethod.RSA_SHA1, Arrays
                .asList(new Object[] { "" }), "S0", "http://tss.accv.es:8318/tsa");

        // Mostramos el resultado
        XMLUtils.writeXML(new FileOutputStream("/tmp/out.xml"), xmlSignature.getBaseElement(),
                false);

        // Verificamos
        List<SignatureStatus> st = xmlSignature.validate();

        boolean error = false;

        for (SignatureStatus status : st)
        {
            if (status.getValidateResult() != ValidateResult.VALID)
            {
                System.out.println("Sign validation error: ");
                System.out.println(status.getReasonsAsText());
                error = true;
            }
        }

        if (!error)
        {
            System.out.println("\nOK");
        }
    }
}
