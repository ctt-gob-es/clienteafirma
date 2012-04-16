package net.java.xades.test.old;

import java.io.File;
import java.io.FileInputStream;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import net.java.xades.security.xml.SignatureStatus;
import net.java.xades.security.xml.ValidateResult;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;
import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;

import org.w3c.dom.Element;

public class VerifyTest
{
    public static X509Certificate readCertificate(String certFile) throws Exception
    {
        FileInputStream fis = new FileInputStream(new File(certFile));
        CertificateFactory certificateFactory = CertificateFactory.getInstance("X.509");
        X509Certificate cert = (X509Certificate) certificateFactory.generateCertificate(fis);
        fis.close();

        return cert;
    }

    public static void main(String[] args) throws Exception
    {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        // Element element = db.parse(new
        // File("../cryptoapplet/uji-crypto-openxades/src/main/resources/signed-output.xml")).getDocumentElement();
        Element element = db.parse(new File("/tmp/out.xml")).getDocumentElement();

        XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(XAdES.EPES, element);

        XMLAdvancedSignature fileXML = XMLAdvancedSignature.newInstance(xades);
        List<SignatureStatus> st = fileXML.validate();

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
            System.out.println("Ok");
        }
    }
}
