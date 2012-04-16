package net.java.xades.test;

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Arrays;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.parsers.ParserConfigurationException;

import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;

import org.junit.Test;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.c14n.CanonicalizationException;
import com.sun.org.apache.xml.internal.security.c14n.InvalidCanonicalizerException;

public class TestCosign extends BaseTest
{
    @Test
    public void xadesT() throws FileNotFoundException, IOException, GeneralSecurityException,
            InvalidCanonicalizerException, CanonicalizationException, MarshalException,
            XMLSignatureException, TransformException, ParserConfigurationException, SAXException
    {
        byte[] data = "<?xml version=\"1.0\"?><root><datafile id=\"test\">contenido</datafile></root>"
                .getBytes();

        // Default signature options
        SignatureOptions signatureOptions = getSignatureOptions(
                "/home/borillo/Dropbox/docs/ca-x509/all.p12", "PKCS12", null, "komun14", "komun14");

        // Build XAdES-EPES signature
        XMLAdvancedSignature xmlSignature = createXAdES_EPES(signatureOptions, data);
        xmlSignature.sign(signatureOptions.getCertificate(), signatureOptions.getPrivateKey(),
                SignatureMethod.RSA_SHA1, Arrays.asList(new Object[] { "test" }), "S0",
                "http://tss.accv.es:8318/tsa");

        // Show results
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        showSignature(xmlSignature, bos);
        System.out.println(new String(bos.toByteArray()));

        // Verify signature
        verify(xmlSignature);

        // Cosign
        xmlSignature = createXAdES_EPES(signatureOptions, bos.toByteArray());
        xmlSignature.sign(signatureOptions.getCertificate(), signatureOptions.getPrivateKey(),
                SignatureMethod.RSA_SHA1, Arrays.asList(new Object[] { "test" }), "S1",
                "http://tss.accv.es:8318/tsa");

        // Show results
        bos = new ByteArrayOutputStream();
        showSignature(xmlSignature, bos);
        System.out.println(new String(bos.toByteArray()));

        // Verify signature
        verify(xmlSignature);

        showSignature(xmlSignature,
                new FileOutputStream("src/main/resources/out-cosign-jxades.xml"));
    }
}