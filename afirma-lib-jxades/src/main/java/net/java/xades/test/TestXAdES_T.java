package net.java.xades.test;

import java.io.FileInputStream;
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

public class TestXAdES_T extends BaseTest
{
    @Test
    public void xadesT() throws FileNotFoundException, IOException, GeneralSecurityException,
            InvalidCanonicalizerException, CanonicalizationException, MarshalException,
            XMLSignatureException, TransformException, ParserConfigurationException, SAXException
    {
        // Default signature options
        SignatureOptions signatureOptions = getSignatureOptions(
                "/etc/4tic/4sign/4sign.keystore", "JKS", "server", "importkey", "importkey");
 
        byte[] data = inputStreamToByteArray(new FileInputStream("src/main/resources/a-firmar.xml"));
        
        // Enveloped signature
        XMLAdvancedSignature xmlSignature = createXAdES_EPES(signatureOptions, data);
        xmlSignature.sign(signatureOptions.getCertificate(), signatureOptions.getPrivateKey(),
                SignatureMethod.RSA_SHA1, Arrays.asList(new Object[] { "" }), "S0",
                "http://tss.accv.es:8318/tsa");

        // Show results
        showSignature(xmlSignature);
        showSignature(xmlSignature, new FileOutputStream("src/main/resources/out.xml"));

        // Verify signature
        data = inputStreamToByteArray(new FileInputStream("src/main/resources/out.xml"));
        
        xmlSignature = createXAdES_EPES(signatureOptions, data);
        verify(xmlSignature);
    }
}