package net.java.xades.test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.java.xades.security.xml.SignatureStatus;
import net.java.xades.security.xml.ValidateResult;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifier;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifierImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;
import net.java.xades.security.xml.XAdES.XAdES_T;
import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;
import net.java.xades.util.XMLUtils;

import org.w3c.dom.Element;
import org.xml.sax.SAXException;

public class BaseTest
{
    protected SignatureOptions getSignatureOptions(String keystorePath, String keystoreType,
            String alias, String keystorePassword, String keyPassword) throws KeyStoreException,
            NoSuchAlgorithmException, UnrecoverableKeyException, CertificateException,
            FileNotFoundException, IOException
    {
        // Keystore
        KeyStore keystore = KeyStore.getInstance(keystoreType);
        keystore.load(new FileInputStream(keystorePath), keystorePassword.toCharArray());

        // Alias del certificado de firma
        String certificateAlias = alias;

        if (certificateAlias == null)
        {
            certificateAlias = (String) keystore.aliases().nextElement();
        }

        // Certificado de firma
        X509Certificate certificate = (X509Certificate) keystore.getCertificate(certificateAlias);

        // Clave privada para firmar
        PrivateKey privateKey = (PrivateKey) keystore.getKey(certificateAlias, keyPassword
                .toCharArray());

        SignatureOptions signatureOptions = new SignatureOptions();
        signatureOptions.setKeystore(keystore);
        signatureOptions.setCertificate(certificate);
        signatureOptions.setPrivateKey(privateKey);

        return signatureOptions;
    }

    protected Element getDocumentToSign(byte[] data) throws SAXException, IOException,
            ParserConfigurationException
    {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        return db.parse(new ByteArrayInputStream(data)).getDocumentElement();
    }

    protected boolean verify(XMLAdvancedSignature xmlSignature)
    {
        List<SignatureStatus> st = xmlSignature.validate();

        boolean validate = true;

        for (SignatureStatus status : st)
        {
            if (status.getValidateResult() != ValidateResult.VALID)
            {
                System.out.println("Sign validation error: ");
                System.out.println(status.getReasonsAsText());
                validate = false;
            }
        }

        if (validate)
        {
            System.out.println("OK");
        }

        return validate;
    }

    protected void showSignature(XMLAdvancedSignature xmlSignature)
    {
        XMLUtils.writeXML(System.out, xmlSignature.getBaseElement(), false);
    }

    protected void showSignature(XMLAdvancedSignature xmlSignature, OutputStream output)
    {
        XMLUtils.writeXML(output, xmlSignature.getBaseElement(), false);
    }

    protected XMLAdvancedSignature createXAdES_EPES(SignatureOptions signatureOptions, byte[] data)
            throws SAXException, IOException, ParserConfigurationException,
            GeneralSecurityException
    {
        // Build XAdES-EPES signature
        XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(XAdES.EPES, getDocumentToSign(data));
        xades.setSigningCertificate(signatureOptions.getCertificate());

        // Add implied policy
        SignaturePolicyIdentifier spi = new SignaturePolicyIdentifierImpl(true);
        xades.setSignaturePolicyIdentifier(spi);

        // Enveloped signature
        return XMLAdvancedSignature.newInstance(xades);
    }

    protected XMLAdvancedSignature createXAdES_T(SignatureOptions signatureOptions, byte[] data)
            throws SAXException, IOException, ParserConfigurationException,
            GeneralSecurityException
    {
        // Build XAdES-T signature
        XAdES_T xades = (XAdES_T) XAdES.newInstance(XAdES.T, getDocumentToSign(data));
        xades.setSigningCertificate(signatureOptions.getCertificate());

        // Add implied policy
        SignaturePolicyIdentifier spi = new SignaturePolicyIdentifierImpl(true);
        xades.setSignaturePolicyIdentifier(spi);

        // Enveloped signature
        return XMLAdvancedSignature.newInstance(xades);
    }
    
    public static byte[] inputStreamToByteArray(InputStream in) throws IOException
    {
        byte[] buffer = new byte[2048];
        int length = 0;

        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        while ((length = in.read(buffer)) >= 0)
        {
            baos.write(buffer, 0, length);
        }

        return baos.toByteArray();
    }    
}