package net.java.xades.test.old;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import net.java.xades.security.xml.SignatureStatus;
import net.java.xades.security.xml.ValidateResult;
import net.java.xades.security.xml.XAdES.CommitmentTypeId;
import net.java.xades.security.xml.XAdES.CommitmentTypeIdImpl;
import net.java.xades.security.xml.XAdES.CommitmentTypeIndication;
import net.java.xades.security.xml.XAdES.CommitmentTypeIndicationImpl;
import net.java.xades.security.xml.XAdES.DataObjectFormat;
import net.java.xades.security.xml.XAdES.DataObjectFormatImpl;
import net.java.xades.security.xml.XAdES.ObjectIdentifier;
import net.java.xades.security.xml.XAdES.ObjectIdentifierImpl;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifier;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifierImpl;
import net.java.xades.security.xml.XAdES.SignatureProductionPlace;
import net.java.xades.security.xml.XAdES.SignatureProductionPlaceImpl;
import net.java.xades.security.xml.XAdES.SignerRole;
import net.java.xades.security.xml.XAdES.SignerRoleImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_X_L;
import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;
import net.java.xades.util.XMLUtils;

import org.w3c.dom.Element;

public class SignTest
{
    private static String testXmlDoc = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n"
            + "<acta>\n" + "  <nombre Id=\"base\">perico</nombre>\n" + "</acta>";

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
        ByteArrayInputStream originalData = new ByteArrayInputStream(testXmlDoc.getBytes());

        // Load test XML document
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Element element = db.parse(originalData).getDocumentElement();

        // Create a XAdES-T profile
        XAdES_X_L xades = (XAdES_X_L) XAdES.newInstance(XAdES.X_L, element);

        // SignaturePolicyIdentifier
        SignaturePolicyIdentifier spi = new SignaturePolicyIdentifierImpl(false);
        spi.setIdentifier("http://www.google.com");
        spi.setDescription("Policy description");
        spi.setQualifier("urn:oid:1.3.6.1.4.1.24491");
        xades.setSignaturePolicyIdentifier(spi);

        // SignatureProductionPlace
        SignatureProductionPlace spp = new SignatureProductionPlaceImpl();
        spp.setCity("Castelló");
        spp.setStateOrProvince("Castelló de la Plana");
        spp.setPostalCode("12071");
        spp.setCountryName("Spain");
        xades.setSignatureProductionPlace(spp);

        // SignerRole
        SignerRole signerRole = new SignerRoleImpl();
        signerRole.addClaimedRole("PDI");
        xades.setSignerRole(signerRole);

        // DataObjectFormat
        ArrayList<DataObjectFormat> objectFormats = new ArrayList<DataObjectFormat>();

        ObjectIdentifier oi = new ObjectIdentifierImpl("q", "i", "desc", new ArrayList<String>());
        DataObjectFormat objectFormat = new DataObjectFormatImpl("test", oi, "text/plain",
                "ISO-8859-1", "#base");
        objectFormats.add(objectFormat);
        xades.setDataObjectFormats(objectFormats);

        // CommitmentTypeIndication
        ArrayList<String> docRefs = new ArrayList<String>();
        docRefs.add("ref1");
        docRefs.add("ref2");

        ArrayList<CommitmentTypeIndication> commitmentTypeIndications = new ArrayList<CommitmentTypeIndication>();

        CommitmentTypeIndication commitmentTypeIndication = new CommitmentTypeIndicationImpl();
        CommitmentTypeId commitmentTypeId = new CommitmentTypeIdImpl("x", "id", "desc", docRefs);
        commitmentTypeIndication.setCommitmentTypeId(commitmentTypeId);

        commitmentTypeIndication.setObjectReference("reference");
        commitmentTypeIndication.setCommitmentTypeQualifiers(docRefs);
        commitmentTypeIndications.add(commitmentTypeIndication);

        xades.setCommitmentTypeIndications(commitmentTypeIndications);

        // // AllDataObjectsTimeStamp
        // ArrayList<AllDataObjectsTimeStamp> allDataObjectsTimeStamps = new
        // ArrayList<AllDataObjectsTimeStamp>();
        //		
        // // TODO: Howto obtain the hash that has to be timestamped
        // allDataObjectsTimeStamps.add(new AllDataObjectsTimeStampImpl("perico".getBytes()));
        // xades.setAllDataObjectsTimeStamps(allDataObjectsTimeStamps);
        //
        // // IndividualDataObjectsTimeStamp
        // ArrayList<IndividualDataObjectsTimeStamp> individualDataObjectsTimeStamps = new
        // ArrayList<IndividualDataObjectsTimeStamp>();
        //        
        // // TODO: Howto obtain the hash that has to be timestamped
        // individualDataObjectsTimeStamps.add(new
        // IndividualDataObjectsTimeStampImpl("perico".getBytes()));
        // xades.setIndividualDataObjectsTimeStamps(individualDataObjectsTimeStamps);

        // Generate XAdES document and sign it
        KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
        ks.load(new FileInputStream("src/main/resources/uji.keystore"), "cryptoapplet"
                .toCharArray());

        X509Certificate certificate = (X509Certificate) ks.getCertificate("uji");
        PrivateKey privateKey = (PrivateKey) ks.getKey("uji", "cryptoapplet".toCharArray());

        X509Certificate caCertificate = (X509Certificate) ks.getCertificate("ca");
        ArrayList<X509Certificate> caCertificateList = new ArrayList<X509Certificate>();
        caCertificateList.add(caCertificate);

        // CompleteCertificateRefs/CompleteCertificateRefs
        xades.setCompleteCertificateRefs(caCertificateList);

        // CertificateValidator certValidator = new CertificateValidator(certificate,
        // caCertificateList);
        // CertValidationInfo certValidationInfo =
        // CertValidationInfo.getCertValidationInfo(certValidator);
        // xades.setCompleteRevocationRefs(certValidationInfo);

        XMLAdvancedSignature xmlSignature = XMLAdvancedSignature.newInstance(xades);
        xmlSignature.sign(certificate, privateKey, SignatureMethod.RSA_SHA1, Arrays
                .asList(new String[] { "base" }), "S0", "http://tss.accv.es:8318/tsa");
        xmlSignature.enrichUnsignedProperties("http://tss.accv.es:8318/tsa");

        XMLUtils.writeXML(new FileOutputStream(new File("src/main/resources/enveloped-out.xml")),
                xmlSignature.getBaseElement(), false);
        XMLUtils.writeXML(System.out, xmlSignature.getBaseElement(), true);

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
            System.out.println("Ok");
        }
    }
}
