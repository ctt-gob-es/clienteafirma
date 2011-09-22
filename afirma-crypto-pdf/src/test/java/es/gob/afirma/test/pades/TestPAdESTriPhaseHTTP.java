package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;

import junit.framework.Assert;

import org.junit.Test;

import com.lowagie.text.pdf.codec.Base64;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.core.util.MultivaluedMapImpl;
import com.sun.jersey.spi.container.servlet.ServletContainer;
import com.sun.jersey.test.framework.JerseyTest;
import com.sun.jersey.test.framework.WebAppDescriptor;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.beans.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.services.PreSignatureResult;
import es.gob.afirma.signers.cades.PKCS1ExternalizableSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;

public class TestPAdESTriPhaseHTTP extends JerseyTest
{
    private static final String CERT_PATH = "ANF_PF_Activo.pfx";
    private static final String CERT_PASS = "12341234";
    private static final String CERT_ALIAS = "anf usuario activo";

    private static final String[] TEST_FILES = { "TEST_PDF.pdf" };

    private WebResource resource;

    public TestPAdESTriPhaseHTTP()
    {
        super(new WebAppDescriptor.Builder("es.gob.afirma.services")
                .contextParam("webAppRootKey", "afirma-services.root")
                .servletClass(ServletContainer.class)
                .initParam("com.sun.jersey.config.property.packages", "es.gob.afirma.services")
                .build());

        this.resource = resource();
    }

    private final static Properties p1;
    static
    {
        p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF);
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT);
        p1.setProperty("signReason", "test");
        p1.setProperty("signatureProductionCity", "madrid");
        p1.setProperty("signerContact", "sink@usa.net");
        p1.setProperty("policyQualifier", "2.16.724.1.3.1.1.2");
        p1.setProperty("policyIdentifier", "http://google.com/");
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA, };

    private byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry,
            Properties extraParams) throws Exception
    {
        X509Certificate[] certChain = (X509Certificate[]) keyEntry.getCertificateChain();

        PreSignatureResult preSignatureResult = preSignature(data, algorithm, extraParams,
                certChain);

        final byte[] signature = PKCS1ExternalizableSigner.sign(algorithm, keyEntry,
                preSignatureResult.getPreSignData());

        return postSignature(data, algorithm, extraParams, certChain, preSignatureResult, signature);
    }

    private PreSignatureResult preSignature(final byte[] data, final String algorithm,
            Properties extraParams, X509Certificate[] certChain)
            throws CertificateEncodingException
    {
        MultivaluedMap<String, String> params = convertParamsToMultivalueMap(data, algorithm,
                extraParams, certChain);

        ClientResponse response = resource.path("pades/pre")
                .type(MediaType.APPLICATION_FORM_URLENCODED).post(ClientResponse.class, params);

        Assert.assertEquals(200, response.getStatus());

        return response.getEntity(PreSignatureResult.class);
    }

    private byte[] postSignature(final byte[] data, final String algorithm, Properties extraParams,
            X509Certificate[] certChain, PreSignatureResult preSignatureResult,
            final byte[] signature) throws CertificateEncodingException
    {
        MultivaluedMap<String, String> params;
        ClientResponse response;
        params = convertParamsToMultivalueMap(data, algorithm, extraParams, certChain, signature,
                preSignatureResult);

        response = resource.path("pades/post").type(MediaType.APPLICATION_FORM_URLENCODED)
                .post(ClientResponse.class, params);

        Assert.assertEquals(200, response.getStatus());

        return Base64.decode(response.getEntity(String.class));
    }

    private MultivaluedMap<String, String> convertParamsToMultivalueMap(byte[] data,
            String algorithm, Properties extraParams, X509Certificate[] certChain,
            byte[] signature, PreSignatureResult preSignatureResult)
            throws CertificateEncodingException
    {
        MultivaluedMap<String, String> params = convertParamsToMultivalueMap(data, algorithm,
                extraParams, certChain);

        params.add("base64Signature", Base64.encodeBytes(signature));
        params.add("base64PreSignData", Base64.encodeBytes(preSignatureResult.getPreSignData()));
        params.add("fileID", preSignatureResult.getFileID());

        return params;
    }

    private MultivaluedMap<String, String> convertParamsToMultivalueMap(final byte[] data,
            final String algorithm, Properties extraParams, X509Certificate[] certChain)
            throws CertificateEncodingException
    {
        MultivaluedMap<String, String> params = new MultivaluedMapImpl();
        params.add("base64Data", Base64.encodeBytes(data));
        params.add("algorithm", algorithm);

        for (X509Certificate certificate : certChain)
        {
            params.add("base64CertificateChain", Base64.encodeBytes(certificate.getEncoded()));
        }

        for (Entry<Object, Object> entry : extraParams.entrySet())
        {
            params.add("extraParamsNames", (String) entry.getKey());
            params.add("extraParamsValues", (String) entry.getValue());
        }

        return params;
    }

    @Test
    public void testTriPhaseSignature() throws Exception
    {
        Assert.assertEquals("file.signed.pdf", AOPDFSigner.getSignedName("file.pdf"));

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        KeyStore ks = KeyStore.getInstance("PKCS12");
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS,
                new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        AOSigner signer = new AOPDFSigner();

        String prueba;

        for (final String algo : ALGOS)
        {
            for (final String file : TEST_FILES)
            {
                final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader
                        .getSystemResourceAsStream(file));

                Assert.assertTrue("No se ha reconocido como un PDF",
                        signer.isValidDataFile(testPdf));

                prueba = "Firma trifasica PAdES con el algoritmo ': " + algo + "' y el fichero '"
                        + file + "'";

                Logger.getLogger("es.gob.afirma").info(prueba);

                byte[] result = sign(testPdf, algo, pke, p1);

                Assert.assertNotNull(prueba, result);
                Assert.assertTrue(signer.isSign(result));

                AOTreeModel tree = signer.getSignersStructure(result, false);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                Assert.assertEquals("ANF Usuario Activo",
                        ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject());

                tree = signer.getSignersStructure(result, true);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot())
                        .getChildAt(0).getUserObject();

                Assert.assertNotNull(simpleSignInfo.getSigningTime());
                Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);
                Assert.assertEquals("application/pdf", signer.getDataMimeType(result));
                Assert.assertEquals(result, signer.getData(result));
                Assert.assertEquals(AOSignConstants.SIGN_FORMAT_PDF, signer.getSignInfo(result)
                        .getFormat());

                final File saveFile = File.createTempFile(algo + "-", ".pdf");
                final OutputStream os = new FileOutputStream(saveFile);
                os.write(result);
                os.flush();
                os.close();
                Logger.getLogger("es.gob.afirma").info(
                        "Temporal para comprobacion manual: " + saveFile.getAbsolutePath());
            }
        }
    }
}