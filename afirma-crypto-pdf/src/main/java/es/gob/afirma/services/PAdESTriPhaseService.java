package es.gob.afirma.services;

import java.io.ByteArrayInputStream;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.lowagie.text.pdf.codec.Base64;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.PAdESTriPhaseSigner;
import es.gob.afirma.signers.pades.PAdESTriPhaseSigner.PdfPreSignResult;

@Path("pades")
public class PAdESTriPhaseService
{
    @POST
    @Path("pre")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_XML)
    public PreSignatureResult pre(@FormParam("base64Data") String base64Data,
            @FormParam("algorithm") String algorithm,
            @FormParam("base64CertificateChain") List<String> base64CertificateChain,
            @FormParam("extraParamsNames") List<String> extraParamsNames,
            @FormParam("extraParamsValues") List<String> extraParamsValues) throws Exception
    {
        byte[] data = Base64.decode(base64Data);
        List<X509Certificate> certChain = buildCertificateChain(base64CertificateChain);
        Properties extraParamsProperties = namesAndValuesListToProperties(extraParamsNames,
                extraParamsValues);

        PAdESTriPhaseSigner padesTri = new PAdESTriPhaseSigner();
        PdfPreSignResult preSignature = padesTri.preSign(
                AOSignConstants.getDigestAlgorithmName(algorithm), data,
                certChain.toArray(new X509Certificate[] {}), null, extraParamsProperties);

        return new PreSignatureResult(preSignature.getPreSign(), preSignature.getFileID());
    }

    @POST
    @Path("post")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_OCTET_STREAM)
    public String post(@FormParam("base64Data") String base64Data,
            @FormParam("algorithm") String algorithm,
            @FormParam("base64CertificateChain") List<String> base64CertificateChain,
            @FormParam("extraParamsNames") List<String> extraParamsNames,
            @FormParam("extraParamsValues") List<String> extraParamsValues,
            @FormParam("base64Signature") String base64Signature,
            @FormParam("base64PreSignData") String base64PreSignData,
            @FormParam("fileID") String fileID) throws Exception
    {
        byte[] data = Base64.decode(base64Data);
        List<X509Certificate> certChain = buildCertificateChain(base64CertificateChain);
        Properties extraParamsProperties = namesAndValuesListToProperties(extraParamsNames,
                extraParamsValues);

        PAdESTriPhaseSigner padesTri = new PAdESTriPhaseSigner();
        byte[] finalSignature = padesTri.postSign(
                AOSignConstants.getDigestAlgorithmName(algorithm), data,
                certChain.toArray(new X509Certificate[] {}), null, extraParamsProperties,
                Base64.decode(base64Signature), Base64.decode(base64PreSignData), fileID);

        return Base64.encodeBytes(finalSignature);
    }

    private List<X509Certificate> buildCertificateChain(List<String> base64CertificateChain)
            throws CertificateException
    {
        List<X509Certificate> certChain = new ArrayList<X509Certificate>();

        CertificateFactory cf = java.security.cert.CertificateFactory.getInstance("X.509");

        for (String base64Certificate : base64CertificateChain)
        {
            byte[] certificateData = Base64.decode(base64Certificate);
            X509Certificate certificate = (X509Certificate) cf
                    .generateCertificate(new ByteArrayInputStream(certificateData));
            certChain.add(certificate);
        }
        return certChain;
    }
    
    public static Properties namesAndValuesListToProperties(List<String> names, List<String> values)
    {
        Properties p = new Properties();

        for (int i = 0; i < names.size(); i++)
        {
            p.put(names.get(i), values.get(i));
        }

        return p;
    }    
}
