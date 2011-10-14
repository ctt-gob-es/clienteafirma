package es.gob.afirma.services;

import java.io.ByteArrayInputStream;
import java.io.IOException;
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
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.lowagie.text.pdf.codec.Base64;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.PAdESTriPhaseSigner;
import es.gob.afirma.signers.pades.PAdESTriPhaseSigner.PdfPreSignResult;

/** Implementaci&oaute;n de referencia del servicio servidor de firma trif&aacute;sica PAdES. */
@Path("pades")
public class PAdESTriPhaseService {
    
//	@javax.ws.rs.core.Context
//	private javax.servlet.ServletConfig servletConfig;
	
    /** Prefirma.
     * @param base64Data Datos a prefirmar codificados en Base64
     * @param algorithm Algoritmo de firma
     * @param base64CertificateChain Cadena de certificados del firmante codificada en Base64
     * @param extraParamsNames Nombres de las opciones adicionales de prefirma
     * @param extraParamsValues Valores de las opciones adicionales de prefirma
     * @return Prefirma PAdES
     * @throws Exception en caso de cualquier error
     */
    @POST
    @Path("pre")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_XML)
    public PreSignatureResult pre(@FormParam("base64Data")             final String base64Data,
                                  @FormParam("algorithm")              final String algorithm,
                                  @FormParam("base64CertificateChain") final List<String> base64CertificateChain,
                                  @FormParam("extraParamsNames")       final List<String> extraParamsNames,
                                  @FormParam("extraParamsValues")      final List<String> extraParamsValues) throws Exception
    {
        final byte[] data = Base64.decode(base64Data);
        
        return executePreOperation(algorithm, base64CertificateChain,
				extraParamsNames, extraParamsValues, data);
    }
    
    /** Prefirma un PDF existente en servidor indicado su referencia.
     * @param reference Referencia del PDF
     * @param algorithm Algoritmo de firma
     * @param base64CertificateChain Cadena de certificados del firmante
     * @param extraParamsNames Nombres de los par&aacute;metros adicionales de la firma
     * @param extraParamsValues Valores de los par&aacute;metros adicionales de la firma (correspondientes por posici&oacute;n con los nombres)
     * @return XML con el resultado de la pre-firma
     * @throws Exception
     */
    @POST
    @Path("pre/{reference}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_XML)
    public PreSignatureResult preWithReference(@PathParam("reference") final String reference,
                                  @FormParam("algorithm")              final String algorithm,
                                  @FormParam("base64CertificateChain") final List<String> base64CertificateChain,
                                  @FormParam("extraParamsNames")       final List<String> extraParamsNames,
                                  @FormParam("extraParamsValues")      final List<String> extraParamsValues) throws Exception
    {
    	byte[] data = new byte[] {};
    	
    	if (reference != null)
    	{
    		if ("SIMPLE".equals(reference)) //$NON-NLS-1$
    		{
    			data = AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream("TEST_PDF.pdf")); //$NON-NLS-1$
    		}

    		if ("CERTIFIED".equals(reference)) //$NON-NLS-1$
    		{
    			data = AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream("TEST_PDF_Certified.pdf")); //$NON-NLS-1$
    		}
    		
    		if ("PASSWORD".equals(reference)) //$NON-NLS-1$
    		{
    			data = AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream("TEST_PDF_Password.pdf")); //$NON-NLS-1$
    		}
    	}
        		
        return executePreOperation(algorithm, base64CertificateChain,
				extraParamsNames, extraParamsValues, data);
    }

	private PreSignatureResult executePreOperation(final String algorithm,
			final List<String> base64CertificateChain,
			final List<String> extraParamsNames,
			final List<String> extraParamsValues, byte[] data)
			throws CertificateException, IOException, AOException {
		
		final List<X509Certificate> certChain = buildCertificateChain(base64CertificateChain);
        final Properties extraParamsProperties = namesAndValuesListToProperties(extraParamsNames, extraParamsValues);

        final PAdESTriPhaseSigner padesTri = new PAdESTriPhaseSigner();
        final PdfPreSignResult preSignature = padesTri.preSign(
                AOSignConstants.getDigestAlgorithmName(algorithm), 
                data,
                certChain.toArray(new X509Certificate[] {}), 
                null, 
                extraParamsProperties
        );

        return new PreSignatureResult(preSignature.getPreSign(), preSignature.getFileID());
	}
    

    /** Postfirma.
     * @param base64Data Datos a prefirmar codificados en Base64
     * @param algorithm Algoritmo de firma
     * @param base64CertificateChain Cadena de certificados del firmante codificada en Base64
     * @param extraParamsNames Nombres de las opciones adicionales de prefirma
     * @param extraParamsValues Valores de las opciones adicionales de prefirma
     * @param base64Signature Firma PKCS#1 v1.5 codificada en Base64
     * @param base64PreSignData Atributos firmados CAdES de la prefirma, codificados en Base64
     * @param fileID FileID del PDF prefirmado
     * @return PDF firmado
     * @throws Exception en caso de cualquier problema
     */
    @POST
    @Path("post")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_OCTET_STREAM)
    public String post(@FormParam("base64Data")             final String base64Data,
                       @FormParam("algorithm")              final String algorithm,
                       @FormParam("base64CertificateChain") final List<String> base64CertificateChain,
                       @FormParam("extraParamsNames")       final List<String> extraParamsNames,
                       @FormParam("extraParamsValues")      final List<String> extraParamsValues,
                       @FormParam("base64Signature")        final String base64Signature,
                       @FormParam("base64PreSignData")      final String base64PreSignData,
                       @FormParam("fileID") String fileID) throws Exception {
        
        final byte[] data = Base64.decode(base64Data);
        return executePostOperation(algorithm, base64CertificateChain,
				extraParamsNames, extraParamsValues, base64Signature,
				base64PreSignData, fileID, data);
    }

    /** Postfirma un PDF existente en servidor indicando su referencia.
     * @param reference Referencia del PDF
     * @param algorithm Algoritmo de firma
     * @param base64CertificateChain Cadena de certificados del firmante codificada en Base64
     * @param extraParamsNames Nombres de las opciones adicionales de prefirma
     * @param extraParamsValues Valores de las opciones adicionales de prefirma
     * @param base64Signature Firma PKCS#1 v1.5 codificada en Base64
     * @param base64PreSignData Prefirma del PDF codificada en Base64
     * @param fileID FILEID del PDF prefirmado
     * @return PDF firmado
     * @throws Exception
     */
    @POST
    @Path("post/{reference}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_OCTET_STREAM)
    public String postWithReference(@PathParam("reference") final String reference,
                       @FormParam("algorithm")              final String algorithm,
                       @FormParam("base64CertificateChain") final List<String> base64CertificateChain,
                       @FormParam("extraParamsNames")       final List<String> extraParamsNames,
                       @FormParam("extraParamsValues")      final List<String> extraParamsValues,
                       @FormParam("base64Signature")        final String base64Signature,
                       @FormParam("base64PreSignData")      final String base64PreSignData,
                       @FormParam("fileID") String fileID) throws Exception {
        
    	byte[] data = new byte[] {};
    	
    	if (reference != null)
    	{
    		if ("SIMPLE".equals(reference)) //$NON-NLS-1$
    		{
    			data = AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream("TEST_PDF.pdf")); //$NON-NLS-1$
    		}

    		if ("CERTIFIED".equals(reference)) //$NON-NLS-1$
    		{
    			data = AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream("TEST_PDF_Certified.pdf")); //$NON-NLS-1$
    		}
    		
    		if ("PASSWORD".equals(reference)) //$NON-NLS-1$
    		{
    			data = AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream("TEST_PDF_Password.pdf")); //$NON-NLS-1$
    		}
    	}

    	return executePostOperation(algorithm, base64CertificateChain,
				extraParamsNames, extraParamsValues, base64Signature,
				base64PreSignData, fileID, data);
    }

	private String executePostOperation(final String algorithm,
			final List<String> base64CertificateChain,
			final List<String> extraParamsNames,
			final List<String> extraParamsValues, final String base64Signature,
			final String base64PreSignData, String fileID, byte[] data)
			throws CertificateException, AOException, IOException {
		final List<X509Certificate> certChain = buildCertificateChain(base64CertificateChain);
        final Properties extraParamsProperties = namesAndValuesListToProperties(extraParamsNames, extraParamsValues);

        final PAdESTriPhaseSigner padesTri = new PAdESTriPhaseSigner();
        final byte[] finalSignature = padesTri.postSign(
            AOSignConstants.getDigestAlgorithmName(algorithm), data,
            certChain.toArray(new X509Certificate[] {}), null, extraParamsProperties,
            Base64.decode(base64Signature), Base64.decode(base64PreSignData), fileID
        );

        return Base64.encodeBytes(finalSignature);
	}
    
    private List<X509Certificate> buildCertificateChain(final List<String> base64CertificateChain) throws CertificateException {
        final List<X509Certificate> certChain = new ArrayList<X509Certificate>();
        final CertificateFactory cf = java.security.cert.CertificateFactory.getInstance("X.509"); //$NON-NLS-1$

        for (final String base64Certificate : base64CertificateChain) {
            final byte[] certificateData = Base64.decode(base64Certificate);
            certChain.add((X509Certificate) cf.generateCertificate(new ByteArrayInputStream(certificateData)));
        }
        return certChain;
    }
    
    /** Genera un <code>Properties</code> en base a listas separadas de nombres de propiedad y valores. Ambas listas deben ser del mismo tama&ntilde;o
     * @param names Nombres de las propiedades
     * @param values Valores de las propiedades
     * @return Fichero de propiedades
     */
    private static Properties namesAndValuesListToProperties(final List<String> names, final List<String> values) {
        final Properties p = new Properties();
        for (int i = 0; i < names.size(); i++) {
            p.put(names.get(i), values.get(i));
        }
        return p;
    }
    
}