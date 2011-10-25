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
import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.filter.LoggingFilter;
import com.sun.jersey.core.util.MultivaluedMapImpl;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.services.PreSignatureResult;
import es.gob.afirma.signers.cades.PKCS1ExternalizableSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de firma PDF en tres fases con comunicaci&oacute;n con servidor de firma v&iacute;a HTTP. 
 * PDF en servidor referenciado mediante una referencia */
public class TestPAdESTriPhaseHTTPRemoteWithReference {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String[] TEST_FILES = { "TEST_PDF.pdf" }; //$NON-NLS-1$

	private WebResource resource;

	/** Prueba de firma PDF en tres fases con comunicaci&oacute;n con servidor de firma v&iacute;a HTTP. 
	 * PDF en servidor referenciado mediante una referencia */
	public TestPAdESTriPhaseHTTPRemoteWithReference() {

		Client client = Client.create();
		client.addFilter(new LoggingFilter());

		this.resource = client
				.resource("http://ujiapps.uji.es/afirma-webapp-pdf/rest/"); //$NON-NLS-1$
	}

	private final static Properties p1;

	static {
		p1 = new Properties();
		p1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
		p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		p1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
		p1.setProperty("signatureProductionCity", "madrid"); //$NON-NLS-1$ //$NON-NLS-2$
		p1.setProperty("signerContact", "sink@usa.net"); //$NON-NLS-1$ //$NON-NLS-2$
		p1.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2"); //$NON-NLS-1$ //$NON-NLS-2$
		p1.setProperty("policyQualifier", "http://google.com/"); //$NON-NLS-1$ //$NON-NLS-2$
		p1.setProperty("policyIdentifierHash", "0"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private final static String[] ALGOS = new String[] {
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA };

	private byte[] sign(String reference, final String algorithm,
			final PrivateKeyEntry keyEntry, final Properties extraParams)
			throws Exception {

		final X509Certificate[] certChain = (X509Certificate[]) keyEntry
				.getCertificateChain();

		final PreSignatureResult preSignatureResult = preSignature(reference,
				algorithm, extraParams, certChain);

		final byte[] signature = PKCS1ExternalizableSigner.sign(algorithm,
				keyEntry, preSignatureResult.getPreSignData());

		return postSignature(reference, algorithm, extraParams, certChain,
				preSignatureResult, signature);
	}

	private PreSignatureResult preSignature(String reference,
			final String algorithm, final Properties extraParams,
			final X509Certificate[] certChain)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = convertParamsToMultivalueMap(
				reference, algorithm, extraParams, certChain);

		final ClientResponse response = this.resource.path("pades/pre/" + reference) //$NON-NLS-1$
				.type(MediaType.APPLICATION_FORM_URLENCODED)
				.post(ClientResponse.class, params);

		Assert.assertEquals(200, response.getStatus());

		return response.getEntity(PreSignatureResult.class);
	}

	private byte[] postSignature(String reference, final String algorithm,
			final Properties extraParams, final X509Certificate[] certChain,
			final PreSignatureResult preSignatureResult, final byte[] signature)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = convertParamsToMultivalueMap(
				reference, algorithm, extraParams, certChain, signature,
				preSignatureResult);
		final ClientResponse response = this.resource.path("pades/post/" + reference) //$NON-NLS-1$
				.type(MediaType.APPLICATION_FORM_URLENCODED)
				.post(ClientResponse.class, params);

		Assert.assertEquals(200, response.getStatus());

		return Base64.decode(response.getEntity(String.class));
	}

	private MultivaluedMap<String, String> convertParamsToMultivalueMap(
			String reference, final String algorithm,
			final Properties extraParams, final X509Certificate[] certChain,
			final byte[] signature, final PreSignatureResult preSignatureResult)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = convertParamsToMultivalueMap(
				reference, algorithm, extraParams, certChain);

		params.add("base64Signature", Base64.encodeBytes(signature)); //$NON-NLS-1$
		params.add("base64PreSignData", //$NON-NLS-1$
				Base64.encodeBytes(preSignatureResult.getPreSignData()));
		params.add("fileID", preSignatureResult.getFileID()); //$NON-NLS-1$

		return params;
	}

	private MultivaluedMap<String, String> convertParamsToMultivalueMap(
			String reference, final String algorithm,
			final Properties extraParams, final X509Certificate[] certChain)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = new MultivaluedMapImpl();
		params.add("reference", reference); //$NON-NLS-1$
		params.add("algorithm", algorithm); //$NON-NLS-1$

		for (final X509Certificate certificate : certChain) {
			params.add("base64CertificateChain", //$NON-NLS-1$
					Base64.encodeBytes(certificate.getEncoded()));
		}

		for (final Entry<Object, Object> entry : extraParams.entrySet()) {
			params.add("extraParamsNames", (String) entry.getKey()); //$NON-NLS-1$
			params.add("extraParamsValues", (String) entry.getValue()); //$NON-NLS-1$
		}

		return params;
	}

	/**
	 * Pruabas de las firmas trif&aacute;sicas PAdES usando llamadas locales.
	 * Necesita un iText modificado
	 * 
	 * @throws Exception
	 *             en caso de cualquier error
	 */
	@Test
	public void testTriPhaseSignature() throws Exception {
		Assert.assertEquals("file.signed.pdf", //$NON-NLS-1$
				AOPDFSigner.getSignedName("file.pdf")); //$NON-NLS-1$

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final X509Certificate cert;

		KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH),
				CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS,
				new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
		cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		AOSigner signer = new AOPDFSigner();

		String prueba;

		for (final String algo : ALGOS) {
			for (final String file : TEST_FILES) {
				prueba = "Firma trifasica PAdES con el algoritmo ': " + algo //$NON-NLS-1$
						+ "' y el fichero '" + file + "'"; //$NON-NLS-1$ //$NON-NLS-2$

				System.out.println(prueba);

				byte[] result = sign("SIMPLE", algo, pke, p1); //$NON-NLS-1$
//				byte[] result = sign("CERTIFIED", algo, pke, p1);
//				byte[] result = sign("PASSWORD", algo, pke, p1);

				Assert.assertNotNull(prueba, result);
				Assert.assertTrue(signer.isSign(result));

				AOTreeModel tree = signer.getSignersStructure(result, false);
				Assert.assertEquals("Datos", //$NON-NLS-1$
						((AOTreeNode) tree.getRoot()).getUserObject());
				Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree //$NON-NLS-1$
						.getRoot()).getChildAt(0).getUserObject());

				tree = signer.getSignersStructure(result, true);
				Assert.assertEquals("Datos", //$NON-NLS-1$
						((AOTreeNode) tree.getRoot()).getUserObject());
				AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree
						.getRoot()).getChildAt(0).getUserObject();

				Assert.assertNotNull(simpleSignInfo.getSigningTime());
				Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);
				Assert.assertEquals(result, signer.getData(result));
				Assert.assertEquals(AOSignConstants.SIGN_FORMAT_PDF, signer
						.getSignInfo(result).getFormat());

				final File saveFile = File.createTempFile(algo + "-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
				final OutputStream os = new FileOutputStream(saveFile);
				os.write(result);
				os.flush();
				os.close();
				System.out.println("Temporal para comprobacion manual: " //$NON-NLS-1$
						+ saveFile.getAbsolutePath());
			}
		}
	}	
}