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

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.beans.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.services.PreSignatureResult;
import es.gob.afirma.signers.cades.PKCS1ExternalizableSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;

public class TestPAdESTriPhaseHTTPRemote {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx";
	private static final String CERT_PASS = "12341234";
	private static final String CERT_ALIAS = "anf usuario activo";

	private static final String[] TEST_FILES = { "TEST_PDF.pdf" };

	private WebResource resource;

	public TestPAdESTriPhaseHTTPRemote() {

		Client client = Client.create();
		client.addFilter(new LoggingFilter());

		this.resource = client
				.resource("http://ujiapps.uji.es/afirma-webapp-pdf/rest/");
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

	private byte[] sign(final byte[] data, final String algorithm,
			final PrivateKeyEntry keyEntry, final Properties extraParams)
			throws Exception {

		final X509Certificate[] certChain = (X509Certificate[]) keyEntry
				.getCertificateChain();

		final PreSignatureResult preSignatureResult = preSignature(data,
				algorithm, extraParams, certChain);

		final byte[] signature = PKCS1ExternalizableSigner.sign(algorithm,
				keyEntry, preSignatureResult.getPreSignData());

		return postSignature(data, algorithm, extraParams, certChain,
				preSignatureResult, signature);
	}

	private PreSignatureResult preSignature(final byte[] data,
			final String algorithm, final Properties extraParams,
			final X509Certificate[] certChain)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = convertParamsToMultivalueMap(
				data, algorithm, extraParams, certChain);

		final ClientResponse response = this.resource.path("pades/pre")
				.type(MediaType.APPLICATION_FORM_URLENCODED)
				.post(ClientResponse.class, params);

		Assert.assertEquals(200, response.getStatus());

		return response.getEntity(PreSignatureResult.class);
	}

	private byte[] postSignature(final byte[] data, final String algorithm,
			final Properties extraParams, final X509Certificate[] certChain,
			final PreSignatureResult preSignatureResult, final byte[] signature)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = convertParamsToMultivalueMap(
				data, algorithm, extraParams, certChain, signature,
				preSignatureResult);
		final ClientResponse response = this.resource.path("pades/post")
				.type(MediaType.APPLICATION_FORM_URLENCODED)
				.post(ClientResponse.class, params);

		Assert.assertEquals(200, response.getStatus());

		return Base64.decode(response.getEntity(String.class));
	}

	private MultivaluedMap<String, String> convertParamsToMultivalueMap(
			final byte[] data, final String algorithm,
			final Properties extraParams, final X509Certificate[] certChain,
			final byte[] signature, final PreSignatureResult preSignatureResult)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = convertParamsToMultivalueMap(
				data, algorithm, extraParams, certChain);

		params.add("base64Signature", Base64.encodeBytes(signature));
		params.add("base64PreSignData",
				Base64.encodeBytes(preSignatureResult.getPreSignData()));
		params.add("fileID", preSignatureResult.getFileID());

		return params;
	}

	private MultivaluedMap<String, String> convertParamsToMultivalueMap(
			final byte[] data, final String algorithm,
			final Properties extraParams, final X509Certificate[] certChain)
			throws CertificateEncodingException {

		final MultivaluedMap<String, String> params = new MultivaluedMapImpl();
		params.add("base64Data", Base64.encodeBytes(data));
		params.add("algorithm", algorithm);

		for (final X509Certificate certificate : certChain) {
			params.add("base64CertificateChain",
					Base64.encodeBytes(certificate.getEncoded()));
		}

		for (final Entry<Object, Object> entry : extraParams.entrySet()) {
			params.add("extraParamsNames", (String) entry.getKey());
			params.add("extraParamsValues", (String) entry.getValue());
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
		Assert.assertEquals("file.signed.pdf",
				AOPDFSigner.getSignedName("file.pdf"));

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);

		final X509Certificate cert;

		KeyStore ks = KeyStore.getInstance("PKCS12");
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH),
				CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS,
				new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
		cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		AOSigner signer = new AOPDFSigner();

		String prueba;

		for (final String algo : ALGOS) {
			for (final String file : TEST_FILES) {
				final byte[] testPdf = AOUtil
						.getDataFromInputStream(ClassLoader
								.getSystemResourceAsStream(file));

				Assert.assertTrue("No se ha reconocido como un PDF",
						signer.isValidDataFile(testPdf));

				prueba = "Firma trifasica PAdES con el algoritmo ': " + algo
						+ "' y el fichero '" + file + "'";

				System.out.println(prueba);

				byte[] result = sign(testPdf, algo, pke, p1);

				Assert.assertNotNull(prueba, result);
				Assert.assertTrue(signer.isSign(result));

				AOTreeModel tree = signer.getSignersStructure(result, false);
				Assert.assertEquals("Datos",
						((AOTreeNode) tree.getRoot()).getUserObject());
				Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree
						.getRoot()).getChildAt(0).getUserObject());

				tree = signer.getSignersStructure(result, true);
				Assert.assertEquals("Datos",
						((AOTreeNode) tree.getRoot()).getUserObject());
				AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree
						.getRoot()).getChildAt(0).getUserObject();

				Assert.assertNotNull(simpleSignInfo.getSigningTime());
				Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);
				Assert.assertEquals("application/pdf",
						signer.getDataMimeType(result));
				Assert.assertEquals(result, signer.getData(result));
				Assert.assertEquals(AOSignConstants.SIGN_FORMAT_PDF, signer
						.getSignInfo(result).getFormat());

				final File saveFile = File.createTempFile(algo + "-", ".pdf");
				final OutputStream os = new FileOutputStream(saveFile);
				os.write(result);
				os.flush();
				os.close();
				System.out.println("Temporal para comprobacion manual: "
						+ saveFile.getAbsolutePath());
			}
		}
	}	
}