import java.io.File;
import java.io.FileOutputStream;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.List;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signfolder.client.MobileApplication;
import es.gob.afirma.signfolder.client.MobileDocument;
import es.gob.afirma.signfolder.client.MobileRequest;
import es.gob.afirma.signfolder.client.MobileRequestFilter;
import es.gob.afirma.signfolder.client.MobileRequestFilterList;
import es.gob.afirma.signfolder.client.MobileRequestList;
import es.gob.afirma.signfolder.client.MobileService;
import es.gob.afirma.signfolder.client.MobileService_Service;
import es.gob.afirma.signfolder.client.MobileSignFormat;
import es.gob.afirma.signfolder.client.MobileStringList;


public class TestBackendConection {

	private static final String CERT_PATH = "ANF-Activo.p12"; //$NON-NLS-1$
	private static final String CERT_PASS = "1111"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	
	@Test
	@Ignore
	public void listRequestUnresolved() throws Exception {

		disableSslChecks();

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Cargamos el certificado
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final X509Certificate cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		// Listado de formatos de firma soportados
		final MobileStringList formatsList = new MobileStringList();
		formatsList.getStr().add(MobileSignFormat.CADES.toString());
		formatsList.getStr().add(MobileSignFormat.PDF.toString());
		formatsList.getStr().add(MobileSignFormat.XADES.toString());

		// Listado de filtros para la consulta
		final MobileRequestFilterList filterList = new MobileRequestFilterList();
		
		System.out.println("CertEncoded: " + Base64.encode(cert.getEncoded()));
		System.out.println("State: " + "unresolved");
		System.out.println("INIT_PAGE: " + "1");
		System.out.println("PAGE_SIZE: " + "50");
		System.out.println("formatsList: " + formatsList.getStr());
		
		// Solicitud de lista de peticiones
		final MobileRequestList requestsList = service.queryRequestList(cert.getEncoded(), "unresolved", "1", "50", formatsList, filterList);
		final List<MobileRequest> list = requestsList.getRequestList();
		
		// Imprimimos
		for (final MobileRequest request : list) {
			if (request.getSubject().getValue().toLowerCase().contains("carlos")) {
				System.out.println(" ==================");
				System.out.println("Identifier: " + request.getIdentifier().getValue());
				System.out.println("Application: " + (request.getApplication() != null ? request.getApplication().getValue() : null));
				System.out.println("RequestTagId: " + request.getRequestTagId());
				System.out.println("Fentry (Fecha): " + request.getFentry().getValue().toString());
				System.out.println("View: " + request.getView());
				System.out.println("Fordward: " + request.getForward().getValue());
				System.out.println("Ref: " + (request.getRef() != null ? request.getRef().getValue() : null));
				System.out.println("Subject: " + request.getSubject().getValue());
				System.out.println("Text: " + (request.getText() != null ? request.getText().getValue() : null));
				System.out.println("Workflow: " + request.getWorkflow().getValue());
				System.out.println("Priority: " + request.getImportanceLevel().getValue());
				System.out.println(" ==================");
			}
		}
		
		final Integer size = requestsList.getSize();
		System.out.println("Numero de elementos totales: " + size);
	}
	
	@Test
	@Ignore
	public void listRequestSigned() throws Exception {

		disableSslChecks();

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Cargamos el certificado
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final X509Certificate cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		// Listado de formatos de firma soportados
		final MobileStringList formatsList = new MobileStringList();
		formatsList.getStr().add(MobileSignFormat.CADES.toString());
		formatsList.getStr().add(MobileSignFormat.PDF.toString());

		// Listado de filtros para la consulta
		final MobileRequestFilter filter = new MobileRequestFilter();
		filter.setKey("clave");
		filter.setValue("valor");
		
		final MobileRequestFilterList filterList = new MobileRequestFilterList();
//		filterList.getRequestFilter().add(filter);
		
		// Solicitud de lista de peticiones
		final List<MobileRequest> list = service.queryRequestList(cert.getEncoded(), "signed", "1", "500", formatsList, filterList).getRequestList();

		// Imprimimos
		for (final MobileRequest request : list) {
			if (request.getSubject().getValue().toLowerCase().contains("carlos")) {
				System.out.println(" ==================");
				System.out.println("Identifier: " + request.getIdentifier().getValue());
				System.out.println("Application: " + (request.getApplication() != null ? request.getApplication().getValue() : null));
				System.out.println("RequestTagId: " + request.getRequestTagId());
				System.out.println("Fentry (Fecha): " + request.getFentry().getValue().toString());
				System.out.println("View: " + request.getView());
				System.out.println("Fordward: " + request.getForward().getValue());
				System.out.println("Ref: " + (request.getRef() != null ? request.getRef().getValue() : null));
				System.out.println("Subject: " + request.getSubject().getValue());
				System.out.println("Text: " + (request.getText() != null ? request.getText().getValue() : null));
				System.out.println("Workflow: " + request.getWorkflow().getValue());
				System.out.println("Priority: " + request.getImportanceLevel().getValue());
				System.out.println(" ==================");
			}
		}
	}
	
	@Test
	@Ignore
	public void listRequestRejected() throws Exception {

		disableSslChecks();

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Cargamos el certificado
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final X509Certificate cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		// Listado de formatos de firma soportados
		final MobileStringList formatsList = new MobileStringList();
		formatsList.getStr().add(MobileSignFormat.CADES.toString());

		// Listado de filtros para la consulta
		final MobileRequestFilterList filterList = new MobileRequestFilterList();

		// Solicitud de lista de peticiones
		final List<MobileRequest> list = service.queryRequestList(cert.getEncoded(), "rejected", "1", "50", formatsList, filterList).getRequestList();

		// Imprimimos
		for (final MobileRequest request : list) {
			if (request.getSubject().getValue().toLowerCase().contains("carlos")) {
				System.out.println(" ==================");
				System.out.println("Identifier: " + request.getIdentifier().getValue());
				System.out.println("Application: " + (request.getApplication() != null ? request.getApplication().getValue() : null));
				System.out.println("RequestTagId: " + request.getRequestTagId());
				System.out.println("Fentry (Fecha): " + request.getFentry().getValue().toString());
				System.out.println("View: " + request.getView());
				System.out.println("Fordward: " + request.getForward().getValue());
				System.out.println("Ref: " + (request.getRef() != null ? request.getRef().getValue() : null));
				System.out.println("Subject: " + request.getSubject().getValue());
				System.out.println("Text: " + (request.getText() != null ? request.getText().getValue() : null));
				System.out.println("Workflow: " + request.getWorkflow().getValue());
				System.out.println("Priority: " + request.getImportanceLevel().getValue());
				System.out.println(" ==================");
			}
		}
	}
	
	@Test
	@Ignore
	public void requestDetails() throws Exception {

		disableSslChecks();

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Cargamos el certificado
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final X509Certificate cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		File temp = File.createTempFile("cert", ".cer");
		FileOutputStream fos = new FileOutputStream(temp);
		fos.write(cert.getEncoded());
		fos.close();
		
		System.out.println(temp.getAbsolutePath());
		
		MobileRequest request = service.queryRequest(cert.getEncoded(), "5r76ZWrzz9");

		// Imprimimos
		System.out.println(" ==================");
		System.out.println("Identifier: " + request.getIdentifier().getValue());
		System.out.println("Application: " + (request.getApplication() != null ? request.getApplication().getValue() : null));
		System.out.println("RequestTagId: " + request.getRequestTagId());
		System.out.println("Fentry (Fecha): " + request.getFentry().getValue().toString());
		System.out.println("View: " + request.getView());
		System.out.println("Fordward: " + request.getForward().getValue());
		System.out.println("Ref: " + (request.getRef() != null ? request.getRef().getValue() : null));
		System.out.println("Subject: " + request.getSubject().getValue());
		System.out.println("Text: " + (request.getText() != null ? request.getText().getValue() : null));
		System.out.println("Workflow: " + request.getWorkflow().getValue());
		System.out.println("Priority: " + request.getImportanceLevel().getValue());
		System.out.println("Documents:");
		for (MobileDocument doc : request.getDocumentList().getDocument()) {
			System.out.println("\tIdentifier: " + doc.getIdentifier());
			System.out.println("\tMime: " + doc.getMime());
			System.out.println("\tName: " + doc.getName());
			System.out.println("\tSignatureAlgorithm: " + doc.getSignAlgorithm().getValue());
			System.out.println("\tSignatureType: " + doc.getSignatureType().getValue().value());
			//System.out.println("\tData: " + doc.getData().getValue());
		}
		System.out.println(" ==================");
	}
	
	@Test
	@Ignore
	public void requestReject() throws Exception {

		disableSslChecks();

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Cargamos el certificado
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final X509Certificate cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		String responseId = service.rejectRequest(cert.getEncoded(), "5r76ZWrzz9", "Prueba de rechazo de peticion");

		// Imprimimos
		System.out.println(" ==================");
		System.out.println("Respuesta del rechazo: " + responseId);
		System.out.println(" ==================");
	}
	
	@Test
	@Ignore
	public void requestDocumentPreview() throws Exception {

		disableSslChecks();

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Cargamos el certificado
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final X509Certificate cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		MobileDocument doc = service.documentPreview(cert.getEncoded(), "k71o3LZrPm");

		// Imprimimos
		System.out.println(" ==================");
		System.out.println("\tIdentifier: " + doc.getIdentifier());
		System.out.println("\tMime: " + doc.getMime());
		System.out.println("\tName: " + doc.getName());
		System.out.println("\tSignatureAlgorithm: " + doc.getSignAlgorithm().getValue());
		System.out.println("\tSignatureType: " + doc.getSignatureType().getValue().value());
		System.out.println("\tData: " + doc.getData().getValue().getContent());
		System.out.println(" ==================");
	}
	
	@Test
	@Ignore
	public void requestAppConfiguration() throws Exception {

		disableSslChecks();

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Cargamos el certificado
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final X509Certificate cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		List<MobileApplication> appList = service.queryApplicationsMobile(cert.getEncoded()).getApplicationList();
		for (MobileApplication app : appList) {
			// Imprimimos
			System.out.println(" ==================");
			System.out.println("\tId: " + app.getId());
			System.out.println("\tName: " + app.getName());
			System.out.println(" ==================");
		}
	}
	
	private static final TrustManager[] DUMMY_TRUST_MANAGER = new TrustManager[] {
		new X509TrustManager() {
			@Override
			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
				return null;
			}
			@Override
			public void checkClientTrusted(final X509Certificate[] certs, final String authType) { /* No hacemos nada */ }
			@Override
			public void checkServerTrusted(final X509Certificate[] certs, final String authType) {  /* No hacemos nada */  }

		}
	};

	private static void disableSslChecks() throws KeyManagementException, NoSuchAlgorithmException {
		final SSLContext sc = SSLContext.getInstance("SSL"); //$NON-NLS-1$
		sc.init(null, DUMMY_TRUST_MANAGER, new java.security.SecureRandom());
		HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
		HttpsURLConnection.setDefaultHostnameVerifier(new HostnameVerifier() {
			@Override
			public boolean verify(final String hostname, final SSLSession session) {
				return true;
			}
		});
	}
}
