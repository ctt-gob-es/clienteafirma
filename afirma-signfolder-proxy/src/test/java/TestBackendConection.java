import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
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
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
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

		final File temp = File.createTempFile("cert", ".cer");
		final FileOutputStream fos = new FileOutputStream(temp);
		fos.write(cert.getEncoded());
		fos.close();

		System.out.println(temp.getAbsolutePath());

		final MobileRequest request = service.queryRequest(cert.getEncoded(), "5r76ZWrzz9");

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
		for (final MobileDocument doc : request.getDocumentList().getDocument()) {
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

		final String responseId = service.rejectRequest(cert.getEncoded(), "5r76ZWrzz9", "Prueba de rechazo de peticion");

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

		final MobileDocument doc = service.documentPreview(cert.getEncoded(), "k71o3LZrPm");

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

		final List<MobileApplication> appList = service.queryApplicationsMobile(cert.getEncoded()).getApplicationList();
		for (final MobileApplication app : appList) {
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

	public static void main(final String[] args) throws IOException {

		final StringBuilder urlBuilder = new StringBuilder()
		.append("http://localhost:8080/afirma-signfolder-proxy/ProxyService?")
		.append("op=0&")	// Prefirma
		.append("dat=")
		.append("PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjxycXR0cmk-PGNlcnQ-TUlJS0FUQ0NCK21nQXdJQkFnSUlCUnBZd2VHdnlrQXdEUVlKS29aSWh2Y05BUUVGQlFBd2dnRWhNUXN3Q1FZRFZRUUdFd0pGVXpFU01CQUdBMVVFQ0F3SlFtRnlZMlZzYjI1aE1WZ3dWZ1lEVlFRSERFOUNZWEpqWld4dmJtRWdLSE5sWlNCamRYSnlaVzUwSUdGa1pISmxjM01nWVhRZ2FIUjBjRG92TDNkM2R5NWhibVl1WlhNdlpYTXZZV1JrY21WemN5MWthWEpsWTJOcGIyNHVhSFJ0YkNBcE1TY3dKUVlEVlFRS0RCNUJUa1lnUVhWMGIzSnBaR0ZrSUdSbElFTmxjblJwWm1sallXTnBiMjR4TGpBc0JnTlZCQXNNSlVGT1JpQkJkWFJ2Y21sa1lXUWdTVzUwWlhKdFpXUnBZU0JrWlNCSlpHVnVkR2xrWVdReEdqQVlCZ2txaGtpRzl3MEJDUUVXQzJsdVptOUFZVzVtTG1Wek1SSXdFQVlEVlFRRkV3bEhOak15T0RjMU1UQXhHekFaQmdOVkJBTU1Fa0ZPUmlCQmMzTjFjbVZrSUVsRUlFTkJNVEFlRncweE5EQTFNakV4TXpJMk1qVmFGdzB4TmpBMU1qQXhNekkyTWpWYU1JSUJEakU1TURjR0ExVUVDd3d3UTJWeWRHbG1hV05oWkc4Z1pHVWdRMnhoYzJVZ01pQmtaU0JRWlhKemIyNWhJRVpwYzJsallTQW9Sa2xTVFVFcE1SMHdHd1lEVlFRRERCUkdTVk5KUTA4Z1FVTlVTVlpQSUZCU1ZVVkNRVEVXTUJRR0ExVUVCQXdOUVVOVVNWWlBJRkJTVlVWQ1FURVBNQTBHQTFVRUtnd0dSa2xUU1VOUE1SSXdFQVlEVlFRRkV3a3pPRGcyTkRFMU9WZ3hKVEFqQmdrcWhraUc5dzBCQ1FFV0ZuTmxjblpwWTJsdmRHVmpibWxqYjBCaGJtWXVaWE14RWpBUUJnTlZCQWNNQ1ZCUFFreEJRMGxQVGpFU01CQUdBMVVFQ0F3SlVGSlBWa2xPUTBsQk1Rc3dDUVlEVlFRR0V3SmxjekVaTUJjR0Npc0dBUVFCZ1pNV0FRRU1DVE00T0RZME1UVTVXRENCbnpBTkJna3Foa2lHOXcwQkFRRUZBQU9CalFBd2dZa0NnWUVBbDZZeUtuVDVlTnNGNGNzUzFKVDJuY1IzSUFuT3RvcW15cEdNa3VtdCs3SS9rTW1RTFNCRDNObk8ybzlCbFp6YVhpbGNmb3RVMFFvdzkxOC95clFXUlNpSlhhaWlFaXJJQVNzTTl4V3poRkFrTllEem9qeWovYktHaEZuSVBUd2M5QU55NkpiQVNnTDZVbjhwSXJuRXpvaUZpeGg2dUlTK3lzTURWbmNiS2ZFQ0F3RUFBYU9DQk00d2dnVEtNQmtHQ2lzR0FRUUJnWk1XQVFFRUN3d0pNemc0TmpReE5UbFlNQjBHQTFVZERnUVdCQlRCMFBkcGoybUdMQzhFUXpCVHlYbWhObE1WdERBSkJnTlZIUk1FQWpBQU1EY0dDQ3NHQVFVRkJ3RUJCQ3N3S1RBbkJnZ3JCZ0VGQlFjd0FZWWJhSFIwY0RvdkwyOWpjM0F1WVc1bUxtVnpMM053WVdsdUwwRldNSUlCaEFZRFZSMGZCSUlCZXpDQ0FYY3dPcUE0b0RhR05HaDBkSEJ6T2k4dlkzSnNMbUZ1Wmk1bGN5OWpjbXd2UVU1R1gwRnpjM1Z5WldSZlNVUmZRMEV4WDFOSVFUSTFOaTVqY213d09xQTRvRGFHTkdoMGRIQnpPaTh2ZDNkM0xtRnVaaTVsY3k5amNtd3ZRVTVHWDBGemMzVnlaV1JmU1VSZlEwRXhYMU5JUVRJMU5pNWpjbXd3Z2Z5Z2dmbWdnZmFHY1d4a1lYQTZMeTlzWkdGd0xtRnVaaTVsY3pvek9Ea3ZZMjQ5UVU1R1gwRnpjM1Z5WldSZlNVUmZRMEV4WDFOSVFUSTFOaTVqY213c2IzVTlRVTVHWDBGemMzVnlaV1JmU1VSZlEwRXhYMU5JUVRJMU5peHZkVDFCVGtaZlIyeHZZbUZzWDFKdmIzUmZRMEVzWkdNOVlXNW1wSUdBTUg0eEV6QVJCZ29Ka2lhSmsvSXNaQUVaRmdOaGJtWXhKakFrQmdOVkJBTU1IVUZPUmw5QmMzTjFjbVZrWDBsRVgwTkJNVjlUU0VFeU5UWXVZM0pzTVNJd0lBWURWUVFMREJsQlRrWmZRWE56ZFhKbFpGOUpSRjlEUVRGZlUwaEJNalUyTVJzd0dRWURWUVFMREJKQlRrWmZSMnh2WW1Gc1gxSnZiM1JmUTBFd0h3WURWUjBqQkJnd0ZvQVVOUzR0NWlrY3Y0dlpTbnllSytDVlI5SEdUejB3RXdZS0t3WUJCQUdCanh3S0NBUUZEQU5PU1VVd0hBWUpLd1lCQkFHQmp4d1RCQThNRFRJd01qQXlPQzAxTURNd016TXdLQVlLS3dZQkJBR0JqeHdUQVFRYURCZ3hOak00T0Mwek5qYzNNRE01TURrek1qSTBORE16TWpnd0V3WUtLd1lCQkFHQmp4d1VDQVFGREFOT1NVVXdTQVlJS3dZQkJRVUhBUU1FUERBNk1Bb0dDQ3NHQVFVRkJ3c0NNQWdHQmdRQWprWUJBVEFMQmdZRUFJNUdBUU1DQVE4d0ZRWUdCQUNPUmdFQ01Bc1RBMFZWVWdJQkF3SUJBVEFMQmdOVkhROEVCQU1DQmtBd0lRWURWUjBSQkJvd0dJRVdjMlZ5ZG1samFXOTBaV051YVdOdlFHRnVaaTVsY3pDQjZRWURWUjBnQklIaE1JSGVNSUhiQmcwckJnRUVBWUdQSEFNRUFRSUxNSUhKTUNrR0NDc0dBUVVGQndJQkZoMW9kSFJ3Y3pvdkwzZDNkeTVoYm1ZdVpYTXZaRzlqZFcxbGJuUnZjekNCbXdZSUt3WUJCUVVIQWdJd2dZNE1nWXRCYm5SbGN5QmtaU0JoWTJWd2RHRnlJR1Z6ZEdVZ1kyVnlkR2xtYVdOaFpHOHNJR052YlhCeWRXVmlaVG90UTI5dVpHbGphVzl1WlhNc0lHeHBiV2wwWVdOcGIyNWxjeUI1SUhWemIzTWdZWFYwYjNKcGVtRmtiM01nYzJWbnc3cHVJRU5RSUdFZ2JHRWdjWFZsSUhObElITnZiV1YwWlM0dFJYTjBZV1J2SUdSbElIWnBaMlZ1WTJsaE1JSElCZ05WSFFrRWdjQXdnYjB3RGdZRFZRUVJNUWNUQlRBd01EQXdNQklHQTFVRUZERUxFd2syTmpZek16SXlNVEV3SFFZRFZRUURNUllURkVaSlUwbERUeUJCUTFSSlZrOGdVRkpWUlVKQk1BOEdBMVVFS2pFSUV3WkdTVk5KUTA4d0ZnWURWUVFFTVE4VERVRkRWRWxXVHlCUVVsVkZRa0V3RWdZRFZRUUZNUXNUQ1RNNE9EWTBNVFU1V0RBVEJnTlZCQUl4REJNS01EY3ZNRFV2TWpBeE5EQVNCZ05WQkFreEN4TUpSRWxTUlVORFNVOU9NQklHQTFVRUVERUxFd2xFU1ZKRlEwTkpUMDR3RFFZSktvWklodmNOQVFFRkJRQURnZ0lCQUlEamtLRVdnRVRselZwYTNXN3UxRkVJSTRZWkdNdkpmMStCdmNwK0FWc1FBeVlsMDZhT0hHaFZMK1VjK3NrQU5kd3NOc25GZHpMQTIyRDNtYXB2SkRlTVRvRmdFbDZhUW51TU1CVXhzQXh0UG4ySUlKNG55MGR1VFlpSXlyN1NpT0J6WUljekJGT3JlWUc1WkcxVVFIVEc1Q0IvOTVhckw5b2FMYVRUOURhbEM4Sm9nQmd2aUlwY2FKcGNlMHZzZEdBSTM1SytIWUFPWWpVWTFFazJWTVdTN0JqTmxSSmhJS3FuQnpVL2ZRamt6RTNucWVnelhBWkRQdTQwbFFmalVsZ2lFczVENlI5RHI5NlJCejZyejRud3p4TmZpZTM3MXFvMG52T0h4MXlBcGh1UExmNVpIOHdtMncwaytpcENreHErWFFwVG4zNFRWcHdhVzlSbkl5WWRjUnVRcmlpL1N3UFdGYjN4dFB4WXo2Ums0VFZRS2F6bW9MUFREeVRwbVVuaE5QOFFyRVJ6b3RoTkg1UDNYbzIrS2h1VlNpRkU2ZENtZzFlNk11citNaVd3L2xpVHN3MzFCa2FyQ3Y1b3hiSitGUXNpeGt0TGkwK1NXQ3BrZFk2YWdZSDRXRU40OUJqU0tMUGp0QmhhT3E1MkpGZVJhaUFnRENpakw0LzYxWTByWVZjKzREM0piOUZNMWd3TFRUMzVzNU1XMUFVelUvVnFvdmdVUkZXaS9HNVQxZktiK0ZMaE5MeHdiOE9GNE5DNnhoNUxsdjFLeHQ1NGNtdFFUOXFvdzhZUWRzU3g1dDdIeWVtT24rVE1JY0lYd09rZnIzNXprWUx0QkRPL1l3cjZ2bzB6SDllT0FaTlN5RTE0WUFTOTRhRDhCakpKQnNVOFJoR2h1OW1TPC9jZXJ0PjxyZXFzPjxyZXEgaWQ9Ikg1cVpOU3M1N3EiPjxkb2MgZG9jaWQ9IjB3cm9sVXp1cU4iIGNvcD0ic2lnbiIgc2lnZnJtdD0iUERGIiBtZGFsZ289IlNIQTEiLz48L3JlcT48L3JlcXM-PC9ycXR0cmk-DQo=");


		final UrlHttpManager manager = UrlHttpManagerFactory.getInstalledManager();

		final byte[] result = manager.readUrl(urlBuilder.toString(), UrlHttpMethod.POST);

		System.out.println(new String(result));
	}
}
