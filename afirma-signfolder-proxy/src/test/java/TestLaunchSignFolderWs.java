import javax.xml.ws.Endpoint;

import juntadeandalucia.cice.pfirma.mobile.v2.MobileServiceTests;


public class TestLaunchSignFolderWs {

	public static void main(final String[] args) {

		final String IP = "10.1.46.158";

		final String endpoint = "http://" + IP + ":8090/afirma/MobileService";

		Endpoint.publish(endpoint, new MobileServiceTests());

		System.out.println("Servicio publicado correctamente en: " + endpoint);
	}
}
