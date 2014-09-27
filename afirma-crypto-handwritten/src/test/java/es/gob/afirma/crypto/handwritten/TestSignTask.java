package es.gob.afirma.crypto.handwritten;

import java.net.URI;
import java.net.URL;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;
import es.gob.afirma.signers.tsp.pkcs7.TsaRequestExtension;

/** Pruebas de tareas de firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestSignTask {

	private static final String BASE64_XML_TASK = "PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9InllcyI/Pg0KPG5zMjpzaWduVGFzayB4bWxuczpuczI9ImVzLmdvYi5hZmlybWEuY3J5cHRvLmhhbmR3cml0dGVuIj4NCiAgICA8dHNhUGFyYW1zPg0KICAgICAgICA8dHNhUmVxdWlyZUNlcnQ+dHJ1ZTwvdHNhUmVxdWlyZUNlcnQ+DQogICAgICAgIDx0c2FQb2xpY3k+NC4zLjIuMTwvdHNhUG9saWN5Pg0KICAgICAgICA8dHNhVVJMPmh0dHA6Ly9rYWthLmthPC90c2FVUkw+DQogICAgICAgIDx0c2FVc3I+dXNlcjwvdHNhVXNyPg0KICAgICAgICA8dHNhUHdkPnBhc3N3b3JkPC90c2FQd2Q+DQogICAgICAgIDxleHRlbnNpb25zPg0KICAgICAgICAgICAgPGV4dGVuc2lvbj4NCiAgICAgICAgICAgICAgICA8b2lkPjEuMi4zLjQ8L29pZD4NCiAgICAgICAgICAgICAgICA8Y3JpdGljYWw+ZmFsc2U8L2NyaXRpY2FsPg0KICAgICAgICAgICAgICAgIDx2YWx1ZT4vL289PC92YWx1ZT4NCiAgICAgICAgICAgIDwvZXh0ZW5zaW9uPg0KICAgICAgICA8L2V4dGVuc2lvbnM+DQogICAgICAgIDx0c2FIYXNoQWxnb3JpdGhtPlNIQS01MTI8L3RzYUhhc2hBbGdvcml0aG0+DQogICAgICAgIDxzc2xQa2NzMTJGaWxlPkFBRUNBdz09PC9zc2xQa2NzMTJGaWxlPg0KICAgICAgICA8c3NsUGtjczEyRmlsZVBhc3N3b3JkPnAxMnBhc3N3b3JkPC9zc2xQa2NzMTJGaWxlUGFzc3dvcmQ+DQogICAgPC90c2FQYXJhbXM+DQogICAgPHJldHJpZXZlVXJsPmh0dHA6Ly93d3cuZ29vZ2xlLmNvbS88L3JldHJpZXZlVXJsPg0KICAgIDxzYXZlVXJsPmh0dHA6Ly93d3cuaWJtLmVzPC9zYXZlVXJsPg0KICAgIDxzYXZlVXJsUG9zdFBhcmFtPmRhdGE8L3NhdmVVcmxQb3N0UGFyYW0+DQogICAgPGNlcnQ+TUlJRHVUQ0NBcUdnQXdJQkFnSUt6WTFaOXRqWWdWQ0NqekFOQmdrcWhraUc5dzBCQVFVRkFEQWdNUjR3SEFZRFZRUURFeFZEYjIxdGRXNXBZMkYwYVc5dWN5QlRaWEoyWlhJd0hoY05NVFF3T1RBeU1EZzBOekl6V2hjTk1UVXdNekF4TURnME56SXpXakFoTVI4d0hRWURWUVFERXhaaGMzUnlhV1F1YVdSdllYUmxRR0YwYjNNdWJtVjBNSUlCSWpBTkJna3Foa2lHOXcwQkFRRUZBQU9DQVE4QU1JSUJDZ0tDQVFFQTQ0UHA4T0RlZWpzTDVDbUYzeUZLVXFsS1k3c3JpSVpzcko4eWNqdGc5NnMwTDRZek81Mnd0SWRqUkc0ZXBpV0Nmb0hlVGdIMVBsbUp3VHF0K3A5WE1YZm8wTG9qSHNCbGNXNVJnamFPNDJRU1hTZFZTYUlCY0ExeVBCbmd3T0pWK1ZlaWNGaldQbUdJVGF4QS9CVmF6SkJZUzRRYzNQSVpyWkw5OTBhTGZqK0lCaEpPTm5PTXEvV0wydXdVQ2hvM1EwWHBGeFFpbEtoaU1zekUxQkNpUGcxYmNKRC93MTRoL0V0YmlKQVo5VWV0NFAvZFZ0cGNIUFBPY3NNcy8rWXN3UVFuVUoxb3dLTG0yU3p2ajczSTlpMHF3YmhFVnNyWDNEMWFLYmN0ekdlZFgrYTZ3NElCa2NMQmpod1hSUHl3Si9ISnE2UkRhR0hSR3NMTk1GZE5hUUlEQVFBQm80SHpNSUh3TUJNR0ExVWRKUVFNTUFvR0NDc0dBUVVGQndNQ01DOEdBMVVkRGdRb0JDWjdPVGcyUkRKRVJUZ3ROVFZDTmkwMVJEUXdMVGhFUVRjdE1VWXdRa0ZEUXpjMk5VVkNmVEJWQmdOVkhTTUVUakJNZ0NOMVl5MWthWEpsWTNSdmNpNXBkQzF6YjJ4MWRHbHZibk11YlhsaGRHOXpMbTVsZEtFbGdpTjFZeTFrYVhKbFkzUnZjaTVwZEMxemIyeDFkR2x2Ym5NdWJYbGhkRzl6TG01bGREQXVCZ05WSFJJRUp6QWxnaU4xWXkxa2FYSmxZM1J2Y2k1cGRDMXpiMngxZEdsdmJuTXViWGxoZEc5ekxtNWxkREFoQmdOVkhSRUVHakFZZ1JaaGMzUnlhV1F1YVdSdllYUmxRR0YwYjNNdWJtVjBNQTBHQ1NxR1NJYjNEUUVCQlFVQUE0SUJBUUNQbUwwZGxicFNUbjJQQXpOcGlMR1A5dXVYOHp1MjhyM1lBYXlKdjVnR1BwazJpMGtKY0FydFZTS25MSXUzMmZIYk13U0FaYkJuaGpEZ2lOaXdYT1VBVzZmc04xRElEc3RkMFhJSXRYNkN1T2h6cFV0ZlRwcHlqdFhoWEVKMWluTFdlOUloWlZVc2xLT0taRWwyeTBKRC9YQ3RkcGVVeDBEZEo5WWxpa2o2aytCcVA2eWYzY3JPSUNoZitZSEM4cERwbVJCZE55SUdpRHBFRmg3Z3lJeXduMk45MmxjcERBY2FvME1ITUcrM1g0b1RzMmFvbFhIZ1pHSnE3UTBqdk5qeVNuZ1ZYdmlsZTlGR0QzMUNEcW9tZFAxYU56a2JBdm5LS3BJWlZqV2pHMnBkTTJnNzNwZUQ3Q05nb05LMTBmREpxU1lxQVBMZmlRNFJDUVkzRUQ4WTwvY2VydD4NCiAgICA8YmlvU2lnbnM+DQogICAgICAgIDxiaW9TaWduPg0KICAgICAgICAgICAgPGlkPmY3YjRiOGVkLTc2MzItNDcyZC04ZGNlLTg1OTE1ZjU3ZDIzNzwvaWQ+DQogICAgICAgICAgICA8c2lnbmVyRGF0YT4NCiAgICAgICAgICAgICAgICA8c2lnbmVyTmFtZT5Bc3RyaWQ8L3NpZ25lck5hbWU+DQogICAgICAgICAgICAgICAgPHNpZ25lclN1cm5hbWUxPklkb2F0ZTwvc2lnbmVyU3VybmFtZTE+DQogICAgICAgICAgICAgICAgPHNpZ25lclN1cm5hbWUyPkdpbDwvc2lnbmVyU3VybmFtZTI+DQogICAgICAgICAgICAgICAgPHNpZ25lcklkPjEyMzQ1Njc4Wjwvc2lnbmVySWQ+DQogICAgICAgICAgICA8L3NpZ25lckRhdGE+DQogICAgICAgICAgICA8aHRtbFRlbXBsYXRlPiZsdDtodG1sJmd0OyZsdDtib2R5Jmd0OyZsdDtoMSZndDtIT0xBJmx0Oy9oMSZndDsmbHQ7L2JvZHkmZ3Q7Jmx0Oy9odG1sJmd0OzwvaHRtbFRlbXBsYXRlPg0KICAgICAgICAgICAgPHNpZ25hdHVyZUFyZWE+DQogICAgICAgICAgICAgICAgPHg+MTA8L3g+DQogICAgICAgICAgICAgICAgPHk+MTA8L3k+DQogICAgICAgICAgICAgICAgPHdpZHRoPjEwMDwvd2lkdGg+DQogICAgICAgICAgICAgICAgPGhlaWdodD4xMDA8L2hlaWdodD4NCiAgICAgICAgICAgIDwvc2lnbmF0dXJlQXJlYT4NCiAgICAgICAgICAgIDxzaWduYXR1cmVSdWJyaWNQb3NpdGlvbk9uUGRmPg0KICAgICAgICAgICAgICAgIDx4PjUwPC94Pg0KICAgICAgICAgICAgICAgIDx5PjMwPC95Pg0KICAgICAgICAgICAgICAgIDx3aWR0aD4yMDA8L3dpZHRoPg0KICAgICAgICAgICAgICAgIDxoZWlnaHQ+NzU8L2hlaWdodD4NCiAgICAgICAgICAgIDwvc2lnbmF0dXJlUnVicmljUG9zaXRpb25PblBkZj4NCiAgICAgICAgICAgIDxzaWduYXR1cmVSdWJyaWNQYWdlT25QZGY+MTwvc2lnbmF0dXJlUnVicmljUGFnZU9uUGRmPg0KICAgICAgICA8L2Jpb1NpZ24+DQogICAgPC9iaW9TaWducz4NCiAgICA8Y29tcGxldGVXaXRoQ3JpcHRvU2lnbj50cnVlPC9jb21wbGV0ZVdpdGhDcmlwdG9TaWduPg0KICAgIDxjb21wbGV0ZUNyaXB0b1NpZ25FeHRyYVBhcmFtcz4NCiAgICAgICAgPGVudHJ5Pg0KICAgICAgICAgICAgPGtleT5jbGF2ZTwva2V5Pg0KICAgICAgICAgICAgPHZhbHVlPnZhbG9yPC92YWx1ZT4NCiAgICAgICAgPC9lbnRyeT4NCiAgICA8L2NvbXBsZXRlQ3JpcHRvU2lnbkV4dHJhUGFyYW1zPg0KPC9uczI6c2lnblRhc2s+"; //$NON-NLS-1$

	/** Prueba una deserializaci&oacute;n desde un XML convertido a Base64.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDeserialBase64() throws Exception {
		System.out.println(new String(Base64.decode(BASE64_XML_TASK)));
		System.out.println(
			SignTask.getInstance(BASE64_XML_TASK)
		);
	}

	/** Prueba una deserializaci&oacute;n desde un XML.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDeserialXml() throws Exception {
		final String xml = new String(AOUtil.getDataFromInputStream(TestSignTask.class.getResourceAsStream("/signTask.xml"))); //$NON-NLS-1$
		System.out.println(xml);
		System.out.println(
			SignTask.getInstance(xml)
		);
	}

	/** Prueba una serializaci&oacute;n.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	public void testSerial() throws Exception {
		final TsaParams tsaParams = new TsaParams(
				true,
				"4.3.2.1", //$NON-NLS-1$
				new URI("http://kaka.ka"), //$NON-NLS-1$
				"user", //$NON-NLS-1$
				"password", //$NON-NLS-1$
				new TsaRequestExtension[] { new TsaRequestExtension("1.2.3.4", false, new byte[] { (byte) 0xff, (byte) 0xfa }) }, //$NON-NLS-1$
				"SHA-512", //$NON-NLS-1$
				new byte[] { 0x00, 0x01, 0x02, 0x03 },
				"p12password" //$NON-NLS-1$
			);

			final SingleBioSignData sbd = new SingleBioSignData(
				new SignerInfoBean("Astrid", "Idoate", "Gil", "12345678Z"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				"<html><body><h1>HOLA</h1></body></html>", //$NON-NLS-1$
				null,
				new Rectangle(10, 10, 100, 100),
				new Rectangle(50, 30, 200, 75),
				1
			);
			final List<SingleBioSignData> signs = new ArrayList<SingleBioSignData>(1);
			signs.add(sbd);

			final Map<String, String> p = new ConcurrentHashMap<String, String>();
			p.put("clave", "valor"); //$NON-NLS-1$ //$NON-NLS-2$

			final SignTask st = new SignTask(
				tsaParams,
				new URL("http://www.google.com/"), //$NON-NLS-1$
				new URL("http://www.ibm.es"), //$NON-NLS-1$
				"data", //$NON-NLS-1$
				Base64.encode(
					((X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
						TestBioSigner.class.getResourceAsStream("/democert.cer") //$NON-NLS-1$
					)).getEncoded()
				),
				signs,
				true,
				p
			);

			final Marshaller m = JAXBContext.newInstance(SignTask.class).createMarshaller();
		    m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
		    m.marshal(st, System.out);

		    System.out.println();
		    System.out.println(st.toString());
	}

}
