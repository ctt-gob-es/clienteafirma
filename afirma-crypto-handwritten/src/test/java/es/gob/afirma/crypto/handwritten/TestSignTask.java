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

	/** Prueba una deserializaci&oacute;n desde un XML convertido a Base64.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDeserialBase64() throws Exception {
		final String xml = new String(AOUtil.getDataFromInputStream(TestSignTask.class.getResourceAsStream("/signTask.xml"))); //$NON-NLS-1$
		final String b64Xml = Base64.encode(xml.getBytes());
		System.out.println(
			SignTask.getInstance(b64Xml)
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
	@Test
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
