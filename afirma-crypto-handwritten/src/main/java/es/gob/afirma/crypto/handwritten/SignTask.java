package es.gob.afirma.crypto.handwritten;

import java.net.URI;
import java.net.URL;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;

/** Tarea completa de firma manuscrita.
 * @author Astrid Idoate. */
@XmlRootElement(namespace = "es.gob.afirma.crypto.handwritten")
public final class SignTask {

	private TsaParams tsaParams;

	@XmlElement(name = "retrieveUrl")
	private URL retrieveUrl;

	@XmlElement(name = "saveUrl")
	private URL saveUrl;

	@XmlElement(name = "saveUrlPostParam")
	private String saveUrlPostParam;

	@XmlElement(name = "cert")
	private String base64Cert;

	@XmlElementWrapper(name = "bioSigns")
	@XmlElement(name = "bioSign")
	private List<SingleBioSignData> bioSigns = new ArrayList<SingleBioSignData>(0);

	@XmlElement(name = "completeWithCriptoSign")
	private boolean completeWithCriptoSign;

	/** Construye una tarea de firma vac&iacute;a. */
	public SignTask() {
		// Vacio para la serializacion JAXB
	}

	/** Construye una tarea de firma.
	 * @param tsa Datos para el sellado de tiempo.
	 * @param retrieveUrlPdf URL para recuperar el PDF.
	 * @param saveUrlPdf URL para guardar el PDF (POST de un servicio Web).
	 * @param saveUrlPostParam Nombre del par&aacute;metro del POST del servicio Web
	 *                         de guarado del PDF donde hay que pasar este.
	 * @param certificate Certificado X.509 para el cifrado de la firma.
	 * @param bioSignsList Lista de firmas biom&eacute;tricas a hacer.
	 * @param complete <code>true</code> si el proceso debe finalizar con una firma
	 *                 criptogr&aacute;fica del operador, <code>false</code> en caso
	 *                 contrario. */
	public SignTask(final TsaParams tsa,
					final URL retrieveUrlPdf,
					final URL saveUrlPdf,
					final String saveUrlPostParam,
					final String certificate,
					final List<SingleBioSignData> bioSignsList,
					final boolean complete) {

		this.tsaParams = tsa;
		this.retrieveUrl = retrieveUrlPdf;
		this.saveUrl = saveUrlPdf;
		this.saveUrlPostParam = saveUrlPostParam;
		this.base64Cert = certificate;
		this.bioSigns = bioSignsList;
		this.completeWithCriptoSign = complete;

	}

	/** Obtiene los datos para el sellado de tiempo.
	 * @return Datos para el sellado de tiempo. */
	public TsaParams getTsaParams() {
		return this.tsaParams;
	}

	/** Obtiene la URL para recuperar el PDF.
	 * @return URL para recuperar el PDF. */
	public URL getRetrieveUrl() {
		return this.retrieveUrl;
	}

	/** Obtiene la URL para guardar el PDF (POST de un servicio Web).
	 * @return URL para guardar el PDF (POST de un servicio Web). */
	public URL getSaveUrl() {
		return this.saveUrl;
	}

	/** Obtiene el nombre del par&aacute;metro del POST del servicio Web de guarado del PDF donde hay que pasar este.
	 * @return nombre del par&aacute;metro del POST del servicio Web de guarado del PDF donde hay que pasar este. */
	public String getSaveUrlPostParam() {
		return this.saveUrlPostParam;
	}

	/** Obtiene el certificado X.509 (en Base64) para el cifrado de la firma.
	 * @return certificado X.509 (en Base64) para el cifrado de la firma.
	 */
	public String getCert() {
		return this.base64Cert;
	}

	/** Obtiene la lista de firmas biom&eacute;tricas a hacer.
	 * @return lista de firmas biom&eacute;tricas a hacer.
	 */
	public List<SingleBioSignData> getBioSigns() {
		return this.bioSigns;
	}

	/** Obtiene <code>true</code> si el proceso debe finalizar con una firma criptogr&aacute;fica del operador,
	 * y en caso contrario se obtiene <code>false</code>.
	 * @return boolean indicando si el proceso debe finalizar con una firma criptogr&aacute;fica del operador.
	 */
	public boolean isCompleteWithCriptoSign() {
		return this.completeWithCriptoSign;
	}

	public static void main(final String[] args) throws Exception {

		SingleBioSignData sbd = new SingleBioSignData(
			new SignerInfoBean("Astrid", "Idoate", "Gil", "12345678Z"),
			"<html><body><h1>HOLA</h1></body></html>",
			null,
			new Rectangle(10, 10, 100, 100),
			new Rectangle(50, 30, 200, 75)
		);
		List<SingleBioSignData> signs = new ArrayList<SingleBioSignData>(1);
		signs.add(sbd);


		SignTask st = new SignTask(
			new TsaParams(
				true,
				null,
				new URI("http://kaka.ka"),
				null,
				null,
				null,
				null,
				null,
				null
			),
			new URL("http://www.google.com/"),
			new URL("http://www.ibm.es"),
			"data",
			Base64.encode(
				((X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
					TestBioSigner.class.getResourceAsStream("/democert.cer") //$NON-NLS-1$
				)).getEncoded()
			),
			signs,
			false
		);

		JAXBContext context = JAXBContext.newInstance(SignTask.class);
		Marshaller m = context.createMarshaller();
	    m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
	    m.marshal(st, System.out);
	}

}
