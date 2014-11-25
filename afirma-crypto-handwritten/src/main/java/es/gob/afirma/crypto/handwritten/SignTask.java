package es.gob.afirma.crypto.handwritten;

import java.io.ByteArrayInputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;

/** Tarea completa de firma manuscrita.
 * @author Astrid Idoate. */
@XmlRootElement(namespace = "es.gob.afirma.crypto.handwritten")
public final class SignTask {

	@XmlElement(name = "writableDirectory")
	private String writableDirectory;

	@XmlElement(name = "tsaParams")
	private SerializableTsaParams tsaParams;

	@XmlElement(name = "retrieveUrl")
	private URL retrieveUrl;

	@XmlElement(name = "saveUrl")
	private URL saveUrl;

	@XmlElement(name = "saveUrlPostParam")
	private final String saveUrlPostParam = "data"; //$NON-NLS-1$

	@XmlElement(name = "saveId")
	private String saveId;

	@XmlElement(name = "saveIdPostParam")
	private final String saveIdPostParam = "id"; //$NON-NLS-1$

	@XmlElement(name = "cert")
	private String base64Cert;

	@XmlElementWrapper(name = "bioSigns")
	@XmlElement(name = "bioSign")
	private final List<SingleBioSignData> bioSigns = new ArrayList<SingleBioSignData>(0);

	@XmlElement(name = "completeWithCriptoSign")
	private boolean completeWithCriptoSign;

	@XmlElement(name = "completeCriptoSignExtraParams")
	private Map<String, String> completeCriptoSignExtraParams;

	@XmlElement(name = "completeCriptoSignpkcs12")
	private Map<String, String> completeCriptoSignPkcs12;

	@XmlElement(name = "completeCriptoSignpkcs12Password")
	private Map<String, String> completeCriptoSignPkcs12Password;

	@XmlElement(name = "completeCriptoSignpkcs12Alias")
	private Map<String, String> completeCriptoSignPkcs12Alias;

	/** Construye una tarea de firma vac&iacute;a. */
	public SignTask() {
		// Vacio para la serializacion JAXB
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("Tarea de firmas biometricas:\n"); //$NON-NLS-1$
		if (this.tsaParams != null) {
			sb.append("  Parametros de sello de tiempo: "); //$NON-NLS-1$
			sb.append(this.tsaParams.toString());
			sb.append('\n');
		}

		sb.append("  URL de descarga de PDF: "); //$NON-NLS-1$
		sb.append(this.retrieveUrl);
		sb.append('\n');

		if (this.saveUrl != null) {
			sb.append("  URL de guardado de PDF: "); //$NON-NLS-1$
			sb.append(this.saveUrl);
			sb.append('\n');

			sb.append("  Parametro para el POST de la URL de guardado del PDF: "); //$NON-NLS-1$
			sb.append(this.saveUrlPostParam);
			sb.append('\n');
		}

		if (this.saveId != null) {
			sb.append("  ID del fichero guardado: "); //$NON-NLS-1$
			sb.append(this.saveId);
			sb.append("  Parametro para el POST de la URL de identificador del PDF firmado: "); //$NON-NLS-1$
			sb.append(this.saveIdPostParam);
			sb.append('\n');
		}

		sb.append("  Certificado de cifrado: "); //$NON-NLS-1$
		sb.append(this.base64Cert == null ? "No" : "Si"); //$NON-NLS-1$ //$NON-NLS-2$
		sb.append('\n');

		sb.append("  Firmas biometricas a realizar:\n"); //$NON-NLS-1$

		for (final SingleBioSignData sbsd : this.bioSigns) {
			sb.append("    "); //$NON-NLS-1$
			sb.append(sbsd.toString());
			sb.append('\n');
		}

		sb.append("  Completar el proceso con firma criptografica: "); //$NON-NLS-1$
		sb.append(this.completeWithCriptoSign ? "Si" : "No"); //$NON-NLS-1$ //$NON-NLS-2$
		sb.append('\n');

		if (this.completeWithCriptoSign) {
			sb.append("  Parametros de la firma criptografica: "); //$NON-NLS-1$
			sb.append(this.completeCriptoSignExtraParams);
		}

		return sb.toString();
	}

	/** Obtiene los datos para el sellado de tiempo.
	 * @return Datos para el sellado de tiempo. */
	public TsaParams getTsaParams() {
		return this.tsaParams.getTsaParams();
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
	 * @return certificado X.509 (en Base64) para el cifrado de la firma. */
	public String getCert() {
		return this.base64Cert;
	}

	/** Obtiene la lista de firmas biom&eacute;tricas a hacer.
	 * @return lista de firmas biom&eacute;tricas a hacer. */
	public List<SingleBioSignData> getBioSigns() {
		return this.bioSigns;
	}

	/** Obtiene <code>true</code> si el proceso debe finalizar con una firma criptogr&aacute;fica del operador,
	 * y en caso contrario se obtiene <code>false</code>.
	 * @return boolean indicando si el proceso debe finalizar con una firma criptogr&aacute;fica del operador. */
	public boolean isCompleteWithCriptoSign() {
		return this.completeWithCriptoSign;
	}

	/** Obtiene los par&aacute;metros adicionales de la firma final con certificado.
	 * @return Par&aacute;metros adicionales de la firma final con certificado. */
	public Map<String, String> getCompleteCriptoSignExtraParams() {
		return this.completeCriptoSignExtraParams;
	}

	/** Obtiene una tarea de firmas biom&eacute;tricas a partir de su XML de definici&oacute;n.
	 * @param xml XML de definici&oacute;n de la tarea de firmas biom&eacute;tricas.
	 *            Este XML puede prporcionarse como texto o codificado en Base64.
	 * @return Objeto de tarea de firmas biom&eacute;tricas.
	 * @throws JAXBException Si falla la deserializaci&oacute;n desde el XML. */
	public static SignTask getInstance(final String xml) throws JAXBException {
		if (xml == null) {
			throw new IllegalArgumentException("El XML de entrada no puede ser nulo"); //$NON-NLS-1$
		}

		byte[] rawXml;
		try {
			rawXml = new String(Base64.decode(xml), "UTF-8").trim().getBytes(); //$NON-NLS-1$
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").info("Los datos de entrada no estaban en Base64: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		rawXml = xml.getBytes();
		final Unmarshaller um = JAXBContext.newInstance(SignTask.class).createUnmarshaller();
		return (SignTask) um.unmarshal(new ByteArrayInputStream(rawXml));
	}

}
