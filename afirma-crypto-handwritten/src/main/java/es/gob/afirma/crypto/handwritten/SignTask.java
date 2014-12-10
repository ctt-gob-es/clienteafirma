package es.gob.afirma.crypto.handwritten;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.crypto.handwritten.pdf.Csv;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;

/** Tarea completa de firma manuscrita.
 * @author Astrid Idoate. */
@XmlRootElement(namespace = "es.gob.afirma.crypto.handwritten")
public final class SignTask {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@XmlElement(name = "CSV")
	private Csv csv;

	@XmlElement(name = "writableDirectory")
	private String writableDirectory;

	@XmlElement(name = "tsaParams")
	private SerializableTsaParams tsaParams;

	/** URL de recuperaci&oacute;n del PDF, puede ser por red con http:// o https:// o
	 * local con file:// */
	@XmlElement(name = "retrieveUrl")
	private URL retrieveUrl;

	@XmlElement(name = "saveUrl")
	private URL saveUrl;

	/** Directorio para guardar el PDF generado.
	 * Es independiente de <i>saveUrl</i>, si est&aacute;n ambos par&aacute;metros se
	 * salva tanto en directorio como en red. */
	@XmlElement(name = "saveDirectory")
	private String saveDirectory;

	/** Nombre con el que se almacena el documento firmado. */
	@XmlElement(name = "signedFileName")
	private String signedFileName;

	@XmlElement(name = "saveUrlPostParam")
	private String saveUrlPostParam = "data"; //$NON-NLS-1$

	/** M&eacute;todo JavaScript al que hay que llamar (mediante <code>JSObject</code>)
	 * para indicar la respuesta del servidor al anv&iacute;o del PDF firmado. */
	@XmlElement(name = "responseJSMethod")
	private String responseJSMethod;

	@XmlElement(name = "saveId")
	private String saveId;

	@XmlElement(name = "saveIdPostParam")
	private String saveIdPostParam = "id"; //$NON-NLS-1$

	@XmlElement(name = "cert")
	private String base64Cert;

	@XmlElement(name = "showTabletButtons")
	private boolean showTabletButtons;

	@XmlElementWrapper(name = "bioSigns")
	@XmlElement(name = "bioSign")
	private List<SingleBioSignData> bioSigns = new ArrayList<SingleBioSignData>(0);

	@XmlElement(name = "completeWithCriptoSign")
	private boolean completeWithCriptoSign;

	@XmlElement(name = "completeCriptoSignExtraParams")
	private Map<String, String> completeCriptoSignExtraParams;

	@XmlElement(name = "completeCriptoSignPkcs12")
	private String completeCriptoSignPkcs12;

	@XmlElement(name = "completeCriptoSignPkcs12Password")
	private String completeCriptoSignPkcs12Password;

	@XmlElement(name = "completeCriptoSignPkcs12Alias")
	private String completeCriptoSignPkcs12Alias;

	/** Construye una tarea de firma vac&iacute;a. */
	public SignTask() {
		// Vacio para la serializacion JAXB
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("Tarea de firmas biometricas:\n"); //$NON-NLS-1$

		sb.append("CSV : " + this.csv); //$NON-NLS-1$
		sb.append('\n');

		if (this.tsaParams != null) {
			sb.append("  Parametros de sello de tiempo: "); //$NON-NLS-1$
			sb.append(this.tsaParams.toString());
			sb.append('\n');
		}

		sb.append("Directorio para escritura de recursos: "); //$NON-NLS-1$
		sb.append(this.writableDirectory);
		sb.append('\n');

		sb.append("  URL de descarga de PDF: "); //$NON-NLS-1$
		sb.append(this.retrieveUrl);
		sb.append('\n');

		if(this.saveDirectory != null) {
			sb.append("Directorio para guardar el PDF generado:"); //$NON-NLS-1$
			sb.append(this.saveDirectory);
			sb.append('\n');
		}

		sb.append("Nombre con el que se almacena el fichero firmado: "); //$NON-NLS-1$
		sb.append(this.signedFileName);
		sb.append("\n"); //$NON-NLS-1$

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

		sb.append(" Mostrar lo botones de Aceptar//Repetir//Cancelar en la tableta de firma: "); //$NON-NLS-1$
		sb.append(this.showTabletButtons);
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

		sb.append("PKCS12 para firmar el documento: "); //$NON-NLS-1$
		sb.append(this.completeCriptoSignPkcs12);
		sb.append("\n"); //$NON-NLS-1$

		sb.append("Alias del PKCS12: " ); //$NON-NLS-1$
		sb.append(this.completeCriptoSignPkcs12Alias);
		sb.append("Constraseña del PKCS12: " ); //$NON-NLS-1$
		sb.append(this.completeCriptoSignPkcs12Password);
		return sb.toString();
	}

	/** Obtiene el CSV asociado al documento que se desea firmar.
	 * @return CSV asociado al documento que se desea firmar. */
	public Csv getCSV() {
		return this.csv;
	}

	/** Obtiene el directorio para escritura de recursos
	 * @return directorio para escritura de recursos. */
	public String getWrtDirectory() {
		if (this.writableDirectory == null ) {
			return null;
		}
		if(!this.writableDirectory.endsWith(File.separator)) {
			this.writableDirectory = this.writableDirectory + File.separator;
		}
		return this.writableDirectory;
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

	/** Obtiene el directorio para guardar el PDF generado.
	 * Es independiente de <i>saveUrl</i>, si est&aacute;n ambos par&aacute;metros se
	 * salva tanto en directorio como en red.
	 * @return directorio para guardar el PDF generado.*/
	public String getSaveDirectory() {
		if(this.saveDirectory == null) {
			return null;
		}
		if(!this.saveDirectory.endsWith(File.separator)) {
			this.saveDirectory = this.saveDirectory + File.separator;
		}
		return this.saveDirectory;
	}

	/** Obtiene el nombre con el que se almacena el documento firmado.
	 * @return nombre con el que se almacena el documento firmado. */
	public String getSignedFileName() {
		return this.signedFileName;
	}

	/** Obtiene el identificador del documento almacenado
	 * @return Identificador del documento firmado. */
	public String getSaveId() {
		return this.saveId;
	}

	/** Obtiene el par&aacute;metro del POST del servicio Web de identificaci&oacute;n del fichero almacenado.
	 * @return nombre del par&aacute;metro del POST del servicio Web de identificaci&oacute;n del fichero almacenado.
	 */
	public String getSaveIdPostParam() {
		return this.saveIdPostParam;
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

	/** Obtiene <code>true</code> si se debe mostrar la botonera con las opciones Aceptar/Repetir/Cancelar
	 * en la tableta de firma y <code>false</code> en caso contrario. No obstante, se mostrar&aacute;n siempre
	 * estas opciones en la interfaz del funcionario, en la pantalla del ordenador.
	 * @return <code>true</code> si se debe mostrar la botonera la tableta de firma.*/
	public boolean showTabletButtons() {
		return this.showTabletButtons;
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
	public Properties getCompleteCriptoSignExtraParams() {
		Properties p = new Properties();
		p.putAll(this.completeCriptoSignExtraParams);
		return p;
	}

	/**Obtiene el PKCS12 de firma.
	 * @return PKCS12 de firma. */
	public String getCompleteCriptoSignPkcs12() {
		return this.completeCriptoSignPkcs12;
	}

	/** Indica si se han definido los par&aacute;metros de firma PKCS12 (Pkcas12, Alias y Password).
	 * 	@return <code>true </code> si se han definido los par&aacute;metros de firma PKCS12 (Pkcas12, Alias y Password),
	 * 			<code>false</code> en caso contrario. */
	public boolean hasCompleteCriptoSignPkcs12Params() {
		if(this.completeCriptoSignPkcs12 != null
				&& this.completeCriptoSignPkcs12Alias != null
				&& this.completeCriptoSignPkcs12Password != null) {
			return true;
		}
		return false;
	}

	/** Obtiene la contraseña de firma del PKCS12.
	 * @return Contraseña de firma del PKCS12. */
	public String getCompleteCriptoSignPkcs12Password() {
		return this.completeCriptoSignPkcs12Password;
	}

	/** Obtiene el alias del PKCS12.
	 * @return Alias del PKCS12. */
	public String getCompleteCriptoSignPkcs12Alias() {
		return this.completeCriptoSignPkcs12Alias;
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
			rawXml = new String(Base64.decode(xml)).trim().getBytes();
		}
		catch(final Exception e) {
			LOGGER.info("Los datos de entrada no estaban en Base64: " + e); //$NON-NLS-1$
			// Usamos los datos sin decodificar

		}
		rawXml = xml.getBytes();
		LOGGER.info("XML de entrada:\n\n" + new String(rawXml)); //$NON-NLS-1$
		final Unmarshaller um = JAXBContext.newInstance(SignTask.class).createUnmarshaller();
		return (SignTask) um.unmarshal(new ByteArrayInputStream(rawXml));
	}

	/** Construye una tarea de firma.
	 * @param wrtDir Directorio para escritura de recursos.
	 * @param csvId CSV asociado al documento que se desea firmar.
	 * @param tsa Datos para el sellado de tiempo.
	 * @param retrieveUrlPdf URL para recuperar el PDF.
	 * @param saveUrlPdf URL para guardar el PDF (POST de un servicio Web).
	 * @param saveDir Directorio para guardar el PDF generado.
	 * @param fileName Nombre con el que almacena el documento firmado.
	 * @param saveUrlPostParam Nombre del par&aacute;metro del POST del servicio Web
	 *                         de guarado del PDF donde hay que pasar este.
	 * @param saveIdStoredDocument Identificador del fichero almacenado.
	 * @param saveIdPostParamStoredDocument Par&aacute;metro del POST del servicio Web de identificaci&oacute;n del fichero almacenado.
	 * @param certificate Certificado X.509 para el cifrado de la firma.
	 * @param tabletButtons Indica si se debe mostrar la botonera con las opciones Aceptar/Repetir/Cancelar en la tableta de firma.
	 * 						No obstante, se mostrar&aacute;n siempre estas opciones en la interfaz del funcionario, en la pantalla del ordenador.
	 * @param bioSignsList Lista de firmas biom&eacute;tricas a hacer.
	 * @param complete <code>true</code> si el proceso debe finalizar con una firma
	 *                 criptogr&aacute;fica del operador, <code>false</code> en caso
	 *                 contrario.
	 * @param completeSignExtraParams par&aacute;metros adicionales de la firma final
	 *                                con certificado.
	 * @param completePkcs12 PKCS12 de firma del documento.
	 * @param completePkcs12Password  Contraseña del PKCS12 de firma.
	 * @param completePkcs12Alias Alias del PKCS12 de firma.*/
	public SignTask(final String wrtDir,
					final Csv csvId,
			        final SerializableTsaParams tsa,
					final URL retrieveUrlPdf,
					final URL saveUrlPdf,
					final String saveDir,
					final String fileName,
					final String saveUrlPostParam,
					final String saveIdStoredDocument,
					final String saveIdPostParamStoredDocument,
					final String certificate,
					final boolean tabletButtons,
					final List<SingleBioSignData> bioSignsList,
					final boolean complete,
					final Map<String, String> completeSignExtraParams,
					final String completePkcs12,
					final String completePkcs12Password,
					final String completePkcs12Alias) {

		this.writableDirectory = wrtDir;
		this.csv = csvId;
		this.tsaParams = tsa;
		this.retrieveUrl = retrieveUrlPdf;
		this.saveUrl = saveUrlPdf;
		this.saveUrlPostParam = saveUrlPostParam;
		this.saveDirectory = saveDir;
		this.signedFileName = fileName;
		this.saveId = saveIdStoredDocument;
		this.saveIdPostParam = saveIdPostParamStoredDocument;
		this.base64Cert = certificate;
		this.showTabletButtons = tabletButtons;
		this.bioSigns = bioSignsList;
		this.completeWithCriptoSign = complete;
		this.completeCriptoSignExtraParams = completeSignExtraParams != null ?
			completeSignExtraParams :
				new ConcurrentHashMap<String, String>();
		this.completeCriptoSignPkcs12 = completePkcs12;
		this.completeCriptoSignPkcs12Password = completePkcs12Password;
		this.completeCriptoSignPkcs12Alias = completePkcs12Alias;

	}

}
