package es.gob.afirma.crypto.handwritten;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/** Informaci&oacute;n necesaria para realizar una &uacute;nica firma biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@XmlRootElement(name = "singleBioSignData")
public final class SingleBioSignData {

	private final String id;

	/** Datos del firmante. */
	@XmlElement(name = "signerData")
	private final SignerInfoBean signerData;

	@XmlElement(name = "signHeader")
	private final String signHeader;

	@XmlElement(name = "signFooter")
	private final String signFooter;

	/** Lista de plantillas definidas para distintos tipos de tabletas.
	 * Dependiendo del tipo de tableta.*/
	@XmlElement(name = "tablet")
	private List<TabletTemplateData> tabletTemplates = new ArrayList<TabletTemplateData>(0);

	/** Rectangulo de firma en la tableta de captura. */
	@XmlElement(name = "signatureArea")
	private final Rectangle signatureArea;

	@XmlElement(name = "signatureRubricPositionOnPdf")
	private final Rectangle signatureRubricPositionOnPdf;

	@XmlElement(name = "signatureRubricPageOnPdf")
	private final int signatureRubricPageOnPdf;

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("Información de una firma biométrica: "); //$NON-NLS-1$
		sb.append("Plantillas definidas para distintos tipos de tabletas: "); //$NON-NLS-1$
		for (final TabletTemplateData ttd : this.tabletTemplates) {
			sb.append("    "); //$NON-NLS-1$
			sb.append(ttd.toString());
			sb.append('\n');
		}

		sb.append("Firma biometrica [id=" + this.id + "; signerData=" + this.signerData + "; htmlTemplate=" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				//(this.htmlTemplate == null ? "No" : "Si") + "; jpegTemplate=" + (this.htmlTemplate == null && this.jpegTemplate != null ? "Si" : "No") + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
				"; signatureArea=[" + this.signatureArea + "]; signatureRubricPositionOnPdf=[" + this.signatureRubricPositionOnPdf + "]]"+ //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				""); //$NON-NLS-1$
		return sb.toString();
	}

	/** Constructor de uso restringido a la serializaci&oacute;n JAXB. */
	@SuppressWarnings("unused")
	private SingleBioSignData() {
		this.signerData = null;
		this.signatureArea = null;
		this.signatureRubricPositionOnPdf = null;
		this.signatureRubricPageOnPdf = 1;
		this.id = UUID.randomUUID().toString();
		this.signHeader = null;
		this.signFooter = null;
	}

	SingleBioSignData(final SignerInfoBean signer,
			                  final List<TabletTemplateData> ttdList,
			                  final Rectangle signatureRectOnPad,
			                  final Rectangle signaturePositionOnPdf,
			                  final int signaturePageOnPdf,
			                  final String header,
			                  final String footer) {
		if (signer == null) {
			throw new IllegalArgumentException(
				"Los datos del firmante no pueden ser nulos" //$NON-NLS-1$
			);
		}
		this.signerData = signer;
		this.tabletTemplates = ttdList;
		this.signatureArea = signatureRectOnPad;
		this.signatureRubricPositionOnPdf = signaturePositionOnPdf;
		this.id = UUID.randomUUID().toString();
		this.signatureRubricPageOnPdf = signaturePageOnPdf;
		this.signHeader = header;
		this.signFooter = footer;
	}

	/** Obtiene los datos personales de un firmante.
	 * @return datos personales del firmante. */
	public SignerInfoBean getSignerData() {
		return this.signerData;
	}

	/** Obtiene la lista de plantillas que corresponden a cada tableta de firma definida.
	 * @return lista de plantillas que corresponden a cada tableta de firma definida. */
	public List<TabletTemplateData> getTabletTemplates() {
		return this.tabletTemplates;
	}

	Rectangle getSignatureArea() {
		return this.signatureArea;
	}

	/** M&eacute;todo para obtener las coordenadas donde se coloca una firma dentro de un PDF.
	 * @return las coordenadas de la posici&oacute;n de una firma. */
	public Rectangle getSignatureRubricPositionOnPdf() {
		return this.signatureRubricPositionOnPdf;
	}
	/** M&eacute;todo para obtener la p&aactue;gina donde se debe colocar la firma dentro del documetno PDF.
	 * @return n&uacute;mero de p&aacute;gina del documento.*/
	public int getSignatureRubricPageOnPdf() {
		return this.signatureRubricPageOnPdf;
	}

	String getId() {
		return this.id;
	}

	/** Obtiene la cabecera de la firma del firmante.
	 * @return Cabecera de la firma del firmante.*/
	public String getHeader() {
		return this.signHeader;
	}


	/** Obtiene el pie de firma de la firma del firmante.
	 * @return el pie de firma de la firma del firmante.*/
	public String getFooter() {
		return this.signFooter;
	}
}
