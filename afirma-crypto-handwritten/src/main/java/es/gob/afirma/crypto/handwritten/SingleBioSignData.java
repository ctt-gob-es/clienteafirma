package es.gob.afirma.crypto.handwritten;

import java.util.UUID;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/** Informaci&oacute;n necesaria para realizar una &uacute;nica firma biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@XmlRootElement(name = "singleBioSignData")
final class SingleBioSignData {

	@XmlElement(name = "id")
	private final String id;

	/** Datos del firmante. */
	@XmlElement(name = "signerData")
	private final SignerInfoBean signerData;

	/** Plantilla HTML a mostrar en la tableta al firmar el firmante. */
	@XmlElement(name = "htmlTemplate")
	private final String htmlTemplate;

	/** Imagen JPEG a mostrar en la tableta al firmar el firmante. */
	@XmlElement(name = "jpegTemplate")
	private final byte[] jpegTemplate;

	/** Rectangulo de firma en la tableta de captura. */
	@XmlElement(name = "signatureArea")
	private final Rectangle signatureArea;

	@XmlElement(name = "signatureRubricPositionOnPdf")
	private final Rectangle signatureRubricPositionOnPdf;

	@Override
	public String toString() {
		return "Firma biometrica [id=" + this.id + "; signerData=" + this.signerData + "; htmlTemplate=" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			(this.htmlTemplate == null ? "No" : "Si") + "; jpegTemplate=" + (this.htmlTemplate == null && this.jpegTemplate != null ? "Si" : "No") + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
			"; signatureArea=[" + this.signatureArea + "]; signatureRubricPositionOnPdf=[" + this.signatureRubricPositionOnPdf + "]]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	/** Constructor de uso restringido a la serializaci&oacute;n JAXB. */
	@SuppressWarnings("unused")
	private SingleBioSignData() {
		this.signerData = null;
		this.htmlTemplate = null;
		this.jpegTemplate = null;
		this.signatureArea = null;
		this.signatureRubricPositionOnPdf = null;
		this.id = UUID.randomUUID().toString();
	}

	SingleBioSignData(final SignerInfoBean signer,
			                  final String template,
			                  final byte[] bgJpegImage,
			                  final Rectangle signatureRectOnPad,
			                  final Rectangle signaturePositionOnPdf) {
		if (signer == null) {
			throw new IllegalArgumentException(
				"Los datos del firmante no pueden ser nulos" //$NON-NLS-1$
			);
		}
		this.signerData = signer;
		this.htmlTemplate = template;
		this.jpegTemplate = bgJpegImage != null ? bgJpegImage.clone() : null;
		this.signatureArea = signatureRectOnPad;
		this.signatureRubricPositionOnPdf = signaturePositionOnPdf;
		this.id = UUID.randomUUID().toString();
	}

	SignerInfoBean getSignerData() {
		return this.signerData;
	}

	String getHtmlTemplate() {
		return this.htmlTemplate;
	}

	byte[] getJpegTemplate() {
		return this.jpegTemplate;
	}

	Rectangle getSignatureArea() {
		return this.signatureArea;
	}

	Rectangle getSignatureRubricPositionOnPdf() {
		return this.signatureRubricPositionOnPdf;
	}


}
