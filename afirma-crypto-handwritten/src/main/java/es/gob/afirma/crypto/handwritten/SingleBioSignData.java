package es.gob.afirma.crypto.handwritten;

import java.util.UUID;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/** Informaci&oacute;n necesaria para realizar una &uacute;nica firma biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@XmlRootElement(name = "singleBioSignData")
final class SingleBioSignData {

	@XmlElement(name = "id")
	private String id;

	/** Datos del firmante. */
	private SignerInfoBean signerData;

	/** Plantilla HTML a mostrar en la tableta al firmar el firmante. */
	private String htmlTemplate;

	/** Imagen JPEG a mostrar en la tableta al firmar el firmante. */
	private byte[] jpegTemplate;

	/** Rectangulo de firma en la tableta de captura. */
	private Rectangle signatureArea;

	private Rectangle signatureRubricPositionOnPdf;

	public SingleBioSignData() {
		// Vacio para JAXB
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

	//TODO:Completar getters y setters

}
