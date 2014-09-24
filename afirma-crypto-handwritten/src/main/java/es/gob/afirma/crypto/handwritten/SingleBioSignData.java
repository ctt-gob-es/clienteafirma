package es.gob.afirma.crypto.handwritten;

import java.util.ArrayList;
import java.util.List;

/** Informaci&oacute;n necesaria para realizar una &uacute;nica firma biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class SingleBioSignData {

	/** Datos del firmante. */
	private final SignerInfoBean signerData;

	/** Plantilla HTML a mostrar en la tableta al firmar el firmante. */
	final String htmlTemplate;

	/** Imagen JPEG a mostrar en la tableta al firmar el firmante. */
	final byte[] jpegTemplate;

	/** Rectangulo de firma en la tableta de captura. */
	final Rectangle signatureArea;

	private SingleBioSignData(final SignerInfoBean signer,
			                  final String template,
			                  final byte[] bgJpegImage,
			                  final Rectangle signatureRectOnPad) {
		if (signer == null) {
			throw new IllegalArgumentException(
				"Los datos del firmante no pueden ser nulos" //$NON-NLS-1$
			);
		}
		this.signerData = signer;
		this.htmlTemplate = template;
		this.jpegTemplate = bgJpegImage.clone();
		this.signatureArea = signatureRectOnPad;
	}

	static List<SingleBioSignData> getSignaturesData() {
		final List<SingleBioSignData> ret = new ArrayList<SingleBioSignData>(1);
		ret.add(
			new SingleBioSignData(
				new SignerInfoBean(
					"Tomas", //$NON-NLS-1$
					"Garcia-Meras", //$NON-NLS-1$
					"Capote", //$NON-NLS-1$
					"123454678Z" //$NON-NLS-1$
				),
				"<html><head></head><body><h1>HOLA</h1></body></html>", //$NON-NLS-1$
				null,
				new Rectangle(10, 10, 400, 200)
			)
		);

		return ret;

	}

	SignerInfoBean getSignerData() {
		return this.signerData;
	}

}
