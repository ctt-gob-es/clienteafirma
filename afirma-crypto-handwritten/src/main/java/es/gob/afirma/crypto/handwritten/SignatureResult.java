package es.gob.afirma.crypto.handwritten;

/** Resultado de una firma biom&eacute;trica.
 * @author Tom&oacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignatureResult {

	private final byte[] signatureIsoData;
	private final byte[] signatureRawData;
	private final byte[] signatureJpegImage;
	private final SignaturePadInfoBean pad;

	/** Crea el resultado de una firma biom&eacute;trica.
	 * @param data Datos biom&eacute;tricos de la firma en formato ISO 19794-7.
	 * @param rawData Datos biom&eacute;tricos de la firma en formato nativo de la tableta.
	 * @param jpegImage Imagen (en formato JPEG) de la r&uacute;brica de la firma.
	 * @param padInfo informaci&oacute;n de la tableta de captura de firmas usada. */
	public SignatureResult(final byte[] data,
			               final byte[] rawData,
			               final byte[] jpegImage,
			               final SignaturePadInfoBean padInfo) {
		if (jpegImage == null || data == null) {
			throw new IllegalArgumentException(
				"Es obligatorio proporcionar tanto imagen como datos de la firma" //$NON-NLS-1$
			);
		}
		this.signatureIsoData = data.clone();
		this.signatureJpegImage = jpegImage.clone();
		this.signatureRawData = rawData != null ? rawData.clone() : null;
		this.pad = padInfo;
	}

	/** Obtiene los metadatos de la tableta de captura usada para la firma.
	 * @return Metadatos de la tableta de captura usada para la firma. */
	public SignaturePadInfoBean getSignaturePadInfo() {
		return this.pad;
	}

	/** Obtiene los datos biom&eacute;tricos de la firma en formato ISO 19794-7.
	 * @return Datos biom&eacute;tricos de la firma. */
	public byte[] getSignatureData() {
		return this.signatureIsoData.clone();
	}

	/** Obtiene la imagen (en formato JPEG) de la r&uacute;brica de la firma.
	 * @return Imagen (en formato JPEG) de la r&uacute;brica de la firma. */
	public byte[] getSignatureJpegImage() {
		return this.signatureJpegImage;
	}

	/** Obtiene los datos biom&eacute;tricos de la firma en formato nativo de la tableta.
	 * @return Datos biom&eacute;tricos de la firma en formato nativo de la tableta. */
	public byte[] getSignatureRawData() {
		return this.signatureRawData != null ? this.signatureRawData.clone() : null;
	}

}
