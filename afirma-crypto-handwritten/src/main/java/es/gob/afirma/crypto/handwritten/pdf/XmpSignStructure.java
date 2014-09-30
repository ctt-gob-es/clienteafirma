package es.gob.afirma.crypto.handwritten.pdf;

import es.gob.afirma.crypto.handwritten.SignerInfoBean;

/** Bean con los datos de una firma biom&eacute;trica que se insertan como XMP.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class XmpSignStructure {

	private final SignerInfoBean signer;
	private final byte[] bioData;
	private final String keyDn;

	/** Crea una estructura con los datos de una firma biom&eacute;trica que se insertan como XMP.
	 * @param signerInfo Datos del firmante.
	 * @param bioSignData Datos biom&eacute;tricos de la firma.
	 * @param decipherKeyDn DN X.500 de la clave de cifrado de los datos biom&eacute;tricos de la firma.
	 *                      Puede ser <code>null</code> si los datos no est&aacute;n cifrados. */
	public XmpSignStructure(final SignerInfoBean signerInfo,
			                final byte[] bioSignData,
			                final String decipherKeyDn) {
		if (signerInfo == null) {
			throw new NullPointerException(
				"Los datos del firmante no pueden ser nulos" //$NON-NLS-1$
			);
		}
		if (bioSignData == null)  {
			throw new NullPointerException(
				"Los datos biometricos no pueden ser nulos" //$NON-NLS-1$
			);
		}
		this.signer = signerInfo;
		this.bioData = bioSignData.clone();
		this.keyDn = decipherKeyDn;
	}

	SignerInfoBean getSigner() {
		return this.signer;
	}

	byte[] getBioData() {
		return this.bioData.clone();
	}

	String getKeyDn() {
		return this.keyDn;
	}

}
