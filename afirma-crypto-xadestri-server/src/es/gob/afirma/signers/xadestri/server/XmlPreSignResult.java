package es.gob.afirma.signers.xadestri.server;



/** Resultado de una prefirma XML.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XmlPreSignResult {

	private final String xmlSignBase64;
	private final String signedInfoBase64;

	XmlPreSignResult(final String xmlSign, final String signedInfo) {
		this.xmlSignBase64 = xmlSign;
		this.signedInfoBase64 = signedInfo;
	}

	/**
	 * Recupera el PKCS#1 de la firma.
	 * @return PKCS#1 de la firma en base 64.
	 */
	public String getXmlSignBase64() {
		return this.xmlSignBase64;
	}

	/**
	 * Recupera el SignedInfo de la firma.
	 * @return SignedInfo de la firma en base 64.
	 */
	public String getSignedInfoBase64() {
		return this.signedInfoBase64;
	}


}
