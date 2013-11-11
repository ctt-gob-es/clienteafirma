package es.gob.afirma.signers.xadestri.server;

import java.util.List;



/** Resultado de una prefirma XML.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XmlPreSignResult {

	private final byte[] xmlSign;
	private final List<byte[]> signedInfos;

	XmlPreSignResult(final byte[] xmlSign, final List<byte[]> signedInfos) {
		this.xmlSign = xmlSign;
		this.signedInfos = signedInfos;
	}

	/**
	 * Recupera el PKCS#1 de la firma.
	 * @return PKCS#1 de la firma.
	 */
	public byte[] getXmlSign() {
		return this.xmlSign;
	}

	/**
	 * Recupera los SignedInfos que hay que firmar.
	 * @return SignedInfos para firmar.
	 */
	public List<byte[]> getSignedInfos() {
		return this.signedInfos;
	}


}
