package es.gob.afirma.signfolder.server.proxy;

public class GenericRequest {

	private final byte[] certEncoded;
	
	public GenericRequest(final byte[] certEncoded) {
		this.certEncoded = certEncoded;
	}
	
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}
}
