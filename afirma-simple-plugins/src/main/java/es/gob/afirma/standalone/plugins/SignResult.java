package es.gob.afirma.standalone.plugins;

import java.util.Properties;

public class SignResult {

	private byte[] signature;

	private byte[] certificate;

	private Properties extraData;

	public byte[] getSignature() {
		return this.signature;
	}

	public void setSignature(final byte[] signature) {
		this.signature = signature;
	}

	public byte[] getCertificate() {
		return this.certificate;
	}

	public void setCertificate(final byte[] certificate) {
		this.certificate = certificate;
	}

	public Properties getExtraData() {
		return this.extraData;
	}

	public void setDataFilename(final Properties extraData) {
		this.extraData = extraData;
	}
}
