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
		return this.certificate != null ? this.certificate.clone() : null;
	}

	public void setCertificate(final byte[] certificate) {
		this.certificate = certificate != null ? certificate.clone() : null;
	}

	public Properties getExtraData() {
		return this.extraData != null ? (Properties) this.extraData.clone() : null;
	}

	public void setDataFilename(final Properties extraData) {
		this.extraData = extraData != null ? (Properties) extraData.clone() : null;
	}
}
