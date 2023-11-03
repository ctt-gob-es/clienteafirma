package es.gob.afirma.standalone.signdetails;

import java.util.ArrayList;

public class PAdESSignDetails extends SignDetails {

	String signatureId;

	public PAdESSignDetails() {
		this.signers = new ArrayList<CertificateDetails>();
	}

	public String getSignatureId() {
		return this.signatureId;
	}

	public void setSignatureId(final String signatureId) {
		this.signatureId = signatureId;
	}
}
