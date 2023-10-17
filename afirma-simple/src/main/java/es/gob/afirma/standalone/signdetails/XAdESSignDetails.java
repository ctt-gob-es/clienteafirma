package es.gob.afirma.standalone.signdetails;

import java.util.ArrayList;

public class XAdESSignDetails extends SignDetails {

	String signatureId;

	public XAdESSignDetails() {
		this.signers = new ArrayList<CertificateDetails>();
	}

	public String getSignatureId() {
		return this.signatureId;
	}

	public void setSignatureId(final String signatureId) {
		this.signatureId = signatureId;
	}
}
