package es.gob.afirma.standalone.signdetails;

import java.util.List;
import java.util.Properties;

import es.gob.afirma.signvalidation.SignValidity;

public abstract class SignDetails {

	protected String format;
	protected String algorithm;
	protected SignaturePolicy policy;
	protected Properties metadata;
	protected Boolean dataIncluded;
	protected String dataLocation;
	protected List<SignValidity> validityResult;
	protected List<DataObjectFormat> dataObjectFormats;
	protected List<SignDetails> signDetails;
	protected List<CertificateDetails> signers;

	public String getFormat() {
		return this.format;
	}
	public String getAlgorithm() {
		return this.algorithm;
	}
	public SignaturePolicy getPolicy() {
		return this.policy;
	}
	public Properties getMetadata() {
		return this.metadata;
	}
	public Boolean getDataIncluded() {
		return this.dataIncluded;
	}
	public String getDataLocation() {
		return this.dataLocation;
	}
	public List<SignValidity> getValidityResult() {
		return this.validityResult;
	}
	public List<SignDetails> getSignDetails() {
		return this.signDetails;
	}
	public List<DataObjectFormat> getDataObjectFormats() {
		return this.dataObjectFormats;
	}
	public void setFormat(final String format) {
		this.format = format;
	}
	public void setAlgorithm(final String algorithm) {
		this.algorithm = algorithm;
	}
	public void setPolicy(final SignaturePolicy policy) {
		this.policy = policy;
	}
	public void setMetadata(final Properties metadata) {
		this.metadata = metadata;
	}
	public void setDataIncluded(final Boolean dataIncluded) {
		this.dataIncluded = dataIncluded;
	}
	public void setDataLocation(final String dataLocation) {
		this.dataLocation = dataLocation;
	}
	public void setValidityResult(final List<SignValidity> validityResult) {
		this.validityResult = validityResult;
	}
	public void setDataObjectFormats(final List<DataObjectFormat> dataObjectFormats) {
		this.dataObjectFormats = dataObjectFormats;
	}
	public void setSignDetails(final List<SignDetails> signDetails) {
		this.signDetails = signDetails;
	}
	public List<CertificateDetails> getSigners() {
		return this.signers;
	}
	public void setSigners(final List<CertificateDetails> signers) {
		this.signers = signers;
	}

}
