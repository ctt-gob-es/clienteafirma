package es.gob.afirma.standalone.signdetails;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signvalidation.SignValidity;

public class SignDetails {

	private static final String POLICY_BUNDLE_NAME = "policy"; //$NON-NLS-1$
	public static final AdESPolicy POLICY_CADES_AGE_1_9;
	public static final AdESPolicy POLICY_CADES_AGE_1_8;
	public static final AdESPolicy POLICY_XADES_AGE_1_8;
	public static final AdESPolicy POLICY_XADES_AGE_1_9;
	public static final AdESPolicy POLICY_PADES_AGE_1_9;

	protected Date signingTime;
	protected String signProfile;
	protected String algorithm;
	protected SignaturePolicy policy;
	protected Map<String, String> metadata;
	protected Boolean dataIncluded;
	protected String dataLocation;
	protected List<SignValidity> validityResult;
	protected List<DataObjectFormat> dataObjectFormats;
	protected CertificateDetails signer;
	protected int certificationLevel;
	protected Boolean certificationSign;
	protected Boolean lastRevisionSign;

	public SignDetails() {
		this.metadata = new HashMap<>();
		this.dataObjectFormats = new ArrayList<>();
		this.validityResult = new ArrayList<>();
	}

	static {

		final ResourceBundle policyBundle = ResourceBundle.getBundle(POLICY_BUNDLE_NAME, Locale.getDefault());

		POLICY_XADES_AGE_1_9 = new AdESPolicy(policyBundle.getString("FirmaAGE19.policyIdentifier"), //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyIdentifierHash.XAdES"), //$NON-NLS-1$
				"SHA1", //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyQualifier") //$NON-NLS-1$
		);

		POLICY_XADES_AGE_1_8 = new AdESPolicy(policyBundle.getString("FirmaAGE18.policyIdentifier"), //$NON-NLS-1$
				policyBundle.getString("FirmaAGE18.policyIdentifierHash.XAdES"), //$NON-NLS-1$
				"SHA1", //$NON-NLS-1$
				policyBundle.getString("FirmaAGE18.policyQualifier") //$NON-NLS-1$
		);

		POLICY_PADES_AGE_1_9 = new AdESPolicy(policyBundle.getString("FirmaAGE19.policyIdentifier"), //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyIdentifierHash.PAdES"), //$NON-NLS-1$
				"SHA1", //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyQualifier") //$NON-NLS-1$
		);

		POLICY_CADES_AGE_1_9 = new AdESPolicy(policyBundle.getString("FirmaAGE19.policyIdentifier"), //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyIdentifierHash.CAdES"), //$NON-NLS-1$
				"SHA1", //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyQualifier") //$NON-NLS-1$
		);

		POLICY_CADES_AGE_1_8 = new AdESPolicy(policyBundle.getString("FirmaAGE18.policyIdentifier"), //$NON-NLS-1$
				policyBundle.getString("FirmaAGE18.policyIdentifierHash.CAdES"), //$NON-NLS-1$
				"SHA1", //$NON-NLS-1$
				policyBundle.getString("FirmaAGE18.policyQualifier") //$NON-NLS-1$
		);
	}

	public String getSignProfile() {
		return this.signProfile;
	}
	public String getAlgorithm() {
		return this.algorithm;
	}
	public SignaturePolicy getPolicy() {
		return this.policy;
	}
	public Map<String, String> getMetadata() {
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
	public List<DataObjectFormat> getDataObjectFormats() {
		return this.dataObjectFormats;
	}
	public void setSignProfile(final String signProfile) {
		this.signProfile = signProfile;
	}
	public void setAlgorithm(final String algorithm) {
		this.algorithm = algorithm;
	}
	public void setPolicy(final SignaturePolicy policy) {
		this.policy = policy;
	}
	public void setMetadata(final Map<String, String> metadata) {
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
	public CertificateDetails getSigner() {
		return this.signer;
	}
	public void setSigner(final CertificateDetails signer) {
		this.signer = signer;
	}
	public Date getSigningTime() {
		return this.signingTime;
	}
	public void setSigningTime(final Date signingTime) {
		this.signingTime = signingTime;
	}
	public int getCertificationLevel() {
		return this.certificationLevel;
	}
	public void setCertificationLevel(final int certificationLevel) {
		this.certificationLevel = certificationLevel;
	}
	public Boolean getCertificationSign() {
		return this.certificationSign;
	}
	public void setCertificationSign(final Boolean certificationSign) {
		this.certificationSign = certificationSign;
	}
	public Boolean getLastRevisionSign() {
		return this.lastRevisionSign;
	}
	public void setLastRevisionSign(final Boolean lastRevisionSign) {
		this.lastRevisionSign = lastRevisionSign;
	}
}
