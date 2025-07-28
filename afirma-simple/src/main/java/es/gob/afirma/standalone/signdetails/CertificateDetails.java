package es.gob.afirma.standalone.signdetails;

import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class CertificateDetails {

	private String name;
	private String issuerName;
	private String expirationDate;
	private Properties validityResult;
	private final List<CertificateDetails> subCertDetails;
	private boolean correctValidation;

	public CertificateDetails(final X509Certificate x509Cert) {
		this.name = AOUtil.getCN(x509Cert);
		this.issuerName = AOUtil.getCN(x509Cert.getIssuerX500Principal().getName());
		this.expirationDate = new SimpleDateFormat("dd-MM-yyyy").format(x509Cert.getNotAfter()).toString(); //$NON-NLS-1$
		this.validityResult = new Properties();
		this.subCertDetails = new ArrayList<CertificateDetails>();
		this.correctValidation = false;
		String validationMessage;
		try {
			x509Cert.checkValidity();
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.40"); //$NON-NLS-1$
			this.correctValidation = true;
		}
		catch (final CertificateExpiredException e) {
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.2"); //$NON-NLS-1$
		}
		catch (final CertificateNotYetValidException e) {
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.3"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.4"); //$NON-NLS-1$
		}
		this.validityResult.put("Validacion", validationMessage); //$NON-NLS-1$
	}

	public String getName() {
		return this.name;
	}
	public void setName(final String name) {
		this.name = name;
	}
	public String getIssuerName() {
		return this.issuerName;
	}
	public void setIssuerName(final String issuerName) {
		this.issuerName = issuerName;
	}
	public String getExpirationDate() {
		return this.expirationDate;
	}
	public void setExpirationDate(final String expirationDate) {
		this.expirationDate = expirationDate;
	}
	public Properties getValidityResult() {
		return this.validityResult;
	}
	public void setValidityResult(final Properties validityResult) {
		this.validityResult = validityResult;
	}
	public List<CertificateDetails> getSubCertDetails() {
		return this.subCertDetails;
	}
	public boolean isCorrectValidation() {
		return this.correctValidation;
	}

}
