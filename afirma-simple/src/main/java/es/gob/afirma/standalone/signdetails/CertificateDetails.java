package es.gob.afirma.standalone.signdetails;

import java.security.InvalidKeyException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.spongycastle.cms.CMSException;
import org.spongycastle.cms.CMSSignerDigestMismatchException;
import org.spongycastle.cms.DefaultCMSSignatureAlgorithmNameGenerator;
import org.spongycastle.cms.SignerInformation;
import org.spongycastle.cms.SignerInformationVerifier;
import org.spongycastle.jce.provider.BouncyCastleProvider;
import org.spongycastle.operator.ContentVerifierProvider;
import org.spongycastle.operator.DefaultSignatureAlgorithmIdentifierFinder;
import org.spongycastle.operator.OperatorCreationException;
import org.spongycastle.operator.bc.BcDigestCalculatorProvider;
import org.spongycastle.operator.jcajce.JcaContentVerifierProviderBuilder;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class CertificateDetails {

	private String name;
	private String issuerName;
	private String expirationDate;
	private Properties validityResult;
	private final List<CertificateDetails> subCertDetails;

	public CertificateDetails(final X509Certificate x509Cert, final SignerInformation signer) {
		this.name = AOUtil.getCN(x509Cert);
		this.issuerName = AOUtil.getCN(x509Cert.getIssuerX500Principal().getName());
		this.expirationDate = new SimpleDateFormat("dd-MM-yyyy").format(x509Cert.getNotAfter()).toString(); //$NON-NLS-1$
		this.validityResult = new Properties();
		this.subCertDetails = new ArrayList<CertificateDetails>();
		String validationMessage;
		try {
			x509Cert.checkValidity();
			
			if (signer != null) {
				final ContentVerifierProvider contentVerifierProvider =
						new JcaContentVerifierProviderBuilder().setProvider(new BouncyCastleProvider()).build(x509Cert);
	
				if (!signer.verify(
						new SignerInformationVerifier(
								new DefaultCMSSignatureAlgorithmNameGenerator(),
								new DefaultSignatureAlgorithmIdentifierFinder(),
								contentVerifierProvider,
								new BcDigestCalculatorProvider()))) {
					throw new CMSException("Firma no valida"); //$NON-NLS-1$
				}
			}
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.40"); //$NON-NLS-1$
		}
		catch (final CertificateExpiredException e) {
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.2"); //$NON-NLS-1$
		}
		catch (final CertificateNotYetValidException e) {
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.3"); //$NON-NLS-1$
		}
		catch (final CMSSignerDigestMismatchException e) {
			// Esta excepcion es controlada en la validacion general del documento como NO_MATCH_DATA
			validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.40");  //$NON-NLS-1$
		}
		catch (final CMSException e) {
			if (e.getCause() != null && e.getCause() instanceof OperatorCreationException
					&& e.getCause().getCause() != null && e.getCause().getCause() instanceof InvalidKeyException) {
				validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.5"); //$NON-NLS-1$
			} else {
				validationMessage = SimpleAfirmaMessages.getString("ValidationInfoDialog.4"); //$NON-NLS-1$
			}
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

}
