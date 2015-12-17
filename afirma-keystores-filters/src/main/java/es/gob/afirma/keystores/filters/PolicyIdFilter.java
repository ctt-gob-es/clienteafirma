package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.x509.CertificatePolicies;
import org.bouncycastle.asn1.x509.PolicyInformation;

/** Filtro de certificados por identificador de pol&iacute;tica de certificaci&oacute;n.
 * Si un certificado tiene varias pol&iacute;ticas declaradas, todas deben estar dentro de la
 * lista de pol&iacute;ticas aceptadas.
 * @author Antoni Nadal Bennasar.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class PolicyIdFilter extends CertificateFilter {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	final List<String> allowedOids;

  /** Contruye un filtro de certificados por identificador de pol&iacute;tica de certificaci&oacute;n.
   * @param oids OID permitidos como pol&iacute;ticas de certificaci&oacute;n del certificado. */
  public PolicyIdFilter(final List<String> oids) {
	  if (oids == null || oids.isEmpty()) {
		  throw new IllegalArgumentException(
			  "La lista de OID permitidos no puede ser nula ni vacia" //$NON-NLS-1$
		  );
	  }
    this.allowedOids = oids;
  }

  @Override
  public boolean matches(final X509Certificate cert) {
	  if (cert == null) {
		  LOGGER.warning("El certificado proporcionado es nulo, se considera que no cumple el filtro"); //$NON-NLS-1$
		  return false;
	  }

	  final List<String> actualPolicies = getCertificatePolicyIds(cert);
	  if (actualPolicies == null || actualPolicies.isEmpty()) {
		  return false;
	  }

	  for (final String oid : actualPolicies) {
		  if (!this.allowedOids.contains(oid)) {
			  return false;
		  }
	  }
	  return true;
  }

  private static List<String> getCertificatePolicyIds(final X509Certificate cert) {
		final byte[] certificatePoliciesBytes = cert.getExtensionValue("2.5.29.32"); //$NON-NLS-1$
		if (certificatePoliciesBytes == null || certificatePoliciesBytes.length < 1) {
			return new ArrayList<String>(0);
		}
		final CertificatePolicies certificatePolicies = CertificatePolicies.getInstance(
			ASN1Sequence.getInstance(
				ASN1OctetString.getInstance(certificatePoliciesBytes).getOctets()
			)
		);
		final PolicyInformation[] pis = certificatePolicies.getPolicyInformation();
		final List<String> policyOids = new ArrayList<String>(pis.length);
		for (final PolicyInformation pi : pis) {
			policyOids.add(pi.getPolicyIdentifier().toString());
		}
		return policyOids;
  }

}
