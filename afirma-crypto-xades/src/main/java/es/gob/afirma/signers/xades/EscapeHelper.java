package es.gob.afirma.signers.xades;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Principal;
import java.security.PublicKey;
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import javax.security.auth.x500.X500Principal;

final class EscapeHelper {

	private EscapeHelper() {
		// No permitimos instanciar
	}

	static List<X509Certificate> getEscapedCertificates(final List<X509Certificate> certs) {
		if (certs == null) {
			return new ArrayList<X509Certificate>(0);
		}
		final List<X509Certificate> ret = new ArrayList<X509Certificate>(certs.size());
		for (int i=0; i<certs.size(); i++) {
			ret.add(i, new EscapedCertificate(certs.get(i)));
		}
		return ret;
	}

	private static class EscapedCertificate extends X509Certificate {

		private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

		private final X509Certificate cert;

		private static Principal escapePrincipal(final Principal p) {
			return new X500Principal(escapeLdapName(p.getName()));
		}

		private static String escapeLdapName(final String in) {
			final LdapName name;
			try {
				name = new LdapName(in);
			}
			catch (final InvalidNameException e) {
				LOGGER.warning("No ha sido posible 'escapar' el nombre X.500 " + in + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				return in;
			}
			final StringBuilder sb = new StringBuilder();
			for (final Rdn rdn : name.getRdns()) {
				sb.append(rdn.getType());
				sb.append('=');
				sb.append(escapeCharacters(Rdn.escapeValue(rdn.getValue())));
				sb.append(", "); //$NON-NLS-1$
			}
			sb.append("<EOF>"); //$NON-NLS-1$
			return sb.toString().replace(", <EOF>", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}

		private static String escapeCharacters(final String str) {
			return str
				.replace("\u00e1", "\\C3\\A1") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00e9", "\\C3\\A9") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00ed", "\\C3\\AD") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00f3", "\\C3\\B3") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00fa", "\\C3\\BA") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00c1", "\\C3\\81") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00c9", "\\C3\\89") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00cd", "\\C3\\8D") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00d3", "\\C3\\93") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00da", "\\C3\\9A") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00f1", "\\C3\\B1") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00d1", "\\C3\\91") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00e7", "\\C3\\A7") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("\u00c7", "\\C3\\87") //$NON-NLS-1$ //$NON-NLS-2$
			;
		}

		EscapedCertificate(final X509Certificate c) {
			if (c==null) {
				throw new IllegalArgumentException("El certificado original no puede ser nulo"); //$NON-NLS-1$
			}
			this.cert = c;
		}

		@Override
		public Principal getSubjectDN() {
			return escapePrincipal(this.cert.getSubjectDN());
		}

		@Override
		public Principal getIssuerDN() {
			return escapePrincipal(this.cert.getIssuerDN());
		}

		@Override
		public Set<String> getCriticalExtensionOIDs() {
			return this.cert.getCriticalExtensionOIDs();
		}

		@Override
		public byte[] getExtensionValue(final String oid) {
			return this.cert.getExtensionValue(oid);
		}

		@Override
		public Set<String> getNonCriticalExtensionOIDs() {
			return this.cert.getNonCriticalExtensionOIDs();
		}

		@Override
		public boolean hasUnsupportedCriticalExtension() {
			return this.cert.hasUnsupportedCriticalExtension();
		}

		@Override
		public void checkValidity() throws CertificateExpiredException, CertificateNotYetValidException {
			this.cert.checkValidity();
		}

		@Override
		public void checkValidity(final Date date) throws CertificateExpiredException, CertificateNotYetValidException {
			this.cert.checkValidity(date);
		}

		@Override
		public int getBasicConstraints() {
			return this.cert.getBasicConstraints();
		}

		@Override
		public boolean[] getIssuerUniqueID() {
			return this.cert.getIssuerUniqueID();
		}

		@Override
		public boolean[] getKeyUsage() {
			return this.cert.getKeyUsage();
		}

		@Override
		public Date getNotAfter() {
			return this.cert.getNotAfter();
		}

		@Override
		public Date getNotBefore() {
			return this.cert.getNotBefore();
		}

		@Override
		public BigInteger getSerialNumber() {
			return this.cert.getSerialNumber();
		}

		@Override
		public String getSigAlgName() {
			return this.cert.getSigAlgName();
		}

		@Override
		public String getSigAlgOID() {
			return this.cert.getSigAlgOID();
		}

		@Override
		public byte[] getSigAlgParams() {
			return this.cert.getSigAlgParams();
		}

		@Override
		public byte[] getSignature() {
			return this.cert.getSignature();
		}

		@Override
		public boolean[] getSubjectUniqueID() {
			return this.cert.getSubjectUniqueID();
		}

		@Override
		public byte[] getTBSCertificate() throws CertificateEncodingException {
			return this.cert.getTBSCertificate();
		}

		@Override
		public int getVersion() {
			return this.cert.getVersion();
		}

		@Override
		public byte[] getEncoded() throws CertificateEncodingException {
			return this.cert.getEncoded();
		}

		@Override
		public PublicKey getPublicKey() {
			return this.cert.getPublicKey();
		}

		@Override
		public String toString() {
			return this.cert.toString();
		}

		@Override
		public void verify(final PublicKey key) throws CertificateException,
				                                        NoSuchAlgorithmException,
				                                        InvalidKeyException,
				                                        NoSuchProviderException,
				                                        SignatureException {
			this.cert.verify(key);
		}

		@Override
		public void verify(final PublicKey key, final String sigProvider) throws CertificateException,
		                                                                  NoSuchAlgorithmException,
		                                                                  InvalidKeyException,
		                                                                  NoSuchProviderException,
		                                                                  SignatureException {
			this.cert.verify(key, sigProvider);
		}

	}

}
