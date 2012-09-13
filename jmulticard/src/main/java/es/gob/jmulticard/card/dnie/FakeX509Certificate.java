/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros 
 * para la realizacion de procesos de autenticacion, firma electronica y validacion 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion 
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha 
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos 
 * e Impulso de la Administracion Electronica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * European Union Public License publicada por la Comision Europea, 
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 * 
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 * 
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.jmulticard.card.dnie;

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
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.security.auth.x500.X500Principal;

/** Certificado X.509 impostado construido a partir de la informaci&oacute:n contenida en un CDF PKCS#15.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class FakeX509Certificate extends X509Certificate {

	private static final long serialVersionUID = 1L;

	private final Principal subjectDn;
	private final Principal issuerDn;
	private final BigInteger serialNumber;

	private final boolean authCert;

	/** Crea un certificado X.509v3 impostado.
	 * @param subject Nombre LDAP del titular
	 * @param issuer Nombre LDAP del emisor
	 * @param serial N&uacute;mero de serie
	 * @param auth <code>true</code> si es un certificado de autenticaci&oacute;n, <code>false</code>
	 *             si lo es de firma digital */
	FakeX509Certificate(final String subject, final String issuer, final BigInteger serial, final boolean auth) {
	    super();
		this.subjectDn = new X500Principal(subject);
		this.issuerDn = new X500Principal(issuer);
		this.serialNumber = serial;
		this.authCert = auth;
	}

	/** {@inheritDoc} */
	public Set getCriticalExtensionOIDs() {
		final Set set = new HashSet(2);
		set.add("2.5.29.15"); //$NON-NLS-1$
		set.add("2.5.29.19"); //$NON-NLS-1$
		return set;
	}

	/** M&eacute;todo no implementado. */
	public byte[] getExtensionValue(final String e) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public Set getNonCriticalExtensionOIDs() {
		final Set set = new HashSet(8);
		set.add("2.5.29.14"); //$NON-NLS-1$
		set.add("2.5.29.9"); //$NON-NLS-1$
		set.add("1.3.6.1.5.5.7.1.1"); //$NON-NLS-1$
		set.add("1.3.6.1.5.5.7.1.2"); //$NON-NLS-1$
		set.add("2.16.724.1.2.2.4.1"); //$NON-NLS-1$
		set.add("2.5.29.32"); //$NON-NLS-1$
		set.add("1.3.6.1.5.5.7.1.3"); //$NON-NLS-1$
		set.add("2.5.29.35"); //$NON-NLS-1$
		return set;
	}

	/** {@inheritDoc} */
	public boolean hasUnsupportedCriticalExtension() {
		return false;
	}

	/** {@inheritDoc} */
	public void checkValidity() throws CertificateExpiredException, CertificateNotYetValidException {
		// Vacio, siempre es valido
	}

	/** {@inheritDoc} */
	public void checkValidity(final Date d) throws CertificateExpiredException, CertificateNotYetValidException {
		// Vacio, siempre es valido
	}

	/** {@inheritDoc} */
	public int getBasicConstraints() {
		return -1;
	}

	/** {@inheritDoc} */
	public Principal getIssuerDN() {
		return this.issuerDn;
	}

	/** M&eacute;todo no implementado. */
	public boolean[] getIssuerUniqueID() {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public boolean[] getKeyUsage() {
		if (this.authCert) {
			return new boolean[] { true, false, false, false, false, false, false, false, false };
		}
		return new boolean[] { false, true, false, false, false, false, false, false, false };
	}

	/** {@inheritDoc} */
	public Date getNotAfter() {
		return new Date();
	}

	/** {@inheritDoc} */
	public Date getNotBefore() {
		return new Date();
	}

	/** {@inheritDoc} */
	public BigInteger getSerialNumber() {
		return this.serialNumber;
	}

	/** {@inheritDoc} */
	public String getSigAlgName() {
		return "SHA1withRSA"; //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	public String getSigAlgOID() {
		return "1.2.840.113549.1.1.5"; //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	public byte[] getSigAlgParams() {
		return null;
	}

	/** M&eacute;todo no implementado. */
	public byte[] getSignature() {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public Principal getSubjectDN() {
		return this.subjectDn;
	}

	/** M&eacute;todo no implementado. */
	public boolean[] getSubjectUniqueID() {
		throw new UnsupportedOperationException();
	}

	/** M&eacute;todo no implementado. */
	public byte[] getTBSCertificate() throws CertificateEncodingException {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public int getVersion() {
		return 3;
	}

	/** M&eacute;todo no implementado. */
	public byte[] getEncoded() throws CertificateEncodingException {
		throw new UnsupportedOperationException();
	}

	/** M&eacute;todo no implementado. */
	public PublicKey getPublicKey() {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public String toString() {
		return "Certificado impostado: " + //$NON-NLS-1$
				"\n Emisor: " + this.issuerDn.toString() + //$NON-NLS-1$
				"\n Titular: " + this.subjectDn.toString() + //$NON-NLS-1$
				"\n Numero de serie: " + this.serialNumber.toString(); //$NON-NLS-1$
	}

	/** M&eacute;todo no implementado. */
	public void verify(final PublicKey puk) throws CertificateException, NoSuchAlgorithmException, InvalidKeyException, NoSuchProviderException, SignatureException {
		throw new UnsupportedOperationException();
	}

	/** M&eacute;todo no implementado. */
	public void verify(final PublicKey puk, final String s) throws CertificateException, NoSuchAlgorithmException, InvalidKeyException, NoSuchProviderException, SignatureException {
		throw new UnsupportedOperationException();
	}

}
