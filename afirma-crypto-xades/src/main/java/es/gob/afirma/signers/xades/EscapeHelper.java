/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Principal;
import java.security.PublicKey;
import java.security.SignatureException;
import java.security.cert.Certificate;
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

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private EscapeHelper() {
		// No permitimos instanciar
	}

	static List<Certificate> getEscapedCertificates(final List<Certificate> certs) {
		if (certs == null) {
			return new ArrayList<>(0);
		}
		final List<Certificate> ret = new ArrayList<>(certs.size());
		for (int i=0; i<certs.size(); i++) {
			ret.add(i, new EscapedCertificate((X509Certificate) certs.get(i)));
		}
		return ret;
	}

	/** Devuelve un nombre X.500 con los caracteres especiales codificados de forma acorde a la
	 * <a href="http://www.ietf.org/rfc/rfc4514.txt">RFC 4514</a>.
	 * En particular, la RFC 4514 detalla la <i>String Representation of Distinguished Names</i> de la siguiente forma:
	 * <ol>
	 *  <li>El algoritmo descrito en el apartado 2 es <b>recomendado</b>, y que se pueden dar por v&aacute;lidos otros algoritmos.</li>
	 *  <li>Cualquier algoritmo <b>deber&aacute;</b> cumplir lo descrito en el apartado 3.</li>
	 * </ol>
	 * <p>Y siguiendo los puntos, el apartado 3 (<i>Parsing a String Back to a Distinguished Name</i>) define:</p>
	 * <pre>
	 *  distinguishedName = [ relativeDistinguishedName *( COMMA relativeDistinguishedName ) ]
     *  relativeDistinguishedName = attributeTypeAndValue *( PLUS attributeTypeAndValue )
     *  attributeTypeAndValue = attributeType EQUALS attributeValue
     *  attributeType = descr / numericoid
     *  attributeValue = string / hexstring
     *
     *  ;The following characters are to be escaped when they appear in the value to be encoded:
     *  ; ESC, one of &lt;escaped&gt;, leading SHARP or SPACE, trailing SPACE, and NULL.
     *  string =   [ ( leadchar / pair ) [ *( stringchar / pair ) ( trailchar / pair ) ] ]
     * </pre>
     * E indica el juego de caracteres soportados:
     * <pre>
     *  leadchar = LUTF1 / UTFMB
     *  LUTF1 = %x01-1F / %x21 / %x24-2A / %x2D-3A / %x3D / %x3F-5B / %x5D-7F
     *
     *  trailchar  = TUTF1 / UTFMB
     *  TUTF1 = %x01-1F / %x21 / %x23-2A / %x2D-3A / %x3D / %x3F-5B / %x5D-7F
     *
     *  stringchar = SUTF1 / UTFMB
     *  SUTF1 = %x01-21 / %x23-2A / %x2D-3A / %x3D / %x3F-5B / %x5D-7F
	 * </pre>
	 * Y tal y como se recoge en <a href="http://tools.ietf.org/html/rfc4512">http://tools.ietf.org/html/rfc4512</a> se puede observar:
	 * <pre>
	 *  UTFMB   = UTF2 / UTF3 / UTF4
     *  UTF2    = %xC2-DF UTF0
     *  UTF3    = %xE0 %xA0-BF UTF0 / %xE1-EC 2(UTF0) / %xED %x80-9F UTF0 / %xEE-EF 2(UTF0)
     *  UTF4    = %xF0 %x90-BF 2(UTF0) / %xF1-F3 3(UTF0) / %xF4 %x80-8F 2(UTF0)
     *  UTF0    = %x80-BF
     * </pre>
     * Dejando as&iacute; claro que caracteres requieren un tratamiento especial (<i>escape</i>):
     * <pre>
     *  pair = ESC ( ESC / special / hexpair )
     *  special = escaped / SPACE / SHARP / EQUALS
     *  escaped = DQUOTE / PLUS / COMMA / SEMI / LANGLE / RANGLE
     *  hexstring = SHARP 1*hexpair
     *  hexpair = HEX HEX
     * </pre>
     * Lo cual deja la posibilidad abierta de hacer el tratamiento RFC4515-3 de forma manual.<br>
     * No obstante, es posible una aproximaci&oacute;n m&aacute;s sencilla atendiendo al punto 2.4 (<i>Converting an AttributeValue
     * from ASN.1 to a String</i>), donde podemos leer:
     * <pre>
     *  [...] the value is converted first to a UTF-8-encoded Unicode string [...]
     *   - a space (' ' U+0020) or number sign ('#' U+0023) occurring at the beginning of the string;
     *   - a space (' ' U+0020) character occurring at the end of the string;
     * </pre>
     * Lo cual se ampl&iacute;a seg&uacute;n XMLDSig (<a href="http://www.w3.org/TR/xmldsig-core/">http://www.w3.org/TR/xmldsig-core/</a>)
     * en su punto 4.5.4.1 indicando que los espacios al inicio y al final se reemplazan por '&#92;20' en lugar de por '&#92; ' adem&aacute;s
     * de las siguientes reglas adicionales:
     * <ul>
     *  <li>Los RDN se separan con ',' (coma).</li>
     *  <li>
     *   Los caracteres ' ' (espacio), '"' (comilla doble), ',' (coma), '+' (mas), '=' (igual), '&gt;' (mayor que), '&lt;' (menor que),
     *   '&#35;' (almohadilla), ';' (punto y coma) y '&#92;' (barra invertida) se pueden proteger con '&#92;' (barra invertida).
     *  </li>
     *  <li>Otros caracteres se protegen con '&#92;xx'.</li>
     * </ul>
     * <p>
     *  Al incorporar Java un codificador Java acorde a la <a href="https://www.ietf.org/rfc/rfc2253.txt">RFC-2253</a> (una versi&oacute;n
     *  obsoleta de la RFC 4514), es posible considerar que este simple tratamiento podr&iacute;a ser aceptable sin m&aacute;s.
     *  Conviene no obstante notar que esta codificaci&oacute;n podr&iacute;a ser insuficiente en ciertos casos, ya que la RFC 4514, en
     *  su punto 2.4 (<i>Converting an AttributeValue from ASN.1 to a String</i>) indica:
     * </p>
     * <pre>
     *  If the AttributeType is of the dotted-decimal form, the
     *  AttributeValue is represented by an number sign ('#' U+0023)
     *  character followed by the hexadecimal encoding of each of the octets
     *  of the BER encoding of the X.500 AttributeValue.  This form is also
     *  used when the syntax of the AttributeValue does not have an LDAP-
     *  specific ([RFC4517], Section 3.1) string encoding defined for it, or
     *  the LDAP-specific string encoding is not restricted to UTF-8-encoded
     *  Unicode characters.  This form may also be used in other cases, such
     *  as when a reversible string representation is desired (see Section
     *  5.2).
     * </pre>
     * Es decir, que si se quiere una representaci&oacute;n reversible, es necesario usar <code>#hex</code>.
     * En el punto 5.2 (<i>Use of Distinguished Names in Security Applications</i>) se lee:
     * <pre>
     *  The transformations of an AttributeValue value from its X.501 form to
     *  an LDAP string representation are not always reversible back to the
     *  same BER (Basic Encoding Rules) or DER (Distinguished Encoding Rules)
     *  form.  An example of a situation that requires the DER form of a
     *  distinguished name is the verification of an X.509 certificate.
     *
     *  For example, a distinguished name consisting of one RDN with one AVA,
     *  in which the type is commonName and the value is of the TeletexString
     *  choice with the letters 'Sam', would be represented in LDAP as the
     *  string &lt;CN=Sam&gt;.  Another distinguished name in which the value is
     *  still 'Sam', but is of the PrintableString choice, would have the
     *  same representation &lt;CN=Sam&gt;.
     *
     *  Applications that require the reconstruction of the DER form of the
     *  value SHOULD NOT use the string representation of attribute syntaxes
     *  when converting a distinguished name to the LDAP format.  Instead,
     *  they SHOULD use the hexadecimal form prefixed by the number sign ('#'
     *  U+0023) as described in the first paragraph of Section 2.4.
     * </pre>
     * Y en el ap&eacute;ndice A:
     * <pre>
     *  It is recommended that human interfaces use the optional hex pair
     *  escaping mechanism (Section 2.3) to produce a string representation
     *  suitable for display to the user.  For example, an application can
     *  generate a DN string for display that escapes all non-printable
     *  characters appearing in the AttributeValue's string representation
     *  (as demonstrated in the final example of Section 4).
     * </pre>
     * Con lo que es posible concluir que ser&iacute;a necesario usar <i>#hex</i> para codificar el
     * <code>KeyName</code> de XAdES cuando contenga un DN X.500, cosa que <u>no</u> se est&aacute; haciendo.
     * <p>
     *  Por &uacute;ltimo, faltar&iacute;a tambi&eacute;n eliminar los espacios entre los RDN X.500 atendiendo a
     *  lo recogido en el Ap&eacute;ndice A:
     * </p>
     * <pre>
     *  DN strings can be quite long. It is often desirable to line-wrap
     *  overly long DN strings in presentations. Line wrapping should be
     *  done by inserting whitespace after the RDN separator character or, if
     *  necessary, after the AVA separator character. It should be noted to
     *  the user that the inserted whitespace is not part of the DN string
     *  and is to be removed before use in LDAP. For example, the following
     *  DN string is long:
     *   CN=Kurt D.Zeilenga, OU=Engineering, L=Redwood Shores, O=OpenLDAP Foundation, ST=California, C=US
     *  So it has been line-wrapped for readability. The extra whitespace is
     *  to be removed before the DN string is used in LDAP:
     *   CN=Kurt D.Zeilenga,OU=Engineering,L=Redwood Shores,O=OpenLDAP Foundation,ST=California,C=US
     *
     *  Inserting whitespace is not advised because it may not be obvious to
     *  the user which whitespace is part of the DN string and which
     *  whitespace was added for readability.
     * </pre>
	 * @param in Nombre X.500 de entrada.
	 * @return Nombre X.500 de entrada con sus caracteres especiales tratados seg&uacute;n la RFC-4514. */
	static String escapeLdapName(final String in) {
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
			sb.append(","); //$NON-NLS-1$
		}
		sb.append("<EOF>"); //$NON-NLS-1$
		return sb.toString().replace(",<EOF>", ""); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static String escapeCharacters(final String str) {
		return str
			.replace("\u00e1", "\\C3\\A1") // a minuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00e9", "\\C3\\A9") // e minuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00ed", "\\C3\\AD") // i minuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00f3", "\\C3\\B3") // o minuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00fa", "\\C3\\BA") // u minuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00c1", "\\C3\\81") // A mayuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00c9", "\\C3\\89") // E mayuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00cd", "\\C3\\8D") // I mayuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00d3", "\\C3\\93") // O mayuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00da", "\\C3\\9A") // U mayuscula con tilde aguda //$NON-NLS-1$ //$NON-NLS-2$

			.replace("\u00f1", "\\C3\\B1") // n minuscula con virgulilla //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00d1", "\\C3\\91") // N mayuscula con virgulilla //$NON-NLS-1$ //$NON-NLS-2$

			.replace("\u00e7", "\\C3\\A7") // c minuscula con cedilla //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00c7", "\\C3\\87") // C mayuscula con cedilla //$NON-NLS-1$ //$NON-NLS-2$

			.replace("\u00e0", "\\C3\\A0") // a minuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00e8", "\\C3\\A8") // e minuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00ec", "\\C3\\AC") // i minuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00f2", "\\C3\\B2") // o minuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00f9", "\\C3\\B9") // u minuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00c0", "\\C3\\80") // A mayuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00c8", "\\C3\\88") // E mayuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00cc", "\\C3\\8C") // I mayuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00d2", "\\C3\\92") // O mayuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00d9", "\\C3\\99") // U mayuscula con tilde grave //$NON-NLS-1$ //$NON-NLS-2$

			.replace("\u00fc", "\\C3\\BC") // u minuscula con dieresis //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00dc", "\\C3\\9C") // U mayuscula con dieresis //$NON-NLS-1$ //$NON-NLS-2$

			.replace("\u00bf", "\\C2\\BF") // Apertura de interrogacion //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00a1", "\\C2\\A1") // Apertura de exclamacion //$NON-NLS-1$ //$NON-NLS-2$

			.replace("\u00b5", "\\C2\\B5") // Simbolo de "micro" //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00ba", "\\C2\\BA") // Simbolo de ordinal masculino //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\u00aa", "\\C2\\AA") //  //$NON-NLS-1$ //$NON-NLS-2$
		;
	}

	private static class EscapedCertificate extends X509Certificate {

		private final X509Certificate cert;

		private static X500Principal escapePrincipal(final Principal p) {
			return new X500Principal(escapeLdapName(p.toString()));
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
		public X500Principal getSubjectX500Principal() {

			return escapePrincipal(this.cert.getSubjectX500Principal());
		}

		@Override
		public Principal getIssuerDN() {
			return escapePrincipal(this.cert.getIssuerDN());
		}

		@Override
		public X500Principal getIssuerX500Principal() {

			return escapePrincipal(this.cert.getIssuerX500Principal());
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
