/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet.keystores.filters;

import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.filters.CertificateFilter;

/**
 * Filtro que selecciona los certificados con un n&uacute;mero de serie concreto,
 * atendiendo que, si no es un certificado de firma se buscar&aacute; un certificado
 * de firma pareja de aquel del que se pas&oacute; el n&uacute;mero de serie. Si se
 * encontrase, se devolver&iacute;a el alias de este segundo certificado, si no, se
 * devolver&iacute;a el de aquel al que corresponde el n&uacute;mero de serie.<br>
 * Por ejemplo, si se pasase el n&uacute;mero de serie del certificado de
 * autenticaci&oacute;n del DNIe, se devolver&iacute;a el alias del certificado de firma
 * de ese DNIe.
 * Los criterios que se siguen para la identificaci&oacute;n de la pareja de un
 * certificado son:
 * <ul>
 * <li>Mismo emisor.</li>
 * <li>Mismo n&uacute;mero de serie del titular.</li>
 * <li>Misma fecha de caducidad.</li>
 * </ul>
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class QualifiedCertificatesFilter extends CertificateFilter {

	private static final String[] SUBJECT_SN_PREFIX = new String[] {
		"serialnumber=", //$NON-NLS-1$
		"SERIALNUMBER=", //$NON-NLS-1$
		"2.5.4.5=" //$NON-NLS-1$
	};

	private final String serialNumber;

	/**
	 * Contruye el filtro a partir del n&uacute;mero de serie en decimal del certificado
	 * que deseamos obtener.
	 * @param serialNumber N&uacute;mero de serie del certificado en decimal.
	 */
	public QualifiedCertificatesFilter(final String serialNumber) {
		this.serialNumber = serialNumber;
	}

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		return QualifiedCertificatesFilter.getCertificateSN(cert).equalsIgnoreCase(this.serialNumber);
	}

	/** {@inheritDoc} */
	@Override
	public String[] matches(final String[] aliases, final AOKeyStoreManager ksm) {

		final X509Certificate[] certs = new X509Certificate[aliases.length];
		for (int i = 0; i < aliases.length; i++) {
			certs[i] = ksm.getCertificate(aliases[i]);
		}

		X509Certificate cert;
		final List<String> filteredCerts = new ArrayList<String>();
		for (int i = 0; i < aliases.length; i++) {
			cert = ksm.getCertificate(aliases[i]);
			try {
				if (this.matches(cert)) {
					if (!this.isSignatureCert(cert)) {
						final String qualifiedCertAlias = searchQualifiedSignatureCertificate(cert, ksm, aliases);
						if (qualifiedCertAlias == null) {
							filteredCerts.add(aliases[i]);
						} else {
							filteredCerts.add(qualifiedCertAlias);
						}
					}
					else {
						filteredCerts.add(aliases[i]);
					}
				}
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"Error en la verificacion del certificado '" + //$NON-NLS-1$
						cert.getSerialNumber() + "': " + e);  //$NON-NLS-1$
			}
		}
		return filteredCerts.toArray(new String[filteredCerts.size()]);
	}

	/**
	 * Recupera el certificado qualificado de firma pareja del certificado indicado.
	 * @param cert Certificado del que buscar la pareja de firma.
	 * @param ksm Gestor del almac&eacute;n de certificados.
	 * @param aliases Alias de los certificados candidatos.
	 * @return Alias del certificado pareja del indicado o {@code null} si no se ha
	 * localizado una pareja satisfactoria.
	 */
	private String searchQualifiedSignatureCertificate(final X509Certificate cert, final AOKeyStoreManager ksm, final String[] aliases) {
		X509Certificate cert2;
		for (int j = 0; j < aliases.length; j++) {
			cert2 = ksm.getCertificate(aliases[j]);
			if (!cert.getSerialNumber().equals(cert2.getSerialNumber())) {
				final boolean sameIssuer = (cert.getIssuerDN() == null ?
						cert2.getIssuerDN() == null : cert.getIssuerDN().equals(cert2.getIssuerDN()));
				final boolean sameSubjectSN = (getSubjectSN(cert) == null ?
						getSubjectSN(cert2) == null : getSubjectSN(cert).equals(getSubjectSN(cert2)));
				final boolean sameExpiredData = (getExpiredDate(cert) == null ?
						getExpiredDate(cert2) == null : getExpiredDate(cert).equals(getExpiredDate(cert2)));
				if (this.isSignatureCert(cert2) && sameIssuer && sameSubjectSN && sameExpiredData) {
					return aliases[j];
				}
			}
		}
		return null;
	}

	/**
	 * Comprueba si el certificado tiene tiene un <em>KeyUsage</em> de firma (nonRepudiation).
	 * @param cert Certificado.
	 * @return {@code true} si el certificado es de firma.
	 */
	private boolean isSignatureCert(final X509Certificate cert) {
		return cert.getKeyUsage() != null && cert.getKeyUsage()[1];
	}

	/**
	 * Recupera el n&uacute;mero de serie del titular de un certificado.
	 * Los ceros ('0') a la izquierda del n&uacute;mero de serie se eliminan durante el
	 * proceso. Si el certificado no tiene n&uacute;mero de serie, devolver&aacute;
	 * {@code null}.
	 * @param cert Certificado.
	 * @return N&uacute;mero de serie del titular.
	 */
	private static String getSubjectSN(final X509Certificate cert) {
		final String principal = cert.getSubjectX500Principal().getName();
    	final List<Rdn> rdns;
		try {
			rdns = new LdapName(principal).getRdns();
		}
		catch (final InvalidNameException e) {
			return null;
		}
		if (rdns != null && !rdns.isEmpty()) {
			for (final Rdn rdn : rdns) {
				final String rdnText = rdn.toString();
				for (final String rdnPrefix : SUBJECT_SN_PREFIX) {
					if (rdnText.startsWith(rdnPrefix)) {
						return rdnText.substring(rdnPrefix.length()).replace("#", ""); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		}
		return null;
	}

	/**
	 * Recupera la fecha de expiraci&oacute;n del certificado en formato "yyyy-MM-dd".
	 * @param cert Certificado.
	 * @return Fecha de caducidad.
	 */
	private static String getExpiredDate(final X509Certificate cert) {
		return new SimpleDateFormat("yyyy-MM-dd").format(cert.getNotAfter()); //$NON-NLS-1$
	}

	/**
	 * Recupera el n&uacute;mero de serie de un certificado en formato decimal.
	 * Los ceros ('0') a la izquierda del n&uacute;mero de serie se eliminan durante el
	 * proceso. Si el certificado no tiene n&uacute;mero de serie, devolver&aacute;
	 * {@code null}.
	 * @param cert Certificado.
	 * @return Numero de serie en decimal.
	 */
	private static String getCertificateSN(final X509Certificate cert) {
    	return cert.getSerialNumber() == null ? null : cert.getSerialNumber().toString();
	}
}
