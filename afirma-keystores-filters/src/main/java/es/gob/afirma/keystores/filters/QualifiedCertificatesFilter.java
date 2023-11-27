/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.keystores.CertificateFilter;

/** Filtro que selecciona los certificados con un n&uacute;mero de serie concreto,
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
 * @author Carlos Gamuci Mill&aacute;n. */
public final class QualifiedCertificatesFilter extends CertificateFilter {

	/** N&uacute;mero de serie en hexadecimal por el que debemos filtrar los certificados. */
	private final String serialNumber;

	/** Contruye el filtro a partir del n&uacute;mero de serie en hexadecimal del certificado
	 * que deseamos obtener.
	 * @param serialNumber N&uacute;mero de serie del certificado en hexadecimal. */
	public QualifiedCertificatesFilter(final String serialNumber) {
		this.serialNumber = prepareSerialNumber(serialNumber);
	}

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		final String certSN = getCertificateSN(cert);
		if (certSN != null) {
			return prepareSerialNumber(certSN).equalsIgnoreCase(this.serialNumber);
		}
		return false;
	}

	/** {@inheritDoc} */
	@Override
	public String[] matches(final String[] aliases, final KeyStoreManager ksm) {

		X509Certificate cert;
		final List<String> filteredCerts = new ArrayList<>();
		for (final String aliase : aliases) {
			cert = ksm.getCertificate(aliase);
			if (cert == null) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se pudo recuperar el certificado: " + aliase); //$NON-NLS-1$
				continue;
			}
			try {
				if (this.matches(cert)) {
					if (QualifiedCertificatesFilter.isSignatureCert(cert)) {
						filteredCerts.add(aliase);
					}
					else {
						final String qualifiedCertAlias = searchQualifiedSignatureCertificate(cert, ksm, aliases);

						if (qualifiedCertAlias == null) {
							// FIXME: En ese caso siempre se deberia agregar el certificado al que
							// corresponde el numero de serie, aunque no sea de firma. Por problemas
							// en algunos sistemas en los que al indicar el certificado de autenticacion
							// del DNIe se localiza este y no el de firma (ocurre al insertar una vez
							// mal la contrasena) no seleccionaremos ninguno para evitar que se utilice
							// el certificado de autenticacion para firma.
							if (!new AuthenticationDNIeFilter().matches(cert)) {
								filteredCerts.add(aliase);
							}
						} else {
							filteredCerts.add(qualifiedCertAlias);
						}
					}
				}
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"Error en la verificacion del certificado '" + //$NON-NLS-1$
						cert.getSerialNumber() + "': " + e);  //$NON-NLS-1$
			}
		}
		return filteredCerts.toArray(new String[filteredCerts.size()]);
	}

	/** Recupera el certificado qualificado de firma pareja del certificado indicado.
	 * @param cert Certificado del que buscar la pareja de firma.
	 * @param ksm Gestor del almac&eacute;n de certificados.
	 * @param aliases Alias de los certificados candidatos.
	 * @return Alias del certificado pareja del indicado o {@code null} si no se ha
	 * localizado una pareja satisfactoria. */
	private static String searchQualifiedSignatureCertificate(final X509Certificate cert, final KeyStoreManager ksm, final String[] aliases) {
		X509Certificate cert2;
		for (final String aliase : aliases) {
			cert2 = ksm.getCertificate(aliase);
			if (!cert.getSerialNumber().equals(cert2.getSerialNumber())) {
				final boolean sameIssuer = cert.getIssuerDN() == null ?
						cert2.getIssuerDN() == null :
							cert.getIssuerDN().equals(cert2.getIssuerDN());
				final boolean sameSubjectSN = FilterUtils.getSubjectSN(cert) == null ?
					FilterUtils.getSubjectSN(cert2) == null :
						FilterUtils.getSubjectSN(cert).equals(FilterUtils.getSubjectSN(cert2));
				final boolean sameExpiredData = getExpiredDate(cert) == null ?
						getExpiredDate(cert2) == null : getExpiredDate(cert).equals(getExpiredDate(cert2));
				if (QualifiedCertificatesFilter.isSignatureCert(cert2) && sameIssuer && sameSubjectSN && sameExpiredData) {
					Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
						"Se selecciona un certificado pareja de firma del certificado del numero de serie indicado" //$NON-NLS-1$
					);
					return aliase;
				}
			}
		}
		return null;
	}

	/** Comprueba si el certificado tiene tiene un <em>KeyUsage</em> de firma (nonRepudiation).
	 * @param cert Certificado.
	 * @return {@code true} si el certificado es de firma. */
	private static boolean isSignatureCert(final X509Certificate cert) {
		if (cert.getKeyUsage() == null) {
			return false;
		}

		final KeyUsagesPattern pattern = new KeyUsagesPattern(cert.getIssuerDN());
		return checkKeyUsages(cert.getKeyUsage(), pattern.getSignaturePattern());
	}

	/**
	 * Comprueba que el KeyUsage del certificado coincide con el patr&oacute;n indicado.
	 * @param certKeyUsages KeyUsages del certificado.
	 * @param pattern Patr&oacute;n que debe cumplir el certificado.
	 * @return {@code true} si el KeyUsage se corresponde con el patr&oacute;n,
	 * {@code false} en caso contrario.
	 */
	private static boolean checkKeyUsages(final boolean[] certKeyUsages, final Boolean[] pattern) {
		for (int i = 0; i < pattern.length; i++) {
			if (pattern[i] == null) {
				continue;
			}
			if (pattern[i].booleanValue() != certKeyUsages[i]) {
				return false;
			}
		}
		return true;

	}

	/** Recupera la fecha de expiraci&oacute;n del certificado en formato "yyyy-MM-dd".
	 * @param cert Certificado.
	 * @return Fecha de caducidad. */
	private static String getExpiredDate(final X509Certificate cert) {
		return new SimpleDateFormat("yyyy-MM-dd", Locale.US).format(cert.getNotAfter()); //$NON-NLS-1$
	}

	/** Recupera el n&uacute;mero de serie de un certificado en formato hexadecimal.
	 * Los ceros ('0') a la izquierda del n&uacute;mero de serie se eliminan durante el
	 * proceso. Si el certificado no tiene n&uacute;mero de serie, devolver&aacute;
	 * {@code null}.
	 * @param cert Certificado.
	 * @return Numero de serie en hexadecimal. */
	private static String getCertificateSN(final X509Certificate cert) {
    	return cert.getSerialNumber() == null ? null : FilterUtils.bigIntegerToHex(cert.getSerialNumber());
	}

	/** Prepara un n&uacute;mero de serie en hexadecimal para que tenga
	 * un formato concreto.
	 * @param sn N&uacute;mero de serie en hexadecimal.
	 * @return N&uacute;mero de serie preparado. */
	private static String prepareSerialNumber(final String sn) {
		final String preparedSn = sn.trim().replace(" ", "").replace("#", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		int n = 0;
		while(n < preparedSn.length()) {
			if (preparedSn.charAt(n) != '0') {
				break;
			}
			n++;
		}
		return preparedSn.substring(n);
	}
}
