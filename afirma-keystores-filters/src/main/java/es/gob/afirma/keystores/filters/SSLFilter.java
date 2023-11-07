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
import java.util.logging.Logger;

import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.keystores.CertificateFilter;

/** Filtro que selecciona los certificados con un n&uacute;mero de serie concreto.
 * Un filtrado de este tipo devuelve com&uacute;nmente un &uacute;nico certificado.
 * Si se indica el n&uacute;mero de serie del certificado del DNIe, se devuelve el
 * certificado de firma.
 * @author Carlos Gamuci Mill&aacute;n. */
public final class SSLFilter extends CertificateFilter {

	private final String serialNumber;
	private final AuthenticationDNIeFilter authenticationDnieCertFilter;
	private final SignatureDNIeFilter signatureDnieCertFilter;

	/** Contruye el filtro a partir del n&uacute;mero de serie en hexadecimal del certificado
	 * que deseamos obtener.
	 * @param serialNumber N&uacute;mero de serie del certificado en hexadecimal. */
	public SSLFilter(final String serialNumber) {
		this.serialNumber = SSLFilter.prepareSerialNumber(serialNumber);
		this.authenticationDnieCertFilter = new AuthenticationDNIeFilter();
		this.signatureDnieCertFilter = new SignatureDNIeFilter();
	}

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		return SSLFilter.prepareSerialNumber(SSLFilter.getCertificateSN(cert)).equalsIgnoreCase(this.serialNumber);
	}

	/** {@inheritDoc} */
	@Override
	public String[] matches(final String[] aliases, final KeyStoreManager ksm) {

		final X509Certificate[] certs = new X509Certificate[aliases.length];
		for (int i = 0; i < aliases.length; i++) {
			certs[i] = ksm.getCertificate(aliases[i]);
		}

		X509Certificate cert;
		final List<String> filteredCerts = new ArrayList<>();
		for (int i = 0; i < aliases.length; i++) {
			cert = ksm.getCertificate(aliases[i]);
			try {
				if (this.matches(cert)) {
					if (isAuthenticationDnieCert(cert)) {
						final String alias = getAssociatedCertAlias(ksm, cert, aliases, i);
						if (alias != null) {
							filteredCerts.add(alias);
						}
					}
					else {
						filteredCerts.add(aliases[i]);
					}
				}
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"Error en la verificacion del certificado '" + //$NON-NLS-1$
						cert.getSerialNumber() + "': " + e  //$NON-NLS-1$
				);
			}
		}
		return filteredCerts.toArray(new String[filteredCerts.size()]);
	}

	private boolean isAuthenticationDnieCert(final X509Certificate cert) {
		return this.authenticationDnieCertFilter.matches(cert);
	}

	private boolean isSignatureDnieCert(final X509Certificate cert) {
		return this.signatureDnieCertFilter.matches(cert);
	}

	/** Obtiene el certificado de firma asociado a un certificado de
	 * autenticaci&oacute;n dado.
	 * @param ksm Gestor del almacen en donde se encuentran los certificados.
	 * @param cert Certificado de autenticaci&oacute;n del que encontrar el asociado.
	 * @param aliases Alias de los certificados del almac&eacute;n.
	 * @param pos Posici&oacute;n del listado de alias en el que se encuentra
	 *            el alias del certificado del que se busca pareja.
	 * @return Alias del certificado asociado o {@code null} si no se encuentra. */
	private String getAssociatedCertAlias(final KeyStoreManager ksm,
			                              final X509Certificate cert,
			                              final String[] aliases,
			                              final int pos) {
		X509Certificate tempCert;
		for (int j = 0; j < aliases.length; j++) {
			if (pos != j) {
				tempCert = ksm.getCertificate(aliases[j]);
				if (isSignatureDnieCert(tempCert) && FilterUtils.getSubjectSN(tempCert) != null &&
					FilterUtils.getSubjectSN(tempCert).equalsIgnoreCase(FilterUtils.getSubjectSN(cert)) &&
						SSLFilter.getExpiredDate(tempCert).equals(SSLFilter.getExpiredDate(cert))) {
							return aliases[j];
				}
			}
		}
		return null;
	}

	/** Recupera la fecha de expiraci&oacute;n del certificado en formato "yyyy-MM-dd".
	 * @param cert Certificado.
	 * @return Fecha de caducidad. */
	private static String getExpiredDate(final X509Certificate cert) {
		return new SimpleDateFormat("yyyy-MM-dd").format(cert.getNotAfter()); //$NON-NLS-1$
	}

	/** Recupera el n&uacute;mero de serie de un certificado en formato hexadecimal.
	 * Los ceros ('0') a la izquierda del n&uacute;mero de serie se eliminan durante el
	 * proceso. Si el certificado no tiene n&uacute;mero de serie, devolver&aacute;
	 * {@code null}.
	 * @param cert Certificado.
	 * @return Numero de serie en hexadecimal. */
	private static String getCertificateSN(final X509Certificate cert) {
    	return FilterUtils.bigIntegerToHex(cert.getSerialNumber());
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
