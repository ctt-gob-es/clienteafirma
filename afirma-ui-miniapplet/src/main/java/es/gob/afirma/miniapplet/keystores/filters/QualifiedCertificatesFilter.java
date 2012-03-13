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

import java.math.BigInteger;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.filters.CertificateFilter;

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

	private static final String[] SUBJECT_SN_PREFIX = new String[] {
		"serialnumber=", //$NON-NLS-1$
		"SERIALNUMBER=", //$NON-NLS-1$
		"2.5.4.5=" //$NON-NLS-1$
	};

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
		return prepareSerialNumber(getCertificateSN(cert)).equalsIgnoreCase(this.serialNumber);
	}

	/** {@inheritDoc} */
	@Override
	public String[] matches(final String[] aliases, final AOKeyStoreManager ksm) {

		X509Certificate cert;
		final List<String> filteredCerts = new ArrayList<String>();
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
							filteredCerts.add(aliase);
						} else {
							filteredCerts.add(qualifiedCertAlias);
						}
					}
				}
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"Error en la verificacion del certificado '" + //$NON-NLS-1$
						cert.getSerialNumber() + "': " + e);  //$NON-NLS-1$
				e.printStackTrace();
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
	private static String searchQualifiedSignatureCertificate(final X509Certificate cert, final AOKeyStoreManager ksm, final String[] aliases) {
		X509Certificate cert2;


		//Logger.getLogger("es.gob.afirma").info(
		final StringBuilder buffer = new StringBuilder();
		buffer.append("El certificado al que corresponde el numero de serie no es un certificado "). //$NON-NLS-1$
				append("de firma, se mostrara su informacion ademas de la del resto de certificados "). //$NON-NLS-1$
				append("del almacen:\n"). //$NON-NLS-1$
				append("Certificado original:\n"). //$NON-NLS-1$
				append("\t- Numero de serie: ").append(cert.getSerialNumber()).append('\n'). //$NON-NLS-1$
				append("\t- Issuer: ").append(cert.getIssuerDN()).append('\n'). //$NON-NLS-1$
				append("\t- Fecha de caducidad: ").append(getExpiredDate(cert)).append('\n'); //$NON-NLS-1$
				if (cert.getKeyUsage() != null) {
					buffer.append("\t- KeyUsages:\n"). //$NON-NLS-1$
					append("\t\t+ digitalSignature: ").append(cert.getKeyUsage()[0]).append('\n'). //$NON-NLS-1$
					append("\t\t+ nonRepudiation: ").append(cert.getKeyUsage()[1]).append('\n'). //$NON-NLS-1$
					append("\t\t+ keyEncipherment: ").append(cert.getKeyUsage()[2]).append('\n'). //$NON-NLS-1$
					append("\t\t+ dataEncipherment: ").append(cert.getKeyUsage()[3]).append('\n'). //$NON-NLS-1$
					append("\t\t+ keyAgreement: ").append(cert.getKeyUsage()[4]).append('\n'). //$NON-NLS-1$
					append("\t\t+ keyCertSign: ").append(cert.getKeyUsage()[5]).append('\n'). //$NON-NLS-1$
					append("\t\t+ cRLSign: ").append(cert.getKeyUsage()[6]).append('\n'). //$NON-NLS-1$
					append("\t\t+ encipherOnly: ").append(cert.getKeyUsage()[7]).append('\n'). //$NON-NLS-1$
					append("\t\t+ decipherOnly: ").append(cert.getKeyUsage()[8]).append('\n'); //$NON-NLS-1$
				}
				else {
					buffer.append("\t- El certificado no tiene definidos KeyUsages\n"); //$NON-NLS-1$
				}
				buffer.append(" -----\n"); //$NON-NLS-1$

		for (final String aliase : aliases) {
			cert2 = ksm.getCertificate(aliase);
			if (!cert.getSerialNumber().equals(cert2.getSerialNumber())) {
				buffer.append("Certificado:\n"). //$NON-NLS-1$
				append("\t- Numero de serie: ").append(cert2.getSerialNumber()).append('\n'). //$NON-NLS-1$
				append("\t- Issuer: ").append(cert2.getIssuerDN()).append('\n'). //$NON-NLS-1$
				append("\t- Fecha de caducidad: ").append(getExpiredDate(cert2)).append('\n'); //$NON-NLS-1$
				if (cert2.getKeyUsage() != null) {
					buffer.append("\t- KeyUsages:\n"). //$NON-NLS-1$
					append("\t\t+ digitalSignature: ").append(cert2.getKeyUsage()[0]).append('\n'). //$NON-NLS-1$
					append("\t\t+ nonRepudiation: ").append(cert2.getKeyUsage()[1]).append('\n'). //$NON-NLS-1$
					append("\t\t+ keyEncipherment: ").append(cert2.getKeyUsage()[2]).append('\n'). //$NON-NLS-1$
					append("\t\t+ dataEncipherment: ").append(cert2.getKeyUsage()[3]).append('\n'). //$NON-NLS-1$
					append("\t\t+ keyAgreement: ").append(cert2.getKeyUsage()[4]).append('\n'). //$NON-NLS-1$
					append("\t\t+ keyCertSign: ").append(cert2.getKeyUsage()[5]).append('\n'). //$NON-NLS-1$
					append("\t\t+ cRLSign: ").append(cert2.getKeyUsage()[6]).append('\n'). //$NON-NLS-1$
					append("\t\t+ encipherOnly: ").append(cert2.getKeyUsage()[7]).append('\n'). //$NON-NLS-1$
					append("\t\t+ decipherOnly: ").append(cert2.getKeyUsage()[8]).append('\n'); //$NON-NLS-1$
				} else {
					buffer.append("\t- El certificado no tiene definidos KeyUsages\n"); //$NON-NLS-1$
				}
				buffer.append(" -----\n"); //$NON-NLS-1$
				final boolean sameIssuer = (cert.getIssuerDN() == null ?
						cert2.getIssuerDN() == null : cert.getIssuerDN().equals(cert2.getIssuerDN()));
				final boolean sameSubjectSN = (getSubjectSN(cert) == null ?
						getSubjectSN(cert2) == null : getSubjectSN(cert).equals(getSubjectSN(cert2)));
				final boolean sameExpiredData = (getExpiredDate(cert) == null ?
						getExpiredDate(cert2) == null : getExpiredDate(cert).equals(getExpiredDate(cert2)));
				if (QualifiedCertificatesFilter.isSignatureCert(cert2) && sameIssuer && sameSubjectSN && sameExpiredData) {
					buffer.append("Se ha elegido el certificado recien mostrado como pareja del original"); //$NON-NLS-1$
					Logger.getLogger("es.gob.afirma").info(buffer.toString()); //$NON-NLS-1$
					return aliase;
				}
			}
		}
		buffer.append("NO se ha elegido ningun certificado como pareja del original"); //$NON-NLS-1$
		Logger.getLogger("es.gob.afirma").info(buffer.toString()); //$NON-NLS-1$
		return null;
	}

	/** Comprueba si el certificado tiene tiene un <em>KeyUsage</em> de firma (nonRepudiation).
	 * @param cert Certificado.
	 * @return {@code true} si el certificado es de firma. */
	private static boolean isSignatureCert(final X509Certificate cert) {
		return cert.getKeyUsage() != null && cert.getKeyUsage()[1];
	}

	/** Recupera el n&uacute;mero de serie del titular de un certificado.
	 * Los ceros ('0') a la izquierda del n&uacute;mero de serie se eliminan durante el
	 * proceso. Si el certificado no tiene n&uacute;mero de serie, devolver&aacute;
	 * {@code null}.
	 * @param cert Certificado.
	 * @return N&uacute;mero de serie del titular. */
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
    	return cert.getSerialNumber() == null ? null : bigIntegerToHex(cert.getSerialNumber());
	}

	/** Convierte un opbjeto BigInteger a Hexadecimal.
	 * @param bi Entero que deseamos convertir.
	 * @return Hexadecimal. */
	private static String bigIntegerToHex(final BigInteger bi) {
		return AOUtil.hexify(bi.toByteArray(), ""); //$NON-NLS-1$
	}

	/** Prepara un n&uacute;mero de serie en hexadecimal para que tenga
	 * un formato concreto.
	 * @param serialNumber N&uacute;mero de serie en hexadecimal.
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
