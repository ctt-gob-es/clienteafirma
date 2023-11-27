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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.CertificateFilter;


/** Filtro de certificados de seud&oacute;nimo.
 * Este filtro muestra los certificados de seud&oacute;nimo ocultando los certificados
 * que tambi&eacute;n contenga el mismo almacen que hayan sido emitidos por el mismo emisor,
 * para el mismo prop&oacute;sito y con el mismo periodo de validez.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class PseudonymFilter extends CertificateFilter {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	public static final String VALUE_PSEUDONYM_AND_OTHERS = "andothers"; //$NON-NLS-1$

	public static final String VALUE_ONLY = "only"; //$NON-NLS-1$

	private final String value;

	public PseudonymFilter() {
		this.value = VALUE_PSEUDONYM_AND_OTHERS;
	}

	public PseudonymFilter(final String value) {
		this.value = value;
	}

	@Override
	public boolean matches(final X509Certificate cert) {
		return AOUtil.isPseudonymCert(cert);
	}

	@Override
	public String[] matches(final String[] aliases, final KeyStoreManager ksm) {

		String[] filteredAlias;
		if (VALUE_ONLY.equalsIgnoreCase(this.value)) {
			filteredAlias = getOnlyCertsWithPseudonymExtension(aliases, ksm);
		}
		else {
			filteredAlias = getCertsWithPseudonymOrOthers(aliases, ksm);
		}

		return filteredAlias;
	}

	/**
	 * Devuelve los alias de los certificados del almac&eacute;n que incluyen la extensi&oacute;n propia
	 * de los filtros de pseud&oacute;nimo.
	 * @param aliases Listado de alias del almac&eacute;n de entre los que filtrar.
	 * @param ksm Gestor del almac&eacute;n de claves.
	 * @return Listado filtrados de alias de certificados.
	 */
	private String[] getOnlyCertsWithPseudonymExtension(final String[] aliases, final KeyStoreManager ksm) {
		final List<String> pseudoByAlias = new ArrayList<>();
		for (final String alias : aliases) {
			final X509Certificate cert = ksm.getCertificate(alias);
			if (matches(cert)) {
				pseudoByAlias.add(alias);
			}
		}
		return pseudoByAlias.toArray(new String[0]);
	}

	/**
	 * Devuelve los alias de los certificados del almac&eacute;n que incluyen la extensi&oacute;n propia
	 * de los filtros de pseud&oacute;nimo o aquellos que no tienen un certificado de seud&oacute;nimo
	 * equivalente.
	 * @param aliases Listado de alias del almac&eacute;n de entre los que filtrar.
	 * @param ksm Gestor del almac&eacute;n de claves.
	 * @return Listado filtrados de alias de certificados.
	 */
	private String[] getCertsWithPseudonymOrOthers(final String[] aliases, final KeyStoreManager ksm) {
		final Map<String, X509Certificate> certsByAlias = new ConcurrentHashMap<>(aliases.length);
		final Map<String, X509Certificate> psudoByAlias = new ConcurrentHashMap<>();

		// Creamos dos listas, una con los certificados de seudonimo y otra con el resto
		for (final String alias : aliases) {
			final X509Certificate cert = ksm.getCertificate(alias);
			if (matches(cert)) {
				psudoByAlias.put(alias, cert);
			}
			else {
				certsByAlias.put(alias, cert);
			}
		}

		// Si no hay certificados de seudonimo, se mostraran todos los que hay
		if (psudoByAlias.isEmpty()) {
			return aliases;
		}

		// Omitimos del listado de certificados normales todos aquellos que tengan un
		// certificado de seudonimo asociado, ya que en ese caso solo se debera mostrar
		// el de seudonimo.
		for (final String pseudoByAlias : psudoByAlias.keySet()) {
			final X509Certificate pseuCert = psudoByAlias.get(pseudoByAlias);
			for (final String certAlias : certsByAlias.keySet()) {
				final X509Certificate normCert = certsByAlias.get(certAlias);
				if (isPseudonymFor(pseuCert, normCert)) {
					LOGGER.info(String.format("Se omite el certificado '%s' por haber un certificado de seudonimo equivalente", certAlias)); //$NON-NLS-1$
					certsByAlias.remove(certAlias);
				}
			}
		}

		// Unificamos los listados de certificados de seudonimo junto con el de
		// certiificados normales que no tienen uno de seudonimo asociado
		psudoByAlias.putAll(certsByAlias);
		return psudoByAlias.keySet().toArray(new String[0]);
	}

	/**
	 * Comprueba que un certificado sea el certificado de seud&oacute;nimo de otro comparando
	 * que compartan ciertas propiedades.
	 * @param pseudonymCert Certificado de seud&oacute;nimo.
	 * @param otherCert Certificado que comprobar.
	 * @return {@code true} si ambos certificados comparten los atributos necesarios como para considerar
	 * que uno es seud&oacute;nimo del otro, {@code false} en caso contrario.
	 */
	private static boolean isPseudonymFor(final X509Certificate pseudonymCert, final X509Certificate otherCert) {
		return pseudonymCert != null && otherCert != null &&
			// Que hayan sido expedidos por el mismo emisor
			pseudonymCert.getIssuerX500Principal().equals(otherCert.getIssuerX500Principal()) &&
			// Que ambos declaren los mismos usos para su clave
			Arrays.equals(pseudonymCert.getKeyUsage(), otherCert.getKeyUsage()) &&
			// Que no transcurriesen mas de 60 segundos entre la emision de ambos certificados
			Math.abs(pseudonymCert.getNotAfter().getTime() - otherCert.getNotAfter().getTime()) < 60000;
	}
}
