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
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import es.gob.afirma.core.keystores.KeyStoreManager;


/** Filtro de certificados de seud&oacute;nimo.
 * Este filtro muestra los certificados de seud&oacute;nimo ocultando los certificados
 * que tambi&eacute;n contenga el mismo almacen que hayan sido emitidos por el mismo emisor
 * para el mismo prop&oacute;sito y con el mismo periodo de validez.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class PseudonymFilter extends CertificateFilter {

	private static final String PSEUDONYM_POLICY_OID = "2.16.724.1.2.1.102.41"; //$NON-NLS-1$

	private static final CertificateFilter PSEUDONYM_POLICY_FILTER = new PolicyIdFilter(
		Collections.singletonList(
			PSEUDONYM_POLICY_OID
		)
	);

	@Override
	public boolean matches(final X509Certificate cert) {
		return PSEUDONYM_POLICY_FILTER.matches(cert);
	}

	@Override
	public String[] matches(final String[] aliases, final KeyStoreManager ksm) {
		final Map<String, X509Certificate> certsByAlias = new ConcurrentHashMap<>(aliases.length);
		final Map<String, X509Certificate> psudoByAlias = new ConcurrentHashMap<>();

		// Creamos dos listas, una con los certificados normales y otros con los de alias
		for (final String alias : aliases) {
			final X509Certificate cert = ksm.getCertificate(alias);
			if (matches(cert)) {
				psudoByAlias.put(alias, cert);
			}
			else {
				certsByAlias.put(alias, cert);
			}
		}

		if (psudoByAlias.isEmpty()) {
			return aliases;
		}

		final Set<String> pseuAliases = psudoByAlias.keySet();
		for (final String pseuAlias : pseuAliases) {
			final X509Certificate pseuCert = psudoByAlias.get(pseuAlias);
			final Set<String> certAliases = certsByAlias.keySet();
			for (final String certAlias : certAliases) {
				final X509Certificate normCert = certsByAlias.get(certAlias);
				if (isPseudonymFor(pseuCert, normCert)) {
					certsByAlias.remove(certAlias);
				}
			}
		}

		psudoByAlias.putAll(certsByAlias);
		return psudoByAlias.keySet().toArray(new String[0]);
	}

	private static boolean isPseudonymFor(final X509Certificate pseudonymCert, final X509Certificate otherCert) {
		if (pseudonymCert == null || otherCert == null) {
			return false;
		}
		if (
			pseudonymCert.getIssuerX500Principal().equals(otherCert.getIssuerX500Principal()) &&
			compareArrays(pseudonymCert.getKeyUsage(), otherCert.getKeyUsage())
		) {
			return true;
		}
		return false;
	}

	private static boolean compareArrays(final boolean[] array1, final boolean[] array2) {
        if (array1 != null && array2 != null){
          if (array1.length != array2.length) {
			return false;
          }
          for (int i = 0; i < array2.length; i++) {
		      if (array2[i] != array1[i]) {
		          return false;
		      }
          }
        }
        else {
          return false;
        }
        return true;
    }



}
