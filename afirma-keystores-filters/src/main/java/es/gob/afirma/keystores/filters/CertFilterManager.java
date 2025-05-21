/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.MultipleCertificateFilter;
import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.keystores.filters.rfc.RFC2254CertificateFilter;
import es.gob.afirma.keystores.filters.rfc.SscdFilter;

/** Identifica y obtiene los filtros de certificados definidos en las propiedades
 * de una operaci&oacute;n de firma/multifirma electr&oacute;nica. *
 * @author Carlos Gamuci Mill&aacute;n. */
public final class CertFilterManager {

	private static final String HEADLESS_PROPERTY = "headless"; //$NON-NLS-1$
	private static final String MANDATORY_CERT_SELECTION_PROPERTY = "mandatoryCertSelection"; //$NON-NLS-1$

	private static final String FILTER_PREFIX_KEY = "filter"; //$NON-NLS-1$
	private static final String FILTERS_PREFIX_KEY = "filters"; //$NON-NLS-1$
	private static final String FILTERS_ENUM_SEPARATOR = "."; //$NON-NLS-1$
	private static final String FILTERS_SEPARATOR = ";"; //$NON-NLS-1$

	/** Filtro especial que indica que no debe permitirse la apertura por parte del usuario de almacenes
	 * de claves distintos al por defecto. */
	private static final String FILTER_TYPE_DISABLE_EXTERNAL_STORES = "disableopeningexternalstores"; //$NON-NLS-1$

	private static final String FILTER_TYPE_DNIE = "dnie:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_SSL = "ssl:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_QUALIFIED = "qualified:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_SIGNING_CERTIFICATE = "signingcert:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_AUTHENTICATION_CERTIFICATE = "authcert:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_NON_EXPIRED = "nonexpired:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_SSCD = "sscd:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_SUBJECT_RFC2254 = "subject.rfc2254:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_SUBJECT_CONTAINS = "subject.contains:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_ISSUER_RFC2254 = "issuer.rfc2254:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_ISSUER_RFC2254_RECURSE = "issuer.rfc2254.recurse:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_ISSUER_CONTAINS = "issuer.contains:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_THUMBPRINT = "thumbprint:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_POLICY_ID = "policyid:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_PSEUDONYM = "pseudonym:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_ENCODED_CERT = "encodedcert:"; //$NON-NLS-1$

	private static final String FILTER_PREFIX_KEYUSAGE = "keyusage.";  //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_DIGITAL_SIGNATURE = FILTER_PREFIX_KEYUSAGE + "digitalsignature:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_NON_REPUDIATION = FILTER_PREFIX_KEYUSAGE + "nonrepudiation:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_KEY_ENCIPHERMENT = FILTER_PREFIX_KEYUSAGE + "keyencipherment:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_DATA_ENCIPHERMENT = FILTER_PREFIX_KEYUSAGE + "dataencipherment:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_KEY_AGREEMENT = FILTER_PREFIX_KEYUSAGE + "keyagreement:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_KEY_CERT_SIGN = FILTER_PREFIX_KEYUSAGE + "keycertsign:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_CRL_SIGN = FILTER_PREFIX_KEYUSAGE + "crlsign:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_ENCIPHER_ONLY = FILTER_PREFIX_KEYUSAGE + "encipheronly:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_KEYUSAGE_DECIPHER_ONLY = FILTER_PREFIX_KEYUSAGE + "decipheronly:"; //$NON-NLS-1$

	private static final String[] PATTERN_KEYUSAGES_FILTER = new String[] {
		FILTER_TYPE_KEYUSAGE_DIGITAL_SIGNATURE,
		FILTER_TYPE_KEYUSAGE_NON_REPUDIATION,
		FILTER_TYPE_KEYUSAGE_KEY_ENCIPHERMENT,
		FILTER_TYPE_KEYUSAGE_DATA_ENCIPHERMENT,
		FILTER_TYPE_KEYUSAGE_KEY_AGREEMENT,
		FILTER_TYPE_KEYUSAGE_KEY_CERT_SIGN,
		FILTER_TYPE_KEYUSAGE_CRL_SIGN,
		FILTER_TYPE_KEYUSAGE_ENCIPHER_ONLY,
		FILTER_TYPE_KEYUSAGE_DECIPHER_ONLY
	};

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private boolean mandatoryCertificate = false;

	private final List<CertificateFilter> filters = new ArrayList<>();

	private boolean allowExternalStores = true;

	/**
	 * Construye el objeto con los filtros que deben aplicarse sobre una serie
	 * de certificados para comprobar cuales de ellos se ajustan a nuestra
	 * necesidades.
	 * @param filters Listado de filtros que se deben establecer.
	 * @param mandatoryCertificate Indica si se debe seleccionar
	 * autom&aacute;ticamente el certificado si s&oacute;lo hay uno que supere
	 * el filtro.
	 * @param allowExternalStores Indica si se deber&iacute;a permitir
	 * seleccionar otros almacenes de certificados sobre los que tambi&eacute;n
	 * se aplicar&aacute;n estos filtros.
	 */
	public CertFilterManager(final List<CertificateFilter> filters, final boolean mandatoryCertificate, final boolean allowExternalStores) {

		if (filters != null) {
			this.filters.addAll(filters);
		}
		this.mandatoryCertificate = mandatoryCertificate;
		this.allowExternalStores = allowExternalStores;
	}

	/**
	 * Construye el objeto con los filtros que deben aplicarse sobre una serie de
	 * certificados para comprobar cuales de ellos se ajustan a nuestra necesidades
	 * a partir de las propiedades de configuraci&oacute;n de una operaci&oacute;n.
	 * @param propertyFilters Conjunto de propiedades entre las que identificar las que
	 * establecen los criterios de filtrado.
	 */
	public CertFilterManager(final Properties propertyFilters) {

		this.mandatoryCertificate = isMandatoryCertificate(propertyFilters);

		// Obtenemos los distintos filtros disyuntivos declarados
		final List<String> filterValues = getFilterValues(propertyFilters);

		// Agregamos el filtro correspondiente a cada uno de los filtros desclarados
		for (final String filterValue : filterValues) {
			this.filters.add(parseFilter(filterValue));
		}

		// Siguiendo los criterios de la ETSI TS 119 102-1, un usuario no deberia firmar nunca con
		// un certificado caducado, asi que, si no se definio ningun tipo de filtrado, se agregara
		// un filtro omitiendo estos certificados. Si se agregaron filtros, se considerara que es
		// el integrador estara definiendo sus preferencias concretas de filtrado
		if (this.filters.isEmpty()) {
			this.filters.add(new ExpiredCertificateFilter(false));
		}
	}

	/**
	 * Indica si las propiedades establecidas para la operaci&oacute;n indican que se debe omitir
	 * la selecci&oacute;n de certificado si es posible.
	 * @param propertyFilters Propiedades de configuraci&oacute;n de la operaci&oacute;n.
	 * @return {@code true} si se debe seleccionar el certificado autom&aacute;ticamente;,
	 * {@code false} cuando se deba mostrar el di&aacute;logo de selecci&oacute;n.
	 */
	private static boolean isMandatoryCertificate(final Properties propertyFilters) {
		final boolean headless = propertyFilters != null
				&& Boolean.parseBoolean(propertyFilters.getProperty(HEADLESS_PROPERTY));
		final boolean omitSelection = propertyFilters != null
				&& propertyFilters.containsKey(MANDATORY_CERT_SELECTION_PROPERTY)
				&& Boolean.FALSE.toString().equalsIgnoreCase(
						propertyFilters.getProperty(MANDATORY_CERT_SELECTION_PROPERTY));

		return headless || omitSelection;
	}

	/** Indica si la apertura de almacenes externos al principal desde el UI del di&aacute;logo est&aacute;
	 * permitida o prohibida.
	 * @return <code>true</code> si se permite la apertura de almacenes externos,
	 *         <code>false</code> en caso contrario. */
	public boolean isExternalStoresOpeningAllowed() {
		return this.allowExternalStores;
	}

	/** Recoge los distintos filtros declarados en el par&aacute;metro de configuraci&oacute;n.
	 * @param config Configuraci&oacute;n de la operaci&oacute;n.
	 * @return Listado de filtros disyuntivos para aplicar al listado decertificados. */
	private static List<String> getFilterValues(final Properties config) {
		final List<String> filterValues = new ArrayList<>();
		if (config.containsKey(FILTER_PREFIX_KEY)) {
			filterValues.add(config.getProperty(FILTER_PREFIX_KEY));
		}
		else if (config.containsKey(FILTERS_PREFIX_KEY)) {
			filterValues.add(config.getProperty(FILTERS_PREFIX_KEY));
		}
		else if (config.containsKey(FILTERS_PREFIX_KEY + FILTERS_ENUM_SEPARATOR + 1)) {
			int i = 1;
			while (config.containsKey(FILTERS_PREFIX_KEY + FILTERS_ENUM_SEPARATOR + i)) {
				filterValues.add(config.getProperty(FILTERS_PREFIX_KEY + FILTERS_ENUM_SEPARATOR + i));
				i++;
			}
		}
		return filterValues;
	}

	private CertificateFilter parseFilter(final String filterValue) {

		final List<CertificateFilter> filtersList = new ArrayList<>();
		final String[] sortedFilterValues = filterValue.split(FILTERS_SEPARATOR);

		// Se ordena para que los KeyUsage esten consecutivos
		Arrays.sort(sortedFilterValues);
		for (int i = 0; i < sortedFilterValues.length; i++) {
			final String filter = sortedFilterValues[i];
			if (filter.toLowerCase().startsWith(FILTER_TYPE_DNIE)) {
				filtersList.add(new SignatureDNIeFilter());
			}
			else if (filter.toLowerCase().equals(FILTER_TYPE_DISABLE_EXTERNAL_STORES)) {
				this.allowExternalStores = false;
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_SSL)) {
				filtersList.add(new SSLFilter(filter.substring(FILTER_TYPE_SSL.length())));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_QUALIFIED)) {
				filtersList.add(new QualifiedCertificatesFilter(filter.substring(FILTER_TYPE_QUALIFIED.length())));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_SIGNING_CERTIFICATE)) {
				filtersList.add(new SigningCertificateFilter());
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_AUTHENTICATION_CERTIFICATE)) {
				filtersList.add(new AuthCertificateFilter());
			}
			else if (filter.toLowerCase().startsWith(FILTER_PREFIX_KEYUSAGE)) {
				final Boolean[] kuPattern = generateKeyUsageFiltersPattern(sortedFilterValues, i);
				filtersList.add(new KeyUsageFilter(kuPattern));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_NON_EXPIRED)) {
				final boolean showExpired = filter.equalsIgnoreCase(FILTER_TYPE_NON_EXPIRED) ?
						false : !Boolean.parseBoolean(filter.substring(FILTER_TYPE_NON_EXPIRED.length()));
				filtersList.add(new ExpiredCertificateFilter(showExpired));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_SSCD)) {
				filtersList.add(new SscdFilter());
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_SUBJECT_RFC2254)) {
				filtersList.add(new RFC2254CertificateFilter(filter.substring(FILTER_TYPE_SUBJECT_RFC2254.length()), null));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_SUBJECT_CONTAINS)) {
				filtersList.add(new TextContainedCertificateFilter(new String[] { filter.substring(FILTER_TYPE_SUBJECT_CONTAINS.length()) }, null));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_ISSUER_RFC2254)) {
				filtersList.add(new RFC2254CertificateFilter(null, filter.substring(FILTER_TYPE_ISSUER_RFC2254.length())));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_ISSUER_RFC2254_RECURSE)) {
				filtersList.add(new RFC2254CertificateFilter(null, filter.substring(FILTER_TYPE_ISSUER_RFC2254_RECURSE.length()), true));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_ISSUER_CONTAINS)) {
				filtersList.add(new TextContainedCertificateFilter(null, new String[] { filter.substring(FILTER_TYPE_ISSUER_CONTAINS.length()) }));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_THUMBPRINT)) {
				final String[] params = filter.substring(FILTER_TYPE_THUMBPRINT.length()).split(":"); //$NON-NLS-1$
				if (params.length == 2) {
					filtersList.add(new ThumbPrintCertificateFilter(params[0], params.length > 1 ? params[1] : null));
				}
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_POLICY_ID)) {
				final String oids = filter.substring(FILTER_TYPE_POLICY_ID.length());
				if (oids != null && !oids.isEmpty()) {
					filtersList.add(new PolicyIdFilter(Arrays.asList(oids.split(",")))); //$NON-NLS-1$
				}
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_PSEUDONYM)) {
				String value = PseudonymFilter.VALUE_PSEUDONYM_AND_OTHERS;
				if (!filter.toLowerCase().equals(FILTER_TYPE_PSEUDONYM)) {
					value = filter.substring(FILTER_TYPE_PSEUDONYM.length());
				}
				filtersList.add(new PseudonymFilter(value));
			}
			else if (filter.toLowerCase().startsWith(FILTER_TYPE_ENCODED_CERT)) {
				filtersList.add(new EncodedCertificateFilter(filter.substring(FILTER_TYPE_ENCODED_CERT.length())));
			}
			else {
				LOGGER.warning("Se omitira el filtro '" + filter + "' por no estar reconocido"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		return filtersList.size() == 1 ?
			filtersList.get(0) :
				new MultipleCertificateFilter(filtersList.toArray(new CertificateFilter[filtersList.size()]));
	}

	/** Obtiene el patr&oacute;n que define el filtro por <i>KeyUsage</i>, que de
	 * cara al exterior era un conjunto de filtros distintos.
	 * @param sortedFilterValues Listado de filtros definidos ordenados
	 * alfab&eacute;ticamente.
	 * @param pos Posici&oacute;n del primer identificador de filtro de <i>KeyUsage</i>.
	 * @return Patr&oacute;n para definir el filtro de <i>KeyUsages</i>. */
	private static Boolean[] generateKeyUsageFiltersPattern(final String[] sortedFilterValues, final int pos) {

		int i = pos;
		String filter = sortedFilterValues[pos];
		final Boolean[] kuPattern = new Boolean[PATTERN_KEYUSAGES_FILTER.length];
		do {
			processKeyUsageFilterDeclaration(filter, kuPattern);
			if (sortedFilterValues.length > i + 1 && sortedFilterValues[i + 1].startsWith(FILTER_PREFIX_KEYUSAGE)) {
				filter = sortedFilterValues[++i];
			}
			else {
				break;
			}
			// Nos valemos de que los filtros se han ordenado alfabeticamente,
			// asi que todos los filtros de KeyUsage estaran juntos
		} while (filter.toLowerCase().startsWith(FILTER_PREFIX_KEYUSAGE));

		return kuPattern;
	}

	private static void processKeyUsageFilterDeclaration (final String filter, final Boolean[] kuPattern) {

		int patternPosition = -1;
		for (int i = 0; patternPosition < 0 && i < PATTERN_KEYUSAGES_FILTER.length; i++) {
			if (filter.toLowerCase().startsWith(PATTERN_KEYUSAGES_FILTER[i])) {
				patternPosition = i;
			}
		}

		if (patternPosition >= 0) {
			final String value = filter.substring(filter.indexOf(':') + 1);
			kuPattern[patternPosition] = value.equalsIgnoreCase("null") ? null : Boolean.valueOf(value); //$NON-NLS-1$
		}
	}

	/** Devuelve la lista de certificados definidos.
	 * @return Listado de certificados. */
	public List<CertificateFilter> getFilters() {
		return new ArrayList<>(this.filters);
	}

	/** Indica si se debe seleccionar autom&aacute;ticamente un certificado si es el &uacute;nico que
	 * cumple los filtros.
	 * @return {@code true} si debe seleccionarse autom&aacute;ticamente el &uacute;nico certificado
	 * que supera el filtrado, {@code false} en caso contrario. */
	public boolean isMandatoryCertificate() {
		return this.mandatoryCertificate;
	}
}
