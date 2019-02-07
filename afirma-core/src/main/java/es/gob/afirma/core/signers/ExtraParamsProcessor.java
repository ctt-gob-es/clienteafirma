/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Method;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Logger;

/** Clase de utilidad para el proceso de propiedades enviadas desde JavaScript
 * y recogidas desde Java en formato <code>Properties</code>. */
public final class ExtraParamsProcessor {

	/** Tama&ntilde;o equivalente a 1 MegaBytes en bytes. */
	private static final int SIZE_1MB = 1024 * 1024;

	private static final String ETSI_CADES_DETACHED = "ETSI.CAdES.detached"; //$NON-NLS-1$

	/** Clave expansible para pol&iacute;ticas de firma. */
	private static final String EXPANDIBLE_POLICY_KEY = "expPolicy"; //$NON-NLS-1$

	/** Manejador del log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ExtraParamsProcessor() {
		/* Constructor no publico */
	}

	/** Transforma la entrada introducida en un <code>Properties</code>.
	 * Las entradas deben estar separadas por salto de l&iacute;nea y tener la forma
	 * {@code CLAVE=VALOR} en donde CLAVE es el identificador del par&aacute;metro y
	 * VALOR el valor asignado a este.
	 * La CLAVE no puede contener ning&uacute;n signo igual ('=') ni empezar por
	 * almohadilla ('#') y se ignorar&aacute;n aquellas entradas que no contengan
	 * el signo igual en una posici&oacute;n la cadena distinta a la primera.
	 * Si se introduce null se devuelve un Properties vac&iacute;o.
	 * @param entries Listado de pares CLAVE - VALOR.
	 * @return Properties con las claves indicadas cargadas como par&aacute;metro. */
	public static Properties convertToProperties(final String entries) {

		final Properties params = new Properties();
		if (entries == null) {
			return params;
		}

		try {
			params.load(new ByteArrayInputStream(entries.getBytes()));
		}
		catch (final Exception e) {
			LOGGER.warning(
				"Se han encontrado entradas no validas en la configuracion de la operacion: " + e//$NON-NLS-1$
			);
		}

		return params;
	}

 	/** Devuelve la colecci&oacute;n de propiedades de entrada con las entradas que correspondan
	 * expandidos. Se expandiran una serie de claves con valores predefinidos y se les
	 * asignar&aacute; el valor correspondiente.
	 * Una vez expandidos, se eliminaran estos par&aacute;metros de la lista. Si el expandir
	 * los par&aacute;metros implica establer otras propiedades y estas ya est&aacute;n
	 * definidas en el Properties, prevalecer&aacute;n los valores expandidos.<br>
	 * Entre los par&aacute;metros clave se encuentran:
	 * <ul>
     *  <li><b>expPolicy</b>: Configuracion de la pol&iacute;tica de firma. Posibles valores:
     *   <ul><li><b>FirmaAGE</b>:
     *    Establece los diversions par&aacute;metros para la configuraci&oacute;n de la
     *    pol&iacute;tica de firma de la AGE.
     *   </li>
     *   </ul>
     *  </li>
     * </ul>
	 * @param params Par&aacute;metros definidos para la operaci&oacute;n.
	 * @return Propiedades expandidas.
	 * @throws IncompatiblePolicyException Si el formato de firma es incompatible con la pol&iacute;tica indicada. */
	public static Properties expandProperties(final Properties params) throws IncompatiblePolicyException {
		return expandProperties(params, null, null);
	}

	/** Devuelve la colecci&oacute;n de propiedades de entrada con las entradas que correspondan
	 * expandidos. Se expandiran una serie de claves con valores predefinidos y se les
	 * asignar&aacute; el valor correspondiente.
	 * Una vez expandidos, se eliminaran estos par&aacute;metros de la lista. Si el expandir
	 * los par&aacute;metros implica establer otras propiedades y estas ya est&aacute;n
	 * definidas en el Properties, prevalecer&aacute;n los valores expandidos.<br>
	 * Entre los par&aacute;metros clave se encuentran:
	 * <ul>
     *  <li><b>expPolicy</b>: Configuracion de la pol&iacute;tica de firma. Posibles valores:
     *   <ul><li><b>FirmaAGE</b>:
     *    Establece los diversions par&aacute;metros para la configuraci&oacute;n de la
     *    pol&iacute;tica de firma de la AGE.
     *   </li>
     *   </ul>
     *  </li>
     * </ul>
	 * @param params Par&aacute;metros definidos para la operaci&oacute;n.
	 * @param signedData Datos firmados.
	 * @param format Formato de firma.
	 * @return Propiedades expandidas.
	 * @throws IncompatiblePolicyException Si el formato de firma es incompatible con la pol&iacute;tica indicada. */
	public static Properties expandProperties(final Properties params, final byte[] signedData, final String format) throws IncompatiblePolicyException {
		final Properties p = new Properties();
		for (final String key : params.keySet().toArray(new String[params.size()])) {
			p.setProperty(key, params.getProperty(key));
		}
		expandPolicyKeys(p, signedData, format);
		return p;
	}

	/** Expande las propiedades de pol&iacute;tica de firma modificando el conjunto de propiedades.
	 * @param p Propiedades configuradas.
	 * @param signedData Datos firmados.
	 * @param format Formato de firma.
	 * @throws IncompatiblePolicyException Si el formato de firma es incompatible con la pol&iacute;tica indicada. */
	private static void expandPolicyKeys(final Properties p, final byte[] signedData, final String format) throws IncompatiblePolicyException {
		if (p.containsKey(EXPANDIBLE_POLICY_KEY)) {

			final String policy = p.getProperty(EXPANDIBLE_POLICY_KEY);
			// Si es AGE 1.8 solo aceptamos CAdES y XAdES
			if (AdESPolicyPropertiesManager.POLICY_ID_AGE_1_8.equals(policy) &&
				!AOSignConstants.SIGN_FORMAT_XADES.toLowerCase(Locale.US).startsWith(
						format.toLowerCase(Locale.US)) &&
				!AOSignConstants.SIGN_FORMAT_XADES_TRI.equalsIgnoreCase(format) &&
					!AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format) &&
					!AOSignConstants.SIGN_FORMAT_CADES_TRI.equalsIgnoreCase(format)) {
				throw new IncompatiblePolicyException(
					"La politica de firma 1.8 de la AGE solo puede usarse con XAdES o CAdES, y no con " + format //$NON-NLS-1$
				);
			}

			// Consideraciones de la politica 1.8 de la AGE y la ultima version de esta misma politica
			if (AdESPolicyPropertiesManager.POLICY_ID_AGE.equals(policy) ||
					AdESPolicyPropertiesManager.POLICY_ID_AGE_1_8.equals(policy)) {

				String normalizedFormat = null;
				if (format != null) {
					if (format.toLowerCase(Locale.US).startsWith(AOSignConstants.SIGN_FORMAT_XADES.toLowerCase(Locale.US)) ||
							format.toLowerCase(Locale.US).startsWith(AOSignConstants.SIGN_FORMAT_XADES_TRI.toLowerCase(Locale.US))) {
						normalizedFormat = AdESPolicyPropertiesManager.FORMAT_XADES;

						// La firma XAdES conforme a la politica de firma de la AGE debe ser Detached o Enveloped
						if (!AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equalsIgnoreCase(p.getProperty("format")) && //$NON-NLS-1$
								!AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equalsIgnoreCase(p.getProperty("format"))) { //$NON-NLS-1$
							if (p.containsKey("format")) { //$NON-NLS-1$
								LOGGER.warning("La siguiente propiedad se ignora en favor del valor derivado de la politica establecida: format"); //$NON-NLS-1$
							}
							p.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
						}
					}
					else if (format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_CADES) ||
							format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_CADES_TRI)) {
						normalizedFormat = AdESPolicyPropertiesManager.FORMAT_CADES;

						// La politica indica que la firma debe ser implicita siempre que el tamano
						// del documento sea razonable. Como no se especifica que tamano es razonable
						// respetaremos el modo indicado por el integrador. En caso de no haberlo
						// indicado, establecemos el limite en 1Mb. Esto solo aplicaria a CAdES ya que
						// PAdES siempre es implicita e ignora este parametro.
						if (!p.containsKey("mode") && signedData != null) { //$NON-NLS-1$
							p.setProperty("mode", signedData.length < SIZE_1MB ? //$NON-NLS-1$
								AOSignConstants.SIGN_MODE_IMPLICIT :
									AOSignConstants.SIGN_MODE_EXPLICIT);
						}
					}
					else if (format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PDF) ||
							 format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PADES) ||
							 format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PDF_TRI) ||
							 format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PADES_TRI)) {
						if (!ETSI_CADES_DETACHED.equals(p.getProperty("signatureSubFilter", ETSI_CADES_DETACHED))) { //$NON-NLS-1$
							throw new IncompatiblePolicyException("En PAdES con politica firma AGE debe usarse siempre el filtro 'ETSI.CAdES.detached'"); //$NON-NLS-1$
						}
						p.setProperty("signatureSubFilter", ETSI_CADES_DETACHED); //$NON-NLS-1$
						normalizedFormat = AdESPolicyPropertiesManager.FORMAT_PADES;
					}
				}
				AdESPolicyPropertiesManager.setProperties(p, policy, normalizedFormat);
			}
			p.remove(EXPANDIBLE_POLICY_KEY);
		}
	}

	/** Pol&iacute;tica de firma incompatible con el formato o la configuraci&oacute;n de firma. */
	public static final class IncompatiblePolicyException extends Exception {

		private static final long serialVersionUID = -6420193548487585455L;

		IncompatiblePolicyException(final String description) {
			super(description);
		}
	}

	/** Establece propiedades de firma concretas para cuando el formato indicado sea "AUTO".
	 * Las propiedades dependen del signer que se vaya a usar.
	 * @param signer Firmador usado.
	 * @param data Datos a firmar.
	 * @param params Par&aacute;metros adicionales. */
	public static void configAutoFormat(final AOSigner signer, final byte[] data, final Properties params) {

		final String signerClassname = signer.getClass().getName();
		if (signerClassname.equals("es.gob.afirma.signers.pades.AOPDFSigner")) { //$NON-NLS-1$
			try {
				final Method configureMethod = signer.getClass().getMethod("configureRespectfulProperties", byte[].class, Properties.class); //$NON-NLS-1$
				configureMethod.invoke(null, data, params);
			}
			catch (final Exception e) {
				LOGGER.warning("Error al configurar una firma PAdES igual a las existentes: " + e); //$NON-NLS-1$
			}
		}
	}
}
