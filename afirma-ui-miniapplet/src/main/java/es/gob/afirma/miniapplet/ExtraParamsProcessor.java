/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet;

import java.io.ByteArrayInputStream;
import java.util.Properties;
import java.util.logging.Logger;

/** Clase de utilidad para el proceso de propiedades enviadas desde JavaScript
 * y recogidas desde java en formato <code>Properties<code>. */
final class ExtraParamsProcessor {

	private ExtraParamsProcessor() {
		/* Constructor no publico */
	}
	
	/**
	 * Transforma la entrada introducida en un properties.
	 * Las entradas deben estar separadas por salto de l&iacute;nea y tener la forma
	 * {@code CLAVE=VALOR} en donde CLAVE es el identificador del par&aacute;metro y
	 * VALOR el valor asignado a este.
	 * La CLAVE no puede contener ning&uacute;n signo igual ('=') ni empezar por
	 * almohadilla ('#') y se ignorar&aacute;n aquellas entradas que no contengan
	 * el signo igual en una posici&oacute;n la cadena distinta a la primera.
	 * Si se introduce null se devuelve un Properties vac&iacute;o.
	 * @param entries Listado de pares CLAVE - VALOR.
	 * @return Properties con las claves indicadas cargadas como par&aacute;metro.
	 */
	static Properties convertToProperties(final String entries) {

		final Properties params = new Properties();
		if (entries == null) {
			return params;
		}

		try {
			params.load(new ByteArrayInputStream(entries.getBytes()));
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"Se han encontrado entradas no validas en la configuracion de la operacion: " //$NON-NLS-1$
					+ e);
			return params;
		}
		
		return params;
	}
	
	private static final String EXPANDIBLE_POLICY_KEY = "expPolicy"; //$NON-NLS-1$
	
	private static final String EXPANDIBLE_POLICY_VALUE_AGE = "FirmaAGE"; //$NON-NLS-1$
	
	/**
	 * Devuelve la colecci&oacute;n de propiedades de entrada con las entradas que correspondan
	 * expandidos. Se expandiran una serie de claves con valores predefinidos y se les
	 * asignar&aacute; el valor correspondiente.
	 * Una vez expandidos, se eliminaran estos par&aacute;metros de la lista. Si el expandir
	 * los par&aacute;metros implica establer otras propiedades y estas ya est&aacute;n
	 * definidas en el Properties, prevalecer&aacute;n los valores expandidos.<br/>
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
	 */
	static Properties expandProperties(final Properties params) {
		
		final Properties p = new Properties();
		for (final String key : params.keySet().toArray(new String[0])) {
			p.setProperty(key, params.getProperty(key));
		}
		
		if (p.containsKey(EXPANDIBLE_POLICY_KEY)) {
			if (EXPANDIBLE_POLICY_VALUE_AGE.equals(p.getProperty(EXPANDIBLE_POLICY_KEY))) {
				p.setProperty("policyIdentifier", //$NON-NLS-1$
						"urn:oid:2.16.724.1.3.1.1.2.1.8");  //$NON-NLS-1$ 
				p.setProperty("policyIdentifierHash", //$NON-NLS-1$
						"V8lVVNGDCPen6VELRD1Ja8HARFk=");  //$NON-NLS-1$ 
				p.setProperty("policyIdentifierHashAlgorithm", //$NON-NLS-1$
						"http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ 
				p.setProperty("policyQualifier", //$NON-NLS-1$
						"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"); //$NON-NLS-1$ 
			}
			p.remove(EXPANDIBLE_POLICY_KEY);
		}
		
		return p;
	}
}
