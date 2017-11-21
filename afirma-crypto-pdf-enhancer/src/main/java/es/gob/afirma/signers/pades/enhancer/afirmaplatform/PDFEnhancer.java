/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades.enhancer.afirmaplatform;

import java.io.IOException;
import java.util.Properties;

import es.gob.afirma.signers.pades.SignEnhancer;

/** Clase para la mejora de las firmas PDF a trav&eacute;s del servicio de actualizaci&oacute;n de
 * la plataforma &#64;firma.
 * Deben proporcionarse como opciones:
 * <ul>
 *  <li>
 *   <code>appName</code>: Nombre de la aplicaci&oacute;n que solicita la mejora (debe estar dado de
 *   alta en la plataforma y cumplir los requisitos establecidos).
 *  </li>
 *  <li>
 *   <code>signType</code>: Tipo de firma: "A": Archivo longevo, "T": Con sello de tiempo...
 *  </li>
 * </ul> */
public final class PDFEnhancer implements SignEnhancer {

	/** Valor clave para la configuraci&oacute;n del nombre de aplicaci&oacute;n que accede
	 * a la plataforma &#64;firma. */
	public static final String APPLICATION_NAME_OPTION = "appName"; //$NON-NLS-1$

	/** Valor clave para la configuraci&oacute;n del tipo de firma extendida que se desea generar. */
	public static final String SIGN_TYPE_OPTION = "signType"; //$NON-NLS-1$

	@Override
	public byte[] enhance(final byte[] signature, final Properties options) throws IOException {
		return AFirmaPlatformPdfEnhancer.upgradeSign(
			signature,
			options.getProperty(APPLICATION_NAME_OPTION),
			options.getProperty(SIGN_TYPE_OPTION)
		);
	}



}
