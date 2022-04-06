/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.Principal;
import java.util.HashMap;
import java.util.Map;

/**
 * Esta clase contiene las configuraciones de KeyUsage fuera de est&aacute;ndar que
 * presentan los certificados cualificados de algunas autoridades de certificaci&oacute;n.
 * Estas CAs establecen de forma err&oacute;nea los KeyUsage a sus certificados de firma,
 * autenticaci&oacute;n... y no es posible distintguir entre ellos siguiendo las condiciones
 * est&aacute;ndar para certificados cualificados. Proporcionando a esta clase una CA te
 * devuelve la configuraci&oacute;n de KeyUsage que utiliza para el tipo de certificado que
 * se solicite. Si la CA no est&aacute; registrada como aquellas que utilizan una
 * configuraci&oacute;n fuera de est&aacute;ndar, se devolver&aacute; la configurac&iacute;n
 * est&aacute;ndar.
 *
 * @author Carlos Gamuci
 */
public class KeyUsagesPattern {

	/** Configuracion de KeyUsage est&aacute;ndar para un certificado de firma. */
	private static final Boolean[] DEFAULT_SIGNATURE_KEYUSAGES = {
		null, Boolean.TRUE, null, null, null, null, null, null
	};

	/** Configuraciones de KeyUsage de certificados de firma de CAs espec&iacute;ficas. */
	private static final Map<String, Boolean[]> SIGNATURE_KEYUSAGES;
	static {
		SIGNATURE_KEYUSAGES = new HashMap<>();

		// Certificado de fima de la CA de la Generalitat Valenciana
		SIGNATURE_KEYUSAGES.put("C=ES, O=Generalitat Valenciana, OU=PKIGVA, CN=ACCV-CA2", //$NON-NLS-1$
				new Boolean[] {Boolean.TRUE, Boolean.FALSE, Boolean.FALSE, Boolean.FALSE,
				Boolean.FALSE, Boolean.FALSE, Boolean.FALSE, Boolean.FALSE} );
	}

	/** Autoridad de certificaci&oacute;n. */
	private final Principal issuer;

	/**
	 * Crea el objeto para obtener cada una de las configuraciones de KeyUsage que utiliza
	 * la entidad emisora indicada para sus certificados.
	 * @param issuer Entidad emisora.
	 */
	public KeyUsagesPattern(final Principal issuer) {
		this.issuer = issuer;
	}

	/**
	 * Recupera el KeyUsage propio para el certificado de firma.
	 * @return KeyUsage del certificado de firma.
	 */
	public Boolean[] getSignaturePattern() {
		if (SIGNATURE_KEYUSAGES.containsKey(this.issuer.toString())) {
			return SIGNATURE_KEYUSAGES.get(this.issuer.toString());
		}
		return DEFAULT_SIGNATURE_KEYUSAGES.clone();
	}
}
