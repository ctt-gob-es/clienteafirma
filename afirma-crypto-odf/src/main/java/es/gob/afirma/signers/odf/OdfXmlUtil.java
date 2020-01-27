/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.odf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import nu.xom.canonical.Canonicalizer;
import nu.xom.converters.DOMConverter;

final class OdfXmlUtil {

	private OdfXmlUtil() {
		// No instanciable
	}

	static byte[] canonicalizeXml(final org.w3c.dom.Element element, final String algorithm) throws IOException {
		final nu.xom.Element xomElement = DOMConverter.convert(element);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final Canonicalizer canonicalizer = new Canonicalizer(baos, algorithm);
		canonicalizer.write(xomElement);
		return baos.toByteArray();
	}
}
