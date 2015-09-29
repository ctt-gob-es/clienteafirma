/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.triphase.signer.xades;

import java.util.List;

/** Resultado de una prefirma XML.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XmlPreSignResult {

	private final byte[] xmlSign;
	private final List<byte[]> signedInfos;

	XmlPreSignResult(final byte[] xmlSign, final List<byte[]> signedInfos) {
		this.xmlSign = xmlSign.clone();
		this.signedInfos = signedInfos;
	}

	/** Recupera el PKCS#1 de la firma.
	 * @return PKCS#1 de la firma. */
	public byte[] getXmlSign() {
		return this.xmlSign;
	}

	/** Recupera los SignedInfos que hay que firmar.
	 * @return SignedInfos para firmar. */
	public List<byte[]> getSignedInfos() {
		return this.signedInfos;
	}


}
