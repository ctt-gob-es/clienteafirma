/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.DERSequence;
import org.spongycastle.asn1.DERUTF8String;
import org.spongycastle.asn1.esf.SignerLocation;

/** Clase de utilidad para el manejo de las estructuras CAdES <i>id-aa-ets-signerLocation</i> y
 * <i>id-aa-ets-signerAttr</i>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CAdESSignerMetadataHelper {

	private CAdESSignerMetadataHelper() {
		// No instanciable
	}

	/** Obtiene los metadatos del firmante a partir de los par&aacute;metros adicionales.
	 * @param extraParams Par&aacute;metros adicionales que contienen las propiedades de los metadatos.
	 * @return Metadatos del firmante */
	public static CAdESSignerMetadata getCAdESSignerMetadata(final Properties extraParams) {
		if (extraParams == null) {
			return null;
		}
		// signerLocationPostalAddress
		final String postalAddressOneLine = extraParams.getProperty(CAdESExtraParams.SIGNATURE_PRODUCTION_POSTAL_CODE);
		final List<String> postalAddress = postalAddressOneLine == null ?
			null :
				Arrays.asList(postalAddressOneLine.split("\n")); //$NON-NLS-1$

		// signerLocationCountryName
		final String country = extraParams.getProperty(CAdESExtraParams.SIGNATURE_PRODUCTION_COUNTRY);

		// signerLocationLocalityName
		final String locality = extraParams.getProperty(CAdESExtraParams.SIGNATURE_PRODUCTION_CITY);
		if (postalAddress != null || country != null || locality != null) {
			return new CAdESSignerMetadata(
				country,
				locality,
				postalAddress
			);
		}
		return null;
	}

	/** Obtiene la estructura ASN.1 <i>SignerLocation</i> (<i>id-aa-ets-signerLocation</i>) a partir
	 * de su Bean-
	 * @param csl Metadatos de situaci&oacute;n del firmante en el momento de la firma.
	 * @return Estructura ASN.1 <i>SignerLocation</i> */
	public static SignerLocation getSignerLocation(final CAdESSignerMetadata.CAdESSignerLocation csl) {
		if (csl == null) {
			return null;
		}
		if (csl.getCountryName() == null && csl.getLocalityName() == null && csl.getPostalAddress() == null) {
			return null;
		}
		final List<String> postalAddress = csl.getPostalAddress();
		List<ASN1Encodable> postalAdressAsn1List = null;
		if (postalAddress != null) {
			postalAdressAsn1List = new ArrayList<>(postalAddress.size());
			for (final String addressLine : postalAddress) {
				postalAdressAsn1List.add(new DERUTF8String(addressLine));
			}
		}
		return new SignerLocation(
			csl.getCountryName() != null ? new DERUTF8String(csl.getCountryName()) : null,
			csl.getLocalityName() != null ? new DERUTF8String(csl.getLocalityName()) : null,
			postalAdressAsn1List != null ?
				new DERSequence(
					postalAdressAsn1List.toArray(new ASN1Encodable[0])
				) :
					null
		);
	}

}
