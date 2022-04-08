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
import java.util.List;

/** Metadatos del firmante de una firma CAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CAdESSignerMetadata {

	private final CAdESSignerLocation signerLocation;

	private static final int POSTAL_ADDRESS_MAX_LINES = 6;

	/** Construye los metadatos del firmante de una firma CAdES.
	 * @param country Pa&iacute;s donde estaba situado el firmante en el momento de la firma.
	 * @param locality Localidad donde estaba situado el firmante en el momento de la firma.
	 * @param address Direcci&oacute;n postal (en m&aacute;ximo 6 l&iacute;neas) donde
	 *                estaba situado el firmante en el momento de la firma. */
	public CAdESSignerMetadata(final String country,
			                   final String locality,
			                   final List<String> address) {
		this.signerLocation = new CAdESSignerLocation(country, locality, address);
	}

	/** Obtiene los metadatos de situaci&oacute;n del firmante en el momento de la firma.
	 * @return Metadatos de situaci&oacute;n del firmante en el momento de la firma. */
	public CAdESSignerLocation getSignerLocation() {
		return this.signerLocation;
	}


	/** Direcci&oacute;n del firmante (<i>id-aa-ets-signerLocation</i>).
	 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
	public static final class CAdESSignerLocation {

		private final String countryName;
		private final String localityName;
		private final List<String> postalAddress;

		/** Construye el metadato de direcci&oacute;n del firmante.
		 * @param country Pa&iacute;s
		 * @param locality Localidad
		 * @param address Direcci&oacute;n postal (en m&aacute;ximo 6 l&iacute;neas) */
		CAdESSignerLocation(final String country, final String locality, final List<String> address) {
			if (address != null && address.size() > POSTAL_ADDRESS_MAX_LINES) {
				throw new IllegalArgumentException(
					"La direccion postal debe tener un maximo de seis lineas, y se han proporcionado " + address.size() //$NON-NLS-1$
				);
			}
			this.countryName = country;
			this.localityName = locality;
			this.postalAddress = address != null ? new ArrayList<String>(address) : null;
		}

		/** Obtiene el nombre del pa&iacute;s donde se encuentra el firmante.
		 * @return Nombre del pa&iacute;s donde se encuentra el firmante. */
		public String getCountryName() {
			return this.countryName;
		}

		/** Obtiene el nombre de la localidad donde se encuentra el firmante.
		 * @return Nombre de la localidad donde se encuentra el firmante. */
		public String getLocalityName() {
			return this.localityName;
		}

		/** Obtiene la direcci&oacute;n postal donde se encuentra el firmante.
		 * <pre>
		 *  PostalAddress ::= SEQUENCE SIZE(1..6) OF DirectoryString
		 * </pre>
		 * @return Direcci&oacute;n postal donde se encuentra el firmante. */
		public List<String> getPostalAddress() {
			return this.postalAddress != null ? new ArrayList<String>(this.postalAddress) : null;
		}

	}

}
