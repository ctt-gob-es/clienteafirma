package es.gob.afirma.signers.cades;

import java.util.List;

/** Metadatos del firmante de una firma CAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CAdESSignerMetadata {

	private final CAdESSignerLocation signerLocation;
	private final String[] signerClaimedRoles;

	/** Construye los metadatos del firmante de una firma CAdES.
	 * @param country Pa&iacute;s donde estaba situado el firmante en el momento de la firma.
	 * @param locality Localidad donde estaba situado el firmante en el momento de la firma.
	 * @param address Direcci&oacute;n postal (en m&aacute;ximo 6 l&iacute;neas) donde
	 *                estaba situado el firmante en el momento de la firma.
	 * @param claimedRoles Cargos atribuidos al firmante. */
	public CAdESSignerMetadata(final String country,
			                   final String locality,
			                   final List<String> address,
			                   final String... claimedRoles) {
		this.signerLocation = new CAdESSignerLocation(country, locality, address);
		this.signerClaimedRoles = claimedRoles != null ?
									claimedRoles :
										new String[0];
	}

	/** Obtiene los cargos atribuidos al firmante.
	 * @return Cargos atribuidos al firmante. */
	public String[] getClaimedRoles() {
		return this.signerClaimedRoles;
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
			if (country == null && locality == null && (address == null || address.size() < 1)) {
				throw new IllegalArgumentException(
					"Alguno de los datos del firmante debe ser distinto de nulo" //$NON-NLS-1$
				);
			}
			if (address != null && address.size() > 6) {
				throw new IllegalArgumentException(
					"La direccion postal debe tener un maximo de seis lineas, y se han proporcionado " + address.size() //$NON-NLS-1$
				);
			}
			this.countryName = country;
			this.localityName = locality;
			this.postalAddress = address;
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
		 * @return Direcci&oacute;n postal donde se encuentra el firmante. */
		public List<String> getPostalAddress() {
			return this.postalAddress;
		}

	}

}
