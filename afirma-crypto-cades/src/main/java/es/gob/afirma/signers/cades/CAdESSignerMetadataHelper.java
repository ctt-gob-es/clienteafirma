package es.gob.afirma.signers.cades;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERUTF8String;
import org.bouncycastle.asn1.esf.SignerLocation;

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
		final String postalAddressOneLine = extraParams.getProperty("signatureProductionPostalCode"); //$NON-NLS-1$
		final String[] postalAddress = postalAddressOneLine == null ?
			null :
				postalAddressOneLine.split("\n"); //$NON-NLS-1$

		// signerLocationCountryName
		final String country = extraParams.getProperty("signatureProductionCountry"); //$NON-NLS-1$

		// signerLocationLocalityName
		final String locality = extraParams.getProperty("signatureProductionCity"); //$NON-NLS-1$
		if (postalAddress != null || country != null || locality != null) {
			return new CAdESSignerMetadata(
				country,
				locality,
				postalAddress != null ?
					Arrays.asList(postalAddress) :
						null
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
			postalAdressAsn1List = new ArrayList<ASN1Encodable>(postalAddress.size());
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
