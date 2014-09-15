package es.gob.afirma.crypto.handwritten.pdf;

import com.lowagie.text.xml.xmp.XmpArray;
import com.lowagie.text.xml.xmp.XmpSchema;

import es.gob.afirma.core.misc.Base64;

/** Esquema XMP de una firma biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class BioMetadataSchema extends XmpSchema {

	private static final long serialVersionUID = -1046785741568736817L;

	/** Datos biom&eacute;tricos de firma manuscrita seg&uacute;n ISO 19795-7. */
	private static final String BIOSIGNDATA = "ls:BioSignData"; //$NON-NLS-1$

	/** DN de la clave de firma. */
	private static final String K_DN = "ls:PublicKeyDN"; //$NON-NLS-1$

	/** Nombre del firmante. */
	private static final String SIGNER_NAME = "ls:SignerName"; //$NON-NLS-1$

	/** Primer apellido del firmante. */
	private static final String SIGNER_SURNAME1 = "ls:SignerSurame1"; //$NON-NLS-1$

	/** Segundo apellido del firmante. */
	private static final String SIGNER_SURNAME2 = "ls:SignerSurname2"; //$NON-NLS-1$

	/** NIF del firmante. */
	private static final String SIGNER_ID = "ls:SignerId"; //$NON-NLS-1$

	private static final String DEFAULT_XPATH_ID = "ls"; //$NON-NLS-1$
    private static final String DEFAULT_XPATH_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"; //$NON-NLS-1$

	BioMetadataSchema() {
		super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	/** A&ntilde;ade los datos biom&acute;tricos ISO 19794-7 al esquema.
	 * @param bioSignData Datos biom&acute;tricos ISO 19794-7. */
	void addBioSignData(final byte[] bioSignData) {
		if (bioSignData == null) {
			throw new IllegalArgumentException("Los datos de la firma biometrica no pueden ser nulos"); //$NON-NLS-1$
		}
		final String base64Data = Base64.encode(bioSignData);
		final XmpArray<String> array = new XmpArray<String>(XmpArray.UNORDERED);
		array.add(base64Data);
		setProperty(BIOSIGNDATA, array);
	}

	/** Agrega al esquema XMP el DN de la clave de firma.
	 * @param kDn DN de la clave de firma. */
	void addKeyDn(final String kDn) {
		if (kDn == null) {
			throw new IllegalArgumentException("El DN de la clave de firma no puede ser nulo"); //$NON-NLS-1$
		}

		final XmpArray<String> array = new XmpArray<String>(XmpArray.UNORDERED);
		array.add(kDn);
		setProperty(K_DN, array);
	}

	/** A&ntilde;ade los datos del firmante al esquema.
	 * @param name Nombre del firmante.
	 * @param surname1 Primer apellido del firmante.
	 * @param surname2 Segundo apellido del firmante.
	 * @param id NIF del firmante. */
	void addSignerData(final String name,
			                  final String surname1,
			                  final String surname2,
			                  final String id) {
		if (name == null &&
			surname1 == null &&
			surname2 == null &&
			id == null) {
				throw new IllegalArgumentException(
					"Al menos uno de los datos del firmante debe ser distinto de nulo" //$NON-NLS-1$
				);
		}
		if (name != null) {
			final XmpArray<String> array = new XmpArray<String>(XmpArray.UNORDERED);
			array.add(name);
			setProperty(SIGNER_NAME, array);
		}
		if (surname1 != null) {
			final XmpArray<String> array = new XmpArray<String>(XmpArray.UNORDERED);
			array.add(surname1);
			setProperty(SIGNER_SURNAME1, array);
		}
		if (surname2 != null) {
			final XmpArray<String> array = new XmpArray<String>(XmpArray.UNORDERED);
			array.add(surname2);
			setProperty(SIGNER_SURNAME2, array);
		}
		if (id != null) {
			final XmpArray<String> array = new XmpArray<String>(XmpArray.UNORDERED);
			array.add(id);
			setProperty(SIGNER_ID, array);
		}
	}

}
