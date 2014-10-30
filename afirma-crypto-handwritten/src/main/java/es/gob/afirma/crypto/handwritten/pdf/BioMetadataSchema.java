package es.gob.afirma.crypto.handwritten.pdf;

import java.util.List;

import com.lowagie.text.xml.xmp.XmpArray;
import com.lowagie.text.xml.xmp.XmpSchema;

import es.gob.afirma.core.misc.Base64;

/** Esquema XMP de una firma biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class BioMetadataSchema extends XmpSchema {

	private static final long serialVersionUID = -1046785741568736817L;

	private static final String BIOSIGNATURES = "ls:BioSignature"; //$NON-NLS-1$

	private static final String DEFAULT_XPATH_ID = "ls"; //$NON-NLS-1$
    private static final String DEFAULT_XPATH_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"; //$NON-NLS-1$

	BioMetadataSchema(final List<XmpSignStructure> signs) {
		super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		for (int i=0; i<signs.size();i++) {
			setProperty(BIOSIGNATURES + Integer.toString(i), createBioSignsArray(signs.get(i)));
		}
	}

	private static XmpArray<String> createBioSignsArray(final XmpSignStructure sign) {
		final XmpArray<String> a = new XmpArray<String>(XmpArray.ORDERED);
		a.add(replaceAccents(sign.getSigner().getName()));
		a.add(replaceAccents(sign.getSigner().getSurname1()));
		a.add(sign.getSigner().getSurname2() != null ? replaceAccents(sign.getSigner().getSurname2()) : ""); //$NON-NLS-1$
		a.add(sign.getSigner().getId());
		a.add(Base64.encode(sign.getBioData()));
		a.add(sign.getKeyDn() != null ? sign.getKeyDn() : ""); //$NON-NLS-1$
		a.add(sign.getSignTime());
		return a;
	}

	/**M&eacute;todo que remplaza los acentos y caracteres especiales.
	 * @param data cadena de caracteres con los datos iniciales.
	 * @return cadena de caracteres con los datos reemplazados.*/
	private static String replaceAccents(final String data) {
		return data.replaceAll("á","&aacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("é","&eacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("í","&iacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("ó","&oacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("ú","&uacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("ñ","&ntilde;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("Á","&Aacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("É","&Eacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("Í","&Iacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("Ó","&Oacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("Ú","&Uacute;") //$NON-NLS-1$ //$NON-NLS-2$
					.replaceAll("Ñ","&Ntilde;"); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
