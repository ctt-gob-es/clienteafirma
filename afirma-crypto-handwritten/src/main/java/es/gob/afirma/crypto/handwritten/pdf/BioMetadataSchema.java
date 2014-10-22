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
		a.add(sign.getSigner().getName());
		a.add(sign.getSigner().getSurname1());
		a.add(sign.getSigner().getSurname2() != null ? sign.getSigner().getSurname2() : ""); //$NON-NLS-1$
		a.add(sign.getSigner().getId());
		a.add(Base64.encode(sign.getBioData()));
		a.add(sign.getKeyDn() != null ? sign.getKeyDn() : ""); //$NON-NLS-1$
		a.add(sign.getSignTime());
		return a;
	}

}
