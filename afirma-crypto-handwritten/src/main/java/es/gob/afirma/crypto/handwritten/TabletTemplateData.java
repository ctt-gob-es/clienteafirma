package es.gob.afirma.crypto.handwritten;

import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/** Informaci&oacute;n necesaria para asociar una plantilla definida con una tableta de firma espec&iacute;fica.
 * @author Astrid Idoat Gil */
@XmlRootElement(name = "tabletTemplateData")
public class TabletTemplateData {


	/** Tipo de tableta. */
	@XmlElement(name = "tabletType")
	private final String tabletType;

	/** Plantilla HTML a mostrar en la tableta al firmar el firmante. */
	@XmlElement(name = "htmlTemplate")
	private final String htmlTemplate;

	/** Imagen JPEG a mostrar en la tableta al firmar el firmante. */
	@XmlElement(name = "jpegTemplate")
	private final byte[] jpegTemplate;


	/** Constructor de uso restringido a la serializaci&oacute;n JAXB. */
	@SuppressWarnings("unused")
	private TabletTemplateData() {
		this.tabletType = null;
		this.htmlTemplate = null;
		this.jpegTemplate = null;
	}

	/** Constructor de la clase.
	 * @param type Tipo de tableta de firma
	 * @param template Plantilla HTML a mostrar en la tableta al firmar el firmante.
	 * @param bgJpegImage Imagen JPEG a mostrar en la tableta al firmar el firmante.
	 */
	public TabletTemplateData(final String type,
							final String template,
							final byte[] bgJpegImage) {

		this.tabletType = type;
		this.htmlTemplate = template;
		this.jpegTemplate = bgJpegImage != null ? bgJpegImage.clone() : null;
	}

	@Override
	public String toString() {
		return "Tipo de tableta: " + this.tabletType + "; " + //$NON-NLS-1$ //$NON-NLS-2$
				"htmlTemplate=" + (this.htmlTemplate == null ? "No" : "Si") + "; " + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				"jpegTemplate=" + (this.htmlTemplate == null && this.jpegTemplate != null ? "Si" : "No") +";"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

	/** Obtiene el tipo de tableta de firma para el que se ha definido una plantilla.
	 * @return 	tipo de tableta de firma para el que se ha definido una plantilla. */
	public String getTabletType() {
		return this.tabletType;
	}

	/** Obtiene la plantilla HTML a mostrar en la tableta al firmar el firmante.
	 * @return plantilla HTML a mostrar en la tableta al firmar el firmante.*/
	String getHtmlTemplate() {
		return this.htmlTemplate;
	}

	/** Obtiene la imagen JPEG a mostrar en la tableta al firmar el firmante. */
	byte[] getJpegTemplate() {
		return this.jpegTemplate;
	}

	/** Obtiene la plantilla HTML para un tipo de tableta dado.
	 * @param type Tipo de tableta del que queremos obtener la plantilla.
	 * @param availableTemplateList Lista de tipos de tabletas con sus plantillas correspondientes.
	 * @return Plantilla HTML para el tipo de tableta seleccionado.
	 * */
	static String getTemplateHtml(final List<TabletTemplateData> availableTemplateList, final String type) {
		for (int i = 0; i < availableTemplateList.size(); i++) {
			TabletTemplateData ttd = availableTemplateList.get(i);
			if(ttd.getTabletType().equals(type) && ttd.getHtmlTemplate() != null)	{
				return ttd.getHtmlTemplate();
			}
		}

		return null;
	}
	/** Obtiene la plantilla JPEG para un tipo de tableta dado.
	 * @param type Tipo de tableta del que queremos obtener la plantilla.
	 * @param availableTemplateList Lista de tipos de tabletas con sus plantillas correspondientes.
	 * @return Plantilla JPEG para el tipo de tableta seleccionado.
	 * */
	static byte[] getTemplateJPEG(final List<TabletTemplateData> availableTemplateList, final String type) {
		for (int i = 0; i < availableTemplateList.size(); i++) {
			TabletTemplateData ttd = availableTemplateList.get(i);
			if(ttd.getTabletType().equals(type) && ttd.getJpegTemplate() != null)	{
				return ttd.getJpegTemplate();
			}
		}

		return null;
	}

}
