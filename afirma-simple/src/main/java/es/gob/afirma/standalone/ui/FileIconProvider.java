package es.gob.afirma.standalone.ui;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

/**
 * Clase para la carga de iconos asociados a tipos de fichero por extensi&oacute;n.
 */
public class FileIconProvider {

	private static final String DEFAULT_ICON = "default_icon.png"; //$NON-NLS-1$
	private static final String XML_ICON = "xml_icon.png"; //$NON-NLS-1$
	private static final String PDF_ICON = "pdf_icon.png"; //$NON-NLS-1$
	private static final String TXT_ICON = "txt_icon.png"; //$NON-NLS-1$
	private static final String IMAGE_ICON = "image_icon.png"; //$NON-NLS-1$
	private static final String ODF_ICON = "odf_icon.png"; //$NON-NLS-1$
	private static final String OOXML_ICON = "ooxml_icon.png"; //$NON-NLS-1$

	private static final Map<String, String> fileTypes;

	static {
		fileTypes = new HashMap<>();
		fileTypes.put("xml", XML_ICON); //$NON-NLS-1$
		fileTypes.put("pdf", PDF_ICON); //$NON-NLS-1$
		fileTypes.put("txt", TXT_ICON); //$NON-NLS-1$
		fileTypes.put("docx", OOXML_ICON); //$NON-NLS-1$
		fileTypes.put("xlsx", OOXML_ICON); //$NON-NLS-1$
		fileTypes.put("pptx", OOXML_ICON); //$NON-NLS-1$
		fileTypes.put("ppsx", OOXML_ICON); //$NON-NLS-1$
		fileTypes.put("odt", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("odp", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("ods", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("odg", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("odc", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("odf", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("odb", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("odi", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("odm", ODF_ICON); //$NON-NLS-1$
		fileTypes.put("jpg", IMAGE_ICON); //$NON-NLS-1$
		fileTypes.put("jpeg", IMAGE_ICON); //$NON-NLS-1$
		fileTypes.put("png", IMAGE_ICON); //$NON-NLS-1$
		fileTypes.put("gif", IMAGE_ICON); //$NON-NLS-1$
	}

	private FileIconProvider() {
		// No se puede instanciar la clase
	}

	/**
	 * Obtiene el icono por defecto asociado a una extensi&oacute;n de fichero concreta.
	 * Si la extensi&oacute;n no se encuentra entre las soportadas o se proporciona una
	 * extensi&oacute;n nula, se devolver&aacute; un icono de fichero gen&eacute;rico.
	 * @param ext Extensi&oacute;n de fichero.
	 * @return Imagen del icono asociado o la del icono gen&eacute;rico.
	 * @throws IOException Cuando ocurre alghun error durante la carga del icono.
	 */
	public static BufferedImage getIcon(final String ext) throws IOException {
		if (ext == null || !fileTypes.containsKey(ext)) {
			return loadIconImage(DEFAULT_ICON);
		}
		return loadIconImage(fileTypes.get(ext));
	}

	/**
	 * Devuelve el ic&oacute;no de fichero gen&eacute;rico.
	 * @param iconName Nombre del fichero de icono.
	 * @return Imagen del icono o {@code null} si no se pudo cargar.
	 * @throws IOException Cuando ocurre alghun error durante la carga de la imagen.
	 */
	private static BufferedImage loadIconImage(final String iconName) throws IOException {
    	BufferedImage iconImage;
    	try (InputStream is = ClassLoader.getSystemResourceAsStream("resources/file_icons/" + iconName)) { //$NON-NLS-1$
    		iconImage = ImageIO.read(is);
    	}
    	catch (final Exception e) {
    		throw new IOException("No se pudo cargar la imagen del icono " + iconName, e); //$NON-NLS-1$
		}
    	return iconImage;
	}

}
