package es.gob.afirma.standalone.ui;

import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.ImageIcon;

import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.Platform;

/**
 * Clase para obtener informaci&oacute;n sobre un documento.
 */
public class DataFileAnalizer {

	private static final int DEFAULT_ICON_SIZE = 64;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final Set<String> EXECUTABLE_EXTENSIONS;
    static {
    	EXECUTABLE_EXTENSIONS = new HashSet<>();
    	EXECUTABLE_EXTENSIONS.add("ACTION"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("APK"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("APP"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("BAT"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("BIN"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("CMD"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("COM"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("COMMAND"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("CPL"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("CSH"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("EXE"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("GADGET"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("INF1"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("INS"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("INX"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("IPA"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("ISU"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("JOB"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("JSE"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("KSH"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("LNK"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("MSC"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("MSI"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("MSP"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("MST"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("OSX"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("OUT"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("PAF"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("PIF"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("PRG"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("PS1"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("REG"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("RGS"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("RUN"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("SCR"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("SCT"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("SHB"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("SHS"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("U3P"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("VB"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("VBE"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("VBS"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("VBSCRIPT"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("WORKFLOW"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("WS"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("WSF"); //$NON-NLS-1$
    	EXECUTABLE_EXTENSIONS.add("WSH"); //$NON-NLS-1$
    }

	private final byte[] data;

	/**
	 * Construye el analizador para la obtenci&oacute;n de datos de un fichero.
	 * @param data
	 */
	public DataFileAnalizer(byte[] data) {
		this.data = data;
	}

	DataFileInfo analize() {

		final MimeHelper mimeHelper = new MimeHelper(this.data);

		final DataFileInfo info = new DataFileInfo();
		info.setExtension(mimeHelper.getExtension());
		info.setDescription(mimeHelper.getDescription());
		info.setSize(this.data.length);
		info.setIcon(getIcon(info.getExtension()));
		info.setData(this.data);

		return info;
	}

	/**
	 * Obtiene el icono de la imagen
	 * @param extension
	 * @return
	 */
    private static BufferedImage getIcon(final String extension) {

    	// Si no sabemos el tipo de fichero, no podemos obtener su icono
    	if (extension == null) {
    		return loadDefaultIcon();
    	}

    	File tempFile;
    	try {
    		tempFile = File.createTempFile("temp-", "." + extension);  //$NON-NLS-1$ //$NON-NLS-2$
    		try (FileOutputStream fos = new FileOutputStream(tempFile)) {
    			final byte[] tempData = extension.getBytes();
    			fos.write(tempData, 0, tempData.length);
    		}
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se pudo crear el temporal para el calculo del icono del fichero: " + e); //$NON-NLS-1$
    		return loadDefaultIcon();
		}

    	final ImageIcon imageIcon;

    	// Extraccion del icono del tipo de fichero desde Windows
    	if (Platform.getOS() == Platform.OS.WINDOWS) {
    		final Icon icon = javax.swing.filechooser.FileSystemView.getFileSystemView()
    				.getSystemIcon(tempFile);

    		imageIcon = iconToImage(icon);
    	}
    	// Extraccion del icono del tipo de fichero desde el resto de sistemas
    	else {
    		final javax.swing.JFileChooser fc = new javax.swing.JFileChooser();
    		imageIcon = (ImageIcon) fc.getUI().getFileView(fc).getIcon(tempFile);
    	}

    	return loadAndResizeIcon(imageIcon, DEFAULT_ICON_SIZE, DEFAULT_ICON_SIZE);
    }

    /**
     * Carga la imagen que se debe mostrar por defecto para los ficheros de los que no
     * se pueda obtener el icono o sean de un tipo no reconocido.
     * @return Imagen del icono por defecto para los ficheros.
     */
    private static BufferedImage loadDefaultIcon() {
    	BufferedImage defaultIcon;
    	try (InputStream is = ClassLoader.getSystemResourceAsStream("resources/default_file_ico.png")) { //$NON-NLS-1$
    		defaultIcon = ImageIO.read(is);
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se pudo cargar la imagen del icono por fichero"); //$NON-NLS-1$
    		return null;
		}
    	return defaultIcon;
    }

    private static ImageIcon iconToImage(Icon icon) {

    	// Si el icono ya era una imagen, la devolvemos
    	if (icon instanceof ImageIcon) {
    		return (ImageIcon) icon;
    	}

    	// Creamos la imagen
    	final int w = icon.getIconWidth();
    	final int h = icon.getIconHeight();
    	final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
    	final GraphicsDevice gd = ge.getDefaultScreenDevice();
    	final GraphicsConfiguration gc = gd.getDefaultConfiguration();
    	final BufferedImage image = gc.createCompatibleImage(w, h);
    	final Graphics2D g = image.createGraphics();
    	icon.paintIcon(null, g, 0, 0);
    	g.dispose();

    	return new ImageIcon(image);
    }

    private static BufferedImage loadAndResizeIcon(ImageIcon imageIcon, int width, int height) {

    	Image image;

    	// Si la imagen ya tiene el tamano solicitado, no la redimensionaremos
    	if (imageIcon.getIconWidth() == width && imageIcon.getIconHeight() == height) {
    		// Si ya esta cargada, la devolvemos directamente
    		if (imageIcon.getImage() instanceof BufferedImage) {
    			return (BufferedImage) imageIcon.getImage();
    		}
    		image = imageIcon.getImage();
    	}
    	else {
    		image = imageIcon.getImage().getScaledInstance(width, height, Image.SCALE_SMOOTH);
    	}

    	// Cargamos la imagen
    	final BufferedImage bImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    	final Graphics2D bGr = bImage.createGraphics();
    	bGr.drawImage(image, 0, 0, null);
    	bGr.dispose();

    	return bImage;
    }

    /**
     * Identifica si la extensi&oacute;n de fichero indicada se corresponde con la de un ejecutable
     * nativo.
     * @param ext Extensi&oacute;n de fichero.
     * @return {@code true} si la extensi&oacute;n es la de un ejecutable, {@code false} en caso
     * contrario.
     */
    static boolean isExecutable(String ext) {
    	return ext != null && EXECUTABLE_EXTENSIONS.contains(ext.toUpperCase());
    }

}
