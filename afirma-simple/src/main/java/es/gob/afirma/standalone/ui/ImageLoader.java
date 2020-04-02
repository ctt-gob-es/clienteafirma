package es.gob.afirma.standalone.ui;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.InputStream;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

public class ImageLoader {

	public static final int XLARGE_ICON = 4;
	public static final int LARGE_ICON = 3;
	public static final int MEDIUM_ICON = 2;
	public static final int SMALL_ICON = 1;

	public static ImageIcon loadIcon(final String filename) {

		ImageIcon icon = null;
		final Image image = loadImage(filename);
		if (image != null) {
			icon = new ImageIcon(image);
		}
		return icon;
	}

	public static ImageIcon loadIcon(final String filename, final int size) {

		ImageIcon icon = null;
		BufferedImage image = loadImage(filename);
		if (image != null) {
			final Dimension dim = getDimension(size);
			if (dim != null) {
				image = resize(image, dim);
			}
			icon = new ImageIcon(image);
		}
		return icon;
	}

	public static BufferedImage loadImage(final String filename) {
		BufferedImage image = null;
		try (final InputStream is = ImageLoader.class.getResourceAsStream("/resources/" + filename)) { //$NON-NLS-1$
			image = ImageIO.read(is);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se pudo cargar la imagen '" + filename + "': " + e);  //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
		return image;
	}

	private static Dimension getDimension(final int size) {
		Dimension dim;
		switch (size) {
		case XLARGE_ICON:
			dim = new Dimension(256, 256);
			break;
		case LARGE_ICON:
			dim = new Dimension(128, 128);
			break;
		case MEDIUM_ICON:
			dim = new Dimension(64, 64);
			break;
		case SMALL_ICON:
			dim = new Dimension(32, 32);
			break;
		default:
			dim = null;
		}
		return dim;
	}

    private static BufferedImage resize(final BufferedImage img, final Dimension size) {
        final Image tmp = img.getScaledInstance((int) size.getWidth(), (int) size.getHeight(), Image.SCALE_SMOOTH);
        final BufferedImage resized = new BufferedImage((int) size.getWidth(), (int) size.getHeight(), BufferedImage.TYPE_INT_ARGB);
        final Graphics2D g2d = resized.createGraphics();
        g2d.drawImage(tmp, 0, 0, null);
        g2d.dispose();
        return resized;
    }
}
