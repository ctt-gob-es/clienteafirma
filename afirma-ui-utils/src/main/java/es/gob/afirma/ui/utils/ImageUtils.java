package es.gob.afirma.ui.utils;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class ImageUtils {

	private static Boolean imageIOExist = null;

	/**
	 * Normaliza la imagen para hacerla compatible con el PDF.
	 * @param image Imagen que se desea codificar.
	 * @return Base 64 de la imagen JPEG.
	 * @throws IOException Cuando ocurre un error en la codificaci&oacute;n o
	 * transformaci&oacute;n de la imagen.
	 */
	public static byte[] normalizeImageToPdf(final byte[] image) throws IOException {

		if (!canNormalize()) {
			throw new UnsupportedOperationException("El entorno actual no soporta esta operacion"); //$NON-NLS-1$
		}

		BufferedImage loadedImage;
		try (final InputStream is = new ByteArrayInputStream(image)) {
			loadedImage = javax.imageio.ImageIO.read(is);
		}

		byte[] imageEncoded;
		try (final ByteArrayOutputStream osImage = new ByteArrayOutputStream()) {
			// Eliminamos las transparencias de la imagen
			final BufferedImage opaqueImage = removeAlphaChannel(loadedImage);
			// Convertimos la imagen a JPEG
			if (!javax.imageio.ImageIO.write(opaqueImage, "jpg", osImage)) { //$NON-NLS-1$
				throw new IOException("No se ha podido convertir la imagen a JPEG"); //$NON-NLS-1$
			}
			// Codificamos la imagen
			imageEncoded = osImage.toByteArray();
		}
        catch (final Exception e) {
        	throw new IOException("No ha podido decodificar la imagen", e); //$NON-NLS-1$
        }
		return imageEncoded;
	}

	/**
	 * Elimina las transparencias de una imagen.
	 * @param img Imagen de la que eliminar las transparencias.
	 * @return Imagen sin transparencias.
	 */
	private static BufferedImage removeAlphaChannel(final BufferedImage img) {
	    if (!img.getColorModel().hasAlpha()) {
	        return img;
	    }

	    final BufferedImage target = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
	    final Graphics2D g = target.createGraphics();
	    // g.setColor(new Color(color, false));
	    g.fillRect(0, 0, img.getWidth(), img.getHeight());
	    g.drawImage(img, 0, 0, null);
	    g.dispose();

	    return target;
	}

	/**
	 * Comprueba si se puede normalizar la imagen en el entorno actual.
	 * @return {@code true} s el entorno permite que se normalice la imagen, {@code false}
	 * en caso contrario.
	 */
	private static boolean canNormalize() {
		if (imageIOExist == null) {
			try {
				Class.forName("javax.imageio.ImageIO", false, ImageUtils.class.getClassLoader()); //$NON-NLS-1$
				imageIOExist = Boolean.TRUE;
			}
			catch (final Exception e) {
				imageIOExist = Boolean.FALSE;
			}
		}
		return imageIOExist.booleanValue();
	}
}
