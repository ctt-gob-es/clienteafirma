package es.gob.afirma.crypto.handwritten;

import java.awt.Color;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.plugins.jpeg.JPEGImageWriteParam;
import javax.imageio.spi.IIORegistry;
import javax.imageio.spi.ImageWriterSpi;
import javax.imageio.spi.ServiceRegistry.Filter;
import javax.imageio.stream.MemoryCacheImageOutputStream;
import javax.swing.JEditorPane;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.ImageView;

import es.gob.afirma.crypto.handwritten.data.Handler;

/** Utilidades varias dependientes de JSE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class JseUtil {

	private JseUtil() {
		// No instanciable
	}

	static {
		Handler.install();
	}

	/** Convierte una <code>BufferedImage</code> de AWT en una imagen JPEG.
	 * @param img <code>BufferedImage</code> de origen
	 * @return Imagen JPEG
	 * @throws IOException Si hay problemas en el tratamiento de la imagen. */
	public static byte[] bufferedImage2Jpeg(final BufferedImage img) throws IOException {
		final IIORegistry registry = IIORegistry.getDefaultInstance();
		final Iterator<ImageWriterSpi> services = registry.getServiceProviders(
			ImageWriterSpi.class,
			new Filter() {

				@Override
				public boolean filter(final Object provider) {
					if (!(provider instanceof ImageWriterSpi)) {
						return false;
					}
		            final ImageWriterSpi writerSPI = (ImageWriterSpi) provider;
		            final String[] formatNames = writerSPI.getFormatNames();
		            for (final String formatName : formatNames) {
		                if (formatName.equalsIgnoreCase("JPEG")) { //$NON-NLS-1$
		                    return true;
		                }
		            }
		            return false;
				}
			},
			true
		);
		if (!services.hasNext()) {
			throw new IOException("No hay un proveedor para el manejo de JPEG"); //$NON-NLS-1$
		}
		final ImageWriterSpi writerSpi = services.next();
		final ImageWriter writer = writerSpi.createWriterInstance();

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final MemoryCacheImageOutputStream os = new MemoryCacheImageOutputStream(baos);
		writer.setOutput(os);

		final JPEGImageWriteParam jpegParams = new JPEGImageWriteParam(null);
		jpegParams.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
		jpegParams.setCompressionQuality(1f);

		writer.write(null, new IIOImage(img, null, null), jpegParams);

		final byte[] ret = baos.toByteArray();
		os.close();

		return ret;
	}

	/** Convierte una p&aacute;gina HTML en una imagen.
	 * @param html HTML origen.
	 * @param width Ancho de la imagen deseada.
	 * @param height Alto de la imagen deseada.
	 * @return Imagen de las dimensiones indicadas con la representaci&oacute;n gr&aacute;fica del HTML.
	 * @throws IOException Si hay problemas en el tratamiento de la imagen. */
	public static Image html2Image(final String html,
			                           final int width,
			                           final int height) throws IOException {

		final BufferedImage image = GraphicsEnvironment.getLocalGraphicsEnvironment()
                .getDefaultScreenDevice()
                .getDefaultConfiguration()
                .createCompatibleImage(width, height);

		final JEditorPane jep = new JEditorPane();
		jep.setEditorKit(new CustomHTMLEditorKit());
		jep.setContentType("text/html"); //$NON-NLS-1$
		jep.setText(html);
		jep.setSize(width, height);
		jep.print(image.createGraphics());
		return image;
	}

    /** Convierte una imagen JPEG en una <code>BufferedImage</code> de AWT.
	 * @param jpegImage Imagen JPEG de origen.
	 * @param bw Si se debe o no convertir la imagen a blanco y negro.
	 * @return <code>BufferedImage</code>.
	 * @throws IOException Si hay problemas en el tratamiento de la imagen. */
	public static BufferedImage jpeg2BufferedImage(final byte[] jpegImage, final boolean bw) throws IOException {
		final BufferedImage original = ImageIO.read(new ByteArrayInputStream(jpegImage));
		if (!bw) {
			return original;
		}
		final BufferedImage binarized = new BufferedImage(original.getWidth(), original.getHeight(),BufferedImage.TYPE_BYTE_BINARY);
        int newPixel;
        final int threshold =230;
        for (int i=0; i<original.getWidth(); i++) {
           for (int j=0; j<original.getHeight(); j++) {
                 final int alpha = new Color(original.getRGB(i, j)).getAlpha();
                 if(new Color(original.getRGB(i, j)).getRed() > threshold) {
                       newPixel = 255;
                 }
                 else {
                       newPixel = 0;
                 }
                 newPixel = colorToRGB(alpha, newPixel, newPixel, newPixel);
                 binarized.setRGB(i, j, newPixel);
           }
        }
		return binarized;
	}

    private static int colorToRGB(final int alpha, final int red, final int green, final int blue) {
        int newPixel = 0;
        newPixel += alpha;
        newPixel = newPixel << 8;
        newPixel += red; newPixel = newPixel << 8;
        newPixel += green; newPixel = newPixel << 8;
        newPixel += blue;
        return newPixel;
    }


    private static class CustomViewFactory extends HTMLEditorKit.HTMLFactory {

	    CustomViewFactory() {
			// vacio
		}

		@Override
	     public View create (final Element elem) {
	         final View view = super.create (elem);
	         if (view instanceof ImageView) {
	             ((ImageView) view).setLoadsSynchronously(true);
	         }
	         return view;
	     }
    }

    private static class CustomHTMLEditorKit extends HTMLEditorKit {

    	private static final long serialVersionUID = -916670986056265502L;

    	CustomHTMLEditorKit() {
			// Vacio
		}

		@Override
    	public ViewFactory getViewFactory () {
    		return new CustomViewFactory ();
    	}
    }
}
