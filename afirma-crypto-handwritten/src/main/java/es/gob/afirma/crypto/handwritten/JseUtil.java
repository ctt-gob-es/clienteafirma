package es.gob.afirma.crypto.handwritten;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Properties;

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

	private static final String FOOTER_PROP_FILE = "/footerText.properties"; //$NON-NLS-1$
	private static final String HEADER_PROP_FILE = "/headerText.properties"; //$NON-NLS-1$


	private JseUtil() {
		// No instanciable
	}

	static {
		Handler.install();
	}

	/** A&ntilde;ade un pie de texto a una imagen JPEG.
	 * @param jpegImage Imagen JPEG de entrada
	 * @param footerTxt Texto a a&ntilde;adir como pie.
	 * @return Imagen JPEG con el pie a&ntilde;adido.
	 * @throws IOException Si hay errores durante el proceso. */
	public static byte[] addFooter(final byte[] jpegImage, final String footerTxt) throws IOException {

		Properties properties = new Properties();
		// Cargamos el fichero de propiedades del estilo de pie de firma
		properties.load(
			JseUtil.class.getResourceAsStream(
				FOOTER_PROP_FILE
			)
		);

		BufferedImage bi = jpeg2BufferedImage(jpegImage, true);

		// Se crea una nueva imagen formada por la firma en jpg y el footer
		BufferedImage biWithFooter = new BufferedImage(
											bi.getWidth(),
											bi.getHeight() + Integer.parseInt(properties.getProperty("font.size")) * 2 + 2,  //$NON-NLS-1$
											BufferedImage.TYPE_BYTE_BINARY
										);

		// Definimos un grafico para insertar la imagen y el pie de firma dentro de la nueva imagen creada
		Graphics2D g = biWithFooter.createGraphics();

		// Definimos el fondo blanco de la imagen
		g.setBackground(Color.WHITE);
		g.clearRect(0, 0, biWithFooter.getWidth(), biWithFooter.getHeight());

		// Dibujamos la firma
		g.drawImage(bi, null, 0,  0);

		// Definimos la fuente del pie de firma
		g.setFont(
			new Font(
				properties.getProperty("font.name"), //$NON-NLS-1$
				Font.PLAIN,
				Integer.parseInt(properties.getProperty("font.size")) //$NON-NLS-1$
			)
		);

		// El texto en color negro
		g.setColor(Color.BLACK);

		// Insertamos el pie de firma
		g.drawString(
			footerTxt,
			Integer.parseInt(properties.getProperty("margin.left")), //$NON-NLS-1$
			biWithFooter.getHeight() - g.getFont().getSize()
		);

		return bufferedImage2Jpeg(biWithFooter);
	}

	/** A&ntilde;ade una cabecera texto a una imagen JPEG.
	* @param jpegImage Imagen JPEG de entrada
	 * @param headerTxt Texto a a&ntilde;adir como cabecera.
	 * @return Imagen JPEG con la cabecera a&ntilde;adido.
	 * @throws IOException Si hay errores durante el proceso. */
	public static byte[] addHeader(final byte[] jpegImage, final String headerTxt) throws IOException {

		Properties properties = new Properties();
		// Cargamos el fichero de propiedades del estilo de la cabecera de firma
		properties.load(
			JseUtil.class.getResourceAsStream(
				HEADER_PROP_FILE
			)
		);

		BufferedImage bi = jpeg2BufferedImage(jpegImage, true);

		// Se crea una nueva imagen formada por la firma en jpg y el header
		BufferedImage biWithHeader = new BufferedImage(
											bi.getWidth(),
											bi.getHeight() + Integer.parseInt(properties.getProperty("font.size")) * 2 + 2,  //$NON-NLS-1$
											BufferedImage.TYPE_BYTE_BINARY
										);

		// Definimos un grafico para insertar la cabecera y la imagen de firma dentro de la nueva imagen creada
		Graphics2D g = biWithHeader.createGraphics();

		// Definimos el fondo blanco de la imagen
		g.setBackground(Color.WHITE);
		g.clearRect(0, 0, biWithHeader.getWidth(), biWithHeader.getHeight());

		// Definimos la fuente del pie de firma
		g.setFont(
			new Font(
				properties.getProperty("font.name"), //$NON-NLS-1$
				Font.PLAIN,
				Integer.parseInt(properties.getProperty("font.size")) //$NON-NLS-1$
			)
		);

		// El texto en color negro
		g.setColor(Color.BLACK);

		// Insertamos el pie de firma
		g.drawString(
			headerTxt,
			Integer.parseInt(properties.getProperty("margin.left")), //$NON-NLS-1$
			g.getFont().getSize() + 2
		);


		// Dibujamos la firma
		g.drawImage(bi, null, 0,  g.getFont().getSize() * 2);



		return bufferedImage2Jpeg(biWithHeader);

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

    private static final class CustomHTMLEditorKit extends HTMLEditorKit {

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
