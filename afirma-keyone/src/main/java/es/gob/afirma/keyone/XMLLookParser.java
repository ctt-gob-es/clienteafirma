package es.gob.afirma.keyone;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.standalone.ui.pdf.ColorResource;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanelPreview;

public final class XMLLookParser {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private final String xml;
	private BufferedImage image;
	private int rectWidth = 0;
	private int rectHeight = 0;
	private String location;
	private String reason;
	private String contactInfo;
	private final SignatureField field;
	private final X509Certificate cer;
	private final Properties prop;
	public Properties getProperties() {
		return this.prop;
	}

	public XMLLookParser(final String xml, final SignatureField field, final Properties p, final PrivateKeyEntry pke) {
		this.xml = xml;
		this.image = null;
		this.prop = p;
		this.field = field;
		this.cer = (X509Certificate) pke.getCertificate();
	}

	public Properties parse() throws XMLException {
		if (this.xml == null) {
			return null;
		}

		try(final InputStream in = new ByteArrayInputStream(this.xml.getBytes(StandardCharsets.UTF_8))) {
			final DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			final DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			final Document doc = docBuilder.parse(in);

			if (this.field == null) {
				final Node page = doc.getElementsByTagName("signaturePosition").item(0); //$NON-NLS-1$
				if (page != null) {
					this.prop.setProperty("imagePage", page.getTextContent()); //$NON-NLS-1$
				}
				final Node rect = doc.getElementsByTagName("Rect").item(0); //$NON-NLS-1$
				if (rect != null) {
					setRectProp(rect);
				}
			}
			else {
				this.prop.setProperty("signatureField", this.field.getName()); //$NON-NLS-1$
				this.rectWidth = this.field.getSignaturePositionOnPageUpperRightX() - this.field.getSignaturePositionOnPageLowerLeftX();
				this.rectHeight = this.field.getSignaturePositionOnPageUpperRightY() - this.field.getSignaturePositionOnPageLowerLeftY();
			}

			final Node params = doc.getElementsByTagName("params").item(0); //$NON-NLS-1$
			if (params != null) {
				setParams(params);
			}
			final Node background = doc.getElementsByTagName("Background").item(0); //$NON-NLS-1$
			if (background != null) {
				setBackgroundImage(background);
			}
			final Node foreground = doc.getElementsByTagName("Foreground").item(0); //$NON-NLS-1$
			if (foreground != null) {
				setForeground(foreground);
			}

			if (this.image != null) {
				this.prop.setProperty("signatureRubricImage", getImageInBase64(this.image)); //$NON-NLS-1$
			}
			return this.prop;
		}
		catch (final Exception e) {
			LOGGER.severe("Error analizando el xml de apariencia: " + e); //$NON-NLS-1$
			throw new XMLException("Error analizando el xml de apariencia: " + e, e); //$NON-NLS-1$
		}
	}

	private void setRectProp(final Node rect) {
		final NamedNodeMap rectAttrib = rect.getAttributes();
		if (rectAttrib != null && rectAttrib.getLength() == 4) {
			final String x0 = rectAttrib.getNamedItem("x0").getTextContent(); //$NON-NLS-1$
			final String y0 = rectAttrib.getNamedItem("y0").getTextContent(); //$NON-NLS-1$
			final String x1 = rectAttrib.getNamedItem("x1").getTextContent(); //$NON-NLS-1$
			final String y1 = rectAttrib.getNamedItem("y1").getTextContent(); //$NON-NLS-1$

			this.prop.setProperty("signaturePositionOnPageLowerLeftX" , x0); //$NON-NLS-1$
			this.prop.setProperty("signaturePositionOnPageLowerLeftY" , y0); //$NON-NLS-1$
			this.prop.setProperty("signaturePositionOnPageUpperRightX" , x1); //$NON-NLS-1$
			this.prop.setProperty("signaturePositionOnPageUpperRightY" , y1); //$NON-NLS-1$

			this.rectWidth = Integer.parseInt(x1) - Integer.parseInt(x0);
			this.rectHeight = Integer.parseInt(y1) - Integer.parseInt(y0);
		}
	}

	private void setParams(final Node params) {
		final NodeList paramsList = params.getChildNodes();
		for (int i = 0; i < paramsList.getLength(); i++) {
			final Node node = paramsList.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				final Element element = (Element) node;
	        	if (element.getNodeName().equals("location")) { //$NON-NLS-1$
	        		this.location = element.getTextContent();
	        	}
	        	else if (element.getNodeName().equals("reason")) { //$NON-NLS-1$
	        		this.reason = element.getTextContent();
	        	}
	        	else if (element.getNodeName().equals("contactInfo")) { //$NON-NLS-1$
	        		this.contactInfo = element.getTextContent();
	        	}
			}
		}
	}

	private void setBackgroundImage(final Node background) throws DOMException, IOException, XMLException {
		final NodeList backgroundList = background.getChildNodes();
		for (int i = 0; i < backgroundList.getLength(); i++) {
			final Node backImage = backgroundList.item(i);
			if (backImage != null && backImage.getNodeName().equals("image")) { //$NON-NLS-1$
				final NodeList list = backImage.getChildNodes();
				final Element imageElement = (Element) backImage;
				BufferedImage im = null;
				int width = 0;
				int height = 0;
				int posX = 0;
				int posY = 0;
				final String encodeType = imageElement.getAttribute("encodeType"); //$NON-NLS-1$
				for (int j = 0; j < list.getLength(); j++) {
					final Node node = list.item(j);
					if (node.getNodeType() == Node.ELEMENT_NODE) {
						final Element element = (Element) node;
			        	if (element.getNodeName().equals("data")) { //$NON-NLS-1$
			        		if (encodeType.equals("uri")) { //$NON-NLS-1$
								try {
									im = ImageIO.read(new File(element.getTextContent()));
								}
								catch (final Exception e) {
									LOGGER.severe("Error extrayendo propiedades del fondo: " + e); //$NON-NLS-1$
									throw new XMLException("Error extrayendo propiedades del fondo: " + e, e); //$NON-NLS-1$
								}
			        		}
			        		else if (encodeType.equals("base64")) { //$NON-NLS-1$
			        			im = getImageFromBase64(element.getTextContent());
			        		}
			        	}
			        	if (element.getNodeName().equals("imageSize")) { //$NON-NLS-1$
			        		final NamedNodeMap imageSize = element.getAttributes();
			        		if (imageSize != null && imageSize.getLength() == 2) {
			        			width = Integer.parseInt(imageSize.getNamedItem("width").getTextContent()); //$NON-NLS-1$
			        			height = Integer.parseInt(imageSize.getNamedItem("height").getTextContent()); //$NON-NLS-1$
			        		}
			        	}
			        	if (element.getNodeName().equals("position")) { //$NON-NLS-1$
			        		final NamedNodeMap imageSize = element.getAttributes();
			        		if (imageSize != null && imageSize.getLength() == 2) {
			        			posX = Integer.parseInt(imageSize.getNamedItem("x").getTextContent()); //$NON-NLS-1$
			        			posY = Integer.parseInt(imageSize.getNamedItem("y").getTextContent()); //$NON-NLS-1$
			        		}
			        	}
					}
				}
				if (im != null) {
					paintImage(im, width, height, posX, posY);
					saveImageProperties();
				}
			}
		}
	}

	private void setForeground(final Node foreground) throws DOMException, IOException, XMLException {
		final NodeList foregroundList = foreground.getChildNodes();
		for (int i = 0; i < foregroundList.getLength(); i++) {
			final Node foreItem = foregroundList.item(i);
			if (foreItem != null && foreItem.getNodeName().equals("image")) { //$NON-NLS-1$
				final NodeList imageList = foreItem.getChildNodes();
				final Element imageElement = (Element) foreItem;
				BufferedImage im = null;
				int width = 0;
				int height = 0;
				int posX = 0;
				int posY = 0;
				final String encodeType = imageElement.getAttribute("encodeType"); //$NON-NLS-1$
				for (int j = 0; j < imageList.getLength(); j++) {
					final Node node = imageList.item(j);
					if (node.getNodeType() == Node.ELEMENT_NODE) {
						final Element element = (Element) node;
			        	if (element.getNodeName().equals("data")) { //$NON-NLS-1$
			        		if (encodeType.equals("uri")) { //$NON-NLS-1$
								try {
									im = ImageIO.read(new File(element.getTextContent()));
								}
								catch (final Exception e) {
									LOGGER.severe("Error extrayendo propiedades del frente: " + e); //$NON-NLS-1$
									throw new XMLException("Error extrayendo propiedades del frente: " + e, e); //$NON-NLS-1$
								}
			        		}
			        		else if (encodeType.equals("base64")) { //$NON-NLS-1$
			        			im = getImageFromBase64(element.getTextContent());
			        		}
			        	}
			        	if (element.getNodeName().equals("imageSize")) { //$NON-NLS-1$
			        		final NamedNodeMap imageSize = element.getAttributes();
			        		if (imageSize != null && imageSize.getLength() == 2) {
			        			width = Integer.parseInt(imageSize.getNamedItem("width").getTextContent()); //$NON-NLS-1$
			        			height = Integer.parseInt(imageSize.getNamedItem("height").getTextContent()); //$NON-NLS-1$
			        		}
			        	}
			        	if (element.getNodeName().equals("position")) { //$NON-NLS-1$
			        		final NamedNodeMap imageSize = element.getAttributes();
			        		if (imageSize != null && imageSize.getLength() == 2) {
			        			posX = Integer.parseInt(imageSize.getNamedItem("x").getTextContent()); //$NON-NLS-1$
			        			posY = Integer.parseInt(imageSize.getNamedItem("y").getTextContent()); //$NON-NLS-1$
			        		}
			        	}
					}
				}
				if (im != null) {
					if (this.image == null) {
						saveImageProperties();
					}
					paintImage(im, width, height, posX, posY);
				}
			}
			if (foreItem != null && foreItem.getNodeName().equals("text")) { //$NON-NLS-1$
				setText(foreItem);
			}
		}
	}

	private void setText(final Node textItem) throws XMLException {
		Color color = Color.BLACK;
		int fontSize = 14;
		int x = 0;
		int y = 0;
		String text = null;
		final NodeList textList = textItem.getChildNodes();
		for (int i = 0; i < textList.getLength(); i++) {
			final Node node = textList.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				final Element element = (Element) node;
	        	if (element.getNodeName().equals("properties")) { //$NON-NLS-1$
	        		final NamedNodeMap textPropAttrib = element.getAttributes();
	        		if (textPropAttrib != null && textPropAttrib.getLength() == 2) {
	        			color = getTextColor(textPropAttrib);
	        			fontSize = Integer.parseInt(textPropAttrib.getNamedItem("fontSize").getTextContent()); //$NON-NLS-1$
	        		}
	        	}
	        	else if (element.getNodeName().equals("position")) { //$NON-NLS-1$
	        		final NamedNodeMap textPropAttrib = element.getAttributes();
	        		if (textPropAttrib != null && textPropAttrib.getLength() == 2) {
		        		x = Integer.parseInt(textPropAttrib.getNamedItem("x").getTextContent()); //$NON-NLS-1$
		        		y = Integer.parseInt(textPropAttrib.getNamedItem("y").getTextContent()); //$NON-NLS-1$
	        		}
	        	}
	        	else if (element.getNodeName().equals("SignatureInfos")) { //$NON-NLS-1$
	        		text = setSignatureInfo(element);
	        		paintText(text, x, y, color, fontSize);
	        	}
	        	else if (element.getNodeName().equals("ArbitraryText")) { //$NON-NLS-1$
	        		setArbitraryText(element);
	        	}
			}
		}
	}

	private void setArbitraryText(final Node arbitrary) {
		String text = ""; //$NON-NLS-1$
		int x = 0;
		int y = 0;
		int fontSize = 0;
		final NodeList arbitraryList = arbitrary.getChildNodes();
		for (int i = 0; i < arbitraryList.getLength(); i++) {
			final Node node = arbitraryList.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				final Element textItem = (Element) node;
	        	if (textItem.getNodeName().equals("textItem")) { //$NON-NLS-1$
	        		final NodeList itemList = textItem.getChildNodes();
					for (int j = 0; j < itemList.getLength(); j++) {
						final Node item = itemList.item(j);
						if (item.getNodeType() == Node.ELEMENT_NODE) {
							final Element element = (Element) item;
				        	if (element.getNodeName().equals("font")) { //$NON-NLS-1$
				        		final NamedNodeMap textPropAttrib = element.getAttributes();
				        		if (textPropAttrib != null && textPropAttrib.getLength() == 1) {
				        			fontSize = Integer.parseInt(textPropAttrib.getNamedItem("fontSize").getTextContent()); //$NON-NLS-1$
				        		}
				        	}
				        	else if (element.getNodeName().equals("position")) { //$NON-NLS-1$
				        		final NamedNodeMap textPropAttrib = element.getAttributes();
				        		if (textPropAttrib != null && textPropAttrib.getLength() == 2) {
					        		x = Integer.parseInt(textPropAttrib.getNamedItem("x").getTextContent()); //$NON-NLS-1$
					        		y = Integer.parseInt(textPropAttrib.getNamedItem("y").getTextContent()); //$NON-NLS-1$
				        		}
				        	}
				        	else if (element.getNodeName().equals("text")) { //$NON-NLS-1$
				        		text = element.getTextContent();
							}
						}
					}
	        	}
				paintText(text, x, y, null, fontSize);
			}
		}
	}

	private String setSignatureInfo(final Node signatureInfos) {
		String text = ""; //$NON-NLS-1$
		final NodeList signatureInfoList = signatureInfos.getChildNodes();
		for (int i = 0; i < signatureInfoList.getLength(); i++) {
			final Node node = signatureInfoList.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				final Element element = (Element) node;
	        	if (element.getNodeName().equals("signatureInfo")) { //$NON-NLS-1$
	        		final NamedNodeMap textAttrib = element.getAttributes();
	        		if (textAttrib != null && textAttrib.getLength() == 2) {
	        			if (textAttrib.getNamedItem("id").getTextContent().equals("Subject")) { //$NON-NLS-1$ //$NON-NLS-2$
	        				text += textAttrib.getNamedItem("title").getTextContent() //$NON-NLS-1$
	        						+ this.cer.getSubjectDN().getName() + "\n"; //$NON-NLS-1$
	        			}
	        			else if (textAttrib.getNamedItem("id").getTextContent().equals("Issuer")) { //$NON-NLS-1$ //$NON-NLS-2$
	        				text += textAttrib.getNamedItem("title").getTextContent() //$NON-NLS-1$
	        						+ this.cer.getIssuerDN().getName() + "\n"; //$NON-NLS-1$
						}
	        			else if (textAttrib.getNamedItem("id").getTextContent().equals("SerialNumber")) { //$NON-NLS-1$ //$NON-NLS-2$
	        				text += textAttrib.getNamedItem("title").getTextContent() //$NON-NLS-1$
	        						+ this.cer.getSerialNumber() + "\n"; //$NON-NLS-1$
						}
	        			else if (textAttrib.getNamedItem("id").getTextContent().equals("Reason") //$NON-NLS-1$ //$NON-NLS-2$
	        					&& this.reason != null) {
	        				text += textAttrib.getNamedItem("title").getTextContent() //$NON-NLS-1$
	        						+ this.reason + "\n"; //$NON-NLS-1$
						}
	        			else if (textAttrib.getNamedItem("id").getTextContent().equals("Location") //$NON-NLS-1$ //$NON-NLS-2$
	        					&& this.location != null) {
	        				text += textAttrib.getNamedItem("title").getTextContent() //$NON-NLS-1$
	        						+ this.location + "\n"; //$NON-NLS-1$
						}
	        			else if (textAttrib.getNamedItem("id").getTextContent().equals("ContactInfo") //$NON-NLS-1$ //$NON-NLS-2$
	        					&& this.contactInfo != null) {
	        				text += textAttrib.getNamedItem("title").getTextContent() //$NON-NLS-1$
	        						+ this.contactInfo + "\n"; //$NON-NLS-1$
						}
	        			else if (textAttrib.getNamedItem("id").getTextContent().equals("Date")) { //$NON-NLS-1$ //$NON-NLS-2$
	        				text += textAttrib.getNamedItem("title").getTextContent() //$NON-NLS-1$
	        						+ new SimpleDateFormat("yyyy/MM/dd").format(new Date()) + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
						}
		        	}
	        	}
			}
		}
		return text;
	}

	private static Color getTextColor(final NamedNodeMap textPropAttrib) throws XMLException {
		final int[] rgb = new int[3];
		final String rgbString[] = textPropAttrib.getNamedItem("color").getTextContent().split(" "); //$NON-NLS-1$ //$NON-NLS-2$
		for (int j = 0 ; j < 3 ; j++) {
			rgb[j] = Integer.parseInt(rgbString[j]);
		}
		final Color col = new Color(rgb[0], rgb[1], rgb[2]);
		for (final ColorResource color : ColorResource.getAllColorResources()) {
			if (color.getColor().equals(col)) {
				return color.getColor();
			}
		}
		throw new XMLException("El color indicado para el texto del frente no se ecuentra entre los permitidos"); //$NON-NLS-1$
	}

	private void paintImage(final BufferedImage im,
							final int width,
							final int height,
							final int x,
							final int y) {

		final BufferedImage newImage = new BufferedImage(
			this.rectWidth, this.rectWidth, BufferedImage.TYPE_INT_RGB
		);

		final Graphics2D g = newImage.createGraphics();
		if (this.image != null) {
			g.drawImage(this.image, 0, 0, null);
		}
		else {
			g.setBackground(Color.WHITE);
			g.clearRect(0, 0, newImage.getWidth(), newImage.getHeight());
		}
		g.drawImage(
			im,
			x,
			y,
			width,
			height,
			null
		);
		g.dispose();
		this.image = newImage;
	}

	private void paintText(final String text, final int x, final int y0, final Color color, final float size) {
		final BufferedImage newImage = new BufferedImage(
			this.rectWidth, this.rectHeight, BufferedImage.TYPE_INT_RGB
		);

		final Graphics2D g = newImage.createGraphics();

		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
        g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
        g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
		g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HRGB);
		g.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_DISABLE);

		if (this.image != null) {
			g.drawImage(this.image, 0, 0, null);
		}
		else {
			g.setBackground(Color.WHITE);
			g.clearRect(0, 0, newImage.getWidth(), newImage.getHeight());
		}

		g.setFont(new Font("Courier", Font.PLAIN, 12)); //$NON-NLS-1$
		if (color != null) {
			g.setColor(color);
		}
		else {
			g.setColor(Color.black);
		}
		if (size != 0) {
			g.setFont(g.getFont().deriveFont(size));
		}
		int textLength;
		int y = y0;
	    for (final String line : text.split("\n")) { //$NON-NLS-1$
	    	textLength = g.getFontMetrics().stringWidth(line);
	    	if (textLength > newImage.getWidth() - x) {
	    		final String lineWrapped = SignPdfUiPanelPreview.breakLines(line, newImage.getWidth() - x, g.getFontMetrics());
	    		for (final String s : lineWrapped.split("\n")) { //$NON-NLS-1$
	    			g.drawString(s, x, y += g.getFontMetrics().getHeight());
	    		}
	    	}
	    	else {
	    		g.drawString(line, x, y += g.getFontMetrics().getHeight());
	    	}
		}
		g.dispose();
		this.image = newImage;
	}

	private void saveImageProperties() {
		this.prop.setProperty(
			"imagePositionOnPageLowerLeftX" , //$NON-NLS-1$
			this.field != null ? Integer.toString(this.field.getSignaturePositionOnPageLowerLeftX())
				: this.prop.getProperty("signaturePositionOnPageLowerLeftX") //$NON-NLS-1$
		);

		this.prop.setProperty(
			"imagePositionOnPageLowerLeftY" , //$NON-NLS-1$
			this.field != null ? Integer.toString(this.field.getSignaturePositionOnPageLowerLeftY())
				: this.prop.getProperty("signaturePositionOnPageLowerLeftY") //$NON-NLS-1$
		);
		this.prop.setProperty(
			"imagePositionOnPageUpperRightX" , //$NON-NLS-1$
			this.field != null ? Integer.toString(this.field.getSignaturePositionOnPageUpperRightX())
				: this.prop.getProperty("signaturePositionOnPageUpperRightX") //$NON-NLS-1$
			);
		this.prop.setProperty("imagePositionOnPageUpperRightY" , //$NON-NLS-1$
			this.field != null ? Integer.toString(this.field.getSignaturePositionOnPageUpperRightY())
				: this.prop.getProperty("signaturePositionOnPageUpperRightY") //$NON-NLS-1$
		);
	}

	private static String getImageInBase64(final BufferedImage image) throws IOException {
		try (final ByteArrayOutputStream osImage = new ByteArrayOutputStream()) {
			ImageIO.write(image, "jpg", osImage); //$NON-NLS-1$
			return Base64.encode(osImage.toByteArray());
		}
        catch (final Exception e) {
        	LOGGER.severe("No ha sido posible pasar la imagen a JPG: " + e); //$NON-NLS-1$
        	throw new IOException("No ha sido posible pasar la imagen a JPG: " + e, e); //$NON-NLS-1$
		}
	}

	private static BufferedImage getImageFromBase64(final String imageB64) throws IOException {
		try ( final ByteArrayInputStream inImage = new ByteArrayInputStream(Base64.decode(imageB64)) ) {
			return ImageIO.read(inImage);
		}
        catch (final Exception e) {
        	LOGGER.severe("No ha sido posible pasar la imagen a JPG: " + e); //$NON-NLS-1$
        	throw new IOException("No ha sido posible pasar la imagen a JPG: " + e, e); //$NON-NLS-1$
		}
	}
}
