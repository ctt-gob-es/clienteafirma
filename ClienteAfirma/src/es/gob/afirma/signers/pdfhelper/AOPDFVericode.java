/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.signers.pdfhelper;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.InputStream;
import java.util.logging.Logger;

import javax.swing.ImageIcon;

import es.gob.afirma.exceptions.AOException;

/**
 * Generador de c&oacute;digos de verificaci&oacute;n para ser insertados en ficheros PDF.
 */
public class AOPDFVericode {

	/**
	 * Orientaci&oacute;n del c&oacute;digo dentro de la p&aacute;gina.
	 */
	public static enum VericodeOrientation {
		/** Indicador de que el codigo de verificaci&oacute;n debe generarse con una orientacion de izquierda a derecha. */
		DIRECTION_LEFT_TO_RIGHT,
		/** Indicador de que el codigo de verificaci&oacute;n debe generarse con una orientacion de arriba a abajo. */
		DIRECTION_UP_TO_DOWN,
		/** Indicador de que el codigo de verificaci&oacute;n debe generarse con una orientacion de izquierda a derecha. */
		DIRECTION_DOWN_TO_UP
	}

    /**
     * Genera una imagen con el c&oacute;digo de verificaci&oacute;n. El c&oacute;digo esta formado por
     * una imagen (opcional), un c&oacute;digo de barras, un texto que se muestra encima del c&oacute;digo
     * de barras (opcional) y un segundo texto que se muestra debajo. En caso de indicarse una imagen,
     * &eacute;sta se mostrar&aacute; con un tama&ntilde;o m&aacute;ximo de 64x64 p&iacute;xeles. Si es
     * necesario reducir la imagen para mostrarla, no se mantendr&aacute; la proporcion anchura-altura.
     * @param logo Imagen que se mostrar&aacute; junto al c&oacute;digo.
     * @param upperText Texto superior.
     * @param bottomText Texto inferior.
     * @param code C&oacute;digo de barras.
     * @param direction Orientaci&oacute;n del c&oacute;digo de verificaci&oacute;n.
     * @return Imagen con el c&oacute;digo de verificaci&oacute;n.
     * @throws AOException Cuando ocurri&oacute; un error al comprobar la anchura que debe tener la imagen final.
     */
    public static BufferedImage getVericode(Image logo, String upperText, String bottomText, String code, VericodeOrientation direction) throws AOException {

        if(code == null) {
            throw new NullPointerException("No se ha indicado el codigo que se desea generar");
        }

        // Tamanyo del logo
        final int LOGO_WIDTH = 64;
        
        int imageWidth = calculateWidth(upperText, bottomText, code);
        if(logo != null) {
            imageWidth += LOGO_WIDTH + 20;
        }
        BufferedImage bImg = new BufferedImage(imageWidth, LOGO_WIDTH,  BufferedImage.TYPE_INT_RGB);

        // Tomamos el grafics y lo configuramos
        Graphics gbImage = bImg.getGraphics();
        gbImage.setColor(Color.white);
        gbImage.fillRect(0, 0, bImg.getWidth(), bImg.getHeight());
        gbImage.setColor(Color.black);

        int xPosition = 10; 

        // Pintamos el logo
        if(logo != null) {
            ImageIcon imageTmp = new ImageIcon(logo);
            int width = Math.min(imageTmp.getIconWidth(), LOGO_WIDTH);
            int height = Math.min(imageTmp.getIconHeight(), LOGO_WIDTH);

            gbImage.drawImage(new ImageIcon(logo.getScaledInstance(width, height, Image.SCALE_SMOOTH)).getImage(), 0, 0, null);
            xPosition += LOGO_WIDTH;
        }

        // Pintamos el texto superior
        if(upperText != null) {
            gbImage.setFont(getBarcodeUpperTextFont());
            gbImage.drawString(upperText, xPosition, 16);
        }

        // Pintamos el codigo de barras
        gbImage.setFont(getBarcodeFont());
        gbImage.drawString(formatBarcode(code), xPosition, 48);

        // Pintamos el texto inferior
        if(bottomText != null) {
            gbImage.setFont(getBarcodeBottomTextFont());
            gbImage.drawString(bottomText, xPosition, 60);
        }

        return rotateImage(bImg, direction);
    }

    /**
     * Gira una imagen seg&uacute;n la orientaci&oacute;n indicada. Las posiciones disponibles
     * son: de izquierda a derecha (por defecto), de arriba a abajo y de abajo a arriba.
     * @param image Imagen a rotar.
     * @param direction Direcc&oacute;n en la que rotar la imagen.
     * @return Imagen rotada. 
     */
    private static BufferedImage rotateImage(BufferedImage image, VericodeOrientation direction) {

        if(image == null) {
            throw new NullPointerException("No se ha indicado la imagen que se desea rotar");
        }

        BufferedImage rotatedImage = image;

        // Si hay que rotar, operamos como corresponde
        if(direction == VericodeOrientation.DIRECTION_UP_TO_DOWN || direction == VericodeOrientation.DIRECTION_DOWN_TO_UP) {

            int initWidth = image.getWidth();
            int initHeight = image.getHeight();

            int[] pixels = image.getRGB(0, 0, initWidth, initHeight, null, 0, image.getWidth());
            rotatedImage = new BufferedImage(initHeight, initWidth, BufferedImage.TYPE_INT_RGB);

            int p=0;
            if(direction == VericodeOrientation.DIRECTION_UP_TO_DOWN) {    // UP-TO-DOWN
                for(int x=0; x<initHeight; x++)
                    for(int y=0; y<initWidth; y++)
                        rotatedImage.setRGB((initHeight-1)-x, y, pixels[p++]);
            } else {   // DOWN-TO-UP
                for(int x=0; x<initHeight; x++)
                    for(int y=0; y<initWidth; y++)
                        rotatedImage.setRGB(x, (initWidth-1)-y, pixels[p++]);
            }
        }

        return rotatedImage;
    }

    /**
     * Calculamos la longitud de los distintos textos con las fuentes asignadas y tomamos la mayor
     * como referencia para la longitud del codigo de barras.
     * @param canvas Lienzo sobre el que se pinta el c&oacute;digo de barras.
     * @param upperText Texto superior.
     * @param bottomText Texto inferior.
     * @param code C&oacute;digo de barras.
     * @return Longitud
     */
    private static int calculateWidth(String upperText, String bottomText, String code) {

        final int DEFAULT_MAX_WIDTH = 100;
        int maxWidth = DEFAULT_MAX_WIDTH;       
        FontMetrics fm = null;
        BufferedImage canvas = new BufferedImage(100, 60,  BufferedImage.TYPE_INT_RGB);

        try {
            // Texto superior
            if(upperText != null) {
                fm = canvas.getGraphics().getFontMetrics(getBarcodeUpperTextFont());
                maxWidth = Math.max(fm.stringWidth(upperText), maxWidth);
            }

            // Codigo de barras
            fm = canvas.getGraphics().getFontMetrics(getBarcodeFont());
            maxWidth = Math.max(fm.stringWidth(code), maxWidth);

            // Texto inferior
            if(bottomText != null) {
                fm = canvas.getGraphics().getFontMetrics(getBarcodeBottomTextFont());
                maxWidth = Math.max(fm.stringWidth(bottomText), maxWidth);
            }

        } catch (Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("Error al calcular el tama\u00F1o m\u00E1ximo de los texto, se devolvera la longitud por defecto: "+e.getMessage());
            maxWidth = DEFAULT_MAX_WIDTH;
        }

        return new Double(maxWidth*1.1).intValue();
    }

    /**
     * Comprueba que el c&oacute;digo textual cumpla con el est&aacute;ndar y lo modifica si no lo hace.
     * Esto es, que la cadena empiece y termine con asterisco ('*'). Por ejemplo: *Mi Texto*.
     * @param code C&oacute;digo textual.
     * @return Texto con el formato v&aacute;lido.
     */
    private static String formatBarcode(String code) {
        String barcode = code;
        if(!code.startsWith("*") || !code.endsWith("*")) {  //$NON-NLS-1$  //$NON-NLS-2$
            barcode = "*"+code+"*";  //$NON-NLS-1$  //$NON-NLS-2$
        }
        return barcode;
    }

    /**
     * Recupera la fuente para el texto superior del c&oacute;digo de barras.
     * @return Fuente de texto.
     */
    private static Font getBarcodeUpperTextFont() {
        return new Font("Arial", Font.PLAIN, 12);
    }

    /**
     * Recupera la fuente para el texto inferior del c&oacute;digo de barras.
     * @return Fuente de texto.
     */
    private static Font getBarcodeBottomTextFont() {
        return new Font("Arial", Font.PLAIN, 10);   
    }

    /** Fuente para la visualizaci&oacute;n de los c&oacute;digos de barras. */
    private static Font barcodeFont = null;

    /**
     * Recupera la fuente para el c&oacute;digo de barras.
     * @return Fuente de texto.
     */
    private static Font getBarcodeFont() throws AOException {
        if(barcodeFont == null) {
            try {
                InputStream is = AOPDFVericode.class.getResourceAsStream("/FREE3OF9.TTF");
                barcodeFont = Font.createFont(Font.TRUETYPE_FONT, is).deriveFont(Font.PLAIN, 36);
            } catch (Exception e) {
                Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar la fuente del codigo de barras");
                throw new AOException("No se ha podido cargar la fuente del codigo de barras", e);
            }
        }
        return barcodeFont;
    }

//    public static void main(String[] args) throws AOException {
//        Image logo = Toolkit.getDefaultToolkit().getImage("C:\\Entrada.jpg");
//        JLabel label = new JLabel(new ImageIcon(getVericode(logo, "Referencia del documento", "Consulte su validez en: www.mpr.es", "1234567890", VericodeOrientation.DIRECTION_LEFT_TO_RIGHT)));
//        label.setSize(100, 100);
//        JOptionPane.showMessageDialog(null, label);
//    }
}
