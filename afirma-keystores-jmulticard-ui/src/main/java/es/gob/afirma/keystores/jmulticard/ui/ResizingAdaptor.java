/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros
 * para la realizacion de procesos de autenticacion, firma electronica y validacion
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos
 * e Impulso de la Administracion Electronica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * European Union Public License publicada por la Comision Europea,
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.afirma.keystores.jmulticard.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.image.BufferedImage;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

/** Adaptador de componentes para su redimensionamiento.
 * @author INTECO */
final class ResizingAdaptor extends ComponentAdapter {
    private transient final AbstractJAccessibilityCustomDialog theCustomDialog;

    /** Constructor.
     * @param customDialog Di&aacute;logo a redimensionar */
    ResizingAdaptor(final AbstractJAccessibilityCustomDialog customDialog) {
        this.theCustomDialog = customDialog;
    }

    @Override
    public void componentResized(final ComponentEvent e) {
        if (this.theCustomDialog != null) {
            adjustFontSize(this.theCustomDialog.getComponents());
        }
    }

    /** Ajusta el tama&ntilde;o de fuente de una ventana.
     * @param components Componentes que conforman la ventana. */
    private void adjustFontSize(final Component[] components) {
        // Se calcula la relacion de aspecto para redimensionar el texto
        double relWidth;
        double relHeight;
        float relation = 1;
        if (this.theCustomDialog != null) {
            //TODO: Se deberia comprobar si esta activado el modo negrita, fuente grande o si es necesario que la ventana sea grande por defecto
            // if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold() || this.theCustomDialog.isBigSizeDefault()) { /* Hacer cosas */ }
			relWidth = this.theCustomDialog.getSize().getWidth() / this.theCustomDialog.getInitialWidth();
			relHeight = this.theCustomDialog.getSize().getHeight() / this.theCustomDialog.getInitialHeight();
            relation = Math.round(relWidth * relHeight * this.theCustomDialog.getMinimumRelation());
        }

        for (final Component component : components) {
            final Component actualComponent = component;
            if (isResizable(actualComponent)) {
                if (relation > 10) {
                    float resizeFactor = 0;
                    if (this.theCustomDialog != null) {
                        resizeFactor = Math.round(relation / getResizingFactorCustomDialog());
                    }
                    else {
                        resizeFactor = Math.round(relation / getResizingFactorFileChooser());
                    }
                    actualComponent.setFont(actualComponent.getFont().deriveFont(getFontSize() + resizeFactor));
                }
                else {
                    if (actualComponent instanceof JComboBox) {
                        // TODO Workaround buscar solucion mejor
                        actualComponent.setPreferredSize(new Dimension(100, 25));
                    }
                    actualComponent.setFont(actualComponent.getFont().deriveFont(getFontSize()));
                }
            }
            // Caso de borde con texto
            if (actualComponent instanceof JPanel) {
                final Border componentBorder = ((JPanel) actualComponent).getBorder();
                if (componentBorder instanceof TitledBorder) {
                    // Se comprueba si el panel tiene un nombre asignado
                    final String name = actualComponent.getName();
                    // Se hara el resize del titulo en el caso de que el componente no sea el panel de botones de accesibilidad de los alerts
                    if (name == null || !"AccessibilityButtonsPanel".equalsIgnoreCase(name)) { //$NON-NLS-1$
                        final TitledBorder b = (TitledBorder) componentBorder;
                        final float resizeFactor = Math.round(relation / getResizingFactorFrame());
                        if (b.getTitleFont() != null) {
                            b.setTitleFont(b.getTitleFont().deriveFont(getFontSize() - 2 + resizeFactor));
                        }
                        else {
                            b.setTitleFont(actualComponent.getFont().deriveFont(getFontSize() - 2 + resizeFactor));
                        }
                    }
                }
            }

            if (actualComponent instanceof Container && !(actualComponent instanceof JComboBox)) {
			    // Si nos encontramos con un contenedor, redimensionamos sus hijos
			    final Container actualContainer = (Container) actualComponent;
			    adjustFontSize(actualContainer.getComponents());
			}
            // Redimensionado de una etiqueta con icono
            if (actualComponent instanceof IconLabel) {
                final int multiplicando = 4;
                final IconLabel iconLabel = (IconLabel) actualComponent;
                if (iconLabel.getOriginalIcon() != null) {
                    final float resizeFactor = getImageResizeFactor(AccesiblityConstants.RESIZING_IMAGES_FACTOR);

                    // Se obtienen las dimensiones del icono original
                    final int width = iconLabel.getOriginalDimension().width;
                    final int height = iconLabel.getOriginalDimension().height;
                    // Se hace el resize de la imagen
                    resizeImage(resizeFactor, actualComponent, width, height, multiplicando);
                }
            }

            // imagenes dentro de JButton
            if (actualComponent instanceof JButton && ((JButton) actualComponent).getIcon() != null) {
                final float resizeFactor;
                if (this.theCustomDialog != null) {
                    resizeFactor = getImageResizeFactor(AccesiblityConstants.RESIZING_IMAGES_FACTOR + 0.0010);
                }
                else {
                    resizeFactor = getImageResizeFactor(AccesiblityConstants.RESIZING_IMAGES_FACTOR);
                }

                resizeImageButton(resizeFactor, actualComponent);
            }
        }
    }

    /** Devuelve el factor final de redimensionado de imagen.
     * @param factor Factor inicial de redimensionado.
     * @return Factor final de redimensionado */
    private float getImageResizeFactor(final double factor) {
        float resizeFactor = 0;
        if (this.theCustomDialog != null) {
            resizeFactor = (float) (this.theCustomDialog.getHeight() * factor);
        }
        return resizeFactor;
    }

    private static Image iconToImage(final Icon icon){
    	return iconToImage(icon, new Dimension(icon.getIconWidth(), icon.getIconWidth()));
    }

    private static Image iconToImage(final Icon icon, final Dimension d) {
        if (icon instanceof ImageIcon) {
            return ((ImageIcon) icon).getImage();
        }
        final int w = d.width;
        final int h = d.height;
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice gd = ge.getDefaultScreenDevice();
        final GraphicsConfiguration gc = gd.getDefaultConfiguration();
        final BufferedImage image = gc.createCompatibleImage(w, h);
        final Graphics2D g = image.createGraphics();
        icon.paintIcon(null, g, 0, 0);
        g.dispose();
        return image;
    }

    /** Redimensiona una imagen.
     * @param factor factor de redimensi&oacute;n.
     * @param c Componente de tipo JLabel en el que se encuentra la imagen.
     * @param w Ancho inicial de la imagen.
     * @param h Alto inicial de la imagen.
     * @param multiplicando Valor de multiplicaci&oacute;n para el nuevo tama&ntilde;o de la imagen.
     *                      Es mayor cuanto menor sea el tama&ntilde;o inicial de la imagen. */
    private static void resizeImage(final double factor,
    		                        final Component c,
    		                        final int w,
    		                        final int h,
    		                        final int multiplicando) {
        Image image = null;
        // Se comprueba si el componente es instancia de IconLabel
        if (c instanceof IconLabel) {
            final IconLabel iconLabel = (IconLabel) c;
            // Se selecciona la imagen original del icono para hacer el resize
            image = iconToImage(iconLabel.getOriginalIcon(), iconLabel.getOriginalDimension());
        }
        else {
            image = iconToImage(((JLabel) c).getIcon());
        }
        final ImageIcon newImage = new ImageIcon(image.getScaledInstance((int) Math.round(w * multiplicando * factor),
                                                      (int) Math.round(h * multiplicando * factor),
                                                      Image.SCALE_SMOOTH));
        ((JLabel) c).setIcon(newImage);
    }

    /** Redimensiona una imagen contenida en un JButton
     * @param factor factor de redimensi&oacute;n
     * @param c Componente de tipo JButton en el que se encuentra la imagen */
    private static void resizeImageButton(final double factor, final Component c) {
        final JButton button = (JButton) c;
        ImageIcon imageIcon = null;

        // Se almacena el factor
        final double factorAux = factor;

        // Se comprueba si se trata del boton de ayuda
        if (button.getName() != null && "maximizar".equalsIgnoreCase(button.getName())) { //$NON-NLS-1$
            imageIcon = AccesiblityConstants.IMAGEICON_MAXIMIZE; // Se carga la imagen original
        }
        else if (button.getName() != null && "restaurar".equalsIgnoreCase(button.getName())) { //$NON-NLS-1$
            imageIcon = AccesiblityConstants.IMAGEICONRESTORE; // Se carga la imagen original
        }
        else {
            imageIcon = new ImageIcon(iconToImage(button.getIcon())); // Se carga la imagen del componente actual
        }
        // Se redimensionan las imagenes
        int lado = (int) Math.round(25 * 2 * factorAux);
        lado = lado < 25 ? 25 : lado;
        final ImageIcon newImage = new ImageIcon(
    		imageIcon.getImage().getScaledInstance(lado, lado, Image.SCALE_SMOOTH)
		);
        button.setIcon(newImage);
        button.setPreferredSize(new Dimension(lado, lado));
    }

    /** Devuelve el tama&ntilde;o de la fuente en funcion de las opciones de accesibilidad.
     * @return Tama&ntilde;o de la fuente en funcion de las opciones de accesibilidad. */
    private static float getFontSize() {
        if (GeneralConfig.isBigFontSize()) {
            return 16;
        }
        return 14;
    }

    /** Identifica los componentes de una ventana para los que se van a realizar el redimensionado.
     * @param a Componente para el que se va a comprobar si se va a redimensionar.
     * @return Boolean que indica si el componente pasado como par&aacute;metro va a ser redimensionado. */
    private static boolean isResizable(final Component a) {
        boolean resizable = false;
        resizable = resizable || a instanceof JButton || a instanceof JLabel;
        resizable = resizable || a instanceof JTextField;
        resizable = resizable || a instanceof JPanel;
        return resizable || a instanceof JCheckBox;
    }

    /** Indica el factor de redimensionado que se aplicara en los componentes de un JFrame. Este metodo es util para aplicar factores distintos a
     * distinto componentes.
     * @return Float con el factor a aplicar. */
    private static float getResizingFactorFrame() {
        return 3f;
    }

    /** Indica el factor de redimensionado que se aplicara en los componentes de un JFileChooser. Este metodo es util para aplicar factores distintos a
     * distinto componentes.
     * @return Float con el factor a aplicar. */
    private static float getResizingFactorFileChooser() {
        return 3f;
    }

    /** Indica el factor de redimensionado que se aplicara en los componentes de un CustomDialog. Este metodo es util para aplicar factores distintos a
     * distinto componentes.
     * @return Float con el factor a aplicar. */
    private static float getResizingFactorCustomDialog() {
        return 2f;
    }
}