package es.gob.afirma.ui.utils;

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
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.table.JTableHeader;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.html.HTMLDocument;
import javax.swing.tree.DefaultTreeCellRenderer;

import es.gob.afirma.core.misc.Platform;

/**
 * Adaptador de componentes para su redimensionamiento
 *
 * @author INTECO
 *
 */
final class ResizingAdaptor extends ComponentAdapter {
	private final JAccessibilityFrame theWindow;
	private final JAccessibilityDialog theDialog;
	private final JAccessibilityDialogWizard theDialogWizard;
	private final JAccessibilityFrameAdvisor theDialogAdvisor;
	private final JAccessibilityFrameAbout theWindowAbout;
	private final JAccessibilityFileChooser theFileChooser;
	private final JAccessibilityCustomDialog theCustomDialog;
	private final JAccessibilityFileChooserToSave theFileChooserToSave;

	/**
	 * Constructor
	 *
	 * @param window
	 *            ventana a redimensionar
	 */
	ResizingAdaptor(final JAccessibilityFrame window, final JAccessibilityDialog dialog, final JAccessibilityDialogWizard dialogWizard,final JAccessibilityFrameAdvisor dialogAdvisor,final JAccessibilityFrameAbout windowAbout,final JAccessibilityFileChooser fileChooser,
			final JAccessibilityCustomDialog customDialog,final JAccessibilityFileChooserToSave fileChooserToSave) {
		this.theWindow = window;
		this.theDialog = dialog;
		this.theDialogWizard = dialogWizard;
		this.theDialogAdvisor = dialogAdvisor;
		this.theWindowAbout = windowAbout;
		this.theFileChooser = fileChooser;
		this.theCustomDialog = customDialog;
		this.theFileChooserToSave = fileChooserToSave;
	}

    private static Image iconToImage(final Icon icon) {
        if (icon instanceof ImageIcon) {
            return ((ImageIcon)icon).getImage();
        }
        final int w = icon.getIconWidth();
        final int h = icon.getIconHeight();
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice gd = ge.getDefaultScreenDevice();
        final GraphicsConfiguration gc = gd.getDefaultConfiguration();
        final BufferedImage image = gc.createCompatibleImage(w, h);
        final Graphics2D g = image.createGraphics();
        icon.paintIcon(null, g, 0, 0);
        g.dispose();
        return image;
    }

	@Override
	/**
	 * Evento de redimensionado
	 */
	public void componentResized(final ComponentEvent e) {
			if (this.theWindow != null) {
				adjustFontSize(this.theWindow.getComponents());
			} else if (this.theDialog != null){
				adjustFontSize(this.theDialog.getComponents());
			} else if (this.theDialogWizard != null){
				adjustFontSize(this.theDialogWizard.getComponents());
			} else if (this.theDialogAdvisor != null){
				adjustFontSize(this.theDialogAdvisor.getComponents());
			} else if (this.theWindowAbout != null){
				adjustFontSize(this.theWindowAbout.getComponents());
			} else if (this.theCustomDialog != null) {
				adjustFontSize(this.theCustomDialog.getComponents());
			}else if (this.theFileChooser != null){
				adjustFontSize(this.theFileChooser.getDialog().getComponents());
			} else {
				adjustFontSize(this.theFileChooserToSave.getDialog().getComponents());
			}
	}

	/**
	 * Ajusta las fuentes
	 */
	public void adjustWindowFonts() {
		if (this.theWindow != null) {
			adjustFontSize(this.theWindow.getComponents());
		} else if (this.theDialog != null){
			adjustFontSize(this.theDialog.getComponents());
		} else if (this.theDialogWizard != null){
			adjustFontSize(this.theDialogWizard.getComponents());
		} else if (this.theDialogAdvisor != null){
			adjustFontSize(this.theDialogAdvisor.getComponents());
		} else if (this.theWindowAbout != null){
			adjustFontSize(this.theWindowAbout.getComponents());
		} else if (this.theCustomDialog != null) {
			adjustFontSize(this.theCustomDialog.getComponents());
		}else if (this.theFileChooser != null){
			adjustFontSize(this.theFileChooser.getDialog().getComponents());
		} else {
			adjustFontSize(this.theFileChooserToSave.getDialog().getComponents());
		}
	}

	/**
	 * Ajusta el tamano de fuente de una ventana
	 *
	 * @param components
	 */
	private void adjustFontSize(final Component[] components) {
		// Se calcula la relacion de aspecto para redimensionar el texto
		double relWidth;
		double relHeight;
		float relation;
		if (this.theWindow!=null) {
			relWidth = this.theWindow.getSize().getWidth() / Constants.WINDOW_INITIAL_WIDTH;
			relHeight = this.theWindow.getSize().getHeight() / Constants.WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * this.theWindow.getMinimumRelation());
		}
		else if (this.theDialog != null) {
			if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
				if (Platform.getOS().equals(Platform.OS.LINUX)){
					relWidth = this.theDialog.getSize().getWidth() / Constants.OPTION_FONT_INITIAL_WIDTH_LINUX;
					relHeight = this.theDialog.getSize().getHeight() / Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX;
					relation = Math.round(relWidth * relHeight * this.theDialog.getMinimumRelation());
				}
				else {
					relWidth = this.theDialog.getSize().getWidth() / Constants.OPTION_FONT_INITIAL_WIDTH;
					relHeight = this.theDialog.getSize().getHeight() / Constants.OPTION_FONT_INITIAL_HEIGHT;
					relation = Math.round(relWidth * relHeight * this.theDialog.getMinimumRelation());
				}
			}
			else{
				relWidth = this.theDialog.getSize().getWidth() / Constants.OPTION_INITIAL_WIDTH;
				relHeight = this.theDialog.getSize().getHeight() / Constants.OPTION_INITIAL_HEIGHT;
				relation = Math.round(relWidth * relHeight * this.theDialog.getMinimumRelation());
			}
		}
		else if (this.theDialogWizard != null){
			//Se comprueba si el sistema operativo es linux
			if (Platform.getOS().equals(Platform.OS.LINUX)){
				//Se comprueba si esta activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					relWidth = this.theDialogWizard.getSize().getWidth() / Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX;
					relHeight = this.theDialogWizard.getSize().getHeight() / Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX;
				}
				else {
					relWidth = this.theDialogWizard.getSize().getWidth() / Constants.WIZARD_INITIAL_WIDTH_LINUX;
					relHeight = this.theDialogWizard.getSize().getHeight() / Constants.WIZARD_INITIAL_HEIGHT_LINUX;
				}
			}
			else {
				//Se comprueba si esta activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					relWidth = this.theDialogWizard.getSize().getWidth() / Constants.WIZARD_FONT_INITIAL_WIDTH;
					relHeight = this.theDialogWizard.getSize().getHeight() / Constants.WIZARD_FONT_INITIAL_HEIGHT;
				}
				else {
					relWidth = this.theDialogWizard.getSize().getWidth() / Constants.WIZARD_INITIAL_WIDTH;
					relHeight = this.theDialogWizard.getSize().getHeight() / Constants.WIZARD_INITIAL_HEIGHT;
				}
			}

			//Se calcula la relacion de los valores obtenidos
			relation = Math.round(relWidth * relHeight * this.theDialogWizard.getMinimumRelation());

		}
		else if (this.theDialogAdvisor != null){
			relWidth = this.theDialogAdvisor.getSize().getWidth() / Constants.INIT_WINDOW_INITIAL_WIDTH;
			relHeight = this.theDialogAdvisor.getSize().getHeight() / Constants.INIT_WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * this.theDialogAdvisor.getMinimumRelation());
		}
		else if (this.theWindowAbout != null){
			if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)){
				relWidth = this.theWindowAbout.getSize().getWidth() / Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX;
				relHeight = this.theWindowAbout.getSize().getHeight() / Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX;
				relation = Math.round(relWidth * relHeight * this.theWindowAbout.getMinimumRelation());
			}
			else {
				relWidth = this.theWindowAbout.getSize().getWidth() / Constants.ABOUT_WINDOW_INITIAL_WIDTH;
				relHeight = this.theWindowAbout.getSize().getHeight() / Constants.ABOUT_WINDOW_INITIAL_HEIGHT;
				relation = Math.round(relWidth * relHeight * this.theWindowAbout.getMinimumRelation());
			}
		}
		else if (this.theWindowAbout != null){
			relWidth = this.theWindowAbout.getSize().getWidth() / Constants.ABOUT_WINDOW_INITIAL_WIDTH;
			relHeight = this.theWindowAbout.getSize().getHeight() / Constants.ABOUT_WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * this.theWindowAbout.getMinimumRelation());
		}
		else if (this.theCustomDialog != null){
			//Se comprueba si esta activado el modo negrita, fuente grande o si es necesario que la ventana sea grande por defecto
			if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold() || this.theCustomDialog.isBigSizeDefault()){
				relWidth = this.theCustomDialog.getSize().getWidth() / Constants.CUSTOMDIALOG_FONT_INITIAL_WIDTH;
				relHeight = this.theCustomDialog.getSize().getHeight() / Constants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT;
			}
			else {
				relWidth = this.theCustomDialog.getSize().getWidth() / Constants.CUSTOMDIALOG_INITIAL_WIDTH;
				relHeight = this.theCustomDialog.getSize().getHeight() / Constants.CUSTOMDIALOG_INITIAL_HEIGHT;
			}
			relation = Math.round(relWidth * relHeight * this.theCustomDialog.getMinimumRelation());
		}
		else if (this.theFileChooser != null){
			if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)){
				relWidth = this.theFileChooser.getDialog().getSize().getWidth() / Constants.FILE_INITIAL_WIDTH_MAC;
			}
			else {
				relWidth = this.theFileChooser.getDialog().getSize().getWidth() / Constants.FILE_INITIAL_WIDTH;
			}
			relHeight = this.theFileChooser.getDialog().getSize().getHeight() / Constants.FILE_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * this.theFileChooser.getMinimumRelation());
		}
		else {
			if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)){
				relWidth = this.theFileChooserToSave.getDialog().getSize().getWidth() / Constants.FILE_INITIAL_WIDTH_MAC;
			}
			else {
				relWidth = this.theFileChooserToSave.getDialog().getSize().getWidth() / Constants.FILE_INITIAL_WIDTH;
			}
			relHeight = this.theFileChooserToSave.getDialog().getSize().getHeight() / Constants.FILE_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * this.theFileChooserToSave.getMinimumRelation());
		}

		for (final Component component : components) {
			final Component actualComponent = component;
			if (isResizable(actualComponent)) {
				if (relation > 10) {
					float resizeFactor = 0;
					if (this.theWindow != null) {
						resizeFactor = Math.round(relation / getResizingFactorFrame());
					}
					else if (this.theDialog != null){
						resizeFactor = Math.round(relation / getResizingFactorDialog());
					}
					else if (this.theDialogWizard != null){
						resizeFactor = Math.round(relation / getResizingFactorDialogWizard());
					}
					else if (this.theDialogAdvisor != null){
						resizeFactor = Math.round(relation / getResizingFactorFrameAdvisor());
					}
					else if (this.theWindowAbout != null) {
						resizeFactor = Math.round(relation / getResizingFactorFrameAbout());
					}
					else if (this.theCustomDialog != null) {
						resizeFactor = Math.round(relation / getResizingFactorCustomDialog());
					}
					else {
						resizeFactor = Math.round(relation / getResizingFactorFileChooser());
					}
					actualComponent.setFont(actualComponent.getFont().deriveFont(getFontSize() + resizeFactor));
				}
				else {
					if (actualComponent instanceof JComboBox){
						// TODO Workaround buscar solucion mejor
						actualComponent.setPreferredSize(new Dimension(100,25));
					}
					actualComponent.setFont(actualComponent.getFont().deriveFont(getFontSize()));
				}
			}
			//Caso de borde con texto
			if(actualComponent instanceof JPanel){
				final Border componentBorder = ((JPanel)actualComponent).getBorder();
				if(componentBorder instanceof TitledBorder){
					//Se comprueba si el panel tiene un nombre asignado
					final String name = actualComponent.getName();
					//Se hara el resize del titulo en el caso de que el componente no sea el panel de botones de accesibilidad de los alerts
					if (name==null || !name.equalsIgnoreCase("AccessibilityButtonsPanel")) { //$NON-NLS-1$
						final TitledBorder b = (TitledBorder) componentBorder;
						final float resizeFactor = Math.round(relation / getResizingFactorFrame());
						if (b.getTitleFont() != null) {
						    b.setTitleFont(b.getTitleFont().deriveFont(getFontSize()-2 + resizeFactor));
						} else {
						    b.setTitleFont(actualComponent.getFont().deriveFont(getFontSize()-2 + resizeFactor));
						}
					}
				}
			}

			if (actualComponent instanceof Container) {
				if(actualComponent instanceof JMenu){
					final JMenu barraMenu = (JMenu) actualComponent;
					adjustFontSize(barraMenu.getMenuComponents());
				} else if (actualComponent instanceof JTree) {
					final JTree arbol = (JTree) actualComponent;
					if(arbol.getCellRenderer() instanceof DefaultTreeCellRenderer){
						adjustFontSize(new Component[]{(DefaultTreeCellRenderer)arbol.getCellRenderer()});
					}
				} else if(actualComponent instanceof JScrollPane){
					final JScrollPane panel = (JScrollPane) actualComponent;
					adjustFontSize(panel.getComponents());
				} else if (actualComponent instanceof JEditorPane){
					final JEditorPane editorPanel = (JEditorPane) actualComponent;
					// Resize del texto contenido en el EditorPane
					final float resizeFactor = Math.round(relation / getResizingFactorDialogWizard());
					final String bodyRule = "body { font-family: " + actualComponent.getFont().getFamily() + "; " + "font-size: " + (7 + resizeFactor) + "pt; }"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					((HTMLDocument)editorPanel.getDocument()).getStyleSheet().addRule(bodyRule);
					// Resize del texto del enlace, porque tiene un estilo a nivel de linea entonces es necesario cambiar el tamano del texto a nivel de linea
					Style link;
					final StyleContext sc = new StyleContext();
					link= sc.addStyle("link", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
			        StyleConstants.setFontSize(link, 7 + (int)resizeFactor);
					((HTMLDocument) editorPanel.getDocument()).setCharacterAttributes(221, 26, link, false);
				} else if (!(actualComponent instanceof JComboBox)){
					// Si nos encontramos con un contenedor, redimensionamos sus hijos
					final Container actualContainer = (Container) actualComponent;
					adjustFontSize(actualContainer.getComponents());
				}
			}
			//Redimensionado de una etiqueta con icono
			if (actualComponent instanceof IconLabel) {
				final int multiplicando = 4;
				final IconLabel iconLabel = (IconLabel)actualComponent;
				if (iconLabel.getOriginalIcon() != null) {
					final float resizeFactor = getImageResizeFactor(Constants.RESIZING_IMAGES_FACTOR);

					//Se obtienen las dimensiones del icono original
					 final int w = iconLabel.getOriginalIcon().getIconWidth();
					 final int h = iconLabel.getOriginalIcon().getIconHeight();
					 //Se hace el resize de la imagen
					resizeImage(resizeFactor, actualComponent, w, h, multiplicando);
				}
			}

			//imagenes dentro de JButton
			if (actualComponent instanceof JButton && ((JButton)actualComponent).getIcon() != null) {
				float resizeFactor = 0;
				if (this.theCustomDialog != null) {
					resizeFactor = getImageResizeFactor(Constants.RESIZING_IMAGES_FACTOR + 0.0010);
				}
				else if(this.theFileChooser != null || this.theFileChooserToSave != null){
					resizeFactor = getImageResizeFactor(Constants.RESIZING_IMAGES_FACTOR + 0.0005);
				}
				else {
					resizeFactor = getImageResizeFactor(Constants.RESIZING_IMAGES_FACTOR);
				}
				resizeImageButton(resizeFactor, actualComponent);
			}
		}
	}

	/**
	 * Devuelve el factor final de redimensionado de imagen.
	 * @param height altura
	 * @param factor factor
	 * @return factor final de redimensionado
	 */
	private float getImageResizeFactor(final double factor){
		float resizeFactor = 0;
		if(this.theWindow != null){
			resizeFactor = (float) (this.theWindow.getHeight() * factor);
		}
		else if(this.theDialog != null) {
			resizeFactor = (float) (this.theDialog.getHeight() * factor);
		}
		else if (this.theDialogWizard != null){
			resizeFactor = (float) (this.theDialogWizard.getHeight() * factor);
		}
		else if (this.theDialogAdvisor != null){
			resizeFactor = (float) (this.theDialogAdvisor.getHeight() * factor);
		}
		else if (this.theWindowAbout != null){
			resizeFactor = (float) (this.theWindowAbout.getHeight() * factor);
		}
		else if (this.theCustomDialog != null){
			resizeFactor = (float) (this.theCustomDialog.getHeight() * factor);
		}
		else if (this.theFileChooser != null){
			resizeFactor = (float) (this.theFileChooser.getHeight() * factor);
		}
		else {
			resizeFactor = (float) (this.theFileChooserToSave.getHeight() * factor);
		}
		return resizeFactor;
	}

	/**
	 * Redimensiona una imagen
	 *
	 * @param factor factor de redimension
	 * @param c Componente de tipo JLabel en el que se encuentra la imagen
	 * @param w Width inicial de la imagen
	 * @para h Height inicial de la imagen
	 * @param multiplicando Valor de multiplicacion para el nuevo tamano de la imagen. Es mayor cuanto menor sea el tamano inicial de la imagen
	 */
	private void resizeImage(final double factor, final Component c, final int w, final int h, final int multiplicando) {
		if (this.theFileChooser==null && this.theFileChooserToSave==null){
			ImageIcon image = new ImageIcon();
			//Se comprueba si el componente es instancia de IconLabel
			if (c instanceof IconLabel) {
				final IconLabel iconLabel = (IconLabel) c;
				//Se selecciona la imagen original del icono para hacer el resize
				image = new ImageIcon(iconToImage(iconLabel.getOriginalIcon()));
			}
			else {
				image = new ImageIcon(iconToImage(((JLabel)c).getIcon()));
			}
			final ImageIcon newImage = new ImageIcon(image.getImage().getScaledInstance((int) Math.round(w * multiplicando * factor), (int) Math.round(h * multiplicando * factor), java.awt.Image.SCALE_SMOOTH));
			((JLabel)c).setIcon(newImage);
		}
	}

	/** Redimensiona una imagen contenida en un <code>JButton</code>.
	 * @param factor Factor de redimensi&oacute;n
	 * @param c Componente de tipo JButton en el que se encuentra la imagen */
	private void resizeImageButton(final double factor, final Component c) {

		if ((this.theFileChooser==null || isAncestor(this.theFileChooser.getAccesibilityButtonsPanel(), c)) && (this.theFileChooserToSave==null || isAncestor(this.theFileChooserToSave.getAccesibilityButtonsPanel(), c))){

		final JButton button = (JButton) c;
		ImageIcon imageIcon = null;

		//Se almacena el factor
		double factorAux = factor;

		//Se comprueba si se trata del boton de ayuda
		if (button.getName() != null && button.getName().equalsIgnoreCase("helpButton")) { //$NON-NLS-1$
			imageIcon = HelpUtils.IMAGEICONHELP; //Se carga la imagen original
		}
		else if (button.getName() != null && button.getName().equalsIgnoreCase("maximizar")) { //$NON-NLS-1$
			if (this.theDialog!=null){
				factorAux = factorAux - 0.4; //0.8999999761581421
			}
			if (this.theDialogWizard!=null){
				factorAux = factorAux - 0.3; //0.8999999761581421
			}
			imageIcon = Constants.IMAGEICON_MAXIMIZE; //Se carga la imagen original
		}
		else if (button.getName() != null && button.getName().equalsIgnoreCase("restaurar")) { //$NON-NLS-1$
			if (this.theDialog!=null){
				factorAux = factorAux - 0.4; //0.8999999761581421
			}
			if (this.theDialogWizard!=null){
				factorAux = factorAux - 0.3; //0.8999999761581421
			}
			imageIcon = Constants.IMAGEICONRESTORE; //Se carga la imagen original
		}
		else {
			imageIcon = new ImageIcon(iconToImage(button.getIcon())); //Se carga la imagen del componente actual
		}
		//Se redimensionan las imagenes
		final ImageIcon newImage = new ImageIcon(imageIcon.getImage().getScaledInstance((int) Math.round(25 * 2 * factorAux),
				(int) Math.round(25 * 2 * factorAux), java.awt.Image.SCALE_SMOOTH));
		button.setIcon(newImage);

		button.setPreferredSize(new Dimension((int) Math.round(25 * 2 * factorAux),(int) Math.round(25 * 2 * factorAux)));
		}
	}

	/**
	 * Devuelve el tamano de la fuente en funcion de las opciones de accesibilidad
	 * @return
	 */
	private float getFontSize(){
		if(GeneralConfig.isBigFontSize()){
			if (this.theDialogWizard != null){
				return 15;
			}
			else if (this.theFileChooser != null || this.theFileChooserToSave != null){
				return 13;
			}
			else {
				return 16;
			}
		}
		if (this.theDialogWizard != null){
			return 12;
		}
		if (this.theDialogAdvisor != null){
			return 15;
		}
		if (this.theFileChooser != null || this.theFileChooserToSave != null) {
			return 11;
		}
		return 13;
	}

	/**
	 * Identifica los componentes de una ventana para los que se van a realizar el redimensionado.
	 *
	 * @param a Componente para el que se va a comprobar si se va a redimensionar.
	 * @return Boolean que indica si el componente pasado como par&aacute;metro va a ser redimensionado.
	 */
	private static boolean isResizable(final Component a){

		if (a instanceof JButton || a instanceof JToggleButton || a instanceof JLabel || a instanceof JMenuItem
				|| a instanceof JComboBox || a instanceof JTextField || a instanceof JPanel || a instanceof JTabbedPane
				|| a instanceof JRadioButton || a instanceof JCheckBox || a instanceof JTextPane || a instanceof JEditorPane
				|| a instanceof JTree || a instanceof JList || a instanceof JFileChooser || a instanceof JTable
				|| a instanceof JTableHeader){
			return true;
		}
		return false;
	}

	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFrame. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private static float getResizingFactorFrame(){
		return 3f;
	}

	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JDialog. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private static float getResizingFactorDialog(){
		return 3f;
	}

	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JDialogWizard. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private static float getResizingFactorDialogWizard(){
		return 2f;
	}

	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFrame. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private static float getResizingFactorFrameAdvisor(){
		return 3f;
	}

	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFrame. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private static float getResizingFactorFrameAbout(){
		return 3f;
	}

	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFileChooser. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private static float getResizingFactorFileChooser(){
		return 3f;
	}

	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un CustomDialog. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private static float getResizingFactorCustomDialog(){
		return 3f;
	}

	/**
	 * Comprueba si un componente es antecesor de otro
	 * @param ancestor Componente padre
	 * @param descendant Componente hijo
	 * @return
	 */
	private static boolean isAncestor(final Component ancestor, final Component descendant){
		if(descendant != null && ancestor != null){
			if(descendant.getParent() != null && descendant.getParent().equals(ancestor)){
				return true;
			}
			return isAncestor(ancestor, descendant.getParent());
		}
		return false;
	}

}