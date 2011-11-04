package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

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

import sun.swing.FilePane;

import es.gob.afirma.core.misc.Platform;

/**
 * Adaptador de componentes para su redimensionamiento
 * 
 * @author INTECO
 * 
 */
public class ResizingAdaptor extends ComponentAdapter {
	private final JAccessibilityFrame theWindow;
	private final JAccessibilityDialog theDialog;
	private final JAccessibilityDialogWizard theDialogWizard;
	private final JAccessibilityDialogAdvisor theDialogAdvisor;
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
	public ResizingAdaptor(JAccessibilityFrame window, JAccessibilityDialog dialog, JAccessibilityDialogWizard dialogWizard,JAccessibilityDialogAdvisor dialogAdvisor,JAccessibilityFrameAbout windowAbout,JAccessibilityFileChooser fileChooser,
			JAccessibilityCustomDialog customDialog,JAccessibilityFileChooserToSave fileChooserToSave) {
		this.theWindow = window;
		this.theDialog = dialog;
		this.theDialogWizard = dialogWizard;
		this.theDialogAdvisor = dialogAdvisor;
		this.theWindowAbout = windowAbout;
		this.theFileChooser = fileChooser;
		this.theCustomDialog = customDialog;
		this.theFileChooserToSave = fileChooserToSave;
	}

	@Override
	/**
	 * Evento de redimensionado
	 */
	public void componentResized(ComponentEvent e) {
			if (this.theWindow != null) {
				this.adjustFontSize(this.theWindow.getComponents());
			} else if (this.theDialog != null){
				this.adjustFontSize(this.theDialog.getComponents());
			} else if (this.theDialogWizard != null){
				this.adjustFontSize(this.theDialogWizard.getComponents());
			} else if (this.theDialogAdvisor != null){
				this.adjustFontSize(this.theDialogAdvisor.getComponents());
			} else if (this.theWindowAbout != null){
				this.adjustFontSize(this.theWindowAbout.getComponents());
			} else if (this.theCustomDialog != null) {
				this.adjustFontSize(this.theCustomDialog.getComponents());
			}else if (this.theFileChooser != null){
				this.adjustFontSize(this.theFileChooser.getDialog().getComponents());
			} else {
				this.adjustFontSize(this.theFileChooserToSave.getDialog().getComponents());
			}
	}

	/**
	 * Ajusta las fuentes
	 */
	public void adjustWindowFonts() {
		if (theWindow != null) {
			this.adjustFontSize(theWindow.getComponents());
		} else if (theDialog != null){
			this.adjustFontSize(theDialog.getComponents());
		} else if (theDialogWizard != null){
			this.adjustFontSize(theDialogWizard.getComponents());
		} else if (theDialogAdvisor != null){
			this.adjustFontSize(theDialogAdvisor.getComponents());
		} else if (theWindowAbout != null){
			this.adjustFontSize(theWindowAbout.getComponents());
		} else if (this.theCustomDialog != null) {
			this.adjustFontSize(this.theCustomDialog.getComponents());
		}else if (this.theFileChooser != null){
			this.adjustFontSize(this.theFileChooser.getDialog().getComponents());
		} else {
			this.adjustFontSize(this.theFileChooserToSave.getDialog().getComponents());
		}
	}

	/**
	 * Ajusta el tamaño de fuente de una ventana
	 * 
	 * @param components
	 */
	private void adjustFontSize(Component[] components) {
		// Se calcula la relacion de aspecto para redimensionar el texto
		double relWidth;
		double relHeight;
		float relation;
		if (theWindow!=null) {
			relWidth = theWindow.getSize().getWidth() / Constants.WINDOW_INITIAL_WIDTH;
			relHeight = theWindow.getSize().getHeight() / Constants.WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theWindow.getMinimumRelation());
		} else if (theDialog != null) {
			if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
				if (Platform.getOS().equals(Platform.OS.LINUX)){
					relWidth = theDialog.getSize().getWidth() / (Constants.OPTION_FONT_INITIAL_WIDTH_LINUX);
					relHeight = theDialog.getSize().getHeight() / (Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX);
					relation = Math.round(relWidth * relHeight * theDialog.getMinimumRelation());
				} else {
					relWidth = theDialog.getSize().getWidth() / (Constants.OPTION_FONT_INITIAL_WIDTH);
					relHeight = theDialog.getSize().getHeight() / (Constants.OPTION_FONT_INITIAL_HEIGHT);
					relation = Math.round(relWidth * relHeight * theDialog.getMinimumRelation());
				}
			} else{
				relWidth = theDialog.getSize().getWidth() / Constants.OPTION_INITIAL_WIDTH;
				relHeight = theDialog.getSize().getHeight() / Constants.OPTION_INITIAL_HEIGHT;
				relation = Math.round(relWidth * relHeight * theDialog.getMinimumRelation());
			}
		} else if (theDialogWizard != null){
			//Se comprueba si el sistema operativo es linux
			if (Platform.getOS().equals(Platform.OS.LINUX)){
				//Se comprueba si está activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					relWidth = theDialogWizard.getSize().getWidth() / Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX;
					relHeight = theDialogWizard.getSize().getHeight() / Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX;
				} else {
					relWidth = theDialogWizard.getSize().getWidth() / Constants.WIZARD_INITIAL_WIDTH_LINUX;
					relHeight = theDialogWizard.getSize().getHeight() / Constants.WIZARD_INITIAL_HEIGHT_LINUX;
				}
			} else {
				//Se comprueba si está activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					relWidth = theDialogWizard.getSize().getWidth() / Constants.WIZARD_FONT_INITIAL_WIDTH;
					relHeight = theDialogWizard.getSize().getHeight() / Constants.WIZARD_FONT_INITIAL_HEIGHT;
				} else {
					relWidth = theDialogWizard.getSize().getWidth() / Constants.WIZARD_INITIAL_WIDTH;
					relHeight = theDialogWizard.getSize().getHeight() / Constants.WIZARD_INITIAL_HEIGHT;
				}
			}
			
			//Se calcula la relación de los valores obtenidos
			relation = Math.round(relWidth * relHeight * theDialogWizard.getMinimumRelation());
			
		} else if (theDialogAdvisor != null){
			relWidth = theDialogAdvisor.getSize().getWidth() / Constants.INIT_WINDOW_INITIAL_WIDTH;
			relHeight = theDialogAdvisor.getSize().getHeight() / Constants.INIT_WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theDialogAdvisor.getMinimumRelation());
		} else if (theWindowAbout != null){
			if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)){
				relWidth = theWindowAbout.getSize().getWidth() / Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX;
				relHeight = theWindowAbout.getSize().getHeight() / Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX;
				relation = Math.round(relWidth * relHeight * theWindowAbout.getMinimumRelation());
			} else {
				relWidth = theWindowAbout.getSize().getWidth() / Constants.ABOUT_WINDOW_INITIAL_WIDTH;
				relHeight = theWindowAbout.getSize().getHeight() / Constants.ABOUT_WINDOW_INITIAL_HEIGHT;
				relation = Math.round(relWidth * relHeight * theWindowAbout.getMinimumRelation());
			}
		} else if (theWindowAbout != null){
			relWidth = theWindowAbout.getSize().getWidth() / Constants.ABOUT_WINDOW_INITIAL_WIDTH;
			relHeight = theWindowAbout.getSize().getHeight() / Constants.ABOUT_WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theWindowAbout.getMinimumRelation());
		} else if (theCustomDialog != null){
			relWidth = theCustomDialog.getSize().getWidth() / Constants.CUSTOMDIALOG_INITIAL_WIDTH;
			relHeight = theCustomDialog.getSize().getHeight() / Constants.CUSTOMDIALOG_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theCustomDialog.getMinimumRelation());
		} else if (theFileChooser != null){
			relWidth = theFileChooser.getDialog().getSize().getWidth() / Constants.FILE_INITIAL_WIDTH;
			relHeight = theFileChooser.getDialog().getSize().getHeight() / Constants.FILE_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theFileChooser.getMinimumRelation());			
		} else {
			relWidth = theFileChooserToSave.getDialog().getSize().getWidth() / Constants.FILE_INITIAL_WIDTH;
			relHeight = theFileChooserToSave.getDialog().getSize().getHeight() / Constants.FILE_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theFileChooserToSave.getMinimumRelation());
		}

		for (int i = 0; i < components.length; i++) {
			Component actualComponent = components[i];
			if (isResizable(actualComponent)) {
				if (relation > 10) {
					if (theWindow != null) {
						float resizeFactor = Math.round(relation / getResizingFactorFrame());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (getFontSize() + resizeFactor)));
					} else if (theDialog != null){
						float resizeFactor = Math.round(relation / getResizingFactorDialog());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (getFontSize() + resizeFactor)));
					} else if (theDialogWizard != null){
						float resizeFactor = Math.round(relation / getResizingFactorDialogWizard());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (getFontSize() + resizeFactor)));
					} else if (theDialogAdvisor != null){
						float resizeFactor = Math.round(relation / getResizingFactorFrameAdvisor());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (getFontSize() + resizeFactor)));
					} else if (theWindowAbout != null) {
						float resizeFactor = Math.round(relation / getResizingFactorFrameAbout());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (getFontSize() + resizeFactor)));
					} else if (theCustomDialog != null) {
						float resizeFactor = Math.round(relation / getResizingFactorCustomDialog());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (getFontSize() + resizeFactor)));
					}else {
						float resizeFactor = Math.round(relation / getResizingFactorFileChooser());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (getFontSize() + resizeFactor)));
					}
				} else {
					if (theWindow != null) {
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) getFontSize()));
					} else if (theDialog != null){
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) getFontSize()));
					} else if (theDialogWizard != null){
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) getFontSize()));
					} else if (theDialogAdvisor != null){
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) getFontSize()));
					} else if (theWindowAbout != null){
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) getFontSize()));
					}  else if (theCustomDialog != null){
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) getFontSize()));
					} else {
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) getFontSize()));
					}
				}
			} 
			//Caso de borde con texto
			if(actualComponent instanceof JPanel){
				Border componentBorder = ((JPanel)actualComponent).getBorder();
				if(componentBorder instanceof TitledBorder){
					TitledBorder b = (TitledBorder) componentBorder;
					float resizeFactor = Math.round(relation / getResizingFactorFrame());
					if (b.getTitleFont() != null) {
					    b.setTitleFont(b.getTitleFont().deriveFont((float) (getFontSize()-2 + resizeFactor)));
					} else {
					    b.setTitleFont(actualComponent.getFont().deriveFont((float) (getFontSize()-2 + resizeFactor)));
					}
				}
			}
			
			if (actualComponent instanceof Container) {
				if(actualComponent instanceof JMenu){
					JMenu barraMenu = (JMenu) actualComponent;
					adjustFontSize(barraMenu.getMenuComponents());
				} else if (actualComponent instanceof JTree) {
					JTree arbol = (JTree) actualComponent;
					if(arbol.getCellRenderer() instanceof DefaultTreeCellRenderer){
						adjustFontSize(new Component[]{(DefaultTreeCellRenderer)arbol.getCellRenderer()});
					}
				} else if(actualComponent instanceof JScrollPane){
					JScrollPane panel = (JScrollPane) actualComponent;
					adjustFontSize(panel.getComponents());
				} else if (actualComponent instanceof JEditorPane){
					JEditorPane editorPanel = (JEditorPane) actualComponent;
					// Resize del texto contenido en el EditorPane
					float resizeFactor = Math.round(relation / getResizingFactorDialogWizard());
					String bodyRule = "body { font-family: " + actualComponent.getFont().getFamily() + "; " + "font-size: " + (7 + resizeFactor) + "pt; }";
					((HTMLDocument)editorPanel.getDocument()).getStyleSheet().addRule(bodyRule);
					// Resize del texto del enlace, porque tiene un estilo a nivel de linea entonces es necesario cambiar el tamaño del texto a nivel de linea
					Style link;
					StyleContext sc = new StyleContext();
					link= sc.addStyle("link", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
			        StyleConstants.setFontSize(link, (7 + (int)resizeFactor));
					((HTMLDocument) editorPanel.getDocument()).setCharacterAttributes(221, 26, link, false);
				}
				else{
					// Si nos encontramos con un contenedor, redimensionamos sus hijos
					Container actualContainer = (Container) actualComponent;
					adjustFontSize(actualContainer.getComponents());
				}
			}
			
			//imagenes dentro de JLabel
			/*if (actualComponent instanceof JLabel) {
				int w = 125;
				int h = 35;
				
				int multiplicando = 2;
				if (((JLabel)actualComponent).getIcon() != null) {
					float resizeFactor;
					if(theWindow != null){
						resizeFactor = (float) (theWindow.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if(theDialog != null) {
						resizeFactor = (float) (theDialog.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theDialogWizard != null){
						resizeFactor = (float) (theDialogWizard.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theDialogAdvisor != null){
						resizeFactor = (float) (theDialogAdvisor.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theWindowAbout != null){
						resizeFactor = (float) (theWindowAbout.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					}else if (theCustomDialog != null){
						resizeFactor = (float) (theCustomDialog.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else {
						resizeFactor = (float) (theFileChooser.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					}
					resizeImage(resizeFactor, actualComponent, w, h, multiplicando);
				}
			} */
			//Redimensionado de una etiqueta con icono
			if (actualComponent instanceof IconLabel) {
				int multiplicando = 4;
				IconLabel iconLabel = (IconLabel)actualComponent;
				if (iconLabel.getOriginalIcon() != null) {
					float resizeFactor;
					if(theWindow != null){
						resizeFactor = (float) (theWindow.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if(theDialog != null) {
						resizeFactor = (float) (theDialog.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theDialogWizard != null){
						resizeFactor = (float) (theDialogWizard.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theDialogAdvisor != null){
						resizeFactor = (float) (theDialogAdvisor.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theWindowAbout != null){
						resizeFactor = (float) (theWindowAbout.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					}else if (theCustomDialog != null){
						resizeFactor = (float) (theCustomDialog.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theFileChooser != null){
						resizeFactor = (float) (theFileChooser.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else {
						resizeFactor = (float) (theFileChooserToSave.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					}
					//Se obtienen las dimensiones del icono original
					 int w = iconLabel.getOriginalIcon().getIconWidth();
					 int h = iconLabel.getOriginalIcon().getIconHeight();
					 //Se hace el resize de la imagen
					resizeImage(resizeFactor, actualComponent, w, h, multiplicando);
				}
			} 
			
			//imagenes dentro de JButton
			if (actualComponent instanceof JButton) {
				if (((JButton)actualComponent).getIcon() != null) {
					float resizeFactor;
					if(theWindow != null){
						resizeFactor = (float) (theWindow.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if(theDialog != null) {
						resizeFactor = (float) (theDialog.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theDialogWizard != null){
						resizeFactor = (float) (theDialogWizard.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theDialogAdvisor != null){
						resizeFactor = (float) (theDialogAdvisor.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theWindowAbout != null){
						resizeFactor = (float) (theWindowAbout.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					}  else if (theCustomDialog != null){
						resizeFactor = (float) (theCustomDialog.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else if (theFileChooser != null){
						resizeFactor = (float) (theFileChooser.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					} else {
						resizeFactor = (float) (theFileChooserToSave.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					}
					resizeImageButton(resizeFactor, actualComponent);
				}
			}
		}
	}
	
	/**
	 * Redimensiona una imagen
	 * 
	 * @param factor factor de redimension
	 * @param c Componente de tipo JLabel en el que se encuentra la imagen
	 * @param w Width inicial de la imagen
	 * @para h Height inicial de la imagen
	 * @param multiplicando Valor de multiplicacion para el nuevo tamaño de la imagen. Es mayor cuanto menor sea el tamaño inicial de la imagen
	 */
	public final void resizeImage(double factor, Component c, int w, int h, int multiplicando) {
		if(JAccessibilityDialogWizard.getJAccessibilityDialogWizard(c)==null){
			if (theFileChooser==null){			
				ImageIcon image = new ImageIcon();
				//Se comprueba si el componente es instancia de IconLabel
				if (c instanceof IconLabel) {
					IconLabel iconLabel = (IconLabel) c;
					//Se selecciona la imagen original del icono para hacer el resize
					image = (ImageIcon)iconLabel.getOriginalIcon();
				} else {
					image = (ImageIcon)((JLabel)c).getIcon();
				}
				ImageIcon newImage = new ImageIcon(image.getImage().getScaledInstance((int) Math.round(w * multiplicando * factor), (int) Math.round(h * multiplicando * factor), java.awt.Image.SCALE_SMOOTH));
				((JLabel)c).setIcon(newImage);
			}
		}
	}
	
	/**
	 * Redimensiona una imagen contenida en un JButton
	 * 
	 * @param factor factor de redimensi&oacute;n
	 * @param c Componente de tipo JButton en el que se encuentra la imagen
	 */
	public final void resizeImageButton(double factor, Component c) {
		
		if (theFileChooser==null && theFileChooserToSave==null){
			
		
		JButton button = (JButton) c;
		ImageIcon imageIcon = null;
		
		//Se comprueba si se trata del botón de ayuda
		if ((button.getName() != null) && (button.getName().equalsIgnoreCase("helpButton"))) {
			imageIcon = HelpUtils.IMAGEICONHELP; //Se carga la imagen original
		} else {
			imageIcon = (ImageIcon)button.getIcon(); //Se carga la imagen del componente actual
		}
		//Se redimensionan las imágenes
		ImageIcon newImage = new ImageIcon(imageIcon.getImage().getScaledInstance((int) Math.round(25 * 2 * factor),
				(int) Math.round(25 * 2 * factor), java.awt.Image.SCALE_SMOOTH));
		button.setIcon(newImage);

		button.setPreferredSize(new Dimension((int) Math.round(25 * 2 * factor),(int) Math.round(25 * 2 * factor)));
		}
	}
	
	/**
	 * Devuelve el tamaño de la fuente en función de las opciones de accesibilidad
	 * @return
	 */
	private float getFontSize(){
		if(GeneralConfig.isBigFontSize()){
			if (theDialogWizard != null){
				return 15;
			} else if (theFileChooser != null || theFileChooserToSave != null){
				return 13;
			} else {
				return 16;
			}
		}
		else{
			if (theDialogWizard != null){
				return 12;
			} else if (theDialogAdvisor != null){
				return 15;
			} else if (theFileChooser != null || theFileChooserToSave != null) {
				return 11;
			} else {
				return 13;
			}
		}
	}
	
	/**
	 * Identifica los componentes de una ventana para los que se van a realizar el redimensionado.
	 * 
	 * @param a Componente para el que se va a comprobar si se va a redimensionar.
	 * @return Boolean que indica si el componente pasado como par&aacute;metro va a ser redimensionado.
	 */
	private boolean isResizable(Component a){

		if(a instanceof JButton)
			return true;
		else if(a instanceof JToggleButton)
			return true;
		else if(a instanceof JLabel)
			return true;
		else if(a instanceof JMenuItem)
			return true;
		else if(a instanceof JComboBox)
			return true;
		else if(a instanceof JTextField)
			return true;
		else if(a instanceof JPanel)
			return true;
		else if(a instanceof JTabbedPane)
			return true;
		else if(a instanceof JRadioButton)
			return true;
		else if(a instanceof JCheckBox)
			return true;
		else if(a instanceof JTextPane)
			return true;
		else if(a instanceof JEditorPane)
			return true;
		else if(a instanceof JTree)
			return true;
		else if(a instanceof JList)
			return true;
		else if(a instanceof JFileChooser)
			return true;
		else if(a instanceof JTable)
			return true;
		else if(a instanceof JTableHeader)
			return true;
		else if(a instanceof FilePane)
			return true;
		return false;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFrame. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorFrame(){
		return 3f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JDialog. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorDialog(){
		return 3f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JDialogWizard. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorDialogWizard(){
		return 2f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFrame. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorFrameAdvisor(){
		return 3f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFrame. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorFrameAbout(){
		return 3f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un JFileChooser. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorFileChooser(){
		return 3f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicara en los componentes de un CustomDialog. Este metodo es util para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorCustomDialog(){
		return 3f;
	}
}