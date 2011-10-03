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
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.tree.DefaultTreeCellRenderer;

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

	/**
	 * Constructor
	 * 
	 * @param window
	 *            ventana a redimensionar
	 */
	public ResizingAdaptor(JAccessibilityFrame window, JAccessibilityDialog dialog, JAccessibilityDialogWizard dialogWizard,JAccessibilityDialogAdvisor dialogAdvisor,JAccessibilityFrameAbout windowAbout) {
		this.theWindow = window;
		this.theDialog = dialog;
		this.theDialogWizard = dialogWizard;
		this.theDialogAdvisor = dialogAdvisor;
		this.theWindowAbout = windowAbout;
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
			} else {
				this.adjustFontSize(this.theWindowAbout.getComponents());
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
		} else {
			this.adjustFontSize(theWindowAbout.getComponents());
		}
	}

	/**
	 * Ajusta el tamaño de fuente de una ventana
	 * 
	 * @param components
	 */
	private void adjustFontSize(Component[] components) {
		// Se calcula la relación de aspecto para redimensionar el texto
		double relWidth;
		double relHeight;
		float relation;
		if (theWindow!=null) {
			relWidth = theWindow.getSize().getWidth() / Constants.WINDOW_INITIAL_WIDTH;
			relHeight = theWindow.getSize().getHeight() / Constants.WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theWindow.getMinimumRelation());
		} else if (theDialog != null) {
			relWidth = theDialog.getSize().getWidth() / Constants.OPTION_INITIAL_WIDTH;
			relHeight = theDialog.getSize().getHeight() / Constants.OPTION_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theDialog.getMinimumRelation());
		} else if (theDialogWizard != null){
			relWidth = theDialogWizard.getSize().getWidth() / Constants.WIZARD_INITIAL_WIDTH;
			relHeight = theDialogWizard.getSize().getHeight() / Constants.WIZARD_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theDialogWizard.getMinimumRelation());
		} else if (theDialogAdvisor != null){
			relWidth = theDialogAdvisor.getSize().getWidth() / Constants.INIT_WINDOW_INITIAL_WIDTH;
			relHeight = theDialogAdvisor.getSize().getHeight() / Constants.INIT_WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theDialogAdvisor.getMinimumRelation());
		} else {
			relWidth = theWindowAbout.getSize().getWidth() / Constants.ABOUT_WINDOW_INITIAL_WIDTH;
			relHeight = theWindowAbout.getSize().getHeight() / Constants.ABOUT_WINDOW_INITIAL_HEIGHT;
			relation = Math.round(relWidth * relHeight * theWindowAbout.getMinimumRelation());
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
					} else {
						float resizeFactor = Math.round(relation / getResizingFactorFrameAbout());
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
					    b.setTitleFont(b.getTitleFont().deriveFont((float) (7 + resizeFactor)));
					} else {
					    b.setTitleFont(actualComponent.getFont().deriveFont((float) (7 + resizeFactor)));
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
				}
				else{
					// Si nos encontramos con un contenedor, redimensionamos sus hijos
					Container actualContainer = (Container) actualComponent;
					adjustFontSize(actualContainer.getComponents());
				}
			}
			
			//imagenes dentro de JLabel
			if (actualComponent instanceof JLabel) {
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
					} else {
						resizeFactor = (float) (theWindowAbout.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
					}
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
					} else {
						resizeFactor = (float) (theWindowAbout.getHeight() * Constants.RESIZING_IMAGES_FACTOR);
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
			ImageIcon image = new ImageIcon();
			image = (ImageIcon)((JLabel)c).getIcon();
			ImageIcon newImage = new ImageIcon(image.getImage().getScaledInstance((int) Math.round(w * multiplicando * factor), (int) Math.round(h * multiplicando * factor), java.awt.Image.SCALE_SMOOTH));
			((JLabel)c).setIcon(newImage);
		}
	}
	
	/**
	 * Redimensiona una imagen contenida en un JButton
	 * 
	 * @param factor factor de redimensi&oacute;n
	 * @param c Componente de tipo JButton en el que se encuentra la imagen
	 */
	public final void resizeImageButton(double factor, Component c) {
		ImageIcon image = new ImageIcon();
		image = (ImageIcon)((JButton)c).getIcon();
			ImageIcon newImage = new ImageIcon(image.getImage().getScaledInstance((int) Math.round(25 * 2 * factor),
					(int) Math.round(25 * 2 * factor), java.awt.Image.SCALE_SMOOTH));
			((JButton)c).setIcon(newImage);
			((JButton)c).setPreferredSize(new Dimension((int) Math.round(25 * 2 * factor),(int) Math.round(25 * 2 * factor)));
	}
	
	/**
	 * Devuelve el tama�o de la fuente en funci�n de las opciones de accesibilidad
	 * @return
	 */
	private float getFontSize(){
		if(GeneralConfig.isBigFontSize()){
			if (theDialogWizard != null){
				return 14;
			} else {
				return 15;
			}
		}
		else{
			if (theDialogWizard != null){
				return 12;
			} else if (theDialogAdvisor != null){
				return 15;
			}
			else {
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
		else if(a instanceof JTree)
			return true;
		else if(a instanceof JList)
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
}