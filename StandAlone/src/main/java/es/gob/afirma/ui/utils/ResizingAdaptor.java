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
import javax.swing.JTree;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreePath;

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

	/**
	 * Constructor
	 * 
	 * @param window
	 *            ventana a redimensionar
	 */
	public ResizingAdaptor(JAccessibilityFrame window, JAccessibilityDialog dialog, JAccessibilityDialogWizard dialogWizard) {
		this.theWindow = window;
		this.theDialog = dialog;
		this.theDialogWizard = dialogWizard;
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
			} else {
				this.adjustFontSize(this.theDialogWizard.getComponents());
			}
	}

	/**
	 * Ajusta las fuentes
	 */
	public void adjustWindowFonts() {
		if (theWindow != null) {
			this.adjustFontSize(theWindow.getComponents());
		} else {
			this.adjustFontSize(theDialog.getComponents());
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
			relWidth = theWindow.getSize().getWidth() / 390;
			relHeight = theWindow.getSize().getHeight() / 352;
			relation = Math.round(relWidth * relHeight * theWindow.getMinimumRelation());
		} else if (theDialog != null) {
			relWidth = theDialog.getSize().getWidth() / 427;
			relHeight = theDialog.getSize().getHeight() / 284;
			relation = Math.round(relWidth * relHeight * theDialog.getMinimumRelation());
		} else {
			relWidth = theDialogWizard.getSize().getWidth() / 630;
			relHeight = theDialogWizard.getSize().getHeight() / 440;
			relation = Math.round(relWidth * relHeight * theDialogWizard.getMinimumRelation());
		}

		for (int i = 0; i < components.length; i++) {
			Component actualComponent = components[i];
			if (isResizable(actualComponent)) {
				if (relation > 10) {
					if (theWindow != null) {
						float resizeFactor = Math.round(relation / getResizingFactorFrame());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (13 + resizeFactor)));
					} else if (theDialog != null){
						System.out.println(actualComponent.getClass().getName());
						float resizeFactor = Math.round(relation / getResizingFactorDialog());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (7 + resizeFactor)));
					} else {
						float resizeFactor = Math.round(relation / getResizingFactorDialogWizard());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) (13 + resizeFactor)));
					}
				} else {
					if (theWindow != null) {
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) 13));
					} else if (theDialog != null){
						System.out.println(actualComponent.getClass().getName());
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) 7));
					} else {
						actualComponent.setFont(actualComponent.getFont().deriveFont((float) 13));
					}
				}
			} 
			//Caso de borde con texto
			if(actualComponent instanceof JPanel){
				Border componentBorder = ((JPanel)actualComponent).getBorder();
				if(componentBorder instanceof TitledBorder){
					TitledBorder b = (TitledBorder) componentBorder;
					float resizeFactor = Math.round(relation / getResizingFactorFrame());
					b.setTitleFont(b.getTitleFont().deriveFont((float) (7 + resizeFactor)));
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
						resizeFactor = (float) (theWindow.getHeight() * 0.0015);
					} else if(theDialog != null) {
						resizeFactor = (float) (theDialog.getHeight() * 0.0015);
					} else {
						resizeFactor = (float) (theDialogWizard.getHeight() * 0.0015);
					}
					resizeImage(resizeFactor, actualComponent, w, h, multiplicando);
				}
			}
			
			//imagenes dentro de JButton
			if (actualComponent instanceof JButton) {
				if (((JButton)actualComponent).getIcon() != null) {
					float resizeFactor;
					if(theWindow != null){
						resizeFactor = (float) (theWindow.getHeight() * 0.0015);
					} else if(theDialog != null) {
						resizeFactor = (float) (theDialog.getHeight() * 0.0015);
					} else {
						resizeFactor = (float) (theDialogWizard.getHeight() * 0.0015);
					}
					resizeImageButton(resizeFactor, actualComponent);
				}
			}
		}
	}
	
	/**
	 * Redimensiona una imagen
	 * 
	 * @param factor factor de redimensión
	 * @param c Componente de tipo JLabel en el que se encuentra la imagen
	 * @param w Width inicial de la imagen
	 * @para h Height inicial de la imagen
	 * @param multiplicando Valor de multiplicacion para el nuevo tamaño de la imagen. Es mayor cuanto menor sea el tamaño inicial de la imagen
	 */
	public final void resizeImage(double factor, Component c, int w, int h, int multiplicando) {
		ImageIcon image = new ImageIcon();
		image = (ImageIcon)((JLabel)c).getIcon();
		ImageIcon newImage = new ImageIcon(image.getImage().getScaledInstance((int) Math.round(w * multiplicando * factor), (int) Math.round(h * multiplicando * factor), java.awt.Image.SCALE_SMOOTH));
		((JLabel)c).setIcon(newImage);
	}
	
	/**
	 * Redimensiona una imagen contenida en un JButton
	 * 
	 * @param factor factor de redimensión
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
	
	private void imprime(Component componente){
		System.out.println(componente.getClass().getName());
		if(componente instanceof Container){
			Component[] hijos = ((Container) componente).getComponents();
			if(componente instanceof JTree){
				TreePath[] paths = ((JTree)componente).getSelectionPaths();
				if(paths != null){
					for(int i = 0 ; i < paths.length; i++){
						for(int j = 0; j < paths[i].getPathCount(); j++){
							System.out.println(paths[i].getPathComponent(j).getClass().getCanonicalName());
							if(paths[i].getPathComponent(j) instanceof DefaultMutableTreeNode){
								DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPathComponent(j);
								for(int k = 0; k < node.getChildCount(); k++){
									System.out.println(node.getUserObject().getClass().getCanonicalName());
									System.out.println(node.getUserObject());
								}
								System.out.println(node.getUserObject().getClass().getCanonicalName());
								System.out.println(node.getUserObject());
							}
							else{
								System.out.println("COMPONENTE RARO: " + paths[i].getPathComponent(j).getClass().getCanonicalName());
							}
						}
					}
				}
			}
			
			for (int i = 0; i < hijos.length; i++) {
				imprime(hijos[i]);
			}
		}
	}
	/**
	 * Identifica los componentes de una ventana para los que se van a realizar el redimensionado.
	 * 
	 * @param a Componente para el que se va a comprobar si se va a redimensionar.
	 * @return Boolean que indica si el componente pasado como parámetro va a ser redimensionado.
	 */
	private boolean isResizable(Component a){
		if(a instanceof JButton)
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
	 * Indica el factor de redimensionado que se aplicará en los componentes de un JFrame. Este método es útil para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorFrame(){
		return 3f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicará en los componentes de un JDialog. Este método es útil para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorDialog(){
		return 3f;
	}
	
	/**
	 * Indica el factor de redimensionado que se aplicará en los componentes de un JDialogWizard. Este método es útil para aplicar factores distintos a distinto componentes.
	 * @return Float con el factor a aplicar.
	 */
	private float getResizingFactorDialogWizard(){
		return 2f;
	}
}