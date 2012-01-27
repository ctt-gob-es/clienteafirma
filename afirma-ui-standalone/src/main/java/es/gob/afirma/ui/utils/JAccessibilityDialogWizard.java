package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.principal.AccessibilityOptionsPane;
import es.gob.afirma.ui.principal.PrincipalGUI;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.BotoneraSuperior;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/**
 * Clase para generar un JDialogWizard con la posibilidad de redimension.
 * Extiende JDialogWizard.
 * @author inteco
 *
 */
public abstract class JAccessibilityDialogWizard extends JDialogWizard{
	
	/**
	 * uid.
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * Posicion X actual.
	 */
	private static int actualPositionX = -1;
	
	/**
	 * Posicion Y actual.
	 */
	private static int actualPositionY = -1;
	
	/**
	 * Ancho actual.
	 */
	private static int actualWidth = -1;
	
	/**
	 * Alto actual.
	 */
	private static int actualHeight = -1;
	
	/**
	 * Botonera.
	 */
	private BotoneraInferior botonera = null;
	
	/**
	 * Botonera.
	 */
	private BotoneraSuperior botoneraSuperior = null;
	
	/**
	 * Constructor.
	 */
	public JAccessibilityDialogWizard(){
		super();
		ResizingAdaptor adaptador = new ResizingAdaptor(null,null,this,null,null,null,null,null);
		this.addComponentListener(adaptador);
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
		
		if (GeneralConfig.isMaximized()){
			
			//Se hace el resize dependiendo del so
			if (!Platform.getOS().equals(Platform.OS.LINUX)){
				this.setBounds(0,0, maxWidth, maxHeight);
			} 
			else {
				this.setBounds(0,0, maxWidth, maxHeight - Constants.maximizeVerticalMarginLinux);
			}
			
			if (Platform.getOS().equals(Platform.OS.LINUX)){
				//Se comprueba si esta activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX));
				} else {
					setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
				}
				
			} else {
				//Se comprueba si esta activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH, Constants.WIZARD_FONT_INITIAL_HEIGHT));
				} else {
					setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
				}
			}
		} else {
			if (PrincipalGUI.getWizardActualPositionX() != -1){
				if (AccessibilityOptionsPane.isContinueBigStyle()){
					if (Platform.getOS().equals(Platform.OS.LINUX)){
						setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
					} else {
						setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
					}
				} else {
					setBounds(PrincipalGUI.getWizardActualPositionX(), PrincipalGUI.getWizardActualPositionY(), PrincipalGUI.getWizardActualWidth(), PrincipalGUI.getWizardActualHeight());
				}
	    		if (Platform.getOS().equals(Platform.OS.LINUX)){
	    			//Se comprueba si esta activado el modo negrita o fuente grande
					if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
						setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX));
					} else {
						setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
					}
	    		} else {
	    			//Se comprueba si esta activado el modo negrita o fuente grande
					if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
						setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH, Constants.WIZARD_FONT_INITIAL_HEIGHT));
					} else {
						setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
					}
	    		}
    		} else {
				if (Platform.getOS().equals(Platform.OS.LINUX)){
					//Se comprueba si esta activado el modo negrita o fuente grande
					if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
						 setBounds((screenSize.width - Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX);
				         setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX));
					} else {
			          setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
			          setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
					}
				} else {
					//Se comprueba si esta activado el modo negrita o fuente grande
					if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
						 setBounds((screenSize.width - Constants.WIZARD_FONT_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_FONT_INITIAL_HEIGHT) / 2, Constants.WIZARD_FONT_INITIAL_WIDTH, Constants.WIZARD_FONT_INITIAL_HEIGHT);
				         setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH, Constants.WIZARD_FONT_INITIAL_HEIGHT));
					} 
					else {
			          setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
			          setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
					}
				}
    		}
		}
		
		
		
	}
	
	/**
	 * Relaci&oacute;n m&iacute;nima que se aplica para la redimensi&oacute;n de los componentes.
	 * Cuanto menor es este n&uacute;mero menor es la redimensi&oacute;n aplicada.
	 * @return int Relaci&oacute;n m&iacute;nima
	 */
	public abstract int getMinimumRelation();
	
	/**
	 * Obtiene un componente de un contenedor a traves de su nombre
	 * @param name Nombre del componente a buscar
	 * @param container Contenedor donde se encuentra el componente a buscar
	 * @return componente
	 */
	private Component getComponentByName(String name, Container container){
		if(name.equals(container.getName())){
			return container;
		}
		Component[] componentes = container.getComponents();
		for(int i = 0; i < componentes.length; i++){
			if(componentes[i] instanceof Container){
				Component res = getComponentByName(name, (Container) componentes[i]);
				if(res != null){
					return res;
				}
			}
			else{
				if(componentes[i].getName().equals(name)){
					return componentes[i];
				}
			}
		}
		return null;
	}
	
	
	/**
	 * Evento de redimensionado. Comprueba el tama&ntilde;o de la ventana para habilitar o deshabilitar el bot&oacute;n
	 *  de Maximizar ventana
	 */
	@Override
	public void componentResized(ComponentEvent e) {
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
		
		//Se comprueba el so
		if (Platform.getOS().equals(Platform.OS.LINUX)){
			maxHeight = maxHeight - Constants.maximizeVerticalMarginLinux;

		} else {
			//Dimensiones que se van a considerar de maximizado
			Dimension fullScreen = new Dimension(maxWidth, maxHeight);//new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);

		    //Dimensiones actuales del dialogo
		    Dimension actualSize = getJAccessibilityDialogWizard(this).getSize();
		    if (actualSize.equals(fullScreen)){
		    	this.setResizable(false);
		    } else {
		    	this.setResizable(true);
		    }
		}
	    
		//Control de activacion y desactivacion de los botones de maximizado y restaurado
	    Component botonMaximizar = getComponentByName("maximizar", getJAccessibilityDialogWizard(this)); //$NON-NLS-1$
		Component botonRestaurar = getComponentByName("restaurar", getJAccessibilityDialogWizard(this)); //$NON-NLS-1$
		
		if (botonMaximizar!=null && botonRestaurar!=null) {
			//Se comprueba el estado de los botones de maximizado y restauracion
			if (this.getSize().equals(new Dimension(maxWidth,maxHeight))){
				botonMaximizar.setEnabled (false);
				botonRestaurar.setEnabled (true);
			} 
			else {
				botonMaximizar.setEnabled (true);
				botonRestaurar.setEnabled (false);
			}
		}
	}

/*	*//**
	 * M&eacute;todo que activa o desactiva el componente seg&uacute;n lo indicado por par&aacute;metros.
	 * @param idComponent identificador del componente
	 * @param enabled estado
	 *//*
	private void setStateComponent(String idComponent, boolean enabled) {
		Component component = getComponentByName(idComponent, getJAccessibilityDialogWizard(this));
		if(component != null){
			component.setEnabled(enabled);
		}
	}*/
	
	
	
	/**
	 * Busca el JAccessibilityDialogWizard padre de un componente.
	 * @param c El componente.
	 * @return El JAccessibilityDialogWizard buscado.
	 */
	public static JAccessibilityDialogWizard getJAccessibilityDialogWizard(final Component c)	{
		JAccessibilityDialogWizard  resultingJAccessibilityDialogWizard = null;
		Component component = c;
		while (component != null && resultingJAccessibilityDialogWizard == null) {
	        if (component instanceof JAccessibilityDialogWizard){
	        	resultingJAccessibilityDialogWizard = (JAccessibilityDialogWizard)component;
	        }
	        else {
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityDialogWizard;
	 }

	/**
	 * Devuelve la botonera.
	 * @return botonera
	 */
	public BotoneraInferior getBotonera() {
		return this.botonera;
	}
	
	/**
	 * Devuelve la botonera superior.
	 * @return botonera
	 */
	public BotoneraSuperior getBotoneraSuperior() {
		return this.botoneraSuperior;
	}

	/**
	 * Asigna la botonera.
	 * @param botonera
	 */
	public void setBotonera(BotoneraInferior botonera) {
		this.botonera=botonera;
	}
	/**
	 * Asigna la botonera.
	 * @param botonera
	 */
	public void setBotoneraSuperior(BotoneraSuperior botonera) {
		this.botoneraSuperior=botonera;
		Utils.setContrastColor(this.botoneraSuperior);
	}

	/**
     * Getter para la variable ActualPositionX.
     * @return ActualPositionX
     */
	public static int getActualPositionX() {
		return actualPositionX;
	}
	/**
     * Setter para la variable ActualPositionX.
     */
	public static void setActualPositionX(int actualPositionX) {
		JAccessibilityDialogWizard.actualPositionX = actualPositionX;
	}
	/**
     * Getter para la variable ActualPositionY.
     * @return ActualPositionY
     */
	public static int getActualPositionY() {
		return actualPositionY;
	}
	/**
     * Setter para la variable ActualPositionY.
     */
	public static void setActualPositionY(int actualPositionY) {
		JAccessibilityDialogWizard.actualPositionY = actualPositionY;
	}
	/**
     * Getter para la variable ActualWidth.
     * @return ActualWidth
     */
	public static int getActualWidth() {
		return actualWidth;
	}
	/**
     * Setter para la variable ActualWidth.
     */
	public static void setActualWidth(int actualWidth) {
		JAccessibilityDialogWizard.actualWidth = actualWidth;
	}
	/**
     * Getter para la variable ActualHeight.
     * @return ActualHeight
     */
	public static int getActualHeight() {
		return actualHeight;
	}
	/**
     * Setter para la variable ActualHeight.
     */
	public static void setActualHeight(int actualHeight) {
		JAccessibilityDialogWizard.actualHeight = actualHeight;
	}
}
