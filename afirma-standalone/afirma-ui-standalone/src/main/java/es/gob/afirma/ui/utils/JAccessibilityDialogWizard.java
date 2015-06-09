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
 * @author INTECO
 *
 */
public abstract class JAccessibilityDialogWizard extends JDialogWizard{

	/** UID. */
	private static final long serialVersionUID = 1L;

	/** Posici&oacute;n X actual. */
	private static int actualPositionX = -1;

	/** Posici&oacute;n Y actual. */
	private static int actualPositionY = -1;

	/** Ancho actual. */
	private static int actualWidth = -1;

	/**
	 * Alto actual.
	 */
	private static int actualHeight = -1;

	/** Botonera inferior. */
	private BotoneraInferior botonera = null;

	/** Botonera superior. */
	private BotoneraSuperior botoneraSuperior = null;

	/**
	 * Constructor.
	 */
	public JAccessibilityDialogWizard(){
		super();
		final ResizingAdaptor adaptador = new ResizingAdaptor(null,null,this,null,null,null,null,null);
		this.addComponentListener(adaptador);
		final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		final Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = (int)rect.getWidth();
		final int maxHeight = (int)rect.getHeight();

		if (GeneralConfig.isMaximized()){

			//Se hace el resize dependiendo del so
			if (!Platform.getOS().equals(Platform.OS.LINUX)){
				this.setBounds(0,0, maxWidth, maxHeight);
			}
			else {
				this.setBounds(0,0, maxWidth, maxHeight - Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX);
			}

			if (Platform.getOS().equals(Platform.OS.LINUX)){
				//Se comprueba si esta activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX));
				}
				else {
					setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
				}

			}
			else {
				//Se comprueba si esta activado el modo negrita o fuente grande
				if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
					setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH, Constants.WIZARD_FONT_INITIAL_HEIGHT));
				}
				else {
					setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
				}
			}
		}
		else {
			if (PrincipalGUI.getWizardActualPositionX() != -1){
				if (AccessibilityOptionsPane.isContinueBigStyle()){
					if (Platform.getOS().equals(Platform.OS.LINUX)){
						setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
					}
					else {
						setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
					}
				}
				else {
					setBounds(PrincipalGUI.getWizardActualPositionX(), PrincipalGUI.getWizardActualPositionY(), PrincipalGUI.getWizardActualWidth(), PrincipalGUI.getWizardActualHeight());
				}
	    		if (Platform.getOS().equals(Platform.OS.LINUX)){
	    			//Se comprueba si esta activado el modo negrita o fuente grande
					if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
						setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX));
					}
					else {
						setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
					}
	    		}
	    		else {
	    			//Se comprueba si esta activado el modo negrita o fuente grande
					if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
						setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH, Constants.WIZARD_FONT_INITIAL_HEIGHT));
					}
					else {
						setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
					}
	    		}
    		} else {
				if (Platform.getOS().equals(Platform.OS.LINUX)){
					//Se comprueba si esta activado el modo negrita o fuente grande
					if(GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
						 setBounds((screenSize.width - Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX);
				         setMinimumSize(new Dimension(Constants.WIZARD_FONT_INITIAL_WIDTH_LINUX, Constants.WIZARD_FONT_INITIAL_HEIGHT_LINUX));
					}
					else {
			          setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
			          setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
					}
				}
				else {
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
	private Component getComponentByName(final String name, final Container container){
		if(name.equals(container.getName())){
			return container;
		}
		final Component[] componentes = container.getComponents();
		for (final Component componente : componentes) {
			if(componente instanceof Container){
				final Component res = getComponentByName(name, (Container) componente);
				if(res != null){
					return res;
				}
			}
			else{
				if(componente.getName().equals(name)){
					return componente;
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
	public void componentResized(final ComponentEvent e) {
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		final Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		final int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();

		//Se comprueba el so
		if (Platform.getOS().equals(Platform.OS.LINUX)){
			maxHeight = maxHeight - Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX;

		} else {
			//Dimensiones que se van a considerar de maximizado
			final Dimension fullScreen = new Dimension(maxWidth, maxHeight);//new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);

		    //Dimensiones actuales del dialogo
		    final Dimension actualSize = getJAccessibilityDialogWizard(this).getSize();
		    if (actualSize.equals(fullScreen)){
		    	this.setResizable(false);
		    } else {
		    	this.setResizable(true);
		    }
		}

		//Control de activacion y desactivacion de los botones de maximizado y restaurado
	    final Component botonMaximizar = getComponentByName("maximizar", getJAccessibilityDialogWizard(this)); //$NON-NLS-1$
		final Component botonRestaurar = getComponentByName("restaurar", getJAccessibilityDialogWizard(this)); //$NON-NLS-1$

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

	/** Asigna la botonera.
	 * @param botonera Botonera a asignar. */
	public void setBotonera(final BotoneraInferior botonera) {
		this.botonera = botonera;
	}

	/**
	 * Asigna la botonera.
	 * @param botonera Botonera a asignar. */
	public void setBotoneraSuperior(final BotoneraSuperior botonera) {
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

	/** Estabece la posici&oacute;n horizontal (X) actual del di&aacute;logo.
	 * @param actualPositionX Posici&oacute;n horizontal (X) actual del di&aacute;logo */
	public static void setActualPositionX(final int actualPositionX) {
		JAccessibilityDialogWizard.actualPositionX = actualPositionX;
	}

	/**
     * Getter para la variable ActualPositionY.
     * @return ActualPositionY
     */
	public static int getActualPositionY() {
		return actualPositionY;
	}

	/** Estabece la posici&oacute;n vertical (Y) actual del di&aacute;logo.
	 * @param actualPositionY Posici&oacute;n vertical (Y) actual del di&aacute;logo */
	public static void setActualPositionY(final int actualPositionY) {
		JAccessibilityDialogWizard.actualPositionY = actualPositionY;
	}

	/**
     * Getter para la variable ActualWidth.
     * @return ActualWidth
     */
	public static int getActualWidth() {
		return actualWidth;
	}

	/** Estabece la altura actual del di&aacute;logo.
	 * @param actualWidth Altura actual del di&aacute;logo. */
	public static void setActualWidth(final int actualWidth) {
		JAccessibilityDialogWizard.actualWidth = actualWidth;
	}

	/**
     * Getter para la variable ActualHeight.
     * @return ActualHeight
     */
	public static int getActualHeight() {
		return actualHeight;
	}

	/** Establece la altura actual del di&aacute;logo.
	 * @param actualHeight Altura actual del di&aacute;logo. */
	public static void setActualHeight(final int actualHeight) {
		JAccessibilityDialogWizard.actualHeight = actualHeight;
	}
}
