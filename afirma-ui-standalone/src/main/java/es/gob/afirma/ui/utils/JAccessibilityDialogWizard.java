package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.principal.PrincipalGUI;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 * Clase para generar un JDialogWizard con la posibilidad de redimension.
 * Extiende JDialogWizard.
 * @author inteco
 *
 */
public abstract class JAccessibilityDialogWizard extends JDialogWizard{
	
	private static final long serialVersionUID = 1L;
	
	public static int actualPositionX = -1;
	
	public static int actualPositionY = -1;
	
	public static int actualWidth = -1;
	
	public static int actualHeight = -1;
	
	public JAccessibilityDialogWizard(){
		super();
		ResizingAdaptor adaptador = new ResizingAdaptor(null,null,this,null,null);
		this.addComponentListener(adaptador);
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		if (GeneralConfig.isMaximized()){
			this.setBounds(0,0,(int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
			if (Platform.getOS().equals(Platform.OS.LINUX)){
				setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
			} else {
				setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
			}
		} else {
			if (PrincipalGUI.wizardActualPositionX != -1){
	    		setBounds(PrincipalGUI.wizardActualPositionX, PrincipalGUI.wizardActualPositionY, PrincipalGUI.wizardActualWidth, PrincipalGUI.wizardActualHeight);
	    		if (Platform.getOS().equals(Platform.OS.LINUX)){
	    			setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
	    		} else {
	    			setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
	    		}
    		} else {
				if (Platform.getOS().equals(Platform.OS.LINUX)){
			          setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
			          setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX));
				} else {
			          setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
			          setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
				}
    		}
		}
		
	}
	
	/**
	 * Relación mínima que se aplica para la redimensión de los componentes.
	 * Cuanto menor es este número menor es la redimensión aplicada.
	 * @return int Relación mínima
	 */
	public abstract int getMinimumRelation();
	
	/**
	 * Obtiene un componente de un contenedor a traves de su nombre
	 * @param name Nombre del componente a buscar
	 * @param container Contenedor donde se encuentra el componente a buscar
	 * @return
	 */
	private Component getComponentByName(String name, Container container){
		if(name.equals(container.getName())){
			return container;
		}
		else {
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
		}
		return null;
	}
	
	@Override
	/**
	 * Evento de redimensionado. Comprueba el tamaÃ±o de la ventana para habilitar o deshabilitar el boton
	 *  de Maximizar ventana
	 */
	public void componentResized(ComponentEvent e) {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
	    Dimension fullScreen = new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
	    Dimension actualSize = getJAccessibilityDialogWizard(this).getSize();
	    Component botonMaximizar = getComponentByName("maximizar", getJAccessibilityDialogWizard(this));
	    Component botonRestaurar = getComponentByName("restaurar", getJAccessibilityDialogWizard(this));
	    if(botonMaximizar != null){
	    	if (actualSize.equals(fullScreen)){
	    		botonMaximizar.setEnabled(false);
	    		if (botonRestaurar != null) {
	    			//Si la ventana está maximizada, el botón de restaurar debe estar visible
	    			botonRestaurar.setEnabled(true);
	    		}
		    } else {
		    	botonMaximizar.setEnabled(true);
		    	if (botonRestaurar != null) {
			    	botonRestaurar.setEnabled(false); //Se deshabilita
		    	}
		    }
	    }
	}
	
	/**
	 * Busca el JAccessibilityDialogWizard padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityDialogWizard buscado.
	 */
	public static JAccessibilityDialogWizard getJAccessibilityDialogWizard(Component component)
	{
		JAccessibilityDialogWizard  resultingJAccessibilityDialogWizard = null;
		while (component != null && resultingJAccessibilityDialogWizard == null)
		{
	        if (component instanceof JAccessibilityDialogWizard){
	        	resultingJAccessibilityDialogWizard = (JAccessibilityDialogWizard)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityDialogWizard;
	 }
}
