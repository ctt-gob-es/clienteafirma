package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;

import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 * Clase para generar un JDialogWizard con la posibilidad de redimension.
 * Extiende JDialogWizard.
 * @author inteco
 *
 */
public abstract class JAccessibilityDialogWizard extends JDialogWizard{
	
	private static final long serialVersionUID = 1L;
	
	public JAccessibilityDialogWizard(){
		super();
		ResizingAdaptor adaptador = new ResizingAdaptor(null,null,this,null,null);
		this.addComponentListener(adaptador);
		if (GeneralConfig.isMaximized()){
			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			this.setBounds(0,0,(int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
		} else {
			setMinimumSize(new Dimension(Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT));
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
	    Component boton = getComponentByName("maximizar", getJAccessibilityDialogWizard(this));
	    if(boton != null){
	    	if (actualSize.equals(fullScreen)){
				boton.setEnabled(false);
		    } else {
		    	boton.setEnabled(true);
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
