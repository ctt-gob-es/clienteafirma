package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;

import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

public abstract class JAccessibilityDialogWizard extends JDialogWizard{
	
	public JAccessibilityDialogWizard(){
		super();
		ResizingAdaptor adaptador = new ResizingAdaptor(null,null,this);
		this.addComponentListener(adaptador);
		setMinimumSize(new Dimension(630, 480));
	}
	
	public abstract int getInitialWidth();
	public abstract int getInitialHeight();
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
	 * Evento de redimensionado. Comprueba el tamaño de la ventana para habilitar o deshabilitar el boton
	 *  de Maximizar ventana
	 */
	public void componentResized(ComponentEvent e) {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
	    Dimension fullScreen = new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
	    Dimension actualSize = BotoneraInferior.getJAccessibilityDialogWizard(this).getSize();
	    Component boton = getComponentByName("maximizar", BotoneraInferior.getJAccessibilityDialogWizard(this));
	    if(boton != null){
	    	if (actualSize.equals(fullScreen)){
				boton.setEnabled(false);
		    } else {
		    	boton.setEnabled(true);
		    }
	    }
	}
}
