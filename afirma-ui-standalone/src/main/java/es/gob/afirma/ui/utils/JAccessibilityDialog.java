package es.gob.afirma.ui.utils;


import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JDialog;

/**
 * Clase para generar un JDialog con la posibilidad de redimension.
 * Extiende JDialog
 * @author inteco
 *
 */
public abstract class JAccessibilityDialog extends JDialog {
	
	private static final long serialVersionUID = 1L;
	
	public JAccessibilityDialog(){
		super();
		ResizingAdaptor adaptador = new ResizingAdaptor(null,this,null);
		this.addComponentListener(adaptador);
		this.addComponentListener(new ComponentAdapter() {
		    public void componentResized(ComponentEvent e)
		    {
		    	resized(e);
		    }
		});

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
	
	/**
	 * Evento de redimensionado. Comprueba el tamaÃ±o de la ventana para habilitar o deshabilitar el boton
	 *  de Maximizar ventana
	 */
	public void resized(ComponentEvent e) {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
	    Dimension fullScreen = new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
	    Dimension actualSize = getJAccessibilityDialog(this).getSize();
	    Component boton = getComponentByName("maximizar", getJAccessibilityDialog(this));
	    if(boton != null){
	    	if (actualSize.equals(fullScreen)){
				boton.setEnabled(false);
		    } else {
		    	boton.setEnabled(true);
		    }
	    }
	}
	
	/**
	 * Busca el JAccessibilityDialog padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityDialog buscado.
	 */
	public static JAccessibilityDialog getJAccessibilityDialog(Component component)
	{
		JAccessibilityDialog  resultingJAccessibilityDialog = null;
		while (component != null && resultingJAccessibilityDialog == null)
		{
	        if (component instanceof JAccessibilityDialog){
	        	resultingJAccessibilityDialog = (JAccessibilityDialog)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityDialog;
	 }
}
