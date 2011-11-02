package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JDialog;

import es.gob.afirma.core.misc.Platform;

/**
 * Clase para generar un JDialog con la posibilidad de redimension.
 * Extiende JDialog.
 * @author inteco
 *
 */
public abstract class JAccessibilityDialogAdvisor extends JDialog {

	private static final long serialVersionUID = 1L;
	
	protected static int actualPositionX = -1;
	
	protected static int actualPositionY = -1;
	
	protected static int actualWidth = -1;
	
	protected static int actualHeight = -1;
	
	private ResizingAdaptor resizingAdaptor;
	
	public JAccessibilityDialogAdvisor(){
		super();
		this.resizingAdaptor = new ResizingAdaptor(null, null, null,this,null,null);
		this.addComponentListener(this.resizingAdaptor);
		this.addComponentListener(new ComponentAdapter() {
		    @Override
            public void componentResized(ComponentEvent e)
		    {
		    	resized(e);
		    }
		    @Override
            public void componentMoved(ComponentEvent e)
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
	
	public final void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}
	
	/**
	 * Busca el JAccessibilityFrameAdvisor padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityFrameAdvisor buscado.
	 */
	public static JAccessibilityDialogAdvisor getJAccessibilityDialogAdvisor(Component component)
	{
		JAccessibilityDialogAdvisor  resultingJAccessibilityDialogAdvisor = null;
		while (component != null && resultingJAccessibilityDialogAdvisor == null)
		{
	        if (component instanceof JAccessibilityDialogAdvisor){
	        	resultingJAccessibilityDialogAdvisor = (JAccessibilityDialogAdvisor)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityDialogAdvisor;
	 }
	
	/**
	 * Evento de redimensionado. Almacena el tamaño y posicion de la ventana para su restauracion.
	 */
	public void resized(final ComponentEvent e) {
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
		
		//Se comprueba el so
		if (Platform.getOS().equals(Platform.OS.LINUX)){
			maxHeight = maxHeight - Constants.maximizeVerticalMarginLinux;
		}
		
		//Dimensiones que se van a considerar de maximizado
	    Dimension fullScreen = new Dimension(maxWidth, maxHeight);//new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);

	    //Dimensiones actuales del dialogo
	    Dimension actualSize = getJAccessibilityDialogAdvisor(this).getSize();
	    if (actualSize.equals(fullScreen)){
	    	this.setResizable(false);
	    } else {
	    	this.setResizable(true);
	    }
			    
		/*Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
	    Dimension fullScreen = new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
	    Dimension actualSize = getJAccessibilityDialogAdvisor(this).getSize();
	    Component boton = getComponentByName("maximizar", getJAccessibilityDialogAdvisor(this));
	    Component botonRestaurar = getComponentByName("restaurar", getJAccessibilityDialogAdvisor(this));
	    if(boton != null){
	    	if (actualSize.equals(fullScreen)){
				boton.setEnabled(false);
				if (botonRestaurar != null) {
	    			//Si la ventana está maximizada, el botón de restaurar debe estar visible
	    			botonRestaurar.setEnabled(true);
	    		}
		    } else {
		    	boton.setEnabled(true);
		    	if (botonRestaurar != null) {
			    	botonRestaurar.setEnabled(false); //Se deshabilita
		    	}
		    }
	    }*/
	}
	
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
	
}