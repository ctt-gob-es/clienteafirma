package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;


/**
 * Clase para generar un JFrame con la posibilidad de redimension.
 * Extiende JFrame.
 * @author inteco
 *
 */
public abstract class JAccessibilityFrameAdvisor extends JFrame {

	private static final long serialVersionUID = 1L;
	
	protected static int actualPositionX = -1;
	
	protected static int actualPositionY = -1;
	
	protected static int actualWidth = -1;
	
	protected static int actualHeight = -1;
	
	/** Ruta del JAR en donde se almacenan los iconos de la aplicaci&oacute;n. */
    private static final String ICON_DIR_PATH = "/resources/images/";
	
	private ResizingAdaptor resizingAdaptor;
	
	public JAccessibilityFrameAdvisor(){
		super();
		// Icono de @firma
        setIconImage(this.loadIcon("afirma_ico.png").getImage());
		this.resizingAdaptor = new ResizingAdaptor(null, null, null,this,null,null,null,null);
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
		/*Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
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
	    }*/
			    
	}
	
	/**
	 * Obtiene un componente de un contenedor a traves de su nombre
	 * @param name Nombre del componente a buscar
	 * @param container Contenedor donde se encuentra el componente a buscar
	 * @return
	 *//*
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
	}*/
	
	/**
     * Carga un icono contenido en el directorio de iconos del proyecto.
     * @param filename Nombre del icono.
     * @return Icono.
     */
    private ImageIcon loadIcon(final String filename) {
        return new ImageIcon(this.getClass().getResource(ICON_DIR_PATH + filename));
    }
	
}