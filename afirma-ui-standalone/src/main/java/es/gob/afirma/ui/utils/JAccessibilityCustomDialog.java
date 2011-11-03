package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JDialog;


/**
 * Componente que define un dialogo de alerta. 
 * @author inteco
 *
 */
public abstract class JAccessibilityCustomDialog extends JDialog {
	
	private static final long serialVersionUID = 1L;
	
	protected static int actualPositionX = -1;
	
	protected static int actualPositionY = -1;
	
	protected static int actualWidth = -1;
	
	protected static int actualHeight = -1;
	
	/**
	 * Constructor con parámetros.
	 */
	public JAccessibilityCustomDialog(JDialog dialog, boolean modal){
		super(dialog, modal);
		ResizingAdaptor adaptador = new ResizingAdaptor(null,null,null,null,null,null,this);
		this.addComponentListener(adaptador);
		this.addComponentListener(new ComponentAdapter() {
		    public void componentResized(ComponentEvent e)
		    {
		    	resized(e);
		    }
		    public void componentMoved(ComponentEvent e)
		    {
		    	resized(e);
		    }
		});
	}
	
	/**
	 * Constructor.
	 */
	public JAccessibilityCustomDialog(){
		super();
		ResizingAdaptor adaptador = new ResizingAdaptor(null,null,null,null,null,null,this);
		this.addComponentListener(adaptador);
		this.addComponentListener(new ComponentAdapter() {
		    public void componentResized(ComponentEvent e)
		    {
		    	resized(e);
		    }
		    public void componentMoved(ComponentEvent e)
		    {
		    	resized(e);
		    }
		});
	}
	
	/**
	 * Relacion minima que se aplica para la redimensión de los componentes.
	 * Cuanto menor es este número menor es la redimensión aplicada.
	 * @return int Relación mínima
	 */
	public abstract int getMinimumRelation();

	
	/**
	 * Evento de redimensionado. Comprueba el tamanio de la ventana para habilitar o deshabilitar el boton
	 *  de Maximizar ventana. Tambien almacena el tamaño y posicion de la ventana para su restauracion.
	 */
	public void resized(ComponentEvent e) {
		//Variable que controlará sin las dimensiones van a exceder el límite
		boolean limitControl = false;
		
		//Se obtienen las dimensiones de maximizado
		int maxWidth = Constants.CUSTOMDIALOG_MAX_WIDTH;
		int maxHeight = Constants.CUSTOMDIALOG_MAX_HEIGHT;

		//Dimensiones que se van a considerar de maximizado
	    Dimension fullScreen = new Dimension(maxWidth, maxHeight);

	    //Dimensiones actuales del dialogo
	    Dimension actualSize = this.getSize();
	    
	    //Se comprueba las bounds del diálogo actual
	    if (e.getSource() instanceof CustomDialog) {
	    	CustomDialog customDialog = (CustomDialog) e.getSource();
	    	Rectangle rect = customDialog.getBounds();
	    	
	    	//Se comprueba que no sobrepasen el límite
	    	if (rect.width > maxWidth) {
	    		rect.width = maxWidth;
	    		limitControl = true;
	    	}
	    	if (rect.height > maxHeight) {
	    		rect.height = maxHeight;
	    		limitControl = true;
	    	}
	    	//Si sobrepasaban el limite, se hace un resize a las dimensiones límite indicadas
	    	if (limitControl) {
	    		this.setBounds(rect.x, rect.y, rect.width, rect.height);
	    	}
	    }
	    
	    Component botonMaximizar = getComponentByName("maximizar", this);
		Component botonRestaurar = getComponentByName("restaurar", this);
	    
	    //Control de posibilidad de redimensionado y habilitado/deshabilitado de botones
	    if (actualSize.equals(fullScreen)){
	    	this.setResizable(false);
	    	if ((botonMaximizar!=null) && (botonRestaurar!=null)) {
	    		botonMaximizar.setEnabled (false);
	    		botonRestaurar.setEnabled (true);
	    	}
	    } else {
	    	this.setResizable(true);
	    	if ((botonMaximizar!=null) && (botonRestaurar!=null)) {
	    		botonMaximizar.setEnabled (true);
	    		botonRestaurar.setEnabled (false);
	    	}
	    }
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
	
	
	/**
	 * Busca el JAccessibilityCustomDialog padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityCustomDialog buscado.
	 */
	public static JAccessibilityCustomDialog getJAccessibilityCustomDialog(Component component)
	{
		JAccessibilityCustomDialog  resultingJAccessibilityCustomDialog = null;
		while (component != null && resultingJAccessibilityCustomDialog == null)
		{
	        if (component instanceof JAccessibilityDialog){
	        	resultingJAccessibilityCustomDialog = (JAccessibilityCustomDialog)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityCustomDialog;
	 }
	


}
