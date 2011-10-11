package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JDialog;

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
		this.resizingAdaptor = new ResizingAdaptor(null, null, null,this,null);
		this.addComponentListener(this.resizingAdaptor);
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
	public void resized(ComponentEvent e) {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		if (this.getWidth()!=(int)screenSize.getWidth() && this.getHeight()!=(int)screenSize.getHeight()-35){
			actualPositionX = this.getX();
			actualPositionY = this.getY();
			actualWidth = this.getWidth();
			actualHeight = this.getHeight();
		}
	}
}