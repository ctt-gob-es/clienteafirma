package es.gob.afirma.ui.utils;
import java.awt.Component;

import javax.swing.JFrame;

/**
 * Clase para generar un JFrame con la posibilidad de redimension.
 * Extiende JFrame.
 * @author inteco
 *
 */
public abstract class JAccessibilityFrame extends JFrame {

	private static final long serialVersionUID = 1L;
	
	private ResizingAdaptor resizingAdaptor;
	
	public JAccessibilityFrame(){
		super();
		this.resizingAdaptor = new ResizingAdaptor(this, null, null,null,null,null, null,null);
		this.addComponentListener(this.resizingAdaptor);
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
	 * Busca el JAccessibilityFrame padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityFrame buscado.
	 */
	public static JAccessibilityFrame getJAccessibilityFrame(Component component)
	{
		JAccessibilityFrame  resultingJAccessibilityFrame = null;
		while (component != null && resultingJAccessibilityFrame == null)
		{
	        if (component instanceof JAccessibilityFrame){
	        	resultingJAccessibilityFrame = (JAccessibilityFrame)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityFrame;
	 }
}