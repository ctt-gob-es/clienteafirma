package es.gob.afirma.ui.utils;
import java.awt.Component;

import javax.swing.JFrame;

/**
 * Clase para generar un JFrame con la posibilidad de redimension.
 * Extiende JFrame.
 * @author inteco
 *
 */
public abstract class JAccessibilityFrameAbout extends JFrame {

	private static final long serialVersionUID = 1L;
	
	private ResizingAdaptor resizingAdaptor;
	
	public JAccessibilityFrameAbout(){
		super();
		this.resizingAdaptor = new ResizingAdaptor(null, null, null,null,this);
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
	public static JAccessibilityFrameAbout getJAccessibilityFrameAbout(Component component)
	{
		JAccessibilityFrameAbout  resultingJAccessibilityFrameAbout = null;
		while (component != null && resultingJAccessibilityFrameAbout == null)
		{
	        if (component instanceof JAccessibilityFrameAbout){
	        	resultingJAccessibilityFrameAbout = (JAccessibilityFrameAbout)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityFrameAbout;
	 }
}