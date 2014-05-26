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

	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * Componente de redimensionado.
	 */
	private ResizingAdaptor resizingAdaptor;
	
	/**
	 * Constructor.
	 */
	public JAccessibilityFrame(){
		super();
		this.resizingAdaptor = new ResizingAdaptor(this, null, null,null,null,null, null,null);
		this.addComponentListener(this.resizingAdaptor);
	}
	
	/**
	 * Relacion minima que se aplica para la redimension de los componentes.
	 * Cuanto menor es este numero menor es la redimension aplicada.
	 * @return int Relacion minima
	 */
	public abstract int getMinimumRelation();
	
	/**
	 * Ajuste de fuentes.
	 */
	public final void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}
	
	/**
	 * Busca el JAccessibilityFrame padre de un componente.
	 * @param c El componente.
	 * @return El JAccessibilityFrame buscado.
	 */
	public static JAccessibilityFrame getJAccessibilityFrame(final Component c)	{
		JAccessibilityFrame  resultingJAccessibilityFrame = null;
		Component component = c;
		while (component != null && resultingJAccessibilityFrame == null) {
	        if (component instanceof JAccessibilityFrame){
	        	resultingJAccessibilityFrame = (JAccessibilityFrame)component;
	        }
	        else {
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityFrame;
	 }
}