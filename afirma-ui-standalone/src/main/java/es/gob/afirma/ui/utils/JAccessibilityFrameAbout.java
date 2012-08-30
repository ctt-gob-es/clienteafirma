package es.gob.afirma.ui.utils;

import javax.swing.JFrame;

/**
 * Clase para generar un JFrame con la posibilidad de redimension.
 * Extiende JFrame.
 * @author inteco
 *
 */
public abstract class JAccessibilityFrameAbout extends JFrame {

	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor.
	 */
	public JAccessibilityFrameAbout(){
		super();
		this.addComponentListener(new ResizingAdaptor(null,null,null,null,this,null,null,null));
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
	/*public final void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}*/

	/**
	 * Busca el JAccessibilityFrame padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityFrame buscado.
	 */
	/*public static JAccessibilityFrameAbout getJAccessibilityFrameAbout(Component component)
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
	 }*/
}