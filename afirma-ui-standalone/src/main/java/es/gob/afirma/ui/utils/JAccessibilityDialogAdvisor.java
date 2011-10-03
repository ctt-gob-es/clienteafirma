package es.gob.afirma.ui.utils;

import java.awt.Component;

import javax.swing.JDialog;

/**
 * Clase para generar un JFrame con la posibilidad de redimension.
 * Extiende JFrame.
 * @author inteco
 *
 */
public abstract class JAccessibilityDialogAdvisor extends JDialog {

	private static final long serialVersionUID = 1L;
	
	private ResizingAdaptor resizingAdaptor;
	
	public JAccessibilityDialogAdvisor(){
		super();
		this.resizingAdaptor = new ResizingAdaptor(null, null, null,this,null);
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
}