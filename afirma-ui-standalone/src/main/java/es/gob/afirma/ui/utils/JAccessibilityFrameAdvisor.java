package es.gob.afirma.ui.utils;


import javax.swing.ImageIcon;
import javax.swing.JFrame;


/**
 * Clase para generar un JFrame con la posibilidad de redimension.
 * Extiende JFrame.
 * @author inteco
 *
 */
public abstract class JAccessibilityFrameAdvisor extends JFrame {
	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;	
	
	/** Ruta del JAR en donde se almacenan los iconos de la aplicaci&oacute;n. */
    private static final String ICON_DIR_PATH = "/resources/images/";
	
    /**
     * Adaptador de redimensionado.,
     */
	private ResizingAdaptor resizingAdaptor;
	
	/**
	 * Constructor.
	 */
	public JAccessibilityFrameAdvisor(){
		super();
		// Icono de @firma
        setIconImage(this.loadIcon("afirma_ico.png").getImage());
		this.resizingAdaptor = new ResizingAdaptor(null, null, null,this,null,null,null,null);
		this.addComponentListener(this.resizingAdaptor);
	}
	
	/**
	 * Relacion mínima que se aplica para la redimension de los componentes.
	 * Cuanto menor es este número menor es la redimension aplicada.
	 * @return int Relacion mínima
	 */
	public abstract int getMinimumRelation();
	
	/**
	 * Ajuste de fuentes.
	 */
	/*public final void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}*/
	
	/**
	 * Busca el JAccessibilityFrameAdvisor padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityFrameAdvisor buscado.
	 */
	/*public static JAccessibilityFrameAdvisor getJAccessibilityFrameAdvisor(Component component)
	{
		JAccessibilityFrameAdvisor  resultingJAccessibilityFrameAdvisor = null;
		while (component != null && resultingJAccessibilityFrameAdvisor == null)
		{
	        if (component instanceof JAccessibilityFrameAdvisor){
	        	resultingJAccessibilityFrameAdvisor = (JAccessibilityFrameAdvisor)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityFrameAdvisor;
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