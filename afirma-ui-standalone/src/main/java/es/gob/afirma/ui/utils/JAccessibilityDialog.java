package es.gob.afirma.ui.utils;


import java.awt.Component;
import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;

import es.gob.afirma.core.misc.Platform;

/**
 * Clase para generar un JDialog con la posibilidad de redimension.
 * Extiende JDialog
 * @author inteco
 *
 */
public abstract class JAccessibilityDialog extends JDialog {
	
	/**
	 * uid.
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * Posicion X actual.
	 */
	private static int actualPositionX = -1;
	
	/**
	 * Posicion Y actual.
	 */
	private static int actualPositionY = -1;
	
	/**
	 * Ancho actual.
	 */
	private static int actualWidth = -1;
	
	/**
	 * Alto actual.
	 */
	private static int actualHeight = -1;
	
	/**
	 * Componente de redimensionado.
	 */
	private ResizingAdaptor resizingAdaptor;
	
	/** Ruta del JAR en donde se almacenan los iconos de la aplicaci&oacute;n. */
    private static final String ICON_DIR_PATH = "/resources/images/";
	
	/**
	 * Constructor.
	 * @param frame ventana padre.
	 */
	public JAccessibilityDialog(JFrame frame){
		super(frame);
		// Icono de @firma
        setIconImage(this.loadIcon("afirma_ico.png").getImage());
		resizingAdaptor = new ResizingAdaptor(null,this,null,null,null,null,null,null);
		this.addComponentListener(resizingAdaptor);
		this.addComponentListener(new ComponentAdapter() {
			/**
			 * Evento que se lanza cuando se redimensiona el componente.
			 */
		    public void componentResized(ComponentEvent e)
		    {
		    	resized(e);
		    }
		    /**
			 * Evento que se lanza cuando se mueve el componente.
			 */
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

	/**
	 * Evento de redimensionado. Comprueba el tamaÃ±o de la ventana para habilitar o deshabilitar el boton
	 *  de Maximizar ventana. Tambien almacena el tamaño y posicion de la ventana para su restauracion.
	 */
	public void resized(ComponentEvent e) {

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


		//Se comprueba el so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
		    //Dimensiones actuales del dialogo
		    Dimension actualSize = this.getSize();
		    if (actualSize.equals(fullScreen)){
		    	this.setResizable(false);
		    } else {
		    	this.setResizable(true);
		    }
		}
	}
	
	/**
	 * Busca el JAccessibilityDialog padre de un componente.
	 * @param component El componente.
	 * @return El JAccessibilityDialog buscado.
	 */
	public static JAccessibilityDialog getJAccessibilityDialog(Component component)
	{
		JAccessibilityDialog  resultingJAccessibilityDialog = null;
		while (component != null && resultingJAccessibilityDialog == null)
		{
	        if (component instanceof JAccessibilityDialog){
	        	resultingJAccessibilityDialog = (JAccessibilityDialog)component;
	        }
	        else{
	        	component = component.getParent();
	        }
		 }
		 return resultingJAccessibilityDialog;
	 }
	
	 /**
     * Carga un icono contenido en el directorio de iconos del proyecto.
     * @param filename Nombre del icono.
     * @return Icono.
     */
    private ImageIcon loadIcon(final String filename) {
        return new ImageIcon(this.getClass().getResource(ICON_DIR_PATH + filename));
    }
   
    /**
     * Ajuste de fuentes.
     */
    public final void callResize(){
    	this.resizingAdaptor.adjustWindowFonts();
	}
    
    /**
     * Getter para la variable ActualPositionX.
     * @return ActualPositionX
     */
	public static int getActualPositionX() {
		return actualPositionX;
	}
	/**
     * Setter para la variable ActualPositionX.
     */
	public static void setActualPositionX(int actualPositionX) {
		JAccessibilityDialog.actualPositionX = actualPositionX;
	}
	/**
     * Getter para la variable ActualPositionY.
     * @return ActualPositionY
     */
	public static int getActualPositionY() {
		return actualPositionY;
	}
	/**
     * Setter para la variable ActualPositionY.
     */
	public static void setActualPositionY(int actualPositionY) {
		JAccessibilityDialog.actualPositionY = actualPositionY;
	}
	/**
     * Getter para la variable ActualWidth.
     * @return ActualWidth
     */
	public static int getActualWidth() {
		return actualWidth;
	}
	/**
     * Setter para la variable ActualWidth.
     */
	public static void setActualWidth(int actualWidth) {
		JAccessibilityDialog.actualWidth = actualWidth;
	}
	/**
     * Getter para la variable ActualHeight.
     * @return ActualHeight
     */
	public static int getActualHeight() {
		return actualHeight;
	}
	/**
     * Setter para la variable ActualHeight.
     */
	public static void setActualHeight(int actualHeight) {
		JAccessibilityDialog.actualHeight = actualHeight;
	}
}
