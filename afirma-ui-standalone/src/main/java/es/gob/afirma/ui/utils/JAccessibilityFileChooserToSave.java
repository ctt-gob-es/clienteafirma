package es.gob.afirma.ui.utils;



import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridLayout;
import java.awt.HeadlessException;
import java.awt.Rectangle;

import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import java.io.File;

import javax.accessibility.AccessibleContext;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import es.gob.afirma.ui.principal.PrincipalGUI;

/**
 * Clase que extiende JFileChooser para hacerla accesible.
 * @author lmerayo
 *
 */
public class JAccessibilityFileChooserToSave extends JAccessibilityFileChooser{
	
	/**
	 * Serial version ID.
	 */
	private static final long serialVersionUID = 1L;
	
	
	private JDialog theDialog;
	private JToolBar jTool;
	private ResizingAdaptor resizingAdaptor;
	private JDialog dialog;

	/**
	 * Relacion minima.
	 */
	public int getMinimumRelation(){
		return 9;
	}

	/**
	 * Constructor.
	 * @param file directorio.
	 */
	public JAccessibilityFileChooserToSave (File file)
	 {
		super(file);
		init();
		

	 }//constructor
	
	/**
	 * Constructor.
	 * @param file directorio.
	 */
	public JAccessibilityFileChooserToSave ()
	 {
		super();
		init();
		

	 }//constructor

	/**
	 * Inicializa el diálogo haciendo accesibles sus componentes.
	 */
	private void init(){
		//Se comprueba si se esta en el modo Alto contraste
		if (GeneralConfig.isHighContrast()){
			setHighContrast(this);
		}
		
		//Asignacion de mnemonics
		
		//Etiqueta buscar en ...
		setLabelMnemonics(this, "FileChooser.lookInLabelText", KeyEvent.VK_U); //$NON-NLS-1$
		
		//Boton Cancelar
		setButtonMnemonics(this, "FileChooser.cancelButtonText", KeyEvent.VK_C); //$NON-NLS-1$
		
		//Boton Abrir
		setButtonMnemonics(this, "FileChooser.openButtonText", KeyEvent.VK_A); //$NON-NLS-1$
		
		//Toggle buttons
		//TODO: Revisar puesto que los botones que se hacen accesibles estan predefinidos
		setToggleButtonMnemonics(this);
	}

	/**
	 * Crea la ventana de diálogo.
	 * @param Component parent
	 */
	@Override
	protected JDialog createDialog(Component parent) throws HeadlessException {
		String title = getUI().getDialogTitle(this);
        putClientProperty(
                AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY,
                title);

        //dialog = new JDialog((Frame) this.getParent(), title, true);
        //Se comprueba el tipo de componente padre, para asignar el correcto y que asi se muestre el icono asociado.
        if (parent instanceof JDialog) {
        	this.dialog = new JDialog((JDialog)parent, title, true);
        } else if (parent instanceof Frame){
        	this.dialog = new JDialog((Frame)parent, title, true);
        }else {
        	//Se obtiene el componente root
	        Component root = SwingUtilities.getRoot(parent);
	        if ((root!=null) && (root instanceof Frame)) {
	        	this.dialog = new JDialog((Frame)root, title, true);
	        } else {
	        	this.dialog = new JDialog();
	        	this.dialog.setTitle(title);
	        	this.dialog.setModal(true);
	        }
        }
        
        this.dialog.setComponentOrientation(this .getComponentOrientation());
        
      //Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
        
        Container contentPane = this.dialog.getContentPane();
        
//        if (!GeneralConfig.isAccessibility()){
//        	contentPane.setLayout(new GridLayout());
//        	contentPane.add(this , BorderLayout.CENTER);
//        } else {
        	removeWindowsToolBar();
        	contentPane.setLayout(new GridLayout());
        	
        	this.dialog.addComponentListener(new ComponentListener() {
    			
    			@Override
    			public void componentShown(ComponentEvent e) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void componentResized(ComponentEvent e) {
    				// TODO Auto-generated method stub
    				resized();
    			}
    			
    			@Override
    			public void componentMoved(ComponentEvent e) {
    				// TODO Auto-generated method stub
    				resized();
    			}
    			
    			@Override
    			public void componentHidden(ComponentEvent e) {
    				// TODO Auto-generated method stub
    				
    			}
    		});
        	// Dimensiones de la ventana
        	if (GeneralConfig.isMaximized()){
        		this.dialog.setPreferredSize(new Dimension(maxWidth, maxHeight));
        	} else {
        		if (PrincipalGUI.fileActualPositionX != -1){
    	    		this.dialog.setPreferredSize(new Dimension(PrincipalGUI.fileActualWidth, PrincipalGUI.fileActualHeight));
        		}
        	}
        	if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
        		this.dialog.setMinimumSize(new Dimension(Constants.FILE_FONT_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT));
        	} else {
        		this.dialog.setMinimumSize(new Dimension(Constants.FILE_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT));
        	}
            for (int i = 0; i<this.getComponentCount();i++){
            	if (this.getComponent(i).getClass().getName().equals("javax.swing.JToolBar")){
            		this.jTool = (JToolBar)this.getComponent(i);
            		for (int j = 0; j<this.jTool.getComponentCount();j++){
                		// Al cambiar entre vista en lista y detalles se llama a adjustWindowFonts para que calcule el tamano del texto
                		if (this.jTool.getComponent(j).getClass().getName().equals("javax.swing.JToggleButton")){
                			final JToggleButton boton = ((JToggleButton)this.jTool.getComponent(j));
                			// Al cambiar entre vista en lista y detalles se llama a adjustWindowFonts para que calcule el tamano del texto
                			((JToggleButton)(this.jTool.getComponent(j))).addMouseListener(new MouseListener() {
        						
        						@Override
        						public void mouseReleased(MouseEvent e) {
        							// TODO Auto-generated method stub
        							
        						}
        						
        						@Override
        						public void mousePressed(MouseEvent e) {
        							// TODO Auto-generated method stub
        							
        						}
        						
        						@Override
        						public void mouseExited(MouseEvent e) {
        							// TODO Auto-generated method stub
        							
        						}
        						
        						@Override
        						public void mouseEntered(MouseEvent e) {
        							// TODO Auto-generated method stub
        							
        						}
        						
        						@Override
        						public void mouseClicked(MouseEvent e) {
        							// TODO Auto-generated method stub
        							callResize();
        						}
        					});
                			((JToggleButton)(this.jTool.getComponent(j))).addKeyListener(new KeyListener() {
        						
        						@Override
        						public void keyTyped(KeyEvent e) {
        							// TODO Auto-generated method stub
        						}
        						
        						@Override
        						public void keyReleased(KeyEvent e) {
        							// TODO Auto-generated method stub
        							if (e.getKeyCode() == KeyEvent.VK_SPACE){
        								boton.doClick();
        								callResize();
        							}
        						}
        						
        						@Override
        						public void keyPressed(KeyEvent e) {
        							// TODO Auto-generated method stub
        						}
        					});
                			
                		}
                		Utils.remarcar((JComponent)this.jTool.getComponent(j));
                    	Utils.setFontBold((JComponent)this.jTool.getComponent(j));
                	}
            	} else {
            		accessibility((JPanel)this.getComponent(i));
            	}
            	
            }
            
            contentPane.add(this , BorderLayout.CENTER);
//        }
        
        if (JDialog.isDefaultLookAndFeelDecorated()) {
            boolean supportsWindowDecorations = UIManager
                    .getLookAndFeel().getSupportsWindowDecorations();
            if (supportsWindowDecorations) {
                this.dialog.getRootPane().setWindowDecorationStyle(
                        JRootPane.FILE_CHOOSER_DIALOG);
            }
        }
        this.dialog.setResizable(true);
        this.dialog.pack();
        this.dialog.setLocationRelativeTo(parent);
        
//        if (GeneralConfig.isAccessibility()) {
        	this.resizingAdaptor = new ResizingAdaptor(null,null,null,null,null,null,null,this);
        	this.theDialog = this.dialog;
     		this.dialog.addComponentListener(this.resizingAdaptor);
//        }
        
        return this.dialog;
	}
	
	/**
	 * Elimina la barra de accesos a carpetas de windows de la ventana
	 */
	private void removeWindowsToolBar(){
		
		for (int i=0; i<this.getComponentCount();i++){
			if (this.getComponent(i) instanceof JToolBar){
        		if (!this.getComponent(i).getClass().getName().equals("javax.swing.JToolBar")){
        			this.remove(this.getComponent(i));
        			
        		}
        	}
        }

	}

	/**
	 * Devuelve el diálogo.
	 * return diálogo
	 */
	@Override
	public JDialog getDialog(){
		return this.theDialog;
	}

	/**
	 * Ajuste de fuentes.
	 */
	@Override
	final void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}
	
	/**
	 * Se almacena la posicion actual.
	 */
	@Override
	void resized(){
		if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.fileActualPositionX = this.dialog.getX();
	    	PrincipalGUI.fileActualPositionY = this.dialog.getY();
	    	PrincipalGUI.fileActualWidth = this.dialog.getWidth();
	    	PrincipalGUI.fileActualHeight = this.dialog.getHeight();
    	}
	}
}