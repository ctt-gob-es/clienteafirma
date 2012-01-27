package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import java.io.File;

import javax.accessibility.AccessibleContext;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.JWindow;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import es.gob.afirma.core.misc.Platform;
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
	
	private JButton restoreButton = null;
	private JButton maximizeButton = null;
	private JPanel accesibilityButtonsPanel = null;
	
	protected static int actualPositionX = -1;
	protected static int actualPositionY = -1;
	protected static int actualWidth = -1;
	protected static int actualHeight = -1;


	/**
	 * Relacion minima.
	 */
	@Override
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
	 */
	public JAccessibilityFileChooserToSave ()
	 {
		super();
		init();
		

	 }//constructor
	
	/**
	 * Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n X
	 */
    public int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 426) / 2 ;
	}
    
    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
	 */
	public int getInitialY() {
        Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.MACOSX)){
        	return (screenSize.height - 485) / 2;
        } else {
        	return (screenSize.height - 456) / 2;
        }
	}

	/**
	 * Inicializa el dialogo haciendo accesibles sus componentes.
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
	 * Crea la ventana de dialogo.
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
        	contentPane.setLayout(new GridBagLayout());
        	
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
        		if (PrincipalGUI.getFileActualPositionX() != -1){
    	    		this.dialog.setPreferredSize(new Dimension(PrincipalGUI.getFileActualWidth(), PrincipalGUI.getFileActualHeight()));
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
            
          //Restricciones para los botones
			GridBagConstraints consButtons = new GridBagConstraints();
			consButtons.fill = GridBagConstraints.BOTH;
			consButtons.gridx = 0;
			consButtons.gridy = 0;
			consButtons.weightx = 1.0;
			consButtons.weighty = 0.05;
            
			this.accesibilityButtonsPanel = createAccessibilityButtonsPanel();
            contentPane.add(this.accesibilityButtonsPanel, consButtons);
            
            consButtons.gridy = 1;
            consButtons.weighty = 0.95;
            contentPane.add(this, consButtons);
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
	 * Devuelve el dialogo.
	 * return dialogo
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
	    	PrincipalGUI.setFileActualPositionX(this.dialog.getX());
	    	PrincipalGUI.setFileActualPositionY(this.dialog.getY());
	    	PrincipalGUI.setFileActualWidth(this.dialog.getWidth());
	    	PrincipalGUI.setFileActualHeight(this.dialog.getHeight());
    	}
	}
	
	/**
	 * Genera el panel que contiene los botones de restaurar y maximizar.
	 * Aplica accesibilidad sobre los botones.
	 * @return JPanel con los botones de restaurar y maximizar
	 */
	private JPanel createAccessibilityButtonsPanel() {
		JPanel panelAccesibilidad = new JPanel(new GridBagLayout());
		
		//Para el tooltip
		final JWindow tip = new JWindow();
		final JLabel tipText = new JLabel();
		
		//Panel que va a contener los botones de accesibilidad
		JPanel panel = new JPanel(new GridBagLayout());
		
		//Restricciones para los botones
		GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.BOTH;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.weightx = 1.0;
		consButtons.weighty = 1.0;
		consButtons.insets = new Insets(0,0,0,0);  //right padding
		
		Dimension dimension = new Dimension(25, 25);
		
		//Restore button
		JPanel restorePanel = new JPanel();
		ImageIcon imageIconRestore= new ImageIcon(JAccessibilityFileChooser.class.getResource("/resources/images/restore.png"));
		imageIconRestore = new ImageIcon(imageIconRestore.getImage().getScaledInstance(dimension.width, dimension.height, Image.SCALE_SMOOTH));
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description"));
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());
		
		this.restoreButton.addFocusListener(new FocusListener() {			
			
			@Override
			public void focusLost(FocusEvent e) {
				Utils.showToolTip(false, tip, restoreButton, tipText);
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				Utils.showToolTip(true, tip, restoreButton, tipText);
			}
		});
		this.restoreButton.setPreferredSize(dimension);

		this.restoreButton.setName("restaurar");
		Utils.remarcar(this.restoreButton);
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
			
			@Override			
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
		
		
		panel.add(restorePanel, consButtons);
		
		
		consButtons.gridx = 1;
		consButtons.insets = new Insets(0,0,0,5);  //right padding
		
		//Maximize button
		JPanel maximizePanel = new JPanel();

		ImageIcon imageIconMaximize = new ImageIcon(JAccessibilityFileChooser.class.getResource("/resources/images/maximize.png"));
		imageIconMaximize = new ImageIcon(imageIconMaximize.getImage().getScaledInstance(dimension.width, dimension.height, Image.SCALE_SMOOTH));
		this.maximizeButton = new JButton(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description"));
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());	
		
		this.maximizeButton.setName("maximizar");
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);
				
		Utils.remarcar(this.maximizeButton);
		maximizePanel.add(this.maximizeButton);	
		
		this.maximizeButton.addFocusListener(new FocusListener() {
			
			@Override
			public void focusLost(FocusEvent e) {
				Utils.showToolTip(false, tip, maximizeButton, tipText);
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				Utils.showToolTip(true, tip, maximizeButton, tipText);
			}
			
		});
		
		this.maximizeButton.addActionListener(new ActionListener() {
		    	public void actionPerformed(ActionEvent e) {
		    		maximizarActionPerformed();
				}
			});

		
		panel.add(maximizePanel, consButtons);

		//Se añade al panel general
		//Restricciones para el panel de botones
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.insets = new Insets(0,0,0,0); 
		c.anchor=GridBagConstraints.EAST;
		panelAccesibilidad.add(panel, c);
		
		
		// Habilitado/Deshabilitado de botones restaurar/maximizar
    	if (GeneralConfig.isMaximized()){
    		//Se deshabilita el botón de maximizado
    		this.maximizeButton.setEnabled(false);
    		//Se habilita el botón de restaurar
    		this.restoreButton.setEnabled(true);
    	} else {
    		//Se habilita el botón de maximizado
    		this.maximizeButton.setEnabled(true);
    		//Se deshabilita el botón de restaurar
    		this.restoreButton.setEnabled(false);
    	}
		return panelAccesibilidad;
	}
	
	/**
	 * Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado
	 */
	public void restaurarActionPerformed(){
		
		if (actualPositionX != -1 && actualPositionY != -1 && actualWidth != -1 && actualHeight != -1){
			theDialog.setBounds(actualPositionX, actualPositionY, actualWidth, actualHeight);
		} else {
			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			setBounds(this.getInitialX(), this.getInitialY(), Constants.FILE_FONT_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT);
    			setMinimumSize(new Dimension(getSize().width, getSize().height));    			
    		} else {
    			setBounds(this.getInitialX(), this.getInitialY(), Constants.FILE_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT);
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
		}
		this.maximizeButton.setEnabled (true);
		this.restoreButton.setEnabled (false);
	}
	
	/**
	 * Maximiza la ventana
	 */
	public void maximizarActionPerformed(){
		actualPositionX = theDialog.getX();
		actualPositionY = theDialog.getY();
		actualWidth = theDialog.getWidth();
		actualHeight = theDialog.getHeight();

		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		
		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
		
		this.maximizeButton.setEnabled (false);
		this.restoreButton.setEnabled (true);
		
		//Se hace el resize dependiendo del so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			theDialog.setBounds(0,0, maxWidth, maxHeight);
		} else {
			theDialog.setBounds(0,0, maxWidth, maxHeight - Constants.maximizeVerticalMarginLinux);
		}
	}

	/**
	 * Devuelve el panel de botones de maximizar/restaurar
	 * @return JPanel con los botones de maximizar/restaurar
	 */
	public JPanel getAccesibilityButtonsPanel(){
		return this.accesibilityButtonsPanel;
	}
}