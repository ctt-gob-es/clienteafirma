package es.gob.afirma.ui.utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridLayout;
import java.awt.HeadlessException;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;

import javax.accessibility.AccessibleContext;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.UIManager;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.principal.PrincipalGUI;

/**
 * Clase que extiende JFileChooser para hacerla accesible.
 * @author lmerayo
 *
 */
public class JAccessibilityFileChooser extends JFileChooser{
	
	/**
	 * Serial version ID.
	 */
	private static final long serialVersionUID = 1L;
	
	private JDialog theDialog;
	private JToolBar jTool;
	private ResizingAdaptor resizingAdaptor;
	private JDialog dialog;
	
	public int getMinimumRelation(){
		return 9;
	}

	/**
	 * Constructor.
	 * @param file directorio.
	 */
	public JAccessibilityFileChooser (File file)
	 {
		super(file);
		
		//Se comprueba si se está en el modo Alto contraste
	    if (GeneralConfig.isHighContrast()){
			setHighContrast((Container)this);
		}
		
		//Asignacion de mnemonics
		
		//Etiqueta buscar en ...
		setLabelMnemonics((Container)this, "FileChooser.lookInLabelText", KeyEvent.VK_B);
		
		//Boton Cancelar
		setButtonMnemonics((Container)this, "FileChooser.cancelButtonText", KeyEvent.VK_C);
		
		//Boton Abrir
		setButtonMnemonics((Container)this, "FileChooser.openButtonText", KeyEvent.VK_A);
		
		//Toggle buttons
		//TODO: Revisar puesto que los botones que se hacen accesibles estan predefinidos
		setToggleButtonMnemonics((Container)this);
		
		

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
	 * Asigna el mnem&oacute;nico indicado al bot&oacute;n identificado por la clave .
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnem&oacute;nico.
	 * @param mnemonic mnem&oacute;nico que se va a asignar al componente
	 */
	public void setButtonMnemonics( Container c, String key, int mnemonic ) {
	    int len = c.getComponentCount(); //N�mero de componentes del contenedor
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo bot�n
	      if (comp instanceof JButton) {
		        JButton button = (JButton)comp;
		        //Se comprueba si su texto es el indicado por la clave
		        if (button.getText() ==  UIManager.get(key)) {
		        	//Se le asigna el mnem�nico
		        	button.setMnemonic(mnemonic);
		        }
		    } else if (comp instanceof Container) {
		    	//Llamada recursiva
		    	setButtonMnemonics((Container)comp, key, mnemonic);
		    }
	    }//for
	  }
	
	/**
	 * Asigna el mnem&oacute;nico indicado a la etiqueta identificada por la clave .
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnem&oacute;nico.
	 * @param mnemonic mnem&oacute;nico que se va a asignar al componente
	 */
	public void setLabelMnemonics( Container c, String key, int mnemonic ) {
		 //N�mero de componentes del contenedor
	    int len = c.getComponentCount();
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i);  //Se obtiene un componente
	      //Se comprueba si es de tipo etiqueta
	      if (comp instanceof JLabel) {
	        JLabel label = (JLabel)comp;
	        //Se comprueba si su texto es el indicado por la clave
	        if (label.getText() ==  UIManager.get(key)) {
	        	//Se le asigna el mnem�nico
	        	label.setDisplayedMnemonic(mnemonic);
		    }
	      } else if (comp instanceof Container) {
	    	  	//Llamada recursiva
		    	setLabelMnemonics((Container)comp, key, mnemonic);
		    }
	    }//for
	  }
	
	/**
	 * Asigna un mnem&oacute;nico predefinido a ciertos toggleButton contenidos en el componente.
	 * @param c contenedor global
	 */
	public void setToggleButtonMnemonics( Container c) {
		 //Numero de componentes del contenedor
	    int len = c.getComponentCount();
	  //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo JToggleButton
	      if (comp instanceof JToggleButton) {
		    	JToggleButton toggleButton = (JToggleButton) comp;
		    	 //Se almacena su texto asociado
		    	String text = toggleButton.getText();
		    	//Se comprueba que no esta vacio
		    	if (text!=null && !text.equalsIgnoreCase("")) {
		    		
		    		//Se tratan los botones segun su texto
			    	if (text.equalsIgnoreCase("<html><center>Equipo</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_E);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		String newText = text.substring(0, 14) +"<u>"+text.charAt(14)+"</u>"+text.substring(15);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Elementos recientes</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_L);
			    		//Como el texto del boton contiene c�digo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Escritorio</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_S);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);
			    		
			    	} else if (text.equalsIgnoreCase("<html><center>Mis documentos</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_I);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		String newText = text.substring(0, 15) +"<u>"+text.charAt(15)+"</u>"+text.substring(16);
			    		toggleButton.setText(newText);

			    	} else if (text.equalsIgnoreCase("<html><center>Red</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_R);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		String newText = text.substring(0, 14) +"<u>"+text.charAt(14)+"</u>"+text.substring(15);
			    		toggleButton.setText(newText);
			    	}
		    	}
		    } else if (comp instanceof Container) {
		    	//Llamada recursiva
		    	setToggleButtonMnemonics((Container)comp);
		    }
	    }//for
	  }
	/**
	 * Define el modo alto contraste para los componentes de la ventana.
	 * @param c contenedor global
	 */
	public void setHighContrast(Container c) {
		 //Numero de componentes del contenedor
	    int len = c.getComponentCount();
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      Component comp = c.getComponent(i);  //Se obtiene un componente
	      //Se comprueba si es de tipo etiqueta
	      if (comp instanceof JLabel) {
	        JLabel label = (JLabel)comp;
	        if (GeneralConfig.isHighContrast()){
	        	label.setForeground(Color.WHITE);
	        } else {
	        	label.setForeground(Color.BLACK);
	        }
	      } else if(comp instanceof JToggleButton){
	    	  JToggleButton toggleButton = (JToggleButton)comp;
	    	  if (GeneralConfig.isHighContrast()){
	    		  toggleButton.setForeground(Color.WHITE);
	    	  } else {
	    		  toggleButton.setForeground(Color.BLACK);
	    	  }
	      } else if (comp instanceof JComboBox){
	    	  JComboBox comboBox = (JComboBox)comp;
	    	  if (GeneralConfig.isHighContrast()){
	    		  comboBox.setBackground(Color.WHITE);
	    	  } else {
	    		  comboBox.setBackground(Color.BLACK);  
	    	  }
	      } else if (comp instanceof Container) {
	    	  	//Llamada recursiva
	    	  setHighContrast((Container)comp);
		    }
	    }//for
	  }
	
	@Override
	protected JDialog createDialog(Component parent) throws HeadlessException {
		String title = getUI().getDialogTitle(this);
        putClientProperty(
                AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY,
                title);

        dialog = new JDialog((Frame) this.getParent(), title, true);
        
        dialog.setComponentOrientation(this .getComponentOrientation());
        
      //Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
        
        Container contentPane = dialog.getContentPane();
        
//        if (!GeneralConfig.isAccessibility()){
//        	contentPane.setLayout(new GridLayout());
//        	contentPane.add(this , BorderLayout.CENTER);
//        } else {
        	removeWindowsToolBar();
        	contentPane.setLayout(new GridLayout());
        	
        	dialog.addComponentListener(new ComponentListener() {
    			
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
        		dialog.setPreferredSize(new Dimension(maxWidth, maxHeight));
        	} else {
        		if (PrincipalGUI.fileActualPositionX != -1){
        			dialog.setPreferredSize(new Dimension(PrincipalGUI.fileActualWidth, PrincipalGUI.fileActualHeight));
        		}
        		
        	}
        	if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
        		dialog.setMinimumSize(new Dimension(Constants.FILE_FONT_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT));
        	} else {
        		dialog.setMinimumSize(new Dimension(Constants.FILE_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT));
        	}
            for (int i = 0; i<this.getComponentCount();i++){
            	if (this.getComponent(i).getClass().getName().equals("javax.swing.JToolBar")){
            		jTool = (JToolBar)this.getComponent(i);
            		for (int j = 0; j<jTool.getComponentCount();j++){
                		// Al cambiar entre vista en lista y detalles se llama a adjustWindowFonts para que calcule el tamaño del texto
                		if (jTool.getComponent(j).getClass().getName().equals("javax.swing.JToggleButton")){
                			final JToggleButton boton = ((JToggleButton)jTool.getComponent(j));
                			// Al cambiar entre vista en lista y detalles se llama a adjustWindowFonts para que calcule el tamaño del texto
                			((JToggleButton)(jTool.getComponent(j))).addMouseListener(new MouseListener() {
        						
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
                			((JToggleButton)(jTool.getComponent(j))).addKeyListener(new KeyListener() {
        						
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
                		Utils.remarcar((JComponent)jTool.getComponent(j));
                    	Utils.setFontBold((JComponent)jTool.getComponent(j));
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
                dialog.getRootPane().setWindowDecorationStyle(
                        JRootPane.FILE_CHOOSER_DIALOG);
            }
        }
        dialog.setResizable(true);
        dialog.pack();
        dialog.setLocationRelativeTo(parent);
        
//        if (GeneralConfig.isAccessibility()) {
        	resizingAdaptor = new ResizingAdaptor(null,null,null,null,null,this,null,null);
        	this.theDialog = dialog;
     		dialog.addComponentListener(resizingAdaptor);
//        }
        
        return dialog;
	}
	
	/**
	 * Elimina la barra de accesos a carpetas de windows de la ventana
	 */
	public void removeWindowsToolBar(){
		
		for (int i=0; i<this.getComponentCount();i++){
			if (this.getComponent(i) instanceof JToolBar){
        		if (!this.getComponent(i).getClass().getName().equals("javax.swing.JToolBar")){
        			this.remove(this.getComponent(i));
        			
        		}
        	}
        }

	}
	
	/**
	 * Aplica la configuraci&oacute;n de accesibilidad a los componentes del panel
	 * @param jPanel Panel con los componentes. Puede contener m&aacute;s paneles
	 */
	public void accessibility(JPanel jPanel){		
		for (int i=0;i<jPanel.getComponentCount();i++){
			if (jPanel.getComponent(i) instanceof JPanel){
				accessibility((JPanel)jPanel.getComponent(i));
			} else {
				Utils.remarcar((JComponent)jPanel.getComponent(i));
				Utils.setFontBold((JComponent)jPanel.getComponent(i));
				Utils.setContrastColor((JComponent)jPanel.getComponent(i));
			}
		}
	}
	
	public JDialog getDialog(){
		return this.theDialog;
	}
	
	public final void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}
	
	private void resized(){
		if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.fileActualPositionX = dialog.getX();
	    	PrincipalGUI.fileActualPositionY = dialog.getY();
	    	PrincipalGUI.fileActualWidth = dialog.getWidth();
	    	PrincipalGUI.fileActualHeight = dialog.getHeight();
    	}
	}
	
	
}

