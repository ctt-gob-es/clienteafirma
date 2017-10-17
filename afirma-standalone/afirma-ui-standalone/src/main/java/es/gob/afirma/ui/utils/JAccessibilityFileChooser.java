/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
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
class JAccessibilityFileChooser extends JFileChooser{

	/**
	 * Serial version ID.
	 */
	private static final long serialVersionUID = 1L;

	private JDialog theDialog;
	private JToolBar jTool;
	private ResizingAdaptor resizingAdaptor;
	private JDialog dialog;

	private JButton restoreButton = new JButton();
	JButton getRestoreButton() {
		return this.restoreButton;
	}

	private final JButton maximizeButton = new JButton();
	JButton getMaximizeButton() {
		return this.maximizeButton;
	}

	private JPanel accesibilityButtonsPanel = null;
	private JButton openButton = null;
	private static boolean isMaximized;

	private static int actualPositionX = -1;
	protected static int getActualPositionX() {
		return actualPositionX;
	}
	protected static void setActualPositionX(final int x) {
		actualPositionX = x;
	}

	private static int actualPositionY = -1;
	private static int actualWidth = -1;
	private static int actualHeight = -1;

	/** Relacion m&iacute;nima. */
	@SuppressWarnings("static-method")
	int getMinimumRelation(){
		return 9;
	}

	/**
	 * Constructor.
	 */
	JAccessibilityFileChooser() {
		super();
		init();
	}//constructor

	/**
	 * Constructor.
	 * @param file directorio.
	 */
	JAccessibilityFileChooser(final File file) {
		super(file);
		init();
	}//constructor

	/**
	 * Inicializa el dialogo haciendo accesibles sus componentes.
	 */
	private final void init(){
		//Se comprueba si se esta en el modo Alto contraste
	    if (GeneralConfig.isHighContrast()){
			setHighContrast(this);
		}

		//Asignacion de mnemonics

		//Etiqueta buscar en ...
		setLabelMnemonics(this, "FileChooser.lookInLabelText", KeyEvent.VK_B); //$NON-NLS-1$

		//Boton Cancelar
		setButtonMnemonics(this, "FileChooser.cancelButtonText", KeyEvent.VK_C); //$NON-NLS-1$

		//Boton Abrir
		setButtonMnemonics(this, "FileChooser.openButtonText", KeyEvent.VK_A); //$NON-NLS-1$

		//Toggle buttons
		//TODO: Revisar puesto que los botones que se hacen accesibles estan predefinidos
		setToggleButtonMnemonics(this);
	}

	/**
	 * Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n X
	 */
    @SuppressWarnings("static-method")
	public int getInitialX() {
		return (Toolkit.getDefaultToolkit().getScreenSize().width - 620) / 2 ;
	}

    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
	 */
	@SuppressWarnings("static-method")
	public int getInitialY() {
        final Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.MACOSX)){
        	return (screenSize.height - 485) / 2;
        }
        return (screenSize.height - 456) / 2;
	}


	/**
	 * Asigna el mnem&oacute;nico indicado al bot&oacute;n identificado por la clave .
	 * @param c contenedor global
	 * @param key clave del componente al que se le va a asignar el mnem&oacute;nico.
	 * @param mnemonic mnem&oacute;nico que se va a asignar al componente
	 */
	protected void setButtonMnemonics( final Container c, final String key, final int mnemonic ) {
	    final int len = c.getComponentCount(); //Nomero de componentes del contenedor
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      final Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo boton
	      if (comp instanceof JButton) {
		        final JButton button = (JButton)comp;
		        //Se comprueba si su texto es el indicado por la clave
		        if (button.getText() ==  UIManager.get(key)) {
		        	//Se le asigna el mnemonico
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
	protected void setLabelMnemonics( final Container c, final String key, final int mnemonic ) {
		 //Nomero de componentes del contenedor
	    final int len = c.getComponentCount();
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      final Component comp = c.getComponent(i);  //Se obtiene un componente
	      //Se comprueba si es de tipo etiqueta
	      if (comp instanceof JLabel) {
	        final JLabel label = (JLabel)comp;
	        //Se comprueba si su texto es el indicado por la clave
	        if (label.getText() ==  UIManager.get(key)) {
	        	//Se le asigna el mnemonico
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
	protected void setToggleButtonMnemonics(final Container c) {
		 //Numero de componentes del contenedor
	    final int len = c.getComponentCount();
	    final String openTag = "<u>";
	    final String closeTag = "</u>";
	  //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      final Component comp = c.getComponent(i); //Se obtiene un componente
	      //Se comprueba si es de tipo JToggleButton
	      if (comp instanceof JToggleButton) {
		    	final JToggleButton toggleButton = (JToggleButton) comp;
		    	 //Se almacena su texto asociado
		    	final String text = toggleButton.getText();
		    	//Se comprueba que no esta vacio
		    	if (text!=null && !text.equalsIgnoreCase("")) { //$NON-NLS-1$

		    		//Se tratan los botones segun su texto
			    	if (text.equalsIgnoreCase("<html><center>Equipo</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_E);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		final String newText = text.substring(0, 14) +openTag+text.charAt(14)+closeTag+text.substring(15);
			    		toggleButton.setText(newText);

			    	}
			    	else if (text.equalsIgnoreCase("<html><center>Elementos recientes</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_L);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		final String newText = text.substring(0, 15) +openTag+text.charAt(15)+closeTag+text.substring(16);
			    		toggleButton.setText(newText);

			    	}
			    	else if (text.equalsIgnoreCase("<html><center>Escritorio</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_S);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		final String newText = text.substring(0, 15) +openTag+text.charAt(15)+closeTag+text.substring(16);
			    		toggleButton.setText(newText);

			    	}
			    	else if (text.equalsIgnoreCase("<html><center>Mis documentos</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_I);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		final String newText = text.substring(0, 15) +openTag+text.charAt(15)+closeTag+text.substring(16);
			    		toggleButton.setText(newText);

			    	}
			    	else if (text.equalsIgnoreCase("<html><center>Red</center></html>")) {
			    		//Se asigna un mnemonico predefinido
			    		toggleButton.setMnemonic(KeyEvent.VK_R);
			    		//Como el texto del boton contiene codigo HTML se hace lo siguiente para que se muestre el mnemonico al usuario
			    		final String newText = text.substring(0, 14) +openTag+text.charAt(14)+closeTag+text.substring(15);
			    		toggleButton.setText(newText);
			    	}
		    	}
		    }
	      else if (comp instanceof Container) {
		    	//Llamada recursiva
		    	setToggleButtonMnemonics((Container)comp);
		    }
	    }//for
	  }
	/**
	 * Define el modo alto contraste para los componentes de la ventana.
	 * @param c contenedor global
	 */
	protected final void setHighContrast(final Container c) {
		 //Numero de componentes del contenedor
	    final int len = c.getComponentCount();
	    //Se recorren los elementos que forman el contenedor
	    for (int i = 0; i < len; i++) {
	      final Component comp = c.getComponent(i);  //Se obtiene un componente
	      //Se comprueba si es de tipo etiqueta
	      if (comp instanceof JLabel) {
	        final JLabel label = (JLabel)comp;
	        if (GeneralConfig.isHighContrast()){
	        	label.setForeground(Color.WHITE);
	        }
	        else {
	        	label.setForeground(Color.BLACK);
	        }
	      }
	      else if(comp instanceof JToggleButton){
	    	  final JToggleButton toggleButton = (JToggleButton)comp;
	    	  if (GeneralConfig.isHighContrast()){
	    		  toggleButton.setForeground(Color.WHITE);
	    	  }
	    	  else {
	    		  toggleButton.setForeground(Color.BLACK);
	    	  }
	      }
	      else if (comp instanceof JComboBox){
	    	  final JComboBox comboBox = (JComboBox)comp;
	    	  if (GeneralConfig.isHighContrast()){
	    		  comboBox.setBackground(Color.WHITE);
	    	  }
	    	  else {
	    		  comboBox.setBackground(Color.BLACK);
	    	  }
	      }
	      else if (comp instanceof Container) {
	    	  	//Llamada recursiva
	    	  setHighContrast((Container)comp);
		    }
	    }//for
	  }

	/** Crea la ventana de di&aacute;logo.
	 * @param Component Componente padre
	 * @throws java.awt.HeadlessException */
	@Override
    protected JDialog createDialog(final Component parent)  {
		final String title = getUI().getDialogTitle(this);
        putClientProperty(
                AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY,
                title);

        //Se comprueba el tipo de componente padre, para asignar el correcto y que asi se muestre el icono asociado.
        if (parent instanceof JDialog) {
        	this.dialog = new JDialog((JDialog)parent, title, true);
        }
        else if (parent instanceof Frame){
        	this.dialog = new JDialog((Frame)parent, title, true);
        }
        else {
        	//Se obtiene el componente root
	        final Component root = SwingUtilities.getRoot(parent);
	        if ((root!=null) && (root instanceof Frame)) {
	        	this.dialog = new JDialog((Frame)root, title, true);
	        }
	        else {
	        	this.dialog = new JDialog();
	        	this.dialog.setTitle(title);
	        	this.dialog.setModal(true);
	        }
        }

        this.dialog.setComponentOrientation(this .getComponentOrientation());

        final Container contentPane = this.dialog.getContentPane();

    	removeWindowsToolBar();
    	contentPane.setLayout(new GridBagLayout());

    	this.dialog.addComponentListener(new ComponentListener() {

			@Override
			public void componentShown(final ComponentEvent e) { /* Vacio */ }

			@Override
			public void componentResized(final ComponentEvent e) {
				resized();
			}

			@Override
			public void componentMoved(final ComponentEvent e) {
				resized();
			}

			@Override
			public void componentHidden(final ComponentEvent e) { /* Vacio */ }
		});

        for (int i = 0; i<this.getComponentCount();i++){
        	if (this.getComponent(i).getClass().getName().equals("javax.swing.JToolBar")){ //$NON-NLS-1$
        		this.jTool = (JToolBar)this.getComponent(i);
        		for (int j = 0; j<this.jTool.getComponentCount();j++){
            		// Al cambiar entre vista en lista y detalles se llama a adjustWindowFonts para que calcule el tamano del texto
            		if (this.jTool.getComponent(j).getClass().getName().equals("javax.swing.JToggleButton")){ //$NON-NLS-1$
            			final JToggleButton boton = ((JToggleButton)this.jTool.getComponent(j));
            			// Al cambiar entre vista en lista y detalles se llama a adjustWindowFonts para que calcule el tamano del texto
            			((JToggleButton)(this.jTool.getComponent(j))).addMouseListener(new MouseListener() {

    						@Override
    						public void mouseReleased(final MouseEvent e) { /* Vacio */ }

    						@Override
    						public void mousePressed(final MouseEvent e) { /* Vacio */ }

    						@Override
    						public void mouseExited(final MouseEvent e) { /* Vacio */ }

    						@Override
    						public void mouseEntered(final MouseEvent e) { /* Vacio */ }

    						@Override
    						public void mouseClicked(final MouseEvent e) {
    							callResize();
    						}
    					});
            			((JToggleButton)(this.jTool.getComponent(j))).addKeyListener(new KeyListener() {

    						@Override
    						public void keyTyped(final KeyEvent e) { /* Vacio */ }

    						@Override
    						public void keyReleased(final KeyEvent e) {
    							if (e.getKeyCode() == KeyEvent.VK_SPACE){
    								boton.doClick();
    								callResize();
    							}
    						}

    						@Override
    						public void keyPressed(final KeyEvent e) { /* Vacio */ }
    					});

            		}
            		Utils.remarcar((JComponent)this.jTool.getComponent(j));
                	Utils.setFontBold((JComponent)this.jTool.getComponent(j));
            	}
        	}
        	else {
        		accessibility((JPanel)this.getComponent(i));
        	}

        }

        //Restricciones para los botones
		final GridBagConstraints consButtons = new GridBagConstraints();
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

        if (JDialog.isDefaultLookAndFeelDecorated()) {
            final boolean supportsWindowDecorations = UIManager
                    .getLookAndFeel().getSupportsWindowDecorations();
            if (supportsWindowDecorations) {
                this.dialog.getRootPane().setWindowDecorationStyle(
                        JRootPane.FILE_CHOOSER_DIALOG);
            }
        }
        this.dialog.getRootPane().setDefaultButton(this.openButton);
        this.dialog.setResizable(true);
        this.dialog.pack();
        this.dialog.setLocationRelativeTo(parent);

    	this.resizingAdaptor = new ResizingAdaptor(null,null,null,null,null,this,null,null);
    	this.theDialog = this.dialog;
 		this.dialog.addComponentListener(this.resizingAdaptor);

     	// Dimensiones de la ventana
 		if (GeneralConfig.isMaximized() || isMaximized){
 			this.theDialog.setBounds(0,0, (int)(getMaxDimension().getWidth()), (int)(getMaxDimension().getHeight()));
        	this.dialog.setPreferredSize(getMaxDimension());
        }
 		else {
        	if (PrincipalGUI.getFileActualPositionX() != -1){
        		this.dialog.setBounds(PrincipalGUI.getFileActualPositionX(),PrincipalGUI.getFileActualPositionY(),PrincipalGUI.getFileActualWidth(), PrincipalGUI.getFileActualHeight());
        		this.dialog.setPreferredSize(new Dimension(PrincipalGUI.getFileActualWidth(), PrincipalGUI.getFileActualHeight()));
        	}
        }
        if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
        	this.dialog.setMinimumSize(new Dimension(Constants.FILE_FONT_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT));
        }
        else {
        	//En entornos Linux y MAC pinta la pantalla con un tamano de fuente diferente al estandar
        	if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)){
        		this.dialog.setMinimumSize(new Dimension(Constants.FILE_INITIAL_WIDTH_MAC, Constants.FILE_INITIAL_HEIGHT));
        	}
        	else {
        		this.dialog.setMinimumSize(new Dimension(Constants.FILE_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT));
        	}
        }
        return this.dialog;
	}

	/**
	 * Elimina la barra de accesos a carpetas de windows de la ventana
	 */
	private void removeWindowsToolBar(){
		for (int i=0; i<this.getComponentCount();i++){
			if ((this.getComponent(i) instanceof JToolBar) && (!this.getComponent(i).getClass().getName().equals("javax.swing.JToolBar"))) { //$NON-NLS-1$
       			this.remove(this.getComponent(i));
        	}
        }
	}

	/**
	 * Aplica la configuraci&oacute;n de accesibilidad a los componentes del panel
	 * @param jPanel Panel con los componentes. Puede contener m&aacute;s paneles
	 */
	protected void accessibility(final JPanel jPanel){
		for (int i=0;i<jPanel.getComponentCount();i++){
			if (jPanel.getComponent(i) instanceof JPanel){
				accessibility((JPanel)jPanel.getComponent(i));
			}
			else {
				if (jPanel.getComponent(i).getClass().getName().equals("com.sun.java.swing.plaf.windows.WindowsFileChooserUI$9")){
					this.openButton = (JButton)jPanel.getComponent(i);
				}

				Utils.remarcar((JComponent)jPanel.getComponent(i));
				Utils.setFontBold((JComponent)jPanel.getComponent(i));
				Utils.setContrastColor((JComponent)jPanel.getComponent(i));
			}
		}
	}

	/**
	 * Devuelve el di&aacute;logo.
	 * return dialogo
	 */
	JDialog getDialog(){
		return this.theDialog;
	}

	/**
	 * Ajuste de fuentes.
	 */
	void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}

	/**
	 * Se almacena la posicion actual.
	 */
	void resized(){
		if (!GeneralConfig.isMaximized() || !isMaximized){
	    	PrincipalGUI.setFileActualPositionX(this.dialog.getX());
	    	PrincipalGUI.setFileActualPositionY(this.dialog.getY());
	    	PrincipalGUI.setFileActualWidth(this.dialog.getWidth());
	    	PrincipalGUI.setFileActualHeight(this.dialog.getHeight());
    	}

		if (getParent().getParent().getParent().getParent().getSize().equals(getMaxDimension())){
	    	this.dialog.setResizable(false);
	    } else {
	    	this.dialog.setResizable(true);
	    }
	}

	/**
	 * Genera el panel que contiene los botones de restaurar y maximizar.
	 * Aplica accesibilidad sobre los botones.
	 * @return JPanel con los botones de restaurar y maximizar
	 */
	private final JPanel createAccessibilityButtonsPanel() {
		final JPanel panelAccesibilidad = new JPanel(new GridBagLayout());

		//Para el tooltip
		final JWindow tip = new JWindow();
		final JLabel tipText = new JLabel();

		//Panel que va a contener los botones de accesibilidad
		final JPanel panel = new JPanel(new GridBagLayout());

		//Restricciones para los botones
		final GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.BOTH;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.weightx = 1.0;
		consButtons.weighty = 1.0;
		consButtons.insets = new Insets(0,0,0,0);  //right padding

		final Dimension dimension = new Dimension(25, 25);

		//Restore button
		final JPanel restorePanel = new JPanel();
		ImageIcon imageIconRestore= new ImageIcon(JAccessibilityFileChooser.class.getResource("/resources/images/restore.png")); //$NON-NLS-1$
		imageIconRestore = new ImageIcon(imageIconRestore.getImage().getScaledInstance(dimension.width, dimension.height, Image.SCALE_SMOOTH));
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());

		this.restoreButton.addFocusListener(new FocusListener() {

			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, JAccessibilityFileChooser.this.getRestoreButton(), tipText);
			}

			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, JAccessibilityFileChooser.this.getRestoreButton(), tipText);
			}
		});
		this.restoreButton.setPreferredSize(dimension);

		this.restoreButton.setName("restaurar");
		Utils.remarcar(this.restoreButton);
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
	    	@Override
			public void actionPerformed(final ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});

		panel.add(restorePanel, consButtons);

		consButtons.gridx = 1;
		consButtons.insets = new Insets(0,0,0,5);  //right padding

		//Maximize button
		final JPanel maximizePanel = new JPanel();

		ImageIcon imageIconMaximize = new ImageIcon(JAccessibilityFileChooser.class.getResource("/resources/images/maximize.png")); //$NON-NLS-1$
		imageIconMaximize = new ImageIcon(imageIconMaximize.getImage().getScaledInstance(dimension.width, dimension.height, Image.SCALE_SMOOTH));
		this.maximizeButton.setIcon(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

		this.maximizeButton.setName("maximizar"); //$NON-NLS-1$
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);

		Utils.remarcar(this.maximizeButton);
		maximizePanel.add(this.maximizeButton);

		this.maximizeButton.addFocusListener(new FocusListener() {

			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, JAccessibilityFileChooser.this.getMaximizeButton(), tipText);
			}

			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, JAccessibilityFileChooser.this.getMaximizeButton(), tipText);
			}

		});

		this.maximizeButton.addActionListener(new ActionListener() {
		    	@Override
				public void actionPerformed(final ActionEvent e) {
		    		maximizarActionPerformed();
				}
			});

		panel.add(maximizePanel, consButtons);

		//Se anade al panel general
		//Restricciones para el panel de botones
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.insets = new Insets(0,0,0,0);
		c.anchor=GridBagConstraints.EAST;
		panelAccesibilidad.add(panel, c);


		// Habilitado/Deshabilitado de botones restaurar/maximizar
    	if (GeneralConfig.isMaximized() || isMaximized){
    		//Se deshabilita el boton de maximizado
    		this.maximizeButton.setEnabled(false);
    		//Se habilita el boton de restaurar
    		this.restoreButton.setEnabled(true);
    	}
    	else {
    		//Se habilita el boton de maximizado
    		this.maximizeButton.setEnabled(true);
    		//Se deshabilita el boton de restaurar
    		this.restoreButton.setEnabled(false);
    	}
		return panelAccesibilidad;
	}

	/**
	 * Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado
	 */
	protected void restaurarActionPerformed(){

		if (JAccessibilityFileChooser.actualPositionX != -1 && getActualPositionY() != -1 && getActualWidth() != -1 && getActualHeight() != -1){
			this.theDialog.setBounds(JAccessibilityFileChooser.actualPositionX, getActualPositionY(), getActualWidth(), getActualHeight());
		}
		else {
			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			setBounds(getInitialX(), getInitialY(), Constants.FILE_FONT_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT);
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
			else {
    			if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)){
    				setBounds(getInitialX(), getInitialY(), Constants.FILE_INITIAL_WIDTH_MAC, Constants.FILE_INITIAL_HEIGHT);
    			}
    			else {
    				setBounds(getInitialX(), getInitialY(), Constants.FILE_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT);
    			}
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
		}
		this.maximizeButton.setEnabled (true);
		this.restoreButton.setEnabled (false);

		isMaximized = false;
	}

	/**
	 * Maximiza la ventana
	 */
	protected void maximizarActionPerformed(){
		setActualPositionX(this.theDialog.getX());
		setActualPositionY(this.theDialog.getY());
		setActualWidth(this.theDialog.getWidth());
		setActualHeight(this.theDialog.getHeight());

		this.maximizeButton.setEnabled (false);
		this.restoreButton.setEnabled (true);

		this.theDialog.setBounds(0,0, (int)(getMaxDimension().getWidth()), (int)(getMaxDimension().getHeight()));

		isMaximized = true;
	}

	/**
	 * Devuelve el panel de botones de maximizar/restaurar
	 * @return JPanel con los botones de maximizar/restaurar
	 */
	protected JPanel getAccesibilityButtonsPanel(){
		return this.accesibilityButtonsPanel;
	}

	/**
	 * Devuelve el tama&ntilde;o m&aacute;ximo disponible para una ventana
	 * @return Dimension m&aacute;ximo disponible.
	 */
	protected static final Dimension getMaxDimension(){

		final Dimension result = new Dimension();
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		final Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = (int)rect.getWidth();
		final int maxHeight = (int)rect.getHeight();

		//Se hace el resize dependiendo del so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			result.setSize(maxWidth, maxHeight);
		}
		else {
			result.setSize(maxWidth, maxHeight - Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX);
		}

		return result;
	}

	protected static final int getActualPositionY() {
		return actualPositionY;
	}
	protected static final void setActualPositionY(final int actualPositionY) {
		JAccessibilityFileChooser.actualPositionY = actualPositionY;
	}
	protected static final int getActualWidth() {
		return actualWidth;
	}
	protected static final void setActualWidth(final int actualWidth) {
		JAccessibilityFileChooser.actualWidth = actualWidth;
	}
	protected static final int getActualHeight() {
		return actualHeight;
	}
	protected static final void setActualHeight(final int actualHeight) {
		JAccessibilityFileChooser.actualHeight = actualHeight;
	}
	protected final JButton getOpenButton(){
		return this.openButton;
	}
	protected static final boolean isMaximized(){
		return isMaximized;
	}
	protected static final void setIsMaximized(final boolean actualIsMaximized){
		isMaximized = actualIsMaximized;
	}

}

