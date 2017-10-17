/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
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

/** Clase que extiende JFileChooser para hacerla accesible.
 * @author lmerayo */
final class JAccessibilityFileChooserToSave extends JAccessibilityFileChooser{

	private static final long serialVersionUID = 1L;

	private JDialog theDialog;
	private JToolBar jTool;
	private ResizingAdaptor resizingAdaptor;
	private JDialog dialog;

	private JPanel accesibilityButtonsPanel = null;

	/** Relaci&oacute;n m&iacute;nima. */
	@Override
	public int getMinimumRelation(){
		return 9;
	}

	/** Constructor.
	 * @param file directorio. */
	JAccessibilityFileChooserToSave (final File file)
	 {
		super(file);
		init();


	 }//constructor

	/** Constructor. */
	JAccessibilityFileChooserToSave() {
		super();
		init();


	 }//constructor

	/**
	 * Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n X
	 */
    @Override
	public int getInitialX() {
		final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 620) / 2 ;
	}

    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
	 */
	@Override
	public int getInitialY() {
        final Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.MACOSX)){
        	return (screenSize.height - 485) / 2;
        }
        return (screenSize.height - 456) / 2;
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

	/** Crea la ventana de di&aacute;logo.
	 * @param Component Componente padre
	 * @throws java.awt.HeadlessException */
	@Override
	protected JDialog createDialog(final Component parent) {
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
	        } else {
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
        	} else {
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
        this.dialog.getRootPane().setDefaultButton(getOpenButton());
        this.dialog.setResizable(true);
        this.dialog.pack();
        this.dialog.setLocationRelativeTo(parent);

    	this.resizingAdaptor = new ResizingAdaptor(null,null,null,null,null,null,null,this);
    	this.theDialog = this.dialog;
 		this.dialog.addComponentListener(this.resizingAdaptor);

     	// Dimensiones de la ventana
 		if (GeneralConfig.isMaximized() || isMaximized()){
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
        	} else {
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
			if (this.getComponent(i) instanceof JToolBar && (!this.getComponent(i).getClass().getName().equals("javax.swing.JToolBar"))) { //$NON-NLS-1$
    			this.remove(this.getComponent(i));
        	}
        }
	}

	/** {@inheritDoc} */
	@Override
	public JDialog getDialog(){
		return this.theDialog;
	}

	/** Ajuste de fuentes. */
	@Override
	void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}

	/** Se almacena la posici&oacute;n actual. */
	@Override
	void resized(){
		if (!GeneralConfig.isMaximized() || isMaximized()){
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
		this.getRestoreButton().setIcon(imageIconRestore);
		this.getRestoreButton().setMnemonic(KeyEvent.VK_R );
		this.getRestoreButton().setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
		this.getRestoreButton().getAccessibleContext().setAccessibleName(this.getRestoreButton().getToolTipText());

		this.getRestoreButton().addFocusListener(new FocusListener() {

			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, JAccessibilityFileChooserToSave.this.getRestoreButton(), tipText);
			}

			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, JAccessibilityFileChooserToSave.this.getRestoreButton(), tipText);
			}
		});
		this.getRestoreButton().setPreferredSize(dimension);

		this.getRestoreButton().setName("restaurar"); //$NON-NLS-1$
		Utils.remarcar(this.getRestoreButton());
		restorePanel.add(this.getRestoreButton());
		this.getRestoreButton().addActionListener(new ActionListener() {

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
		this.getMaximizeButton().setIcon(imageIconMaximize);
		this.getMaximizeButton().setMnemonic(KeyEvent.VK_M );
		this.getMaximizeButton().setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
		this.getMaximizeButton().getAccessibleContext().setAccessibleName(this.getMaximizeButton().getToolTipText());

		this.getMaximizeButton().setName("maximizar"); //$NON-NLS-1$
		//Se asigna una dimension por defecto
		this.getMaximizeButton().setPreferredSize(dimension);

		Utils.remarcar(this.getMaximizeButton());
		maximizePanel.add(this.getMaximizeButton());

		this.getMaximizeButton().addFocusListener(new FocusListener() {

			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, JAccessibilityFileChooserToSave.this.getMaximizeButton(), tipText);
			}

			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, JAccessibilityFileChooserToSave.this.getMaximizeButton(), tipText);
			}

		});

		this.getMaximizeButton().addActionListener(new ActionListener() {
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
    	if (GeneralConfig.isMaximized() || isMaximized()){
    		//Se deshabilita el boton de maximizado
    		this.getMaximizeButton().setEnabled(false);
    		//Se habilita el boton de restaurar
    		this.getRestoreButton().setEnabled(true);
    	}
    	else {
    		//Se habilita el boton de maximizado
    		this.getMaximizeButton().setEnabled(true);
    		//Se deshabilita el boton de restaurar
    		this.getRestoreButton().setEnabled(false);
    	}
		return panelAccesibilidad;
	}

	/** Restaura el tama&ntilde;o de la ventana a la posici&oacute;n anterior al maximizado. */
	@Override
	public void restaurarActionPerformed(){

		if (getActualPositionX() != -1 && getActualPositionY() != -1 && getActualWidth() != -1 && getActualPositionX() != -1){
			this.theDialog.setBounds(getActualPositionX(), getActualPositionY(), getActualWidth(), getActualHeight());
		}
		else {
			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			setBounds(this.getInitialX(), this.getInitialY(), Constants.FILE_FONT_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT);
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
			else {
				if (Platform.getOS().equals(Platform.OS.MACOSX)){
    				setBounds(this.getInitialX(), this.getInitialY(), Constants.FILE_INITIAL_WIDTH_MAC, Constants.FILE_INITIAL_HEIGHT);
    			}
				else {
    				setBounds(this.getInitialX(), this.getInitialY(), Constants.FILE_INITIAL_WIDTH, Constants.FILE_INITIAL_HEIGHT);
    			}
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
		}
		this.getMaximizeButton().setEnabled (true);
		this.getRestoreButton().setEnabled (false);

		setIsMaximized(false);
	}

	/**
	 * Maximiza la ventana
	 */
	@Override
	public void maximizarActionPerformed(){
		setActualPositionX(this.theDialog.getX());
		setActualPositionY(this.theDialog.getY());
		setActualWidth(this.theDialog.getWidth());
		setActualHeight(this.theDialog.getHeight());

		this.getMaximizeButton().setEnabled (false);
		this.getRestoreButton().setEnabled (true);

		this.theDialog.setBounds(0,0, (int)(getMaxDimension().getWidth()), (int)(getMaxDimension().getHeight()));

		setIsMaximized(true);
	}

	/**
	 * Devuelve el panel de botones de maximizar/restaurar
	 * @return JPanel con los botones de maximizar/restaurar
	 */
	@Override
	public JPanel getAccesibilityButtonsPanel(){
		return this.accesibilityButtonsPanel;
	}
}