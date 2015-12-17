package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
/**
 * Componente dialogo que define los alerts de la aplicacion.
 * @author inteco
 *
 */
public final class CustomDialog extends JAccessibilityCustomDialog implements ActionListener {

	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Etiqueta con la informacion de la alerta.
	 *
	 */
	private InfoLabel infoLabel = null;

	/**
	 * Panel de botones.
	 */
	private JPanel buttonsPanel = null;

	/**
	 * Panel de botones relacionados con la accesibilidad.
	 */
	private JPanel accessibilityButtonsPanel = null;

	/**
	 * Panel principal.
	 */
	private JPanel mainPanel = null;

	/**
	 * Campo de texto o campo de contrase&ntilde;a.
	 */
	JComponent component = null;


	/**
	 * Etiqueta que contiene el icono de la alerta.
	 */
	private final IconLabel iconLabel = new IconLabel();

	/**
	 * Boton de OK.
	 */
	private JButton okButton = null;

	/**
	 * Boton de NO.
	 */
	private JButton noButton = null;

	/**
	 * Boton de Cancel.
	 */
	private static JButton cancelButton = null;
	static JButton getCancelButton() {
		return cancelButton;
	}

	/**
	 * Boton de restaurar.
	 */
	private JButton restoreButton = null;
	JButton getRestoreButton() {
		return this.restoreButton;
	}

	/** Bot&oacute;n de maximizar. */
	JButton maximizeButton = null;

	/**
	 * Respuesta al mensaje
	 */
	private int answer;

	/**
	 * Texto para el boton
	 */
	private static String cancellText = Messages.getString("PrincipalGUI.cancelar"); //$NON-NLS-1$


	/**
	 * Constructor.
	 *
	 * @param JDialog componente padre.
	 * @param modal modal
	 * @param message mensaje
	 * @param title titulo
	 * @param typeMessage tipo de mensaje
	 * @param isInputDialog indica sies una alerta de tipo input
	 */
	private CustomDialog(final JDialog componentParent, final boolean modal, final String message, final String title, final int typeMessage, final boolean isInputDialog){
		super(componentParent, modal);
		initComponents(message, title, typeMessage, isInputDialog);
		setLocationRelativeTo(componentParent);
	}

	/**
	 * Constructor.
	 *
	 * @param componentParent componente padre.
	 * @param modal modal
	 * @param message mensaje
	 * @param title titulo
	 * @param typeMessage tipo de mensaje
	 * @param isInputDialog indica sies una alerta de tipo input
	 */
	private CustomDialog(final Component componentParent, final boolean modal, final String message, final String title, final int typeMessage, final boolean isInputDialog){
		super();
		this.setModal(modal);
		initComponents(message, title, typeMessage, isInputDialog);
		setLocationRelativeTo(componentParent);
	}

	/**
	 * Constructor.
	 *
	 * @param JFrame componente padre.
	 * @param modal modal
	 * @param message mensaje
	 * @param title titulo
	 * @param typeMessage tipo de mensaje
	 * @param isInputDialog indica sies una alerta de tipo input
	 */
	private CustomDialog(final JFrame componentParent, final boolean modal, final String message, final String title, final int typeMessage, final boolean isInputDialog){
		super(componentParent, modal);
		initComponents(message, title, typeMessage, isInputDialog);
		setLocationRelativeTo(componentParent);
	}

    /**
	 * Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n X
	 */
    private static int getInitialX(final int width) {
		final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		return screenSize.width/2 - width / 2 ;
	}

    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
	 */
	private static int getInitialY(final int height) {
        final Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        return screenSize.height/2 - height / 2 ;
	}

	/**
	 * Metodo que inicializa los componentes de la alerta.
	 * @param message mensaje que se mostrara en la alerta
	 * @param title titulo de la alerta
	 * @param typeMessage tipo de mensaje
	 * @param isInputDialog indica sies una alerta de tipo input
	 */
	private void initComponents(final String message, final String title, final int typeMessage, final boolean isInputDialog){

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = Constants.CUSTOMDIALOG_MAX_WIDTH;
		final int maxHeight = Constants.CUSTOMDIALOG_MAX_HEIGHT;

		//Se establece el tamano minimo
		setMinimumSize(new Dimension(Constants.CUSTOMDIALOG_INITIAL_WIDTH, Constants.CUSTOMDIALOG_INITIAL_HEIGHT));
		//Se establece el tamano maximo
		setMaximumSize(new Dimension(maxWidth,maxHeight));


		// Dimensiones de la ventana en Windows y Linux
    	if (GeneralConfig.isMaximized()){
    		//Se maximiza
    		this.setBounds(0,0, maxWidth, maxHeight);
    	} else {
    		//Se establece el tamano minimo en base a las opciones activas
    		if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			setMinimumSize(new Dimension(Constants.CUSTOMDIALOG_FONT_INITIAL_WIDTH, Constants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT));
    		}
    	}

		this.setTitle(title);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

		this.answer = JOptionPane.NO_OPTION;
		//Contenedor del dialogo
		final Container container = getContentPane();
		//Layout del contenedor
		container.setLayout(new GridBagLayout());

		//Panel con los datos del dialogo
		 this.mainPanel = new JPanel(new GridBagLayout());

		//Restricciones para el panel de datos
		final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 0;
        c.insets = new Insets(5,10,0,10);

        //Icono del dialogo
       setIconLabel(typeMessage);
       final JPanel iconPanel = new JPanel(new GridBagLayout());
       final GridBagConstraints  consIconPanel = new GridBagConstraints();
       consIconPanel.fill = GridBagConstraints.BOTH;

       iconPanel.add(this.iconLabel, consIconPanel);

       c.insets = new Insets(0,0,0,10);  //right padding
       c.gridx = 1;
       c.weightx = 1.0;
       c.weighty = 1.0;

        //Etiqueta del dialogo
        if (isInputDialog) {
        	//Se crea una etiqueta sencilla
        	this.infoLabel = new InfoLabel(message);
        	this.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); //Se alinea a la izqda
        	this.infoLabel.setVerticalAlignment(SwingConstants.TOP); //Se alinea arriba el texto
        }
        else {
        	//Se crea una etiqueta focusable
			this.infoLabel = new InfoLabel(message, false);
			this.infoLabel.setHorizontalAlignment(SwingConstants.CENTER); //Se centra el texto
			//Foco a la etiqueta
			this.infoLabel.addAncestorListener(new RequestFocusListener());
			this.infoLabel.setVerticalAlignment(SwingConstants.TOP); //Se alinea arriba el texto
        }

		//Se anade la etiqueta al panel de informacion general
		this.mainPanel.add(this.infoLabel, c);

		//Panel de botones
		createMainButtonsPanel();
		createAccessibilityButtonsPanel();

		//Restricciones del contenedor general

		final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.BOTH;
        cons.gridx = 0;
        cons.gridy = 0;
        cons.weighty = 0.0;
        cons.weightx = 0.15;

        cons.gridwidth = 1;
        cons.gridheight = 2;

        //Se anade el icono
        container.add(iconPanel, cons);

        cons.gridwidth = 2;
        cons.gridheight = 1;
        cons.gridx = 1;
        cons.gridy = 0;
        cons.weighty = 0.0;
        cons.weightx = 0.0;
        //Se anade el panel de botones relacionados con la accesibilidad
        container.add(this.accessibilityButtonsPanel, cons);

        cons.gridwidth = 2;
        cons.gridx = 1;
        cons.gridy = 1;
        cons.weightx = 1.0;
        cons.weighty = 2.0;

        //se anade el panel de informacion
        container.add(this.mainPanel, cons);

        cons.gridx = 0;
        cons.gridy = 2;
        cons.weighty = 0.5;
        cons.weightx = 1.0;
        cons.gridwidth = 3;

        //buttonsPanel.setBackground(Color.green);

        //Se anade el panel de botones
        container.add(this.buttonsPanel, cons);
	}

	/**
	 * Se asigna el icono a la etiqueta.
	 * @param typeMessage tipo de alerta.
	 */
	private void setIconLabel(final int typeMessage) {
		//Segun el tipo de mensaje se selecciona el icono
  		Icon icon = null;
  		//Se comprueba el tipo de mensaje para poner o no, un icono
  		if (typeMessage != JOptionPane.PLAIN_MESSAGE) {
	  		if (typeMessage == JOptionPane.ERROR_MESSAGE) {
	  			icon = UIManager.getIcon("OptionPane.errorIcon"); //$NON-NLS-1$
	  		}
	  		else if (typeMessage == JOptionPane.WARNING_MESSAGE) {
	  			icon = UIManager.getIcon("OptionPane.warningIcon"); //$NON-NLS-1$
	  		}
	  		else if (typeMessage == JOptionPane.QUESTION_MESSAGE) {
	  			icon = UIManager.getIcon("OptionPane.questionIcon"); //$NON-NLS-1$
	  		}
	  		else {
				icon = UIManager.getIcon("OptionPane.informationIcon"); //$NON-NLS-1$
	  		}
	  		this.iconLabel.setIcon(icon);
	  		this.iconLabel.setOriginalIcon(icon);
  		}
	}

	/**
	 * Se crea el panel de botones de accesibilidad.
	 */
	private void createAccessibilityButtonsPanel() {
		this.accessibilityButtonsPanel = new JPanel(new GridBagLayout());

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
		//consButtons.anchor=GridBagConstraints.EAST;

		//Restore button
		final JPanel restorePanel = new JPanel();
		final ImageIcon imageIconRestore= new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png")); //$NON-NLS-1$
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());

		this.restoreButton.addFocusListener(new FocusListener() {

			/**
			 * Evento que se produce cuando el componente pierde el foco.
			 */
			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, CustomDialog.this.getRestoreButton(), tipText);
			}
			/**
			 * Evento que se produce cuando el componente tiene el foco.
			 */
			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, CustomDialog.this.getRestoreButton(), tipText);
			}
		});
		final Dimension dimension = new Dimension(20,20);
		this.restoreButton.setPreferredSize(dimension);

		this.restoreButton.setName("restaurar"); //$NON-NLS-1$
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
			/**
			 * Accion del boton.
			 */
	    	@Override
			public void actionPerformed(final ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
		Utils.remarcar(this.restoreButton);


		panel.add(restorePanel, consButtons);


		consButtons.gridx = 1;
		consButtons.insets = new Insets(0,0,0,0);  //right padding

		//Maximize button
		final JPanel maximizePanel = new JPanel();

		final ImageIcon imageIconMaximize= new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png")); //$NON-NLS-1$
		this.maximizeButton = new JButton(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

		this.maximizeButton.setName("maximizar"); //$NON-NLS-1$
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);

		Utils.remarcar(this.maximizeButton);
		maximizePanel.add(this.maximizeButton);

		this.maximizeButton.addFocusListener(new FocusListener() {
			/**
			 * Evento que se produce cuando el componente pierde el foco.
			 */
			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, CustomDialog.this.maximizeButton, tipText);
			}
			/**
			 * Evento que se produce cuando el componente tiene el foco.
			 */
			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, CustomDialog.this.maximizeButton, tipText);
			}
		});


		this.maximizeButton.addActionListener(new ActionListener() {
				/**
				 * Accion del boton.
				 */
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
		this.accessibilityButtonsPanel.add(panel, c);


		// Habilitado/Deshabilitado de botones restaurar/maximizar
    	if (GeneralConfig.isMaximized()){
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

	}

	/**
	 * Panel que contiene los botones principales de las alerta.
	 */
	void createMainButtonsPanel() {
		this.buttonsPanel = new JPanel(new GridBagLayout());

		//Restricciones para el panel de botones
		final GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.NONE;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.insets = new Insets(0,10,0,10);  //right padding
		consButtons.anchor=GridBagConstraints.CENTER;

		//OK button
		final JPanel okPanel = new JPanel();
		this.okButton = getButton(Messages.getString("PrincipalGUI.aceptar"), KeyEvent.VK_A); //$NON-NLS-1$

		okPanel.add(this.okButton);
		this.buttonsPanel.add(okPanel, consButtons);

		this.okButton.addActionListener(this);
		//Se asigna este boton como boton por defecto de la ventana.
		this.getRootPane().setDefaultButton(this.okButton);

	}

	/**
	 * Muestra un dialogo con un mensaje.
	 * @param componentParent componente padre
	 * @param modal modal
	 * @param message mensaje a mostrar
	 * @param title titulo del dialogo
	 * @param typeMessage tipo de mensaje
	 */
	public static void showMessageDialog(final Component componentParent, final boolean modal, final String message, final String title, final int typeMessage){
		//Instancia de CustomDialog
		final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(componentParent, modal, message, title, typeMessage, false);
		customDialog.infoLabel.setHorizontalAlignment(SwingConstants.CENTER); //Se centra el texto
		customDialog.setVisible(true);
	}

	/**
	 * Muestra un dialogo con un mensaje que pide la interacion del usuario.
	 * @param componentParent componente padre
	 * @param modal modal
	 * @param message mensaje a mostrar
	 * @param title titulo del dialogo
	 * @param typeOption opciones de interacion
	 * @param typeMessage tipo de mensaje
	 * @return respuesta del usuario.
	 */
	public static int showConfirmDialog(final Component componentParent, final boolean modal, final String message, final String title, final int typeOption, final int typeMessage){

		final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(componentParent, modal, message, title, typeMessage, false);

		//Restricciones
		final GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridy = 0;
		cons.insets = new Insets(0,0,0,10);  //right padding

		//Se comprueba el tipo de dialogo
		if (typeOption == JOptionPane.YES_NO_OPTION) {
			//Boton Si
			customDialog.okButton.setText(Messages.getString("CustomDialog.confirmDialog.yes")); //$NON-NLS-1$
			customDialog.okButton.setMnemonic(KeyEvent.VK_S);
			//Boton no
			customDialog.noButton = customDialog.getButton(Messages.getString("CustomDialog.confirmDialog.no"), KeyEvent.VK_N); //$NON-NLS-1$
			final JPanel noPanel = new JPanel();
			noPanel.add(customDialog.noButton);
			customDialog.buttonsPanel.add(noPanel, cons);
			customDialog.noButton.addActionListener(customDialog);
		}
		else if (typeOption == JOptionPane.YES_NO_CANCEL_OPTION) {
			//Boton Si
			customDialog.okButton.setText(Messages.getString("CustomDialog.confirmDialog.yes")); //$NON-NLS-1$
			customDialog.okButton.setMnemonic(KeyEvent.VK_S);
			//Boton No
			customDialog.noButton = customDialog.getButton(Messages.getString("CustomDialog.confirmDialog.no"), KeyEvent.VK_N); //$NON-NLS-1$
			final JPanel noPanel = new JPanel();
			noPanel.add(customDialog.noButton);
			customDialog.buttonsPanel.add(noPanel, cons);
			customDialog.noButton.addActionListener(customDialog);
			//Boton Cancelar
			cancelButton = customDialog.getButton(cancellText, KeyEvent.VK_C);
			final JPanel cancelPanel = new JPanel();
			cancelPanel.add(cancelButton);
			customDialog.buttonsPanel.add(cancelPanel, cons);
			cancelButton.addActionListener(customDialog);
		}
		else {
			//Boton Cancelar
			cancelButton = customDialog.getButton(cancellText, KeyEvent.VK_C);
			final JPanel cancelPanel = new JPanel();
			cancelPanel.add(cancelButton);
			customDialog.buttonsPanel.add(cancelPanel, cons);
			cancelButton.addActionListener(customDialog);
		}

		customDialog.infoLabel.setHorizontalAlignment(SwingConstants.CENTER); //Se centra el texto

		//Se fuerza el repintado de la pantalla para entornos MAC por problemas de renderizado
		if (Platform.getOS().equals(Platform.OS.MACOSX)){
			customDialog.repaint();
		}

        customDialog.setVisible(true);
		return customDialog.getAnswer();
	}

	/**
	 * Muestra un dialogo de insercion.
	 * @param componentParent componente padre
	 * @param modal modal
	 * @param message mensaje a mostrar
	 * @param mnemonic atajo
	 * @param title titulo del dialogo
	 * @param typeMessage tipo de alerta
	 * @return respuesta del usuario
	 */
	public static String showInputDialog(final Component componentParent, final boolean modal, final String message, final int mnemonic, final String title, final int typeMessage){
		final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(componentParent, modal, message, title, typeMessage, true);

		//Restricciones para el panel de datos
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 1;
		c.gridy = 1;
		c.weightx = 0.0;
        c.weighty = 0.5;
        c.gridwidth = 2;
        c.insets = new Insets(10,0,0,10);  //right padding

        //campo de texto del dialogo
        customDialog.component = new JTextField(""); //$NON-NLS-1$
        customDialog.component.addAncestorListener(new RequestFocusListener());
        Utils.remarcar(customDialog.component);
        Utils.setContrastColor(customDialog.component);
        Utils.setFontBold(customDialog.component);
        customDialog.component.getAccessibleContext().setAccessibleName(message.replaceAll(Constants.HTML_SALTO_LINEA, "") +".  ALT + " + mnemonic + ". "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
       //Se anade el campo de texto al panel de informacion general
        customDialog.mainPanel.add(customDialog.component, c);

        //Etiqueta principal
        //Se relaciona la etiqueta con el componente
	    customDialog.infoLabel.setLabelFor(customDialog.component);
		//Se asigna un atajo
		customDialog.infoLabel.setDisplayedMnemonic(mnemonic);
		//Se muestra el atajo
		final String text = Utils.remarkMnemonic(customDialog.infoLabel.getText(), mnemonic);
		customDialog.infoLabel.setText(text);

		//Restricciones botones
		final GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridy = 0;
		cons.insets = new Insets(0,0,0,10);  //right padding

		//Cancel button
		cancelButton = customDialog.getButton(cancellText, KeyEvent.VK_C);
		final JPanel cancelPanel = new JPanel();
		cancelPanel.add(cancelButton);
		customDialog.buttonsPanel.add(cancelPanel, cons);
		cancelButton.addActionListener(customDialog);

		customDialog.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); //Se centra el texto
		customDialog.component.setVisible(true); //Se hace visible el campo de texto

		cancelButton.addActionListener(customDialog);

		customDialog.setVisible(true);

		//Control para saber si se ha pulsado el boton cancelar
		if (customDialog.getAnswer()== JOptionPane.YES_OPTION) {
			return ((JTextField)customDialog.component).getText();
		}
		return null;

	}

	/**
	 * Muestra un dialogo de insercion. El componente de insercion sera un combo.
	 * @param componentParent componente padre
	 * @param modal modal
	 * @param message mensaje a mostrar
	 * @param mnemonic atajo
	 * @param title titulo del dialogo
	 * @param typeMessage tipo de mensaje
	 * @param selectionValues elementos del combo
	 * @param initialSelectionValue seleccion del combo
	 * @return seleccion del usuario.
	 */
	public static Object showInputDialog(final Component componentParent, final boolean modal, final String message, final int mnemonic, final String title, final int typeMessage,
			final Object[] selectionValues, final Object initialSelectionValue){

		final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(componentParent, modal, message, title, typeMessage, true);

		//Restricciones para el panel de datos
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 1;
		c.gridy = 1;
		c.weightx = 0.0;
		c.weighty = 0.02;
        c.gridwidth = 2;
        c.insets = new Insets(0,0,0,10);  //right padding

        //campo de texto del dialogo
        customDialog.component = new JComboBox(selectionValues.clone());
        customDialog.component.addAncestorListener(new RequestFocusListener());
        if (initialSelectionValue != null) {
        	((JComboBox)customDialog.component).setSelectedItem(initialSelectionValue);
        }
        Utils.remarcar(customDialog.component);
        Utils.setContrastColor(customDialog.component);
        Utils.setFontBold(customDialog.component);
        customDialog.component.getAccessibleContext().setAccessibleName(message.replaceAll(Constants.HTML_SALTO_LINEA, "") +".  ALT + " + mnemonic + ". "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
       //Se anade el campo de texto al panel de informacion general
        customDialog.mainPanel.add(customDialog.component, c);

       //Etiqueta principal
        //Se relaciona la etiqueta con el componente
	    customDialog.infoLabel.setLabelFor(customDialog.component);
		//Se asigna un atajo
		customDialog.infoLabel.setDisplayedMnemonic(mnemonic);
		//Se muestra el atajo
		final String text = Utils.remarkMnemonic(customDialog.infoLabel.getText(), mnemonic);
		customDialog.infoLabel.setText(text);

		//Restricciones botones
		final GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridy = 0;
		cons.insets = new Insets(0,0,0,10);  //right padding

		//Cancel button
		cancelButton = customDialog.getButton(cancellText, KeyEvent.VK_C);
		final JPanel cancelPanel = new JPanel();
		cancelPanel.add(cancelButton);
		customDialog.buttonsPanel.add(cancelPanel, cons);
		cancelButton.addActionListener(customDialog);

		customDialog.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); //Se centra el texto
		customDialog.component.setVisible(true); //Se hace visible el campo de texto

		cancelButton.addActionListener(customDialog);

		//Se fuerza el repintado de la pantalla para entornos MAC por problemas de renderizado
		if (Platform.getOS().equals(Platform.OS.MACOSX)){
			customDialog.repaint();
		}

		customDialog.setVisible(true);

		//Control para saber si se ha pulsado el boton cancelar
		if (customDialog.getAnswer()== JOptionPane.YES_OPTION) {
			return ((JComboBox)customDialog.component).getSelectedItem();
		}
		return null;

	}

	/**
	 * Muestra un dialogo de insercion que estara compuesto por dos etiquetas, una de ellas con posibilidad de mostrar
	 * barras de scroll y un cuadro de texto.
	 * @param componentParent componente padre
	 * @param modal modal
	 * @param message mensaje a mostrar
	 * @param mnemonic atajo
	 * @param list lista de elementos
	 * @param nameListElements nombre generico para los elementos de la lista
	 * @param title titulo del dialogo
	 * @param typeMessage  tipo de mensaje
	 * @return nombre del perfil
	 */
	public static String showInputDialog(final Component componentParent, final boolean modal, final String message, final int mnemonic, final List<String> list, final String nameListElements, final String title, final int typeMessage){

		final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(componentParent, modal, message, title, typeMessage, true);
		customDialog.setBigSizeDefault(true); //Se indica que este tipo de dialogo requiere un tamano grande por defecto
		//Se establece un tamano minimo por defecto
		customDialog.setMinimumSize(new Dimension(Constants.CUSTOMDIALOG_FONT_INITIAL_WIDTH, Constants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT));

		//Nombre accesible para el cuadro de texto
		String fullAccesibleName = message;
		//Nombre accesible para la lista - scrollPane
		String fullList=""; //$NON-NLS-1$

		//Restricciones para el panel de datos
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 1;
		c.gridy = 1;
		c.weightx = 0.0;
        c.weighty = 3.0;
        c.gridwidth = 2;
        c.insets = new Insets(0,10,5,10);  //right padding

        //Se crea un panel de scrolls
        final JScrollPane scrPane = new JScrollPane(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
		ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrPane.setFocusable(true);

		//Panel para insertar las etiquetas
		final JPanel panel = new JPanel(new GridLayout(list.size(), 1));
		final GridBagConstraints constraints = new GridBagConstraints();
    	c.insets = new Insets(0,10,0,10);
		panel.setVisible(true);
		//Se inserta el panel asignando unos insets
		scrPane.getViewport().add(panel, constraints);

		//Se crean etiquetas con cada elemento que contiene la lista
		//y se anaden al scroll
        if (list.size()>0) {
        	int index = 1;
        	 for (final String str : list) {
        		 //Etiqueta con un espacio en blanco al principio.
        		 final InfoLabel infoLabel = new InfoLabel("&nbsp;" + str, true); //$NON-NLS-1$
        		 infoLabel.setFocusable(false);
        		 panel.add(infoLabel);
        		 fullList+=index + ": " + str + ", "; //$NON-NLS-1$ //$NON-NLS-2$
        		 index++;
             }
        }
       //Se le da un nombre accesible al elemento para que lo lea el lector de pantallas
        scrPane.getAccessibleContext().setAccessibleName(nameListElements + ": " + fullList); //$NON-NLS-1$
        //Se crea el nombre accesible para el cuadro de texto
        fullAccesibleName += fullList;

		//Se asignan las configuraciones de accesibilidad
		Utils.remarcar(scrPane);
        Utils.setContrastColor(scrPane);
        Utils.setFontBold(scrPane);
        //Se anade el scroll al panel principal
		customDialog.mainPanel.add(scrPane, c);

		c.insets = new Insets(7,10,0,10);  //right padding
		c.gridy = 3;
		c.weighty = 0.5;

        //campo de texto del dialogo
		customDialog.component = new JTextField(""); //$NON-NLS-1$
        customDialog.component.addAncestorListener(new RequestFocusListener());
        Utils.remarcar(customDialog.component);
        Utils.setContrastColor(customDialog.component);
        Utils.setFontBold(customDialog.component);
       //Se anade el campo de texto al panel de informacion general
        customDialog.mainPanel.add(customDialog.component, c);
        //Se le asigna el nombre accesible
        customDialog.component.getAccessibleContext().setAccessibleName(fullAccesibleName.replaceAll(Constants.HTML_SALTO_LINEA, "") +" ALT + " + mnemonic + ". "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        //Etiqueta principal
        //Se relaciona la etiqueta con el componente
	    customDialog.infoLabel.setLabelFor(customDialog.component);
		//Se asigna un atajo
		customDialog.infoLabel.setDisplayedMnemonic(mnemonic);
		//Se muestra el atajo
		final String text = Utils.remarkMnemonic(customDialog.infoLabel.getText(), mnemonic);
		customDialog.infoLabel.setText(text);

		//Restricciones botones
		final GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridy = 0;
		cons.insets = new Insets(0,0,0,10);  //right padding

		//Cancel button
		cancelButton = customDialog.getButton(cancellText, KeyEvent.VK_C);
		final JPanel cancelPanel = new JPanel();
		cancelPanel.add(cancelButton);
		customDialog.buttonsPanel.add(cancelPanel, cons);

		cancelButton.addActionListener(customDialog);

		customDialog.setVisible(true);

		//Control para saber si se ha pulsado el boton cancelar
		if (customDialog.getAnswer()== JOptionPane.YES_OPTION) {
			return ((JTextField)customDialog.component).getText();
		}
		return null;

	}

	/**
	 * Muestra un dialogo de solicitado de password.
	 * @param componentParent componente padre
	 * @param modal modal
	 * @param charSet filtro
	 * @param beep
	 * @param message mensaje a mostrar
	 * @param mnemonic tajo
	 * @param title titulo del dialogo
	 * @param typeMessage tipo de mensaje
	 * @return pass
	 */
	public static char[] showInputPasswordDialog(final Component componentParent, final boolean modal, final String charSet, final boolean beep, final String message, final int mnemonic, final String title, final int typeMessage){

		final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(componentParent, modal, message, title, typeMessage, true);

		//Restricciones para el panel de datos
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 1;
		c.gridy = 1;
		c.weightx = 0.0;
		c.weighty = 0.5;
        c.gridwidth = 2;
        c.insets = new Insets(10,0,0,10);  //right padding

        //campo de password del dialogo
        customDialog.component = new JPasswordField(""); //$NON-NLS-1$
        customDialog.component.addAncestorListener(new RequestFocusListener());
        Utils.remarcar(customDialog.component);
        Utils.setContrastColor(customDialog.component);
        Utils.setFontBold(customDialog.component);
        customDialog.component.getAccessibleContext().setAccessibleName(message.replaceAll(Constants.HTML_SALTO_LINEA, "") +"  ALT + " + mnemonic + ". "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        //Se anade el campo de texto al panel de informacion general
        customDialog.mainPanel.add(customDialog.component, c);

	   	 if (charSet != null) {
	   		((JPasswordField)customDialog.component).setDocument(new JTextFieldFilter(charSet, beep));
	     }

	    //Etiqueta principal
        //Se relaciona la etiqueta con el componente
	    customDialog.infoLabel.setLabelFor(customDialog.component);
		//Se asigna un atajo
		customDialog.infoLabel.setDisplayedMnemonic(mnemonic);
		//Se muestra el atajo
		final String text = Utils.remarkMnemonic(customDialog.infoLabel.getText(), mnemonic);
		customDialog.infoLabel.setText(text);

		//Check de mostrar contrasena o no
		final JPanel panelCheckShowPass = new JPanel(new GridLayout(1, 1));
		final JCheckBox showPassCheckBox= new JCheckBox(Messages.getString("CustomDialog.showInputPasswordDialog.showPassCheckBox.text")); //$NON-NLS-1$
		showPassCheckBox.setToolTipText(Messages.getString("CustomDialog.showInputPasswordDialog.showPassCheckBox.tooltip")); //$NON-NLS-1$
		showPassCheckBox.getAccessibleContext().setAccessibleDescription(showPassCheckBox.getToolTipText());
		//se asigna un mnemonico que debe ser diferente al del campo de contrasena
		if (mnemonic != KeyEvent.VK_T) {
			showPassCheckBox.setMnemonic(KeyEvent.VK_T);
		}
		//Se almacena el caracter por defecto para ocultar la contrasena
		final char defaultChar = ((JPasswordField)customDialog.component).getEchoChar();
		showPassCheckBox.setSelected(false); //Check noseleccionado por defecto
		showPassCheckBox.addItemListener(new ItemListener() {
			/**
			 * Evento que se produce cuando el componente cambia de estado.
			 */
			@Override
            public void itemStateChanged(final ItemEvent evt) {
				if (evt.getStateChange() == ItemEvent.SELECTED){
					//Se muestra la contrasena
					((JPasswordField)customDialog.component).setEchoChar((char)0);

				} else if (evt.getStateChange() == ItemEvent.DESELECTED){
					//Se oculta la contrasena
					((JPasswordField)customDialog.component).setEchoChar(defaultChar);
				}

				//Foco al input
				customDialog.component.requestFocus();
			}
		});
		Utils.remarcar(showPassCheckBox);
		Utils.setContrastColor(showPassCheckBox);
		Utils.setFontBold(showPassCheckBox);
		panelCheckShowPass.add(showPassCheckBox);

		//Restricciones para el check
		c.insets = new Insets(0,0,0,10);  //right padding
		c.gridy = 2;

		//Se anade el check al panel de informacion general
        customDialog.mainPanel.add(panelCheckShowPass, c);

		//Restricciones del panel de botones
		final GridBagConstraints cons = new GridBagConstraints();
		cons.insets = new Insets(0,0,0,10);  //right padding

		//Cancel button
		cancelButton = customDialog.getButton(cancellText, KeyEvent.VK_C);
		final JPanel cancelPanel = new JPanel();
		cancelPanel.add(cancelButton);
		customDialog.buttonsPanel.add(cancelPanel, cons);
		cancelButton.addActionListener(customDialog);

		customDialog.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); //Se centra el texto
		customDialog.component.setVisible(true); //Se hace visible el campo de texto

		cancelButton.addActionListener(customDialog);

		customDialog.setVisible(true);

		//Control para saber si se ha pulsado el boton cancelar
		if (customDialog.getAnswer()== JOptionPane.YES_OPTION) {
			return ((JPasswordField)customDialog.component).getPassword();
		}
		throw new AOCancelledOperationException("La insercion de contrasena ha sido cancelada por el usuario"); //$NON-NLS-1$

	}

	/**
	 * Metodo que crea un boton.
	 * Si el boton corresponde al de cancelar, se le asigna la tecla esc.
	 * @param text texto del boton
	 * @param mnemonic atajo
	 * @return boton
	 */
	private JButton getButton(final String text, final int mnemonic){
		final JButton button = new JButton (text);
		button.setMnemonic(mnemonic);
		Utils.remarcar(button);
        Utils.setContrastColor(button);
        Utils.setFontBold(button);
        //Se comprueba si el boton es el de cancelar
        if (text.equalsIgnoreCase(cancellText)) {
        	//Se asigna la tecla escape a dicho boton
        	final String cancelKey = "cancel"; //$NON-NLS-1$
    		this.getRootPane().getInputMap (JComponent.WHEN_IN_FOCUSED_WINDOW).put (KeyStroke.getKeyStroke (KeyEvent.VK_ESCAPE, 0), cancelKey);
    		this.getRootPane ().getActionMap ().put (cancelKey, new AbstractAction () {
    			/**
				 * UID.
				 */
				private static final long serialVersionUID = 1L;
				/**
				 * Indica que la accion es la de pulsar el boton cancelar.
				 */
				@Override
				public void actionPerformed (final ActionEvent event) {
    				CustomDialog.getCancelButton().doClick ();
    			}

    		});
        }

		return button;
	}

	/** {@inheritDoc} */
	@Override
	public int getMinimumRelation() {
		return 9;
	}


	/**
	 * @return the answer
	 */
	private int getAnswer() {
		return this.answer;
	}

	/**
	 * Cambia el tama&ntilde;o de la ventana al tama&ntilde;o m&aacute;ximo de pantalla menos el tama&ntilde;o de la barra de tareas de windows
	 */
	void maximizarActionPerformed(){
		setActualPositionX(this.getX());
		setActualPositionY(this.getY());
		setActualWidth(this.getWidth());
		setActualHeight(this.getHeight());

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = Constants.CUSTOMDIALOG_MAX_WIDTH;
		final int maxHeight = Constants.CUSTOMDIALOG_MAX_HEIGHT;

		//Se hace el resize
		this.setBounds(getInitialX(maxWidth), getInitialY(maxHeight), maxWidth, maxHeight);

		// Habilitado/Deshabilitado de botones restaurar/maximizar
		this.maximizeButton.setEnabled (false);
		this.restoreButton.setEnabled (true);
	}

	/**
	 * Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado
	 */
	void restaurarActionPerformed(){
		//Dimensiones de restaurado
		int minWidth = Constants.CUSTOMDIALOG_INITIAL_WIDTH;
		int minHeight = Constants.CUSTOMDIALOG_INITIAL_HEIGHT;
		//Se comprueba las opciones de accesibilidad activas
		if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold() || isBigSizeDefault()){
			minWidth = Constants.CUSTOMDIALOG_FONT_INITIAL_WIDTH;
			minHeight = Constants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT;
		}
		//Se establece el tamano minimo
		setMinimumSize(new Dimension(minWidth, minHeight));

		//Se situa el dialogo
		if (getActualPositionX() != -1 && getActualPositionY() != -1 && getActualWidth() != -1 && getActualHeight() != -1){
			this.setBounds(getActualPositionX(), getActualPositionY(), getActualWidth(), getActualHeight());
		} else {
    		setBounds(getInitialX(minWidth), getInitialY(minHeight), minWidth, minHeight);
		}

		// Habilitado/Deshabilitado de botones restaurar/maximizar
		this.maximizeButton.setEnabled (true);
		this.restoreButton.setEnabled (false);
	}

	/**
	 * Metodo que devuelve una instancia de CustomDialog.
	 * @param componentParent Componente padre
	 * @param modal modal
	 * @param message mensaje
	 * @param title titulo del dialogo
	 * @param typeMessage tipo de alerta.
	 * @param isInputDialog indica sies una alerta de tipo input
	 * @return instancia de CustomDialog.
	 */
	public static CustomDialog getInstanceCustomDialog(final Component componentParent, final boolean modal, final String message, final String title, final int typeMessage, final boolean isInputDialog){
		CustomDialog customDialog = null;
		//Se chequea cual sera el componente padre.
		if (componentParent instanceof JDialog) {
			customDialog = new CustomDialog((JDialog)componentParent, modal, message, title, typeMessage, isInputDialog);
		}
		else if (componentParent instanceof JFrame){
			customDialog = new CustomDialog((JFrame)componentParent, modal, message, title, typeMessage, isInputDialog);
		}
		else {
			customDialog = new CustomDialog(componentParent, modal, message, title, typeMessage, isInputDialog);
		}
		return customDialog;
	}

	/**
	 * Accion correspondiente a los botones de las alertas.
	 */
	@Override
	public void actionPerformed(final ActionEvent e) {

		if (e.getSource().equals(this.okButton)) {
			this.answer = JOptionPane.YES_OPTION;
		}
		else if (e.getSource().equals(this.noButton)) {
			this.answer = JOptionPane.NO_OPTION;
		}
		else {
			this.answer = JOptionPane.CANCEL_OPTION;
		}
		setVisible(false);
	}



	 /** Original code: <a
     * href="http://tactika.com/realhome/realhome.html">http://
     * tactika.com/realhome/realhome.html</a>
     * @author Real Gagnon */
    private static final class JTextFieldFilter extends PlainDocument {
    	/**
    	 * UID.
    	 */
        private static final long serialVersionUID = -5746396042117084830L;

        /**
         * Caracteres aceptados.
         */
        private String acceptedChars = null;

        /** Crea un nuevo filtro para campo de entrada de texto.
         * @param acceptedchars
         *        Cadena que debe contener todos los caracteres aceptados.
         *        Cualquier caracter no incluido en esta cadena ser&aacute;
         *        considerado inv&aacute;lido
         * @param beepOnError
         *        <code>true</code> si desea que se reproduzca un sonido
         *        cuando el usuario introduce un caracter no v&aacute;lido,
         *        false en caso contrario */
        JTextFieldFilter(final String acceptedchars, final boolean beepOnError) {
            this.beep = beepOnError;
            this.acceptedChars = acceptedchars;
        }

        private boolean beep = false;

        /**
         * Insercion de texto.
         */
        @Override
        public void insertString(final int offset, final String str, final AttributeSet attr) throws BadLocationException {
            if (str == null) {
                return;
            }
            for (int i = 0; i < str.length(); i++) {
                if (this.acceptedChars.indexOf(String.valueOf(str.charAt(i))) == -1) {
                    if (this.beep) {
                        Toolkit.getDefaultToolkit().beep();
                    }
                    return;
                }
            }
            super.insertString(offset, str, attr);
        }

    }

}
