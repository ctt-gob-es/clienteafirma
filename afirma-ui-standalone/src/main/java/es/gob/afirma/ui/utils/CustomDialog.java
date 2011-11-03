package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.UIManager;

/**
 * Componente dialogo que define los alerts de la aplicacion.
 * @author inteco
 *
 */
public class CustomDialog extends JAccessibilityCustomDialog implements ActionListener {

	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * Etiqueta con la información de la alerta.
	 * 
	 */
	private InfoLabel infoLabel = null;
	
	/**
	 * Panel de botones.
	 */
	private JPanel buttonsPanel = null;
	
	/**
	 * Campo de texto.
	 */
	private JTextField textField = null;
	
	/**
	 * Etiqueta que contiene el icono de la alerta.
	 */
	private IconLabel iconLabel = new IconLabel();
	
	/**
	 * Boton de OK.
	 */
	private JButton okButton = null;
	
	/**
	 * Boton de Cancel.
	 */
	private JButton cancelButton = null;
	
	/**
	 * Boton de restaurar.
	 */
	private JButton restoreButton = null;
	
	/**
	 * Boton de maximizar.
	 */
	private JButton maximizeButton = null;
	
	
	/**
	 * Respuesta al mensaje
	 */
	private int answer;
	
	
	
	/**
	 * Constructor.
	 *
	 * @param componentParent componente padre.
	 * @param modal modal
	 * @param message mensaje
	 * @param title titulo
	 * @param typeMessage tipo de mensaje
	 */
	public CustomDialog(JDialog componentParent, boolean modal, String message, String title, int typeMessage){
		super(componentParent, modal);
		initComponents(message, title, typeMessage);
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
	 */
	public CustomDialog(Component componentParent, boolean modal, String message, String title, int typeMessage){
		super();
		this.setModal(modal);
		initComponents(message, title, typeMessage);
		setLocationRelativeTo(componentParent);
		
	}
	
    /**
	 * Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n X
	 */
    public int getInitialX(int width) {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); 
		return ((screenSize.width/2) - (width / 2)) ;
	}
    
    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
	 */
	public int getInitialY(int height) {
        Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        return ((screenSize.height/2) - (height / 2)) ;
	}

	/**
	 * Metodo que inicializa los componentes de la alerta.
	 * @param message mensaje que se mostrara en la alerta
	 * @param title titulo de la alerta
	 * @param typeMessage tipo de mensaje
	 */
	private void initComponents(String message, String title, int typeMessage){

		//Se obtienen las dimensiones de maximizado
		int maxWidth = Constants.CUSTOMDIALOG_MAX_WIDTH;
		int maxHeight = Constants.CUSTOMDIALOG_MAX_HEIGHT;
		
		//Se establece el tamaño mínimo
		setMinimumSize(new Dimension(Constants.CUSTOMDIALOG_INITIAL_WIDTH, Constants.CUSTOMDIALOG_INITIAL_HEIGHT));
		//Se establece el tamaño máximo
		setMaximumSize(new Dimension(maxWidth,maxHeight));
		
		
		// Dimensiones de la ventana en Windows y Linux
    	if (GeneralConfig.isMaximized()){
    		//Se maximiza
    		this.setBounds(0,0, maxWidth, maxHeight);
    	}
		
		this.setTitle(title);
		
		this.answer = JOptionPane.NO_OPTION;
		//Contenedor del diálogo
		Container container = getContentPane();
		//Layout del contenedor
		container.setLayout(new GridBagLayout());
		
		//Panel con los datos del diálogo
		JPanel mainPanel = new JPanel(new GridBagLayout());

		//Restricciones para el panel de datos
		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 0;
       // c.gridwidth = 1;
        c.insets = new Insets(0,10,0,10);  //right padding
        
        //Icono del diálogo
       setIconLabel(typeMessage);
       // this.iconLabel.setBackground(Color.red);
       mainPanel.add(iconLabel, c);

        c.gridx = 1;
        c.weightx = 1.0;
        //c.weighty = 1.0;
        c.gridwidth = 2;
	       
        //Etiqueta del diálogo
		this.infoLabel = new InfoLabel(message, false);
		this.infoLabel.setHorizontalAlignment(JLabel.CENTER); //Se centra el texto
		//Foco a la etiqueta
		this.infoLabel.addAncestorListener(new RequestFocusListener());
		//Se añade la etiqueta al panel de información general
		mainPanel.add(this.infoLabel, c);
		
		c.gridy = 1;
		c.weightx = 0.0;
        //c.weighty = 0.0;
        c.gridwidth = 2;
        c.insets = new Insets(0,10,0,10);  //right padding
        
        //campo de texto del diálogo
        this.textField = new JTextField("");
        this.textField.setVisible(false); //Se oculta el campo
        Utils.remarcar(this.textField);
        Utils.setContrastColor(this.textField);
        Utils.setFontBold(this.textField);
      //Se añade el campo de texto al panel de información general
        mainPanel.add(this.textField, c);

		//Panel de botones
		createButtonsPanel();
		
		//Restricciones del contenedor general
		
		GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.BOTH;
        cons.gridx = 0;
        cons.gridy = 0;
        cons.weightx = 1.0;
        cons.weighty = 1.0;
        //cons.gridheight = 1;
        
        //se añade el panel de información
        container.add(mainPanel, cons);        
        cons.gridy = 1;
        cons.gridx = 0;
       // cons.weightx = 1.0;
        //cons.weighty = 1.0;
       // cons.gridheight = 1;
        
        //Se añade el panel de botones
        container.add(this.buttonsPanel, cons);
	}

	/**
	 * Se asigna el icono a la etiqueta.
	 * @param typeMessage
	 */
	private void setIconLabel(int typeMessage) {
		//Según el tipo de mensaje se selecciona el icono
  		Icon icon = null;
  		if (typeMessage == JOptionPane.ERROR_MESSAGE) {
  			icon = UIManager.getIcon("OptionPane.errorIcon");
  		} else if (typeMessage == JOptionPane.WARNING_MESSAGE) {
  			icon = UIManager.getIcon("OptionPane.warningIcon");
  		} else if (typeMessage == JOptionPane.QUESTION_MESSAGE) {
  			icon = UIManager.getIcon("OptionPane.questionIcon");
  		} else {
			icon = UIManager.getIcon("OptionPane.informationIcon");
  		}
  		this.iconLabel.setIcon(icon);
  		this.iconLabel.setOriginalIcon(icon);
	}
	
	/**
	 * Se crea el panel de botones
	 * @param typeMessage
	 */
	private void createButtonsPanel() {
		this.buttonsPanel = new JPanel(new GridBagLayout());
		
		//Restricciones para el panel de botones
		GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.HORIZONTAL;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.insets = new Insets(0,10,0,10);  //right padding
		
		//Maximize button
		JPanel maximizePanel = new JPanel();
		this.maximizeButton = getButton(Messages.getString("Wizard.maximizar"), KeyEvent.VK_M );
		this.maximizeButton.setName("maximizar");
		maximizePanel.add(this.maximizeButton);
		this.maximizeButton.addActionListener(new ActionListener() {
		    	public void actionPerformed(ActionEvent e) {
		    		maximizarActionPerformed();
				}
			});

        buttonsPanel.add(maximizePanel, consButtons);
		
		consButtons.gridx = 1;
		consButtons.insets = new Insets(0,0,0,30);  //right padding
		
		//Restore button
		JPanel restorePanel = new JPanel();
		this.restoreButton = getButton(Messages.getString("Wizard.restaurar"), KeyEvent.VK_R );
		this.restoreButton.setName("restaurar");
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
        buttonsPanel.add(restorePanel, consButtons);
		
		consButtons.gridx = 2;
		consButtons.insets = new Insets(0,0,0,10);  //right padding

		//OK button
		JPanel okPanel = new JPanel();
		this.okButton = getButton(Messages.getString("PrincipalGUI.aceptar"), KeyEvent.VK_A);
		okPanel.add(this.okButton);
		this.buttonsPanel.add(okPanel, consButtons);
		
		this.okButton.addActionListener(this);
		
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
		
	}
	
	/**
	 * Muestra un dialogo con un mensaje.
	 * @param componentParent componente padre
	 * @param message mensaje a mostrar
	 * @param title titulo del dialogo
	 * @param messageType tipo de mensaje
	 */
	public static void showMessageDialog(Component componentParent, boolean modal, String message, String title, int typeMessage){
	
		CustomDialog customDialog = new CustomDialog(componentParent, modal, message, title, typeMessage);
		customDialog.infoLabel.setHorizontalAlignment(JLabel.CENTER); //Se centra el texto
		customDialog.textField.setVisible(false); //Se oculta el campo de texto
		customDialog.setVisible(true);
	}
	
	/**
	 * Muestra un dialogo con un mensaje.
	 * @param componentParent componente padre
	 * @param message mensaje a mostrar
	 * @param title titulo del dialogo
	 * @param messageType tipo de mensaje
	 */
	public static int showConfirmDialog(Component componentParent, boolean modal, String message, String title, int typeMessage){
		
		CustomDialog customDialog = new CustomDialog(componentParent, modal, message, title, JOptionPane.QUESTION_MESSAGE);
		
		//Restricciones
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridx = 3;
		cons.gridy = 0;
		cons.insets = new Insets(0,0,0,10);  //right padding

		//Se comprueba el tipo de diálogo
		if (typeMessage == JOptionPane.YES_NO_OPTION) {
			//Botón Sí
			customDialog.okButton.setText(Messages.getString("CustomDialog.confirmDialog.yes"));
			customDialog.okButton.setMnemonic(KeyEvent.VK_S);
			//Botón no
			customDialog.cancelButton = customDialog.getButton(Messages.getString("CustomDialog.confirmDialog.no"), KeyEvent.VK_N);
			JPanel cancelPanel = new JPanel();
			cancelPanel.add(customDialog.cancelButton);
			customDialog.buttonsPanel.add(cancelPanel, cons);
			customDialog.cancelButton.addActionListener(customDialog);
		} else {
			//Botón Cancelar
			customDialog.cancelButton = customDialog.getButton(Messages.getString("PrincipalGUI.cancelar"), KeyEvent.VK_C);
			JPanel cancelPanel = new JPanel();
			cancelPanel.add(customDialog.cancelButton);
			customDialog.buttonsPanel.add(cancelPanel, cons);
			customDialog.cancelButton.addActionListener(customDialog);
		}
		
		customDialog.infoLabel.setHorizontalAlignment(JLabel.CENTER); //Se centra el texto
		customDialog.textField.setVisible(false);//Se oculta el campo de texto
				
		customDialog.setVisible(true);
		return customDialog.getAnswer();
	}
	
	/**
	 * Muestra un dialogo de inserccion.
	 * @param componentParent componente padre
	 * @param message mensaje a mostrar
	 * @param title titulo del dilogo
	 * @param messageType tipo de mensaje
	 */
	public static String showInputDialog(Component componentParent, boolean modal, String message, String title, int typeMessage){
		CustomDialog customDialog = new CustomDialog(componentParent, modal, message, title, typeMessage);
		
		//Restricciones
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridx = 3;
		cons.gridy = 0;
		cons.insets = new Insets(0,0,0,10);  //right padding

		//Cancel button
		customDialog.cancelButton = customDialog.getButton(Messages.getString("PrincipalGUI.cancelar"), KeyEvent.VK_C);
		JPanel cancelPanel = new JPanel();
		cancelPanel.add(customDialog.cancelButton);
		customDialog.buttonsPanel.add(cancelPanel, cons);
		customDialog.cancelButton.addActionListener(customDialog);
        
		customDialog.infoLabel.setHorizontalAlignment(JLabel.LEFT); //Se centra el texto
		customDialog.textField.setVisible(true); //Se hace visible el campo de texto
		
		customDialog.cancelButton.addActionListener(customDialog);
		
		customDialog.setVisible(true);
		
		return customDialog.textField.getText();
		
		
	}
	
	private JButton getButton(String text, int mnemonic){
		JButton button = new JButton (text);
		button.setMnemonic(mnemonic);
		Utils.remarcar(button);
        Utils.setContrastColor(button);
        Utils.setFontBold(button);
		return button;
	}

	@Override
	public int getMinimumRelation() {
		return 9;
	}


	/**
	 * @return the answer
	 */
	public final int getAnswer() {
		return answer;
	}

	/**
	 * @param answer
	 *            the answer to set
	 */
	public final void setAnswer(int answer) {
		this.answer = answer;
	}
	
	/**
	 * Cambia el tama&ntilde;o de la ventana al tama&ntilde;o m&aacute;ximo de pantalla menos el tama&ntilde;o de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		actualPositionX = this.getX();
		actualPositionY = this.getY();
		actualWidth = this.getWidth();
		actualHeight = this.getHeight();
		
		//Se obtienen las dimensiones de maximizado
		int maxWidth = Constants.CUSTOMDIALOG_MAX_WIDTH;
		int maxHeight = Constants.CUSTOMDIALOG_MAX_HEIGHT;
		
		//Se hace el resize
		this.setBounds(getInitialX(maxWidth), getInitialY(maxHeight), maxWidth, maxHeight);
		//this.setBounds(0, 0, maxWidth, maxHeight);
		
		// Habilitado/Deshabilitado de botones restaurar/maximizar
		this.maximizeButton.setEnabled (false);
		this.restoreButton.setEnabled (true);
	}
	
	/**
	 * Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado
	 */
	public void restaurarActionPerformed(){
		
		//Dimensiones de restaurado
		int minWidth = Constants.CUSTOMDIALOG_INITIAL_WIDTH;
		int minHeight = Constants.CUSTOMDIALOG_INITIAL_HEIGHT;
		
		setMinimumSize(new Dimension(minWidth, minHeight));
		
		if (actualPositionX != -1 && actualPositionY != -1 && actualWidth != -1 && actualHeight != -1){
			this.setBounds(actualPositionX, actualPositionY, actualWidth, actualHeight);
		} else {
    		setBounds(getInitialX(minWidth), getInitialY(minHeight), minWidth, minHeight);
			//setBounds(0, 0, minWidth, minHeight);	
		}
		// Habilitado/Deshabilitado de botones restaurar/maximizar
		this.maximizeButton.setEnabled (true);
		this.restoreButton.setEnabled (false);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource().equals(okButton)) {
			setVisible(false);
			answer = JOptionPane.YES_OPTION;
		} else {
			setVisible(false);
			answer = JOptionPane.NO_OPTION;
		}
		
	}
}
