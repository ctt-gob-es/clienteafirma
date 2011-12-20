package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityFrameAdvisor;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.ProfileManager;

/**
 * Ventana de selección de perfiles.
 * @author inteco
 *
 */
public class UserProfile extends JAccessibilityFrameAdvisor {

	private static final long serialVersionUID = 1L;	

	/**
	 * Devuelve la relación mínima para el redimensionado de elementos.
	 */
	@Override
	public int getMinimumRelation() {
		// TODO Auto-generated method stub
		return 9;
	}
   
	/**
	 * Perfil actual.
	 */
    public static String currentProfileId;
        
    JComboBox comboPerfiles = new JComboBox();
	
	/**
	 * Posición X inicial de la ventana dependiendo de la resolución de pantalla.
	 * @return int Posición X
	 */
	public int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 500) / 2 ;
	}
	
	/**
	 * Posición Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resolución de pantalla.
	 * @return int Posición Y
	 */
	public int getInitialY() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		if (Platform.getOS().equals(Platform.OS.MACOSX)){
			return (screenSize.height - 340) / 2;
		}else{
			return (screenSize.height - 320) / 2;
		}
	}

	/**
	 * Constructor.
	 */
	public UserProfile() {
		super();
		initComponents();
		HelpUtils.visualize("perfiles.usuario");
	}
	
	/**
	 * Inicializacion de los componentes
	 */
	private void initComponents() {
		
		if (getBackground().getRGB()==-16777216){
			Main.isOSHighContrast = true;			
		}
		
		// Dimensiones de la ventana
		setBounds(this.getInitialX(), this.getInitialY(), Constants.INIT_WINDOW_INITIAL_WIDTH, Constants.INIT_WINDOW_INITIAL_HEIGHT);
		
		// Parametros ventana
		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); // NOI18N
		setTitle(Messages.getString("UserProfile.title")); // NOI18N
		getContentPane().setLayout(new BorderLayout(11, 7));
		setMinimumSize(getSize());

		// Icono de @firma
		setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage());
		
		this.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
        
		c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty=0.1;
        c.insets = new Insets(5, 13, 5, 13);
        c.gridy = 0;
        
        // Logotipo de la aplicacion
        c.gridwidth = 3;
        JLabel logotipo = new JLabel();
		logotipo.setIcon(new ImageIcon(getClass().getResource("/resources/images/logo_cliente.png")));//$NON-NLS-1$
		logotipo.setHorizontalAlignment(SwingConstants.CENTER);
		add(logotipo, c);
		c.gridy = c.gridy + 1;
		c.gridwidth = 1;

		// Cuerpo del mensaje
		InfoLabel text = new InfoLabel("<p>"+Messages.getString("UserProfile.welcome")+"</p>"+"<p>"+Messages.getString("UserProfile.body1")+"</p>"+"<p>"+Messages.getString("UserProfile.body2")+"</p>"+"<p>"+Messages.getString("UserProfile.body3")+"</p>", false);
		config(text);
		
		add(text,c);
		c.gridy = c.gridy + 1;
		
        // Etiqueta de la lista de usuarios
		JLabel label = new JLabel();
		label.setText(Messages.getString("UserProfile.list.label")); // NOI18N
		label.setDisplayedMnemonic(KeyEvent.VK_P);
		label.setLabelFor(this.comboPerfiles);
		
		add(label,c);
		c.gridy = c.gridy + 1;
		
		// Lista de usuarios
		List<String> profileNames = new ArrayList<String>();
		profileNames.add(ProfileManager.DEFAULT_PROFILE_NAME);
		for (String profileName : ProfileManager.getProfilesNames()) {
			profileNames.add(profileName);
		}
		
		this.comboPerfiles.setModel(new DefaultComboBoxModel(profileNames.toArray(new String[0])));
		// Preselecionado el ultimo perfil cargado
		if (ProfileManager.getLastProfileName() != null) {
			this.comboPerfiles.setSelectedItem(ProfileManager.getLastProfileName());
		}
		
		config(this.comboPerfiles);
 
		add(this.comboPerfiles,c);
		
		c.gridy = c.gridy + 1;
		c.insets = new Insets(0, 0, 0, 0);
		add(createButtonsPanel(), c);
		
        //Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.comboPerfiles, "perfil.cargar");
       
		
	}

	/**
	 * Método que genera el panel de botones.
	 * @return panel de botones.
	 */
	private Component createButtonsPanel() {
    	// Panel inferior
        JPanel bottomPanel = new JPanel(new GridBagLayout());
        
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 0;
		cons.gridx = 0;
		cons.insets = new Insets(5, 0, 5, 0);
		
	    JPanel panelAceptar = new JPanel(new GridLayout(1, 1));
		// Boton aceptar
		JButton aceptar = new JButton();
		aceptar.setText("Aceptar");
		aceptar.setMnemonic(KeyEvent.VK_A);
		aceptar.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				aceptarPerformed();
			}
		});
		//Se asigna este botón como botón por defecto de la ventana.
		this.getRootPane().setDefaultButton(aceptar);
		panelAceptar.add(aceptar);
		config(aceptar);
		
	    //Espacio entre botones
		JPanel panelVacio = new JPanel();
		panelVacio.setPreferredSize(new Dimension(100, 10));
        
        // Panel en donde se insertan los botones maximizar y aceptar
        JPanel buttonPanel = new JPanel();
       
        buttonPanel.add(panelAceptar, BorderLayout.CENTER);
		
        cons.ipadx = 0;
		cons.weightx = 1.0;
		cons.gridx = 1;
		
		bottomPanel.add(buttonPanel, cons);
        
		JPanel panelAyuda = new JPanel(new GridLayout(1, 1));
		// Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("perfiles.usuario");
		botonAyuda.setName("helpButton");
		config(botonAyuda);
		
        cons.ipadx = 15;
		cons.weightx = 0.02;
		cons.gridx = 2;
        panelAyuda.add(botonAyuda);
        bottomPanel.add(panelAyuda, cons);
        
      //Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(aceptar, "perfil.aceptar");
        
        return bottomPanel;
    }
	
	/**
	 * Muestra la ventana de la aplicaci&oacute;n
	 */
	public void main() {	
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				setVisible(true);
			}
		});	
	}
   
	/**
	 * Acción aceptar.
	 */
    private void aceptarPerformed(){
    	
    	final String profileName = this.comboPerfiles.getSelectedItem().toString();
    	
    	// Establecemos como ultimo perfil cargado: el por defecto si se selecciono el primer
    	// elemento de la lista o el seleccionado si es cualquier otro  
    	ProfileManager.setLastProfileName(
    			(this.comboPerfiles.getSelectedIndex() == 0) ?
    					null : profileName);
    	
    	if (ProfileManager.DEFAULT_PROFILE_NAME.equals(profileName)){
    		currentProfileId = null;
    		GeneralConfig.loadConfig(ProfileManager.getDefaultConfiguration());
    	} else {
    		System.out.println("Nombre del perfil seleccionado: " + profileName);
    		System.out.println("Id del perfil seleccionado: " + ProfileManager.getProfileIdByName(profileName));
    		
    		currentProfileId = ProfileManager.getProfileIdByName(profileName);
    		
    		
    		Properties c = ProfileManager.getConfiguration(currentProfileId);
    		for (String key : c.keySet().toArray(new String[0])) {
    			System.out.println(key + ": " + c.getProperty(key));
    		}
    		
    		
    		GeneralConfig.loadConfig(ProfileManager.getConfiguration(profileName));
    	}
    	
    	new PrincipalGUI().main();
    	dispose();
    }
    
    /**
     * Configuracion de accesibilidad por defecto de la pantalla.
     * Marcado de elementos con foco. 
     * @param component
     */
    private void config(JComponent component){
    	if (component instanceof JLabel){
			final JLabel label = (JLabel) component;
			label.addFocusListener(new FocusListener() {
				public void focusLost(FocusEvent e) {
					label.setBorder(BorderFactory.createEmptyBorder());
				}
				public void focusGained(FocusEvent e) {
					label.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
				}
			});
			
		}
    	if (component instanceof JComboBox){
			final JComboBox combo = (JComboBox) component;
			combo.addFocusListener(new FocusListener() {
				public void focusLost(FocusEvent e) {
					combo.setBorder(BorderFactory.createEmptyBorder());
				}
				public void focusGained(FocusEvent e) {
					combo.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
				}
			});
		}
    	if (component instanceof JButton){
			final JButton button = (JButton) component;
			button.addFocusListener(new FocusListener() {
				public void focusLost(FocusEvent e) {
					((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
				}		
				public void focusGained(FocusEvent e) {
					((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
				}
			});
		}
    }
	
}
