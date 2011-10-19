package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractListModel;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogAdvisor;
import es.gob.afirma.ui.utils.Messages;

public class UserProfile extends JAccessibilityDialogAdvisor {

	private static final long serialVersionUID = 1L;
	
	@Override
	public int getMinimumRelation() {
		// TODO Auto-generated method stub
		return 9;
	}
    
    public static String currentUser;
    
    JList list = new JList();
       
	private int actualPositionX = -1;
	
	private int actualPositionY = -1;
	
	private int actualWidth = -1;
	
	private int actualHeight = -1;
	
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
	
	public UserProfile() {
		super();
		initComponents();
		this.addComponentListener(new ComponentAdapter() {
		    public void componentResized(ComponentEvent e)
		    {
		    	resized(e);
		    }
		    public void componentMoved(ComponentEvent e)
		    {
		    	resized(e);
		    }
		});
	}
	
	/**
	 * Inicializacion de los componentes
	 */
	private void initComponents() {
		
		// Dimensiones de la ventana
		setBounds(this.getInitialX(), this.getInitialY(), Constants.INIT_WINDOW_INITIAL_WIDTH, Constants.INIT_WINDOW_INITIAL_HEIGHT);
		
		// Parametros ventana
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE); // NOI18N
		setTitle(Messages.getString("UserProfile.title")); // NOI18N
		getContentPane().setLayout(new BorderLayout(11, 7));
		setMinimumSize(getSize());

		// Icono de @firma
		setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage());
		
		this.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
        
		c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
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
		label.setLabelFor(list);
		
		add(label,c);
		c.gridy = c.gridy + 1;
		
		// Lista de usuarios
		int j;
		if (Main.preferences.get("users", "0").equals("0")){
			j=1;
		} else {
			j =Integer.parseInt(Main.preferences.get("users", "0"))+1; 
		}
		final String[] users = new String[j];
		final String[] userssss = new String[j];
		for (int i = 0;i<=Integer.parseInt(Main.preferences.get("users", "0"))-1;i++){
	        	userssss[i] = "user" + (i+1);
	        	
	        	users[i]=(Main.preferences.get(userssss[i], "error"));
	        	
	    }
		users[users.length-1] = Constants.defaultUser;
		final String[] strings = users;

		list.setModel(new AbstractListModel() {
			private static final long serialVersionUID = 1L;

				public int getSize() { 
					return strings.length; 
				}

				public Object getElementAt(int i) { 
					return strings[i]; 
				}
			});
		list.setSelectedIndex(strings.length-1);
		config(list);
		if (strings.length<4){
			list.setVisibleRowCount(strings.length);
		} else {
			list.setVisibleRowCount(4);
		}
		JScrollPane barra = new JScrollPane(list);
		add(barra, c); 
		
		c.gridy = c.gridy + 1;
		c.insets = new Insets(0, 0, 0, 0);
		add(createButtonsPanel(), c);
		
        //Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(list, "perfil.cargar");
       
		
	}
	
	private Component createButtonsPanel() {
    	// Panel inferior
        JPanel bottomPanel = new JPanel(new GridBagLayout());
        
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 0;
		cons.gridx = 0;
		cons.insets = new Insets(5, 0, 5, 0);
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		bottomPanel.add(label, cons);
        
		JPanel panelMaximizar = new JPanel(new GridLayout(1, 1));
		//Boton maximizar ventana
		JButton maximizar = new JButton();
		maximizar.setText(Messages.getString("Wizard.maximizar"));
	    maximizar.setName("maximizar");
	    maximizar.setMnemonic(KeyEvent.VK_M);
	    maximizar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		maximizarActionPerformed();
			}
		});
	    panelMaximizar.add(maximizar);
	    config(maximizar);
		
	    JPanel panelRestaurar = new JPanel(new GridLayout(1, 1));
	    // Boton restaurar
	    JButton restaurar = new JButton();
	    restaurar.setText(Messages.getString("Wizard.restaurar"));
	    restaurar.setName("restaurar");
	    restaurar.setMnemonic(KeyEvent.VK_R);
	    restaurar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
	    config(restaurar);
	    panelRestaurar.add(restaurar);
	    
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
		panelAceptar.add(aceptar);
		config(aceptar);
		
	    //Espacio entre botones
		Panel panelVacio = new Panel();
		panelVacio.setPreferredSize(new Dimension(100, 10));
        
        // Panel en donde se insertan los botones maximizar y aceptar
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelMaximizar, BorderLayout.CENTER);
        buttonPanel.add(panelRestaurar, BorderLayout.CENTER);
        buttonPanel.add(panelVacio, BorderLayout.CENTER);
        buttonPanel.add(panelAceptar, BorderLayout.CENTER);
		
        cons.ipadx = 0;
		cons.weightx = 1.0;
		cons.gridx = 1;
		
		bottomPanel.add(buttonPanel, cons);
        
		JPanel panelAyuda = new JPanel(new GridLayout(1, 1));
		// Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("perfiles.usuario");
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
	 * Evento de redimensionado. Almacena los valores actuales de posicion y 
	 * tamaño de la ventana.
	 * 
	 */
	public void resized(ComponentEvent e) {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

	    Dimension fullScreen = new Dimension((int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
	    Dimension actualSize = getJAccessibilityDialogAdvisor(this).getSize();
	    Component boton = getComponentByName("maximizar", getJAccessibilityDialogAdvisor(this));
	    Component botonRestaurar = getComponentByName("restaurar", getJAccessibilityDialogAdvisor(this));
	    if(boton != null){
	    	if (actualSize.equals(fullScreen)){
				boton.setEnabled(false);
				if (botonRestaurar != null) {
	    			//Si la ventana está maximizada, el botón de restaurar debe estar visible
	    			botonRestaurar.setEnabled(true);
	    		}
		    } else {
		    	boton.setEnabled(true);
		    	if (botonRestaurar != null) {
			    	//Se comprueba si la ventana está restaurada
			    	if ((this.getX() == actualPositionX) && (this.getY() == actualPositionY) 
			    			&& (this.getWidth() == actualWidth) && (this.getHeight() == actualHeight)) {
			    		botonRestaurar.setEnabled(false); //Se deshabilita
			    	} else {
			    		botonRestaurar.setEnabled(true); //Se habilita
			    	}
		    	}
		    }
	    }
	    
		if (this.getWidth()!=(int)screenSize.getWidth() && this.getHeight()!=(int)screenSize.getHeight()-35){
			actualPositionX = this.getX();
			actualPositionY = this.getY();
			actualWidth = this.getWidth();
			actualHeight = this.getHeight();
		}
	}
	/**
	 * Muestra la ventana de la aplicaciï¿½n
	 */
	public void main() {	
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				setVisible(true);
			}
		});	
	}
    
    /**
     * 
     */
    public void aceptarPerformed(){
    	if (list.getSelectedValue()==null){
    		currentUser = Constants.defaultUser;
    	} else {
    		currentUser = list.getSelectedValue().toString();
    	}
    	GeneralConfig.loadConfig(GeneralConfig.getConfig());
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
    	if (component instanceof JList){
			final JList list = (JList) component;
			list.addFocusListener(new FocusListener() {
				public void focusLost(FocusEvent e) {
					list.setBorder(BorderFactory.createEmptyBorder());
				}
				public void focusGained(FocusEvent e) {
					list.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
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
    
    /**
	 * Cambia el tamaÃ±o de la ventana al tamaÃ±o mÃ¡ximo de pantalla menos el tamaÃ±o de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		JAccessibilityDialogAdvisor j = getJAccessibilityDialogAdvisor(this);
		j.setBounds(0,0,(int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
		
	}
	
	/**
	 * Restaura el tamaÃ±o de la ventana a la posicion anterior al maximizado
	 */
	public void restaurarActionPerformed(){
		this.setBounds(actualPositionX, actualPositionY, actualWidth, actualHeight);
		
	}
	
	/**
	 * Obtiene un componente de un contenedor a traves de su nombre
	 * @param name Nombre del componente a buscar
	 * @param container Contenedor donde se encuentra el componente a buscar
	 * @return
	 */
	private Component getComponentByName(String name, Container container){
		if(name.equals(container.getName())){
			return container;
		}
		else {
			Component[] componentes = container.getComponents();
			for(int i = 0; i < componentes.length; i++){
				if(componentes[i] instanceof Container){
					Component res = getComponentByName(name, (Container) componentes[i]);
					if(res != null){
						return res;
					}
				}
				else{
					if(componentes[i].getName().equals(name)){
						return componentes[i];
					}
				}
			}
		}
		return null;
	}
	
	
}
