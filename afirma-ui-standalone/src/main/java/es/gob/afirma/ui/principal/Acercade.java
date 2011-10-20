/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityFrameAbout;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardcifradocontrasenia.PanelContrasenia;

/**
 * Muestra el panel de acerca de
 */
public class Acercade extends JAccessibilityFrameAbout {

	private static final long serialVersionUID = 1L;	

	@Override
	public int getMinimumRelation() {
		return 9;
	}
	
	/**
	 * Posicion X inicial de la ventana dependiendo de la resolucion de pantalla.
	 * @return int Posicion X
	 */
	public int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)){
			return (screenSize.width - 420) / 2;
		}
		return (screenSize.width - 380) / 2;
	}
	
	/**
	 * Posicion Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resolucion de pantalla.
	 * @return int Posicion Y
	 */
	public int getInitialY() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)){
			return (screenSize.height - 360) / 2;
		}
		return (screenSize.height - 320) / 2;
	}
	
	public Acercade() {
		initComponents();
	}

	/**
	 * Iniciamos componentes
	 */
	private void initComponents() {
		// Dimensiones de la pestana	
		
		if (GeneralConfig.isMaximized()){
    		this.setExtendedState(MAXIMIZED_BOTH);
			if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)){
    			setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX, Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX));
    		} else {
    			setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT));
    		}
    	} else {
    		this.setExtendedState(0);
    		if (PrincipalGUI.aboutActualPositionX != -1){
	    		setBounds(PrincipalGUI.aboutActualPositionX, PrincipalGUI.aboutActualPositionY, PrincipalGUI.aboutActualWidth, PrincipalGUI.aboutActualHeight);
	    		if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)){
	    			setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX, Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX));
	    		} else {
	    			setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT));
	    		}
    		} else {
    			if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)){
    				setBounds(this.getInitialX(), this.getInitialY(), Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX, Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX);
    				setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX, Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX));
        		} else {
        			setBounds(this.getInitialX(), this.getInitialY(), Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT);
        			setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT));
        		}
    		}
    	}
		
		// Icono de @firma
		setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage());
		
		// Configuracion de la ventana Acerca de
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setTitle(Messages.getString("ayuda.contenido")); // NOI18N
		setResizable(true);
		getContentPane().setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth = 3;
		c.weightx = 1.0;
		
		c.gridx = 0;
		c.gridy = 0;
		
		// Logotipo de la ventana
		JLabel logotipo = new JLabel();
		logotipo.setIcon(new ImageIcon(getClass().getResource("/resources/images/logo_cliente.png")));//$NON-NLS-1$
		logotipo.setHorizontalAlignment(SwingConstants.CENTER);
		getContentPane().add(logotipo, c);

		c.insets = new Insets(10, 20, 0, 20);
		c.gridy = 1;
		
		// Version del interfaz y Version de la aplicacion
		String version = Messages.getString("version.interfaz") + "  " + Main.VERSION + "<br>"
		+ Messages.getString("version") + "  " + getVersion();
		InfoLabel versionInterfaz = new InfoLabel(version, false);
		getContentPane().add(versionInterfaz, c);

		c.insets = new Insets(15, 20, 0, 20);
		c.gridy = 2;
		// Parrafo con el texto Cliente @firma... y con el texto El Ministerio de Politica...
		String desc = Messages.getString("acercade.descripcion2") + "<br><br>" + Messages.getString("acercade.descripcion"); 
		InfoLabel descLabel = new InfoLabel(desc, false);
		getContentPane().add(descLabel, c);
		
		c.fill = GridBagConstraints.NONE;
		c.insets = new Insets(20, 0, 20, 0);
		c.gridwidth = 1;
		c.gridy = 3;
		
		JPanel panelAceptar = new JPanel(new GridLayout(1, 1));
		// Boton aceptar
		JButton aceptar = new JButton();
		aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
		aceptar.setMnemonic(KeyEvent.VK_A); //Se asigna un atajo al boton aceptar
		aceptar.getAccessibleContext().setAccessibleName(aceptar.getText());
		aceptar.addActionListener(new java.awt.event.ActionListener() {
			@Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
				aceptarActionPerformed();
			}
		});
		Utils.setContrastColor(aceptar);
		Utils.setFontBold(aceptar);
		Utils.remarcar(aceptar);
		
		panelAceptar.add(aceptar);
		getContentPane().add(panelAceptar, c);
	}

	/**
	 * Escondemos la ventana
	 */
	void aceptarActionPerformed() {
		PrincipalGUI.aboutActualPositionX = this.getX();
		PrincipalGUI.aboutActualPositionY = this.getY();
		PrincipalGUI.aboutActualWidth = this.getWidth();
		PrincipalGUI.aboutActualHeight =  this.getHeight();
		dispose();
	}

	public String getVersion() {

		Properties p = new Properties();
		try {
			p.load(this.getClass().getResourceAsStream("/version.properties")); //$NON-NLS-1$
		} catch (Exception e) {
			Logger.getLogger(PanelContrasenia.class.getName()).warning(
					"No se han podido obtener los datos de version del cliente de firma"); //$NON-NLS-1$
		}
		StringBuilder version = new StringBuilder();
		version.append(p.getProperty("version.mayor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append(p.getProperty("version.minor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append(p.getProperty("version.build", "0")); //$NON-NLS-1$ //$NON-NLS-2$

		String desc = p.getProperty("build"); //$NON-NLS-1$
		if(desc != null && !desc.trim().equals("")) { //$NON-NLS-1$
			version.append(" ").append(desc); //$NON-NLS-1$
		}
		return version.toString();
	}
	
	public static void main() {
		EventQueue.invokeLater(new Runnable() {
			@Override
            public void run() {
				new Acercade().setVisible(true);
			}
		});
	}
}
