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
import java.awt.Insets;
import java.awt.Toolkit;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextPane;
import javax.swing.WindowConstants;

import es.gob.afirma.misc.Platform;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;
import es.gob.afirma.ui.wizardcifradocontrasenia.PanelContrasenia;

/**
 * Muestra el panel de acerca de
 */
public class Acercade extends JFrame {

	private static final long serialVersionUID = 1L;	

	public Acercade() {
		initComponents();
	}

	/**
	 * Iniciamos componentes
	 */
	private void initComponents() {
		// Dimensiones de la pestana
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX))
			setBounds((screenSize.width - 420) / 2, (screenSize.height-360) / 2, 420, 360);
		else
			setBounds((screenSize.width - 380) / 2, (screenSize.height-320) / 2, 380, 320);			
		
		// Icono de @firma
		setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage());
		
		// Configuracion de la ventana Acerca de
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setTitle(Messages.getString("ayuda.contenido")); // NOI18N
		setResizable(false);
		getContentPane().setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.NONE;
		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth = 3;
		c.weightx = 1.0;
		c.gridx = 1;
		
		// Logotipo de la ventana
		JLabel logotipo = new JLabel();
		logotipo.setIcon(new ImageIcon(getClass().getResource("/resources/images/logo_cliente.png"))); // NOI18N
		getContentPane().add(logotipo, c);

		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(10, 20, 0, 20);
		c.gridwidth = 3;
		c.gridx = 0;
		
		// Version del interfaz
		JTextPane versionInterfaz = new JTextPane();
		versionInterfaz.setOpaque(false);
		versionInterfaz.setText(Messages.getString("version.interfaz") + "  " + Main.VERSION); // NOI18N
		versionInterfaz.setBorder(null);
		getContentPane().add(versionInterfaz, c);
		
		c.insets = new Insets(5, 20, 0, 20);
		
		// Version de la aplicacion
		JTextPane version = new JTextPane();
		version.setOpaque(false);
		version.setText(Messages.getString("version") + "  " + getVersion()); // NOI18N
		version.setBorder(null);
		getContentPane().add(version, c);

		c.insets = new Insets(15, 20, 0, 20);
		
		// Parrafo con el texto Cliente @firma...
		getContentPane().add(PanelesTexto.generarPanelTexto(
				"acercade.descripcion2", false), c);

		// Parrafo con el texto El Ministerio de Politica...
		getContentPane().add(PanelesTexto.generarPanelTexto(
				"acercade.descripcion", false), c);
		
		c.fill = GridBagConstraints.NONE;
		c.insets = new Insets(20, 0, 20, 0);
		c.gridwidth = 1;
		c.gridx = 1;
		
		// Boton aceptar
		JButton aceptar = new JButton();
		aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
		aceptar.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				aceptarActionPerformed();
			}
		});
		getContentPane().add(aceptar, c);
	}

	/**
	 * Escondemos la ventana
	 */
	private void aceptarActionPerformed() {
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
	
	/**
	 * @param args the command line arguments
	 */
	public static void main() {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				new Acercade().setVisible(true);
			}
		});
	}
}
