/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardUtils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JWindow;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.Utils;

/**
 * Clase para generar la parte superior de los asistentes
 */
public class CabeceraAsistente extends JPanel {

	private static final long serialVersionUID = 1L;
	
	private String MessagesTitulo;
	private String MessagesDescripcion;
	private String MessagesDescripcion2 = "";
	private Dimension dimensiones = new Dimension(607, 175);
	private boolean bloqueTexto = false;
	/**
	 * Botonera.
	 */
	private BotoneraSuperior botoneraSuperior = null;
	
	/**
	 * Boton de restaurar.
	 */
	private JButton restoreButton = null;
	
	/**
	 * Boton de maximizar.
	 */
	JButton maximizeButton = null;

	/**
	 * Genera una cabecera para un asistente
	 * @param MessagesTitulo		Texto para obtener del ResourceMessages el titulo del asistente
	 * @param MessagesDescripcion	Texto para obtener del ResourceMessages la descripcion del asistente
	 */
	public CabeceraAsistente(String MessagesTitulo, String MessagesDescripcion) {
		this.MessagesTitulo = MessagesTitulo;
		this.MessagesDescripcion = MessagesDescripcion;
		
        initComponents();
    }
	
	/**
	 * Genera una cabecera para un asistente
	 * @param MessagesTitulo		Texto para obtener del ResourceMessages el titulo del asistente
	 * @param MessagesDescripcion	Texto para obtener del ResourceMessages la descripcion del asistente
	 * @param dimensiones		Dimensiones de la cabecera
	 */
	public CabeceraAsistente(String MessagesTitulo, String MessagesDescripcion, Dimension dimensiones) {
		this.MessagesTitulo = MessagesTitulo;
		this.MessagesDescripcion = MessagesDescripcion;
		this.dimensiones = dimensiones;
		
        initComponents();
    }
	
	/**
	 * Genera una cabecera para un asistente
	 * @param MessagesTitulo		Texto para obtener del ResourceMessages el titulo del asistente
	 * @param MessagesDescripcion	Texto para obtener del ResourceMessages la descripcion del asistente
	 * @param dimensiones		Dimensiones de la cabecera. Puede tomar el valor null y en tal caso se
	 * 							asignaran las dimensiones predeterminadas
	 * @param bloqueTexto		True: La descripcion tiene mas de una linea
	 * 							False: La descripcion tiene solo una linea
	 */
	public CabeceraAsistente(final String MessagesTitulo, final String MessagesDescripcion, final Dimension dimensiones, final boolean bloqueTexto) {
		this.MessagesTitulo = MessagesTitulo;
		this.MessagesDescripcion = MessagesDescripcion;
		if (dimensiones != null)
			this.dimensiones = dimensiones;
		
		this.bloqueTexto = bloqueTexto;
		
        initComponents();
    }
	/**
	 * Genera una cabecera para un asistente. Con un texto de cabecera de dos l&iacute;neas.
	 * @param MessagesTitulo		Texto para obtener del ResourceMessages el titulo del asistente
	 * @param MessagesDescripcion	Texto para obtener del ResourceMessages la descripcion del asistente
	 * @param MessagesDescripcion2	Segunda parte del texto para obtener del ResourceMessages la descripcion del asistente
	 * @param dimensiones		Dimensiones de la cabecera. Puede tomar el valor null y en tal caso se
	 * 							asignaran las dimensiones predeterminadas
	 * @param bloqueTexto		True: La descripcion tiene mas de una linea
	 * 							False: La descripcion tiene solo una linea
	 */
	public CabeceraAsistente(String MessagesTitulo, String MessagesDescripcion, String MessagesDescripcion2, Dimension dimensiones, 
			Boolean bloqueTexto) {
		this.MessagesTitulo = MessagesTitulo;
		this.MessagesDescripcion = MessagesDescripcion;
		this.MessagesDescripcion2 = MessagesDescripcion2;
		if (dimensiones != null)
			this.dimensiones = dimensiones;
		
		this.bloqueTexto = bloqueTexto;
		
        initComponents();
    }

    /**
     * Inicializa componentes
     */
    private void initComponents() {
    	// Configuracion de la ventana
    	setBackground(Color.WHITE);
    	// si el color de fondo ya no es blanco
        if (Main.isOSHighContrast){
        	setOpaque(false);
        }
    	setPreferredSize(this.dimensiones);
        setBorder(BorderFactory.createEtchedBorder());
        setLayout(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(0, 10, 0, 10);
		c.weightx = 1.0;
		c.gridx = 0;
		c.weighty = 1.0;
        
		createAccessibilityButtonsPanel();
		c.gridy = 1;
		
    	// Etiqueta con el titulo de la ventana
    	JLabel etiquetaTitulo = new JLabel();
    	etiquetaTitulo.setFont(new Font(getFont().getFamily(), 1, getFont().getSize()));
    	etiquetaTitulo.setText(Messages.getString(this.MessagesTitulo)); // NOI18N
    	etiquetaTitulo.setFocusable(true);
    	//Foco al contenido
    	etiquetaTitulo.addAncestorListener(new RequestFocusListener(false));
    	Utils.remarcar(etiquetaTitulo);
        Utils.setContrastColor(etiquetaTitulo);
    	add(etiquetaTitulo, c);
    	
    	c.gridy = 2;
    	c.insets = new Insets(0, 15, 0, 10);
    	c.weighty = 1.0;
    	c.weightx = 0;
 
    	// Etiqueta HTML con la descripcion de la ventana
    	InfoLabel etiquetaDescripcion;
    	if (this.MessagesDescripcion2.equals("")){
    		etiquetaDescripcion = new InfoLabel(Messages.getString(this.MessagesDescripcion), false);
    	} else {
    		String text = Messages.getString(this.MessagesDescripcion) + "<br>" + Messages.getString(this.MessagesDescripcion2);
    		etiquetaDescripcion = new InfoLabel(text, false);
    		this.MessagesDescripcion2 = "";
    	}
    	add(etiquetaDescripcion, c);
    }
    
    /**
	 * Asigna la botonera.
	 * @param botonera
	 */
	public void setBotoneraSuperior(BotoneraSuperior botonera) {
		this.botoneraSuperior=botonera;
	}
	
	/**
	 * Devuelve la botonera superior.
	 * @return botonera
	 */
	public BotoneraSuperior getBotoneraSuperior() {
		return this.botoneraSuperior;
	}
	/**
	 * Se crea el panel de botones de accesibilidad.
	 */
	private void createAccessibilityButtonsPanel() {
		//this.accessibilityButtonsPanel = new JPanel(new GridBagLayout());
		
		//Para el tooltip
		final JWindow tip = new JWindow();
		final JLabel tipText = new JLabel();
		
		//Panel que va a contener los botones de accesibilidad
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast){
        	panel.setOpaque(false);
        }
		Utils.setContrastColor(panel);
		
		//panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		//panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		//panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
		//panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		//panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		//panel.setBorder(BorderFactory.createCompoundBorder());
		//panel.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.BLACK));
		
		//Restricciones para los botones
		GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.NONE;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.weightx = 0;
		consButtons.weighty = 0;
		consButtons.insets = new Insets(0,0,0,0);  //right padding
		//consButtons.anchor=GridBagConstraints.EAST;
		
		//Restore button
		JPanel restorePanel = new JPanel();
		//this.restoreButton = getButton("r", KeyEvent.VK_R );
		ImageIcon imageIconRestore= new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png"));
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description"));
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());
		
		this.restoreButton.addFocusListener(new FocusListener() {
			
			@Override
			public void focusLost(FocusEvent e) {
				Utils.showToolTip(false, tip, CabeceraAsistente.this.restoreButton, tipText);
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				Utils.showToolTip(true, tip, CabeceraAsistente.this.restoreButton, tipText);
			}
		});
		Dimension dimension = new Dimension(20,20);
		this.restoreButton.setPreferredSize(dimension);
		
		//this.restoreButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		//this.restoreButton.setContentAreaFilled(false); //area del boton invisible
		this.restoreButton.setName("restaurar");
		Utils.remarcar(this.restoreButton);
		restorePanel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast){
        	restorePanel.setOpaque(false);
        }
		Utils.setContrastColor(restorePanel);
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
		
		
		panel.add(restorePanel, consButtons);
		
		
		consButtons.gridx = 1;
		//consButtons.weightx = 0.5;
		consButtons.insets = new Insets(0,0,0,0);  //right padding
		
		//Maximize button
		JPanel maximizePanel = new JPanel();

		ImageIcon imageIconMaximize= new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png"));
		this.maximizeButton = new JButton(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description"));
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

		//this.maximizeButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		//this.maximizeButton.setContentAreaFilled(false); //area del boton invisible
		
		this.maximizeButton.setName("maximizar");
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);
				
		Utils.remarcar(this.maximizeButton);
		//maximizePanel.add(this.maximizeButton, consMaximizePanel);
		maximizePanel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast){
        	maximizePanel.setOpaque(false);
        }
		Utils.setContrastColor(maximizePanel);
		maximizePanel.add(this.maximizeButton);
		
		this.maximizeButton.addFocusListener(new FocusListener() {
			
			@Override
			public void focusLost(FocusEvent e) {
				Utils.showToolTip(false, tip, CabeceraAsistente.this.maximizeButton, tipText);
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				Utils.showToolTip(true, tip, CabeceraAsistente.this.maximizeButton, tipText);
			}
		});
		
		this.maximizeButton.addActionListener(new ActionListener() {
		    	@Override
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
		c.weightx = 0;
		c.weighty = 0;
		//c.insets = new Insets(3,3,0,3);
		c.insets = new Insets(0,0,0,0); 
		c.anchor=GridBagConstraints.EAST;
		this.add(panel, c);
		
		
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
	 * Cambia el tamaño de la ventana al tamaño maximo de pantalla menos el tamaño de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);

		JAccessibilityDialogWizard.actualPositionX = j.getX();
		JAccessibilityDialogWizard.actualPositionY = j.getY();
		JAccessibilityDialogWizard.actualWidth = j.getWidth();
		JAccessibilityDialogWizard.actualHeight = j.getHeight();
		
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
				
		//Se hace el resize dependiendo del so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			j.setBounds(0,0, maxWidth, maxHeight);
		} else {
			j.setBounds(0,0, maxWidth, maxHeight - Constants.maximizeVerticalMarginLinux);
		}
				
		//Se deshabilita el botón de maximizar puesto que se ha pulsado.
		this.maximizeButton.setEnabled(false);
		this.restoreButton.setEnabled(true);
	}
	
	/**
	 * Restaura el tamaño de la ventana a la posicion anterior al maximizado
	 */
	public void restaurarActionPerformed(){
		JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
		if (JAccessibilityDialogWizard.actualPositionX != -1 && JAccessibilityDialogWizard.actualPositionY != -1 && JAccessibilityDialogWizard.actualWidth != -1 && JAccessibilityDialogWizard.actualHeight != -1){
			j.setBounds(JAccessibilityDialogWizard.actualPositionX, JAccessibilityDialogWizard.actualPositionY, JAccessibilityDialogWizard.actualWidth, JAccessibilityDialogWizard.actualHeight);
		} else {
			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			if (Platform.getOS().equals(Platform.OS.LINUX)){
	            j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
			} else{
	            j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
			}
    		j.setMinimumSize(new Dimension(j.getSize().width, j.getSize().height));
		}
		//Se deshabilita el botón de restaurar puesto que se ha pulsado.
		this.maximizeButton.setEnabled(true);
		this.restoreButton.setEnabled(false);
	}
}
