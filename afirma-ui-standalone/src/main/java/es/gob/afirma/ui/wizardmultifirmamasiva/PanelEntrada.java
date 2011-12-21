/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.Caret;

import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 * Panel de entrada para el wizard de multifima masiva.
 * @author inteco
 *
 */
class PanelEntrada extends JAccessibilityDialogWizard {
	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Indica si el asistente se ha saltado la pagina anterior.
	 */
	private boolean salto = false;
	
	/**
	 * Relacion minima para el redimensionado de componentes.
	 */
	@Override
	public int getMinimumRelation(){
		return 9;
	}
	
	/**
	 * Registra un salto dentro del asistente de la pagina 2 a la 3.
	 * @param salto	Indica si se ha realizado un salto
	 */
	void setSalto(boolean salto) {
		this.salto = salto;
	}
	
	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	void setVentanas(List<JDialogWizard> ventanas) {
		this.setBotonera(new Botonera(ventanas, 3));
    	getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
	}
	/**
	 * Constructor.
	 */
    PanelEntrada() {
        initComponents();
    }
    
    /**
     * Caja de texto donde se guarda el directorio.
     */
    private JTextField campoDirectorio = new JTextField();
    /**
     * Caja de texto para escribir las extensiones.
     */
    private JTextField campoExtensiones = new JTextField();
    /**
     * Checkbox con el texto "Incluir subdirectorios...".
     */
    private JCheckBox checkIncluir = new JCheckBox();
    
    /**
     * Inicializacion de componentes.
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.multifirma.titulo.ventana")); //$NON-NLS-1$
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.multifirma.ventana3.titulo", "Wizard.multifirma.ventana3.titulo.descripcion", null, true); //$NON-NLS-1$ //$NON-NLS-2$
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);
		
        // Panel central
    	JPanel panelCentral = new JPanel();
    	panelCentral.setMinimumSize(new Dimension(603, 289));
    	panelCentral.setLayout(new GridBagLayout());
        
    	// Configuramos el layout
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 2;
		c.weightx = 1.0;
    	
    	// Etiqueta con el texto "Directorio con los..."
    	JLabel etiquetaFirma = new JLabel();
    	etiquetaFirma.setText(Messages.getString("Wizard.multifirma.ventana3.directorio")); //$NON-NLS-1$
    	Utils.setContrastColor(etiquetaFirma);
    	Utils.setFontBold(etiquetaFirma);
        panelCentral.add(etiquetaFirma, c);
    	
        c.insets = new Insets(0, 20, 0, 0);
        c.gridwidth = 1;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 1;
        
        // Caja de texto donde se guarda el directorio de entrada
        this.campoDirectorio.setToolTipText(Messages.getString("Wizard.multifirma.ventana3.directorio.description")); //$NON-NLS-1$
        this.campoDirectorio.getAccessibleContext().setAccessibleName(etiquetaFirma.getText() + " " + this.campoDirectorio.getToolTipText() + "ALT + D.");
        this.campoDirectorio.getAccessibleContext().setAccessibleDescription(this.campoDirectorio.getToolTipText());
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			this.campoDirectorio.setCaret(caret);
		}
        Utils.remarcar(this.campoDirectorio);
        Utils.setContrastColor(this.campoDirectorio);
        Utils.setFontBold(this.campoDirectorio);
        panelCentral.add(this.campoDirectorio, c);
        
        //Relación entre etiqueta y campo de texto
        etiquetaFirma.setLabelFor(this.campoDirectorio);
  		//Asignación de mnemónico
        etiquetaFirma.setDisplayedMnemonic(KeyEvent.VK_D);
        
        c.insets = new Insets(0, 10, 0, 20);
        c.gridwidth = 1;
		c.weightx = 0.0;
		c.gridx = 1;
        
		JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        examinar.getAccessibleContext().setAccessibleName(examinar.getText() + " " + examinar.getToolTipText());
        examinar.getAccessibleContext().setAccessibleDescription(examinar.getToolTipText());
        examinar.addActionListener(new ActionListener() {
        	/**
        	 * Accion del boton examinar.
        	 */
            @Override
            public void actionPerformed(ActionEvent evt) {
                examinarActionPerformed();
            }
        });
        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);
        panelExaminar.add(examinar);
        panelCentral.add(panelExaminar, c);
        
        c.insets = new Insets(5, 20, 0, 20);
        c.gridwidth = 2;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 2;
        
		JPanel panelCheckIncluir = new JPanel(new GridLayout(1, 1));
        // Checkbox con el texto "Incluir subdirectorios..."
        this.checkIncluir.setText(Messages.getString("Wizard.multifirma.ventana3.check.incluir")); //$NON-NLS-1$
        this.checkIncluir.setToolTipText(Messages.getString("Wizard.multifirma.ventana3.check.incluir.description")); //$NON-NLS-1$
        this.checkIncluir.getAccessibleContext().setAccessibleName(this.checkIncluir.getText() + " " + this.checkIncluir.getToolTipText());
        this.checkIncluir.getAccessibleContext().setAccessibleDescription(this.checkIncluir.getToolTipText());
        this.checkIncluir.setMnemonic(KeyEvent.VK_I); //Se asigna un atajo al checkbox
        Utils.remarcar(this.checkIncluir);
        Utils.setContrastColor(this.checkIncluir);
        Utils.setFontBold(this.checkIncluir);
        panelCheckIncluir.add(this.checkIncluir);
        panelCentral.add(panelCheckIncluir, c);
        
        c.insets = new Insets(20, 20, 0, 20);
		c.gridy = 3;
        
        // Etiqueta con el texto "Aplicar solo a los..."
        JLabel etiquetaAplicar = new JLabel();
        etiquetaAplicar.setText(Messages.getString("Wizard.multifirma.ventana3.aplicar")); //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAplicar);
        Utils.setFontBold(etiquetaAplicar);
        panelCentral.add(etiquetaAplicar, c);
        
        c.insets = new Insets(0, 20, 0, 20);
		c.gridy = 4;
        
        // Caja de texto para escribir las extensiones
        this.campoExtensiones.setToolTipText(Messages.getString("Wizard.multifirma.ventana3.aplicar.description"));
        this.campoExtensiones.getAccessibleContext().setAccessibleName(etiquetaAplicar.getText() + " " + this.campoExtensiones.getToolTipText() + "ALT + P.");
        this.campoExtensiones.getAccessibleContext().setAccessibleDescription(etiquetaAplicar.getToolTipText());
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			this.campoExtensiones.setCaret(caret);
		}
        Utils.remarcar(this.campoExtensiones);
        Utils.setContrastColor(this.campoExtensiones);
        Utils.setFontBold(this.campoExtensiones);
        panelCentral.add(this.campoExtensiones, c);
        
        //Relación entre etiqueta y campo de texto
        etiquetaAplicar.setLabelFor(this.campoExtensiones);
  		//Asignación de mnemónico
        etiquetaAplicar.setDisplayedMnemonic(KeyEvent.VK_P);
    
        c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(20, 20, 0, 20);
		c.gridwidth = 2;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridy = 5;
		
		// Panel introducido para poder mantener la linea superior correcta
		Panel panelVacio = new Panel();
		panelCentral.add(panelVacio, c);
        
        getContentPane().add(panelCentral, BorderLayout.CENTER);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoDirectorio,"multifirma.masiva.wizard.firma.directorios"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkIncluir,"multifirma.masiva.wizard.firma.incluir"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.campoExtensiones,"multifirma.masiva.wizard.firma.extension"); //$NON-NLS-1$
    }

    /**
     * Comprueba que el fichero seleccionado es valido y guarda su nombre en el campo de texto
     */
    void examinarActionPerformed() {
    	File selectedFile = SelectionDialog.showDirOpenDialog(this, Messages.getString("PrincipalGUI.chooser.dir.title"));    	 //$NON-NLS-1$
    	if (selectedFile != null) {
    		this.campoDirectorio.setText(selectedFile.getAbsolutePath());
    	}
    }
    
    /**
	 * Botonera con funciones para la pagina panel de multifirma - cofirma
	 */
	private class Botonera extends BotoneraInferior {
		/**
		 * UID.
		 */
		private static final long serialVersionUID = 1L;
		/**
		 * Constructor.
		 * @param ventanas Lista de ventanas que componen el wizard.
		 * @param posicion posicion de la ventana donde se inserta esta botonera.
		 */
		public Botonera(List<JDialogWizard> ventanas, int posicion) {
			super(ventanas, posicion);
		}
		/**
		 * Accion para el boton siguiente.
		 */
		@Override
		protected void siguienteActionPerformed(JButton anterior,
				JButton siguiente, JButton finalizar) {
			
			boolean continuar = true;
			continuar = verificarFicheros();
			
			if (continuar) {
				// Carga las extensiones
				((PanelMultifirmaMasiva) getVentanas().get(4)).setExtensiones(PanelEntrada.this.campoExtensiones.getText());
				
				// Carga el directorio de entrada
				((PanelMultifirmaMasiva) getVentanas().get(4)).setDirectorioEntrada(PanelEntrada.this.campoDirectorio.getText());
				
				// Indica si se deben incluir los subdirectorios
				((PanelMultifirmaMasiva) getVentanas().get(4)).setIncluir(PanelEntrada.this.checkIncluir.isSelected());
				
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			}
		}
		/**
		 * Accion para el boton anterior.
		 */
		@Override
		protected void anteriorActionPerformed(JButton anterior,
				JButton siguiente, JButton finalizar) {
			
			if (PanelEntrada.this.salto) {
				//Se asigna un botón por defecto al wizard
				if (getVentanas().get(1) instanceof JAccessibilityDialogWizard) {
					getVentanas().get(1).getRootPane().setDefaultButton(((JAccessibilityDialogWizard)getVentanas().get(1)).getBotonera().getSiguiente());
				}
			    getVentanas().get(1).setVisibleAndHide(true, getVentanas().get(3));
			}
			else
				super.anteriorActionPerformed(anterior, siguiente, finalizar);
		}
	}

	/**
	 * Verifica que los archivos del directorio seleccionado son correctos
	 * @return	True o false segun la verificacion
	 */
	boolean verificarFicheros() {
		//comprobacion de la ruta de fichero de entrada.
		String directorio = this.campoDirectorio.getText();
		if (directorio == null || directorio.equals("")) { //$NON-NLS-1$
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.error.directorio.origen"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		} 
		else if (!new File(directorio).exists() || !new File(directorio).isDirectory()) {
			CustomDialog.showMessageDialog(this, true, Messages.getString("Wizard.multifirma.error.directorio.origen2"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		}
		
		return true;
	}
}
