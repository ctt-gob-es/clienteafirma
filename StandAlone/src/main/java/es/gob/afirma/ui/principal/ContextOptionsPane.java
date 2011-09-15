package es.gob.afirma.ui.principal;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.Caret;

import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/**
 * Panel con la configuraci&oacute;n de contexto de las firmas de la interfaz.
 */
public class ContextOptionsPane {

	/** Clave para el algoritmo de firma por defecto. */
	public static final String KEY_SUBJECT = "context.subject";
	
	/** Clave para el algoritmo de firma por defecto. */
	public static final String KEY_PRODUCTION_PLACE = "context.productionPlace";
	
	/** Clave para el algoritmo de firma por defecto. */
	public static final String KEY_CONTACT_INFO = "context.contactInfo";
	
	
	/** Panel sobre el que se montan los componentes. */
	private final JPanel panel;
	
	 /** Motivo de la firma. */
	 private JTextField campoMotivo;
	 
	 /** Lugar en donde se realiza la firma. */
	 private JTextField campoLugar;
	 
	 /** Informaci&oacute;n de contacto para la firma. */
	 private JTextField campoDatos;
	
	/**
	 * Crea la vista y componentes de la pesta&ntilde;a principal de configuraci&oacute;n.
	 */
	public ContextOptionsPane() {
		
    	panel = new JPanel(new GridBagLayout());
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;
        
        // Panel firmas de documentos
        JPanel contextPanel = new JPanel(new GridBagLayout());
        contextPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.firmas"))); // NOI18N
        
        GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(4, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;
        
        // Etiqueta motivo / razon de la firma
        JLabel etiquetaMotivo = new JLabel();
        etiquetaMotivo.setText(Messages.getString("Opciones.firmas.motivo")); // NOI18N
        contextPanel.add(etiquetaMotivo, c2);
        
        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 1;
        
        // Caja de texto para el motivo de la firma
        campoMotivo = new JTextField();
        campoMotivo.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.motivo")); // NOI18N
        campoMotivo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.motivo")); // NOI18N
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			campoMotivo.setCaret(caret);
		}
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(campoMotivo);
        }
        contextPanel.add(campoMotivo, c2);
        
        //Relación entre etiqueta y campo de texto
        etiquetaMotivo.setLabelFor(campoMotivo);
  		//Asignación de mnemónico
        etiquetaMotivo.setDisplayedMnemonic(KeyEvent.VK_O);
        
        
        c2.insets = new Insets(13, 13, 0, 13);
        c2.gridy = 2;
        
        // Etiqueta lugar donde se realiza la firma
        JLabel etiquetaLugar = new JLabel();
        etiquetaLugar.setText(Messages.getString("Opciones.firmas.lugar")); // NOI18N
        contextPanel.add(etiquetaLugar, c2);
        
        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 3;
        
        // Caja de texto para el lugar donde se realiza la firma
        campoLugar = new JTextField();
        campoLugar.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.lugar")); // NOI18N
        campoLugar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.lugar")); // NOI18N
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			campoLugar.setCaret(caret);
		}
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(campoLugar);
        }
        contextPanel.add(campoLugar, c2);
        
        //Relación entre etiqueta y campo de texto
        etiquetaLugar.setLabelFor(campoLugar);
  		//Asignación de mnemónico
        etiquetaLugar.setDisplayedMnemonic(KeyEvent.VK_L);
        
        c2.insets = new Insets(13, 13, 0, 13);
        c2.gridy = 4;
        
        // Etiqueta de los datos de contacto
        JLabel etiquetaDatos = new JLabel();
        etiquetaDatos.setText(Messages.getString("Opciones.firmas.datos")); // NOI18N
        contextPanel.add(etiquetaDatos, c2);

        c2.insets = new Insets(5, 13, 5, 13);
        c2.gridy = 5;
        
        // Caja de texto para los datos de contacto
        campoDatos = new JTextField();
        campoDatos.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.datos")); // NOI18N
        campoDatos.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.datos")); // NOI18N
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			campoDatos.setCaret(caret);
		}
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(campoDatos);
        }
        contextPanel.add(campoDatos, c2);
        
        //Relación entre etiqueta y campo de texto
        etiquetaDatos.setLabelFor(campoDatos);
  		//Asignación de mnemónico
        etiquetaDatos.setDisplayedMnemonic(KeyEvent.VK_D);

        panel.add(contextPanel, c);
        
        // Rellenamos el hueco libre con un panel vacio
        c.gridy = 1;
        c.weighty = 1.0;
        panel.add(new JPanel(), c);
        
        
		// Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoMotivo, "opciones.pdf.motivo");
        HelpUtils.enableHelpKey(campoLugar, "opciones.pdf.lugar");
        HelpUtils.enableHelpKey(campoDatos, "opciones.pdf.datos");
    }
	
	public JPanel getConfigurationPanel() {
		return panel;
	}

	/**
	 * Carga en el panel la configuraci&oacute;n indicada en un properties.
	 * @param config Configuraci&oacute;n para cargar en el panel.
	 */
	public void loadConfig(Properties config) {
		campoMotivo.setText(config.getProperty(ContextOptionsPane.KEY_SUBJECT));
		campoLugar.setText(config.getProperty(ContextOptionsPane.KEY_PRODUCTION_PLACE));
		campoDatos.setText(config.getProperty(ContextOptionsPane.KEY_CONTACT_INFO));
	}
	
	/**
	 * Recupera el estado actual del panel.
	 * return Relaci&oacute;n con toda la configuraci&oacute;n del panel.
	 */
	public Properties getConfig() {
		Properties config = new Properties();
    	config.setProperty(ContextOptionsPane.KEY_SUBJECT, campoMotivo.getText());
    	config.setProperty(ContextOptionsPane.KEY_PRODUCTION_PLACE, campoLugar.getText());
    	config.setProperty(ContextOptionsPane.KEY_CONTACT_INFO, campoDatos.getText());
    	
    	return config;
	}
	
	/**
	 * Recupera la configuraci&oacute;n de firma establecida en este panel.
	 * @return Propiedades para la configuraci&oacute;n de la firma.
	 */
	public Properties getSignatureConfig() {
		Properties config = new Properties();
		if (campoMotivo.getText().trim().length() > 0) {
			config.setProperty("signReason", campoMotivo.getText().trim());
		}
		if (campoLugar.getText().trim().length() > 0) {
			config.setProperty("signatureProductionCity", campoLugar.getText().trim());
		}
		if (campoDatos.getText().trim().length() > 0) {
			config.setProperty("signerContact", campoDatos.getText().trim());
		}
		return config;
	}
}
