package es.gob.afirma.ui.principal;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.Caret;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/** Panel con la configuraci&oacute;n de contexto de las firmas de la interfaz. */
public final class ContextOptionsPane {

    /** Clave para el algoritmo de firma por defecto. */
    public static final String KEY_CONTACT_INFO = "context.contactInfo"; //$NON-NLS-1$

    /** Clave para el algoritmo de firma por defecto. */
    public static final String KEY_PRODUCTION_PLACE = "context.productionPlace"; //$NON-NLS-1$

    /** Clave para el algoritmo de firma por defecto. */
    public static final String KEY_SUBJECT = "context.subject"; //$NON-NLS-1$

    /** Clave para el formato b&aacute;sico de firma PAdES. */
    public static final String KEY_PADES_FORMAT = "context.pades.format"; //$NON-NLS-1$

    private static final String PADES_FORMAT_BASIC_TEXT = Messages.getString("Opciones.firmas.pades.basic"); //$NON-NLS-1$
	private static final String PADES_FORMAT_BES_TEXT = Messages.getString("Opciones.firmas.pades.bes"); //$NON-NLS-1$

    /** Informaci&oacute;n de contacto para la firma. */
    private final JTextField campoDatos;

    /** Lugar en donde se realiza la firma. */
    private final JTextField campoLugar;

    /** Motivo de la firma. */
    private final JTextField campoMotivo;

    /** Formato b&aacute;sico para las firmas PAdES. */
    private final JComboBox<ValueTextPair> campoFormato;

    /** Panel sobre el que se montan los componentes. */
    private final JPanel panel;

    /** Crea la vista y componentes de la pesta&ntilde;a principal de configuraci&oacute;n. */
    public ContextOptionsPane() {

        this.panel = new JPanel(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;

        // Panel firmas de documentos
        final JPanel contextPanel = new JPanel(new GridBagLayout());
        contextPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.firmas"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(contextPanel);
        Utils.setFontBold(contextPanel);

        final GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(4, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;

        // Etiqueta motivo / razon de la firma
        final JLabel etiquetaMotivo = new JLabel();
        etiquetaMotivo.setText(Messages.getString("Opciones.firmas.motivo")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaMotivo);
        Utils.setFontBold(etiquetaMotivo);
        contextPanel.add(etiquetaMotivo, c2);

        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 1;

        // Caja de texto para el motivo de la firma
        this.campoMotivo = new JTextField();
        this.campoMotivo.getAccessibleContext().setAccessibleName(etiquetaMotivo.getText() + " ALT + O."); // NOI18N //$NON-NLS-1$
        this.campoMotivo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.motivo")); // NOI18N //$NON-NLS-1$
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoMotivo.setCaret(caret);
        }
        Utils.remarcar(this.campoMotivo);
        Utils.setContrastColor(this.campoMotivo);
        Utils.setFontBold(this.campoMotivo);
        contextPanel.add(this.campoMotivo, c2);

        // Relacion entre etiqueta y campo de texto
        etiquetaMotivo.setLabelFor(this.campoMotivo);
        // Asignacion de mnemonico
        etiquetaMotivo.setDisplayedMnemonic(KeyEvent.VK_T);

        c2.insets = new Insets(13, 13, 0, 13);
        c2.gridy = 2;

        // Etiqueta lugar donde se realiza la firma
        final JLabel etiquetaLugar = new JLabel();
        etiquetaLugar.setText(Messages.getString("Opciones.firmas.lugar")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaLugar);
        Utils.setFontBold(etiquetaLugar);
        contextPanel.add(etiquetaLugar, c2);

        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 3;

        // Caja de texto para el lugar donde se realiza la firma
        this.campoLugar = new JTextField();
        this.campoLugar.getAccessibleContext().setAccessibleName(etiquetaLugar.getText() + " ALT + L."); // NOI18N //$NON-NLS-1$
        this.campoLugar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.lugar")); // NOI18N //$NON-NLS-1$
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoLugar.setCaret(caret);
        }
        Utils.remarcar(this.campoLugar);
        Utils.setContrastColor(this.campoLugar);
        Utils.setFontBold(this.campoLugar);
        contextPanel.add(this.campoLugar, c2);

        // Relacion entre etiqueta y campo de texto
        etiquetaLugar.setLabelFor(this.campoLugar);
        // Asignacion de mnemonico
        etiquetaLugar.setDisplayedMnemonic(KeyEvent.VK_L);

        c2.insets = new Insets(13, 13, 0, 13);
        c2.gridy = 4;

        // Etiqueta de los datos de contacto
        final JLabel etiquetaDatos = new JLabel();
        etiquetaDatos.setText(Messages.getString("Opciones.firmas.datos")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaDatos);
        Utils.setFontBold(etiquetaDatos);
        contextPanel.add(etiquetaDatos, c2);

        c2.insets = new Insets(5, 13, 5, 13);
        c2.gridy = 5;

        // Caja de texto para los datos de contacto
        this.campoDatos = new JTextField();
        this.campoDatos.getAccessibleContext().setAccessibleName(etiquetaDatos.getText() + " ALT + D."); // NOI18N //$NON-NLS-1$
        this.campoDatos.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.datos")); // NOI18N //$NON-NLS-1$
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.campoDatos.setCaret(caret);
        }
        Utils.remarcar(this.campoDatos);
        Utils.setContrastColor(this.campoDatos);
        Utils.setFontBold(this.campoDatos);
        contextPanel.add(this.campoDatos, c2);

        // Relacion entre etiqueta y campo de texto
        etiquetaDatos.setLabelFor(this.campoDatos);
        // Asignacion de mnemonico
        etiquetaDatos.setDisplayedMnemonic(KeyEvent.VK_D);








        c2.insets = new Insets(13, 13, 0, 13);
        c2.gridy++;

        // Etiqueta de los datos de contacto
        final JLabel etiquetaPadesFormat = new JLabel();
        etiquetaPadesFormat.setText(Messages.getString("Opciones.firmas.formatoPades")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaPadesFormat);
        Utils.setFontBold(etiquetaPadesFormat);
        contextPanel.add(etiquetaPadesFormat, c2);

        c2.insets = new Insets(5, 13, 5, 13);
        c2.gridy++;

        // Combo para la seleccion del formato de PAdES
        this.campoFormato = new JComboBox<>();
        this.campoFormato.getAccessibleContext().setAccessibleName(etiquetaDatos.getText() + " ALT + F."); // NOI18N //$NON-NLS-1$
        this.campoFormato.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.formatoPades")); // NOI18N //$NON-NLS-1$
        this.campoFormato.setModel(
    		new DefaultComboBoxModel<>(
				new ValueTextPair[] {
    				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BASIC, PADES_FORMAT_BASIC_TEXT),
    				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BES, PADES_FORMAT_BES_TEXT)
				}
			)
		);

        Utils.remarcar(this.campoFormato);
        Utils.setContrastColor(this.campoFormato);
        Utils.setFontBold(this.campoFormato);
        contextPanel.add(this.campoFormato, c2);

        // Relacion entre etiqueta y campo
        etiquetaPadesFormat.setLabelFor(this.campoFormato);
        // Asignacion de mnemonico
        etiquetaPadesFormat.setDisplayedMnemonic(KeyEvent.VK_F);











        this.panel.add(contextPanel, c);

        c.gridy++;

        // Botones
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 1, 1));

        // Definicion de botones
        final JButton valores = new JButton();

        final JPanel panelValores = new JPanel(new GridLayout(1, 1));
        // Boton Valores por defecto
        valores.setText(Messages.getString("Opciones.accesibilidad.valores")); //$NON-NLS-1$
        valores.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent e) {
                valoresActionPerformed();

            }
        });
        valores.setMnemonic(KeyEvent.VK_O);
        Utils.remarcar(valores);
        Utils.setContrastColor(valores);
        Utils.setFontBold(valores);
        panelValores.add(valores);
        buttonPanel.add(panelValores);

        this.panel.add(buttonPanel, c);
        // Rellenamos el hueco libre con un panel vacio
        c.gridy = c.gridy + 1;
        c.weighty = 1.0;
        this.panel.add(new JPanel(), c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.campoMotivo, "opciones.pdf.motivo"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.campoLugar, "opciones.pdf.lugar"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.campoDatos, "opciones.pdf.datos"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.campoFormato, "opciones.pdf.formato"); //$NON-NLS-1$
    }

    /** Recupera el estado actual del panel.
     * return Relaci&oacute;n con toda la configuraci&oacute;n del panel. */
    Properties getConfig() {
        final Properties config = new Properties();
        config.setProperty(ContextOptionsPane.KEY_SUBJECT, this.campoMotivo.getText());
        config.setProperty(ContextOptionsPane.KEY_PRODUCTION_PLACE, this.campoLugar.getText());
        config.setProperty(ContextOptionsPane.KEY_CONTACT_INFO, this.campoDatos.getText());
        config.setProperty(ContextOptionsPane.KEY_PADES_FORMAT, ((ValueTextPair) this.campoFormato.getSelectedItem()).getValue());

        return config;
    }

    JPanel getConfigurationPanel() {
        return this.panel;
    }

    /** Recupera la configuraci&oacute;n de firma establecida en este panel.
     * @return Propiedades para la configuraci&oacute;n de la firma. */
    public Properties getSignatureConfig() {
        final Properties config = new Properties();
        if (this.campoMotivo.getText().trim().length() > 0) {
            config.setProperty("signReason", this.campoMotivo.getText().trim()); //$NON-NLS-1$
        }
        if (this.campoLugar.getText().trim().length() > 0) {
            config.setProperty("signatureProductionCity", this.campoLugar.getText().trim()); //$NON-NLS-1$
        }
        if (this.campoDatos.getText().trim().length() > 0) {
            config.setProperty("signerContact", this.campoDatos.getText().trim()); //$NON-NLS-1$
        }
        config.setProperty("signatureSubFilter", ((ValueTextPair) this.campoFormato.getSelectedItem()).getValue()); //$NON-NLS-1$

        return config;
    }

    /** Carga en el panel la configuraci&oacute;n indicada en un properties.
     * @param config Configuraci&oacute;n para cargar en el panel. */
    public void loadConfig(final Properties config) {
        this.campoMotivo.setText(config.getProperty(ContextOptionsPane.KEY_SUBJECT));
        this.campoLugar.setText(config.getProperty(ContextOptionsPane.KEY_PRODUCTION_PLACE));
        this.campoDatos.setText(config.getProperty(ContextOptionsPane.KEY_CONTACT_INFO));

        final String selectedValue = config.getProperty(ContextOptionsPane.KEY_PADES_FORMAT);
        final ComboBoxModel<ValueTextPair> padesFormatModel = this.campoFormato.getModel();
		for (int i = 0; i < padesFormatModel.getSize(); i++) {
			if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
				this.campoFormato.setSelectedIndex(i);
				break;
			}
		}
    }

    /** Aplica el estado por defecto de los componentes de la ventana */
    private void restore(final JPanel panel1) {
        for (int i = 0; i < panel1.getComponentCount(); i++) {
            if (panel1.getComponent(i) instanceof JTextField) {
                ((JTextField) panel1.getComponent(i)).setText(""); //$NON-NLS-1$
            }
            else if (panel1.getComponent(i) instanceof JComboBox) {
                ((JComboBox) panel1.getComponent(i)).setSelectedIndex(0);
            }
            else if (panel1.getComponent(i) instanceof JPanel) {
                final JPanel interiorPanel = (JPanel) panel1.getComponent(i);
                restore(interiorPanel);
            }
        }
    }

    /** Aplica los valores por defecto. */
    void valoresActionPerformed() {
        Opciones.setUpdate(true);
        restore(this.panel);
    }

    /**
	 * Par de cadenas para su uso en ComboBox. Una cadena es el valor del elemento seleccionado y
	 * la otra el texto que se debe mostrar.
	 */
	private class ValueTextPair {

		private final String value;
		private final String text;

		ValueTextPair(final String value, final String text) {
			this.value = value;
			this.text = text;
		}

		String getValue() {
			return this.value;
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj instanceof ValueTextPair) {
				return this.value.equals(((ValueTextPair) obj).value);
			}
			return this.value.equals(obj.toString());
		}

		@Override
		public String toString() {
			return this.text;
		}

		@Override
		public int hashCode() {
			// Funciona aleatoria para calcular el hashcode
			return 5 * this.text.length() + 7 * this.value.length();
		}
	}
}
