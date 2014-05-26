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
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/** Manejador de la configuraci&oacute;n de accesibilidad de la interfaz.
 * @author inteco */
public final class AccessibilityOptionsPane {

    /** Control de estilo grande. */
    private static boolean continueBigStyle = false;

    /** Clave para la configuraci&oacute;n de tama&ntilde;o del cursor de texto. */
    public static final String MAIN_CURSOR_SIZE = "main.cursorSize"; //$NON-NLS-1$

    /** Clave para la configuraci&oacute;n de visibilidad del foco. */
    public static final String MAIN_FOCUS_VISIBLE = "main.focusVisible"; //$NON-NLS-1$

    /** Clave para la configuraci&oacute;n de tama&ntilde;o de fuente. */
    public static final String MAIN_FONT_SIZE = "main.fontSize"; //$NON-NLS-1$

    /** Clave para la configuraci&oacute;n de estilo de fuente. */
    public static final String MAIN_FONT_STYLE = "main.fontStyle"; //$NON-NLS-1$

    /** Clave para la configuraci&oacute;n de vista en alto contraste. */
    public static final String MAIN_HIGHT_CONTRAST = "main.hightContrast"; //$NON-NLS-1$

    /** Clave para la configuraci&oacute;n de tama&ntilde;o de ventana. */
    static final String MAIN_WINDOWS_ACCESSIBILITY = "main.windowsAccessibility"; //$NON-NLS-1$

    /** Clave para la configuraci&oacute;n de tama&ntilde;o de ventana. */
    public static final String MAIN_WINDOWS_SIZE = "main.windowsSize"; //$NON-NLS-1$

    /** Boton aplicar cambios. */
    private final JButton aplicar = new JButton();

    /** Casilla de verificacion del tama&ntilde;o del cursor de texto. */
    private JCheckBox checkCursorSize;

    /** Casilla de verificacion de la visibilidad del foco. */
    private JCheckBox checkFocusVisible;

    /** Casilla de verificacion de tama&ntilde;o de fuente grande. */
    private JCheckBox checkFontSize;

    /** Casilla de verificacion de fuente en negrita. */
    private JCheckBox checkFontStyle;

    /** Casilla de verificacion de alto contraste. */
    private JCheckBox checkHighContrast;

    /** Casilla de verificacion del tama&ntilde;o de las ventanas. */
    private JCheckBox checkWindowSize;

    private boolean isBigStyle = false;

    private boolean isChangeHighContrast = false;
    boolean getChangeHighContrast() {
    	return this.isChangeHighContrast;
    }
    void setChangeHighContrast(final boolean hc) {
    	this.isChangeHighContrast = hc;
    }

    /** Pantalla principal de la aplicaci&oacute;n. */
    private final PrincipalGUI mainGui;

    /** Panel sobre el que se montan los componentes. */
    private final JPanel panel;

    /** Componente padre. */
    private JDialog parent = null;

    /** Obtiene el valor de la variable continueBigStyle.
     * @return Indica si esta activado el estilo de letra grande. */
    public static boolean isContinueBigStyle() {
		return continueBigStyle;
	}

    /** Modifica el valor de la variable continueBigStyle */
	static void setContinueBigStyle(final boolean continueBigStyle) {
		AccessibilityOptionsPane.continueBigStyle = continueBigStyle;
	}

	/** Obtiene la referencia al bot&oacute;n aplicar */
	JButton getAplicar() {
		return this.aplicar;
	}

	/** Constructor.
     * @param parent componente padre
     * @param mainGui principalGUI */
    public AccessibilityOptionsPane(final JDialog parent, final PrincipalGUI mainGui) {
        this.parent = parent;
        this.panel = new JPanel(new GridBagLayout());
        this.mainGui = mainGui;
        initComponents();
    }

    /** Aplica la configuraci&oacute;n de accesibilidad indicada en la pantalla */
    void aplicar() {
        // Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
        if (!GeneralConfig.isMaximized()) {
            PrincipalGUI.setOptionActualPositionX(this.parent.getX());
            PrincipalGUI.setOptionActualPositionY(this.parent.getY());
            PrincipalGUI.setOptionActualWidth(this.parent.getWidth());
            PrincipalGUI.setOptionActualHeight(this.parent.getHeight());
        }
        ((Opciones) this.parent).setAplicar(true);
        if (!this.isChangeHighContrast) {
            // no se ha modificado el estado del Alto Contraste

            // se guarda el estado actual de la configuracion de la herramienta
            final Properties config = new Properties();
            config.putAll(getConfig());
            config.putAll(((Opciones) this.parent).getMainOptions().getConfig());
            config.putAll(((Opciones) this.parent).getContextOptions().getConfig());
            GeneralConfig.loadConfig(config);

            // aplicamos los cambios a la pantalla principal
            this.mainGui.crearPaneles();
            this.mainGui.generarMenuHerramientas();
            this.mainGui.generarMenuAccesibilidad();
            this.mainGui.generarMenuAyuda();

            // aplicamos los cambios a la pantalla de opciones
            ((Opciones) this.parent).initComponents();
            ((Opciones) this.parent).callResize();
        }
        else {
            // Se ha modificado el estado del Alto Contraste por lo que es necesario ocultar y volver a mostrar la ventana de opciones para que cargue
            // el alto contraste
            ((Opciones) this.parent).getAceptar().doClick();
            this.mainGui.setAplicar(true);
            ((JMenuItem) this.mainGui.getMenu().getMenu(0).getMenuComponent(0)).doClick();
        }
    }

    /** Recupera el estado actual del panel.
     * return Relaci&oacute;n con toda la configuraci&oacute;n del panel. */
    Properties getConfig() {
        final Properties config = new Properties();
        config.setProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE, Boolean.toString(this.checkFontSize.isSelected()));
        config.setProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, Boolean.toString(this.checkFontStyle.isSelected()));
        config.setProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, Boolean.toString(this.checkHighContrast.isSelected()));
        config.setProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, Boolean.toString(this.checkFocusVisible.isSelected()));
        config.setProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, Boolean.toString(this.checkWindowSize.isSelected()));
        config.setProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, Boolean.toString(this.checkCursorSize.isSelected()));

        // Comprobamos si se han desactivados las dos opciones de accesibilidad sobre texto
        if (this.isBigStyle && !this.checkFontSize.isSelected() && !this.checkFontStyle.isSelected()) {
            continueBigStyle = true;
        }

        return config;
    }

    /** Devuelve el panel de configuraciones.
     * @return panel de configuraciones. */
    public JPanel getConfigurationPanel() {
        return this.panel;
    }

    /** Inicializacion de componentes. */
    private void initComponents() {
        this.panel.removeAll();
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.insets = new Insets(5, 13, 5, 13);
        c.gridy = 0;

        // Panel texto
        final JPanel textPanel = new JPanel(new GridBagLayout());
        textPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.texto"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(textPanel);
        Utils.setFontBold(textPanel);

        final GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(0, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;

        final JPanel panelFontSize = new JPanel(new GridLayout(1, 1));
        panelFontSize.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.texto")); //$NON-NLS-1$
        // Checkbox para habilitar la opcion de configuracion del tama&ntilde;o de fuente
        this.checkFontSize = new JCheckBox();
        this.checkFontSize.setText(Messages.getString("Opciones.accesibilidad.texto.tamano")); // NOI18N //$NON-NLS-1$
        this.checkFontSize.setSelected(GeneralConfig.isAvanzados());
        this.checkFontSize.setBounds(12, 20, 340, 23);
        this.checkFontSize.setMnemonic(KeyEvent.VK_D); // Asignacion de mnemonico al checkbox
        Utils.remarcar(this.checkFontSize);
        Utils.setContrastColor(this.checkFontSize);
        Utils.setFontBold(this.checkFontSize);

        panelFontSize.add(this.checkFontSize);
        textPanel.add(panelFontSize, c2);

        final JPanel panelFontStyle = new JPanel(new GridLayout(1, 1));
        panelFontStyle.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.texto")); //$NON-NLS-1$
        // Checkbox para habilitar la opcion de configuracion del estilo de fuente
        this.checkFontStyle = new JCheckBox();
        this.checkFontStyle.setText(Messages.getString("Opciones.accesibilidad.texto.estilo")); // NOI18N //$NON-NLS-1$
        this.checkFontStyle.setSelected(GeneralConfig.isAvanzados());
        this.checkFontStyle.setBounds(12, 20, 340, 23);
        this.checkFontStyle.setMnemonic(KeyEvent.VK_N); // Asignacion de mnemonico al checkbox
        Utils.remarcar(this.checkFontStyle);
        Utils.setContrastColor(this.checkFontStyle);
        Utils.setFontBold(this.checkFontStyle);

        panelFontStyle.add(this.checkFontStyle);
        textPanel.add(panelFontStyle, c2);

        this.panel.add(textPanel, c);
        c.gridy = c.gridy + 1;

        // Panel Color
        final JPanel colorPanel = new JPanel(new GridBagLayout());
        colorPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.color"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(colorPanel);
        Utils.setFontBold(colorPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;

        final JPanel panelHighContrast = new JPanel(new GridLayout(1, 1));
        panelHighContrast.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.color")); //$NON-NLS-1$
        // Checkbox para habilitar la opcion de configuracion de alto contraste
        this.checkHighContrast = new JCheckBox();
        this.checkHighContrast.setText(Messages.getString("Opciones.accesibilidad.color.contraste")); // NOI18N //$NON-NLS-1$
        this.checkHighContrast.setSelected(GeneralConfig.isAvanzados());
        this.checkHighContrast.setBounds(12, 20, 340, 23);
        this.checkHighContrast.setMnemonic(KeyEvent.VK_L); // Asignacion de mnemonico al checkbox
        this.checkHighContrast.addActionListener(new ActionListener() {
        	/** {@inheritDoc} */
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (AccessibilityOptionsPane.this.getChangeHighContrast()) {
                    AccessibilityOptionsPane.this.setChangeHighContrast(false);
                }
                else {
                    AccessibilityOptionsPane.this.setChangeHighContrast(true);
                }
            }
        });
        Utils.remarcar(this.checkHighContrast);
        Utils.setContrastColor(this.checkHighContrast);
        Utils.setFontBold(this.checkHighContrast);

        panelHighContrast.add(this.checkHighContrast);
        colorPanel.add(panelHighContrast, c2);

        this.panel.add(colorPanel, c);
        c.gridy = c.gridy + 1;

        // Panel Foco
        final JPanel focusPanel = new JPanel(new GridBagLayout());
        focusPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.foco"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(focusPanel);
        Utils.setFontBold(focusPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;

        final JPanel panelFocusVisible = new JPanel(new GridLayout(1, 1));
        panelFocusVisible.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.foco")); //$NON-NLS-1$
        // Checkbox para habilitar la opcion de configuracion de la visibilidad del foco
        this.checkFocusVisible = new JCheckBox();
        this.checkFocusVisible.setText(Messages.getString("Opciones.accesibilidad.foco.remarcar")); // NOI18N //$NON-NLS-1$
        this.checkFocusVisible.setSelected(GeneralConfig.isAvanzados());
        this.checkFocusVisible.setBounds(12, 20, 340, 23);
        this.checkFocusVisible.setMnemonic(KeyEvent.VK_F); // Asignacion de mnemonico al checkbox
        Utils.remarcar(this.checkFocusVisible);
        Utils.setContrastColor(this.checkFocusVisible);
        Utils.setFontBold(this.checkFocusVisible);

        panelFocusVisible.add(this.checkFocusVisible);
        focusPanel.add(panelFocusVisible, c2);

        this.panel.add(focusPanel, c);
        c.gridy = c.gridy + 1;

        // Panel Ventana
        final JPanel windowPanel = new JPanel(new GridBagLayout());
        windowPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.ventana"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(windowPanel);
        Utils.setFontBold(windowPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;

        final JPanel panelWindowSize = new JPanel(new GridLayout(1, 1));
        panelWindowSize.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.ventana")); //$NON-NLS-1$
        // Checkbox para habilitar la opcion de configuracion de ventanas maximizadas
        this.checkWindowSize = new JCheckBox();
        this.checkWindowSize.setText(Messages.getString("Opciones.accesibilidad.ventana.tamano")); // NOI18N //$NON-NLS-1$
        this.checkWindowSize.setSelected(GeneralConfig.isAvanzados());
        this.checkWindowSize.setBounds(12, 20, 340, 23);
        this.checkWindowSize.setMnemonic(KeyEvent.VK_V); // Asignacion de mnemonico al checkbox
        Utils.remarcar(this.checkWindowSize);
        Utils.setContrastColor(this.checkWindowSize);
        Utils.setFontBold(this.checkWindowSize);

        panelWindowSize.add(this.checkWindowSize);
        windowPanel.add(panelWindowSize, c2);

        this.panel.add(windowPanel, c);
        c.gridy = c.gridy + 1;

        // Panel Cursor
        final JPanel cursorPanel = new JPanel(new GridBagLayout());
        cursorPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.cursor"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(cursorPanel);
        Utils.setFontBold(cursorPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;

        final JPanel panelCursorSize = new JPanel(new GridLayout(1, 1));
        panelCursorSize.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.cursor")); //$NON-NLS-1$
        // Checkbox para habilitar la opcion de configuracion del tama&ntilde;o del cursor
        this.checkCursorSize = new JCheckBox();
        this.checkCursorSize.setText(Messages.getString("Opciones.accesibilidad.cursor.tamano")); // NOI18N //$NON-NLS-1$
        this.checkCursorSize.setSelected(GeneralConfig.isAvanzados());
        this.checkCursorSize.setBounds(12, 20, 340, 23);
        this.checkCursorSize.setMnemonic(KeyEvent.VK_E); // Asignacion de mnemonico al checkbox
        Utils.remarcar(this.checkCursorSize);
        Utils.setContrastColor(this.checkCursorSize);
        Utils.setFontBold(this.checkCursorSize);
        panelCursorSize.add(this.checkCursorSize);
        cursorPanel.add(panelCursorSize, c2);

        this.panel.add(cursorPanel, c);
        c.gridy = c.gridy + 1;

        // Botones
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 1, 1));

        // Definicion de botones
        final JButton valores = new JButton();
        // final JButton guardar = new JButton();

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

        final JPanel panelAplicar = new JPanel(new GridLayout(1, 1));
        // Boton aplicar
        this.aplicar.setText(Messages.getString("Opciones.accesibilidad.aplicar")); //$NON-NLS-1$
        this.aplicar.setMnemonic(KeyEvent.VK_I);
        this.aplicar.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent e) {
                aplicar();

            }
        });
        Utils.remarcar(this.aplicar);
        Utils.setContrastColor(this.aplicar);
        Utils.setFontBold(this.aplicar);

        panelAplicar.add(this.aplicar);
        buttonPanel.add(panelAplicar);

        this.panel.add(buttonPanel, c);
        // Rellenamos el hueco libre con un panel vacio
        c.gridy = c.gridy + 1;
        c.weighty = 1.0;
        this.panel.add(new JPanel(), c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.checkFontSize, "accesibilidad.texto"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkFontStyle, "accesibilidad.texto"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkHighContrast, "accesibilidad.color"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkFocusVisible, "accesibilidad.foco"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkWindowSize, "accesibilidad.ventana"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkCursorSize, "accesibilidad.cursor"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(valores, "accesibilidad.defecto"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.aplicar, "accesibilidad.aplicar"); //$NON-NLS-1$
    }

    /** Introduce en un properties la configuraci&oacute;n establecida en el panel.
     * @param config Configuraci&oacute;n para cargar en el panel. */
    public void loadConfig(final Properties config) {
        this.checkFontSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE, String.valueOf(false))));
        this.checkFontStyle.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, String.valueOf(false))));
        this.checkHighContrast.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, String.valueOf(false))));
        this.checkFocusVisible.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, String.valueOf(false))));
        this.checkWindowSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, String.valueOf(false))));
        this.checkCursorSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, String.valueOf(false))));

        // Comprobamos si esta activada al menos una de las opciones de accesibilidad sobre textos
        if (Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE)) || Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE))) {
            this.isBigStyle = true;
        }
    }

    /** Aplica el estado por defecto de los componentes de la ventana */
    private void restore(final JPanel p) {
        for (int i = 0; i < p.getComponentCount(); i++) {
            if (p.getComponent(i) instanceof JCheckBox) {
                if (((JCheckBox) p.getComponent(i)).getName() != null) {
                    ((JCheckBox) p.getComponent(i)).setSelected(true);
                }
                else {
                    ((JCheckBox) p.getComponent(i)).setSelected(false);
                }
            }
            else if (p.getComponent(i) instanceof JPanel) {
                final JPanel interiorPanel = (JPanel) p.getComponent(i);
                restore(interiorPanel);
            }
        }
    }

    /** Aplica los valores por defecto. */
    void valoresActionPerformed() {
        Opciones.setUpdate(true);
        restore(this.panel);
    }
}
