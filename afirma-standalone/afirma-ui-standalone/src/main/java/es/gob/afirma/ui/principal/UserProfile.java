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

/** Ventana de seleccion de perfiles.
 * @author inteco */
public final class UserProfile extends JAccessibilityFrameAdvisor {

    /** Perfil actual. */
    private static String currentProfileId;

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Combo perfiles. */
    private final JComboBox<String> comboPerfiles = new JComboBox<>();

    /** Constructor. */
    public UserProfile() {
        super();
        initComponents();
        HelpUtils.visualize("perfiles.usuario"); //$NON-NLS-1$
    }

    /** Obtiene el identificador del perfil actual de usuario.
     * @return Identificador del perfil actual de usuario. */
    public static String getCurrentProfileId() {
		return currentProfileId;
	}

    /** Establece el identificador de perfil actual.
     * @param currentProfileId Identificador de perfil actual. */
	public static void setCurrentProfileId(final String currentProfileId) {
		UserProfile.currentProfileId = currentProfileId;
	}

	/** Accion aceptar. */
    void aceptarPerformed() {

        final String profileName = this.comboPerfiles.getSelectedItem().toString();

        // Establecemos como ultimo perfil cargado: el por defecto si se selecciono el primer
        // elemento de la lista o el seleccionado si es cualquier otro
        ProfileManager.setLastProfileName(this.comboPerfiles.getSelectedIndex() == 0 ? null : profileName);

        if (ProfileManager.DEFAULT_PROFILE_NAME.equals(profileName)) {
            currentProfileId = null;
            GeneralConfig.loadConfig(ProfileManager.getDefaultConfigurationModified());
        }
        else {

            currentProfileId = ProfileManager.getProfileIdByName(profileName);

            //TODO: Revisar si esta linea sirve para algo
            ProfileManager.getConfiguration(currentProfileId);

            GeneralConfig.loadConfig(ProfileManager.getConfiguration(profileName));
        }

        new PrincipalGUI().main();
        dispose();
    }

    /** Configuracion de accesibilidad por defecto de la pantalla.
     * Marcado de elementos con foco.
     * @param component */
    private static void config(final JComponent component) {
        if (component instanceof JLabel) {
            final JLabel label = (JLabel) component;
            label.addFocusListener(new FocusListener() {
                /** Evento que se lanza cuando un componente tiene el foco. */
                @Override
                public void focusGained(final FocusEvent e) {
                    label.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                }

                /** Evento que se lanza cuando un componente pierde el foco. */
                @Override
                public void focusLost(final FocusEvent e) {
                    label.setBorder(BorderFactory.createEmptyBorder());
                }
            });

        }
        if (component instanceof JComboBox) {
            final JComboBox combo = (JComboBox) component;
            combo.addFocusListener(new FocusListener() {
                /** Evento que se lanza cuando un componente tiene el foco. */
                @Override
                public void focusGained(final FocusEvent e) {
                    combo.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                }

                /** Evento que se lanza cuando un componente pierde el foco. */
                @Override
                public void focusLost(final FocusEvent e) {
                    combo.setBorder(BorderFactory.createEmptyBorder());
                }
            });
        }
        if (component instanceof JButton) {
            final JButton button = (JButton) component;
            button.addFocusListener(new FocusListener() {
                /** Evento que se lanza cuando un componente tiene el foco. */
                @Override
                public void focusGained(final FocusEvent e) {
                    ((JPanel) button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                }

                /** Evento que se lanza cuando un componente pierde el foco. */
                @Override
                public void focusLost(final FocusEvent e) {
                    ((JPanel) button.getParent()).setBorder(BorderFactory.createEmptyBorder());
                }
            });
        }
    }

    /** Metodo que genera el panel de botones.
     * @return panel de botones. */
    private Component createButtonsPanel() {
        // Panel inferior
        final JPanel bottomPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.ipadx = 0;
        cons.gridx = 0;
        cons.insets = new Insets(5, 0, 5, 0);

        final JPanel panelAceptar = new JPanel(new GridLayout(1, 1));
        // Boton aceptar
        final JButton aceptar = new JButton();
        aceptar.setText("Aceptar");
        aceptar.setMnemonic(KeyEvent.VK_A);
        aceptar.addActionListener(new ActionListener() {
            /** Accion del boton aceptar. */
            @Override
            public void actionPerformed(final ActionEvent e) {
                aceptarPerformed();
            }
        });
        // Se asigna este boton como boton por defecto de la ventana.
        this.getRootPane().setDefaultButton(aceptar);
        panelAceptar.add(aceptar);
        config(aceptar);

        // Espacio entre botones
        final JPanel panelVacio = new JPanel();
        panelVacio.setPreferredSize(new Dimension(100, 10));

        // Panel en donde se insertan los botones maximizar y aceptar
        final JPanel buttonPanel = new JPanel();

        buttonPanel.add(panelAceptar, BorderLayout.CENTER);

        cons.ipadx = 0;
        cons.weightx = 1.0;
        cons.gridx = 1;

        bottomPanel.add(buttonPanel, cons);

        final JPanel panelAyuda = new JPanel(new GridLayout(1, 1));
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("perfiles.usuario"); //$NON-NLS-1$
        botonAyuda.setName("helpButton"); //$NON-NLS-1$
        config(botonAyuda);

        cons.ipadx = 15;
        cons.weightx = 0.02;
        cons.gridx = 2;
        panelAyuda.add(botonAyuda);
        bottomPanel.add(panelAyuda, cons);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(aceptar, "perfil.aceptar"); //$NON-NLS-1$

        return bottomPanel;
    }

    /** Posicion X inicial de la ventana dependiendo de la resolucion de pantalla.
     * @return int Posicion X */
    private static int getInitialX() {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); // 329
        return (screenSize.width - 500) / 2;
    }

    /** Posicion Y inicial de la ventana dependiendo del sistema operativo y de la
     * resolucion de pantalla.
     * @return int Posicion Y */
    private static int getInitialY() {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); // 329
        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            return (screenSize.height - 340) / 2;
        }
        return (screenSize.height - 320) / 2;
    }

    /** Devuelve la relaci&oacute;n m&iacute;nima para el redimensionado de elementos. */
    @Override
    public int getMinimumRelation() {
        // TODO Auto-generated method stub
        return 9;
    }

    /** Inicializacion de los componentes */
    private void initComponents() {

        if (getBackground().getRGB() == -16777216) {
            Main.setOSHighContrast(true);
        }

        // Dimensiones de la ventana
        setBounds(UserProfile.getInitialX(), UserProfile.getInitialY(), Constants.INIT_WINDOW_INITIAL_WIDTH, Constants.INIT_WINDOW_INITIAL_HEIGHT);

        // Parametros ventana
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); // NOI18N
        setTitle(Messages.getString("UserProfile.title")); // NOI18N //$NON-NLS-1$
        getContentPane().setLayout(new BorderLayout(11, 7));
        setMinimumSize(getSize());

        // Icono de @firma
        setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage()); //$NON-NLS-1$

        this.setLayout(new GridBagLayout());
        final GridBagConstraints c = new GridBagConstraints();

        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 0.1;
        c.insets = new Insets(5, 13, 5, 13);
        c.gridy = 0;

        // Logotipo de la aplicacion
        c.gridwidth = 3;
        final JLabel logotipo = new JLabel();
        logotipo.setIcon(new ImageIcon(getClass().getResource("/resources/images/logo_cliente.png")));//$NON-NLS-1$
        logotipo.setHorizontalAlignment(SwingConstants.CENTER);
        add(logotipo, c);
        c.gridy = c.gridy + 1;
        c.gridwidth = 1;

        // Cuerpo del mensaje
        final InfoLabel text =
            new InfoLabel(Constants.HTML_PARRAFO   + Messages.getString("UserProfile.welcome") + Constants.HTML_PARRAFO_CIERRE + Constants.HTML_PARRAFO + Messages.getString("UserProfile.body1") + Constants.HTML_PARRAFO_CIERRE + Constants.HTML_PARRAFO + Messages.getString("UserProfile.body2") + Constants.HTML_PARRAFO_CIERRE + Constants.HTML_PARRAFO + Messages.getString("UserProfile.body3") + Constants.HTML_PARRAFO_CIERRE, false);  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        config(text);

        add(text, c);
        c.gridy = c.gridy + 1;

        // Etiqueta de la lista de usuarios
        final JLabel label = new JLabel();
        label.setText(Messages.getString("UserProfile.list.label")); // NOI18N //$NON-NLS-1$
        label.setDisplayedMnemonic(KeyEvent.VK_P);
        label.setLabelFor(this.comboPerfiles);

        add(label, c);
        c.gridy = c.gridy + 1;
        c.weighty = 0.02;

        // Lista de usuarios
        final List<String> profileNames = new ArrayList<>();
        profileNames.add(ProfileManager.DEFAULT_PROFILE_NAME);
        for (final String profileName : ProfileManager.getProfilesNames()) {
            profileNames.add(profileName);
        }

        this.comboPerfiles.setModel(new DefaultComboBoxModel<>(profileNames.toArray(new String[0])));
        // Preselecionado el ultimo perfil cargado
        if (ProfileManager.getLastProfileName() != null) {
            this.comboPerfiles.setSelectedItem(ProfileManager.getLastProfileName());
        }

        config(this.comboPerfiles);

        add(this.comboPerfiles, c);

        c.gridy = c.gridy + 1;
        c.insets = new Insets(0, 0, 0, 0);
        add(createButtonsPanel(), c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.comboPerfiles, "perfil.cargar"); //$NON-NLS-1$

    }

    /** Muestra la ventana de la aplicaci&oacute;n */
    public void main() {
        EventQueue.invokeLater(new Runnable() {
            /** Se muestra. */
            @Override
            public void run() {
                setVisible(true);
            }
        });
    }

}
