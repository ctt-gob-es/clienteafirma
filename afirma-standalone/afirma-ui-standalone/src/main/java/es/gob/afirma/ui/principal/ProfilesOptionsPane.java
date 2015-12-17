package es.gob.afirma.ui.principal;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;

import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.ProfileManager;
import es.gob.afirma.ui.utils.Utils;

/** Manejador de la configuraci&oacute;n principal de la interfaz. */
final class ProfilesOptionsPane {

    /** Nombre del perfil actual. */
    private String currentProfileName = null;

    /** Etiqueta con el nombre del perfil cargado actualmente. */
    private JLabel currentProfileTitleLabel = null;

    private boolean isBigStyle = false;

    /** Panel sobre el que se montan los componentes. */
    private final JPanel panel;

    /** Componente padre sobre el que se mostrar&aacute;n los di&aacute;logos modales. */
    private final Opciones parent;

    /** Listado con los perfiles detectados por la aplicaci&oacute;n. */
    final JList<String> profileManagmentList;

    /** Crea la vista y componentes de la pesta&ntilde;a principal de configuraci&oacute;n.
     * @param parent Ventana de opciones sobre la que se muestra el panel. */
    ProfilesOptionsPane(final Opciones parent) {

        this.parent = parent;
        this.panel = new JPanel(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;

        // Perfil cargado
        this.currentProfileTitleLabel = new JLabel("Perfil actual: " + ProfileManager.getProfileName(ProfilesOptionsPane.getCurrentProfileId()));
        this.currentProfileTitleLabel.setFocusable(true);
        Utils.remarcar(this.currentProfileTitleLabel);
        Utils.setContrastColor(this.currentProfileTitleLabel);
        Utils.setFontBold(this.currentProfileTitleLabel);

        final JPanel currentProfilePanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
        currentProfilePanel.add(this.currentProfileTitleLabel);

        this.panel.add(currentProfilePanel, c);

        // Panel con el listado de perfiles disponibles
        final JPanel profilesPanel = new JPanel(new GridBagLayout());
        profilesPanel.setBorder(BorderFactory.createTitledBorder("Perfiles"));
        Utils.setContrastColor(profilesPanel);
        Utils.setFontBold(profilesPanel);

        final GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(0, 13, 0, 13);
        c2.weightx = 1.0;

        // Etiqueta del listado de perfiles cargados
        final JLabel profileManagmentLabel = new JLabel("Perfiles disponibles");
        Utils.setContrastColor(profileManagmentLabel);
        Utils.setFontBold(profileManagmentLabel);

        c2.gridy = 0;
        profilesPanel.add(profileManagmentLabel, c2);

        // Listado de perfiles cargados

        // Panel que contiene a la lista de destintatarios
        this.profileManagmentList = new JList<>(ProfileManager.getProfilesNames());
        final DefaultListModel<String> listModel = new DefaultListModel<>();
        listModel.addElement(ProfileManager.DEFAULT_PROFILE_NAME);
        for (final String name : ProfileManager.getProfilesNames()) {
            listModel.addElement(name);
        }
        this.profileManagmentList.setModel(listModel);
        this.profileManagmentList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        final JScrollPane profileManagmentScrollList = new JScrollPane();
        profileManagmentScrollList.setViewportView(this.profileManagmentList);
        Utils.remarcar(this.profileManagmentList);
        Utils.setFontBold(this.profileManagmentList);

        // Relacion entre etiqueta y lista de perfiles
        profileManagmentLabel.setLabelFor(this.profileManagmentList);
        // Asignacion de mnemonico
        profileManagmentLabel.setDisplayedMnemonic(KeyEvent.VK_F);

        c2.fill = GridBagConstraints.BOTH;
        c2.weighty = 1.0;
        c2.gridy = 1;
        profilesPanel.add(profileManagmentScrollList, c2);

        // Carga las opciones del perfil seleccionado
        final JButton loadProfileButton = new JButton("Cargar");
        loadProfileButton.setMnemonic(KeyEvent.VK_R);
        loadProfileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                if (ProfilesOptionsPane.this.profileManagmentList.getSelectedIndex() > -1) {
                    loadAction(ProfilesOptionsPane.this.profileManagmentList.getSelectedValue().toString());
                }
            }
        });
        // Accesibilidad del boton
        Utils.remarcar(loadProfileButton);
        Utils.setContrastColor(loadProfileButton);
        Utils.setFontBold(loadProfileButton);
        // Panel del boton cargar -- utilizado para remarcar foco
        final JPanel loadPanel = new JPanel(new GridLayout(1, 1));
        loadPanel.add(loadProfileButton);

        // Elimina un perfil del listado
        final JButton deleteProfileButton = new JButton("Eliminar");
        deleteProfileButton.setMnemonic(KeyEvent.VK_E);
        deleteProfileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                deleteAction();
            }
        });
        // Accesibilidad del boton
        Utils.remarcar(deleteProfileButton);
        Utils.setContrastColor(deleteProfileButton);
        Utils.setFontBold(deleteProfileButton);
        // Panel del boton eliminar -- utilizado para remarcar foco
        final JPanel deletePanel = new JPanel(new GridLayout(1, 1));
        deletePanel.add(deleteProfileButton);

        final JPanel profilesManagmentButtonsPanel = new JPanel(new FlowLayout(FlowLayout.TRAILING));
        profilesManagmentButtonsPanel.add(loadPanel);
        profilesManagmentButtonsPanel.add(deletePanel);

        c2.weighty = 0.0;
        c2.gridy = 2;
        profilesPanel.add(profilesManagmentButtonsPanel, c2);

        c.fill = GridBagConstraints.BOTH;
        c.weighty = 0.5;
        c.gridy = 1;

        this.panel.add(profilesPanel, c);

        // Botones de guardado
        final JButton saveProfileButton = new JButton("Guardar");
        saveProfileButton.setMnemonic(KeyEvent.VK_U);
        saveProfileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                saveAction();
            }
        });
        // Accesibilidad del boton
        Utils.remarcar(saveProfileButton);
        Utils.setContrastColor(saveProfileButton);
        Utils.setFontBold(saveProfileButton);
        // Panel del boton Guardar -- utilizado para remarcar foco
        final JPanel savePanel = new JPanel(new GridLayout(1, 1));
        savePanel.add(saveProfileButton);

        final JButton saveAsProfileButton = new JButton("Guardar como");
        saveAsProfileButton.setMnemonic(KeyEvent.VK_D);
        saveAsProfileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                saveAsAction();
            }
        });
        // Accesibilidad del boton
        Utils.remarcar(saveAsProfileButton);
        Utils.setContrastColor(saveAsProfileButton);
        Utils.setFontBold(saveAsProfileButton);
        // Panel del boton Guardar como -- utilizado para remarcar foco
        final JPanel saveAsPanel = new JPanel(new GridLayout(1, 1));
        saveAsPanel.add(saveAsProfileButton);

        final JPanel saveProfileButtonsPanel = new JPanel(new FlowLayout(FlowLayout.TRAILING));
        saveProfileButtonsPanel.add(savePanel);
        saveProfileButtonsPanel.add(saveAsPanel);

        c.weighty = 0.0;
        c.gridy = 2;

        this.panel.add(saveProfileButtonsPanel, c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.currentProfileTitleLabel, "Operfil.cargado"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.profileManagmentList, "Operfil.perfiles"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(loadProfileButton, "Operfil.cargar"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(deleteProfileButton, "Operfil.eliminar"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(saveProfileButton, "Operfil.guardar"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(saveAsProfileButton, "Operfil.modificar"); //$NON-NLS-1$
    }

    void deleteAction() {
        if (this.profileManagmentList.getSelectedIndex() > -1) {
            if (!this.profileManagmentList.getSelectedValue().equals(ProfileManager.DEFAULT_PROFILE_NAME)) {
                final int idx = this.profileManagmentList.getSelectedIndex();
                final String profileName = this.profileManagmentList.getSelectedValue().toString();

                if (UserProfile.getCurrentProfileId() != null && profileName.equals(ProfileManager.getProfileName(UserProfile.getCurrentProfileId()))) {
                    final int confirm =
                        CustomDialog.showConfirmDialog(this.parent,
                                                       true,
                                                       "Se dispone a eliminar el perfil actual. Si hace esto se cargar\u00E1 el perfil por defecto y se cerraran las opciones de configuraci\u00F3n. \u00BFDesea continuar?",
                                                       "Perfiles",
                                                       JOptionPane.YES_NO_OPTION,
                                                       JOptionPane.QUESTION_MESSAGE);

                    if (confirm == JOptionPane.YES_OPTION) {
                        ((DefaultListModel) this.profileManagmentList.getModel()).remove(idx);
                        this.currentProfileTitleLabel.setText(ProfileManager.DEFAULT_PROFILE_NAME);
                        UserProfile.setCurrentProfileId(null);
                        this.parent.aceptarActionPerformed(ProfileManager.getDefaultConfigurationModified(), this.getProfiles());
                    }
                }
                else {
                    final int confirm =
                        CustomDialog.showConfirmDialog(this.parent,
                                                       true,
                                                       "Se dispone a eliminar el perfil " + profileName
                                                       + ". Esta acci\u00F3n no se puede deshacer. \u00BFDesea continuar?",
                                                       "Perfiles",
                                                       JOptionPane.YES_NO_OPTION,
                                                       JOptionPane.QUESTION_MESSAGE);

                    if (confirm == JOptionPane.YES_OPTION) {
                        ((DefaultListModel) this.profileManagmentList.getModel()).remove(idx);
                    }
                }
            }
            else {
                CustomDialog.showMessageDialog(this.parent,
                                               true,
                                               Messages.getString("ProfileDeleteDefault.text"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$//$NON-NLS-2$
                this.profileManagmentList.requestFocusInWindow();
            }
        }
    }

    /** Recupera el panel con las opciones disponibles en esta pantalla.
     * @return Panel de opciones. */
    JPanel getConfigurationPanel() {
        return this.panel;
    }

    private Properties getCurrentConfig() {
        final Properties config = new Properties();
        config.putAll(this.parent.getConfiguration());

        return config;
    }

    static String getCurrentProfileId() {
        return UserProfile.getCurrentProfileId();
    }

    private String getCurrentProfileName() {
        if (this.currentProfileName == null) {
            this.currentProfileName = ProfileManager.getProfileName(getCurrentProfileId());
        }
        return this.currentProfileName;
    }

    /** Recupera los nombres de perfil que aparecen en el listado del panel.
     * @return Listado de nombres de perfil. */
    String[] getProfiles() {
        final List<String> profilesNames = new ArrayList<>();
        for (int i = 0; i < this.profileManagmentList.getModel().getSize(); i++) {
            profilesNames.add(this.profileManagmentList.getModel().getElementAt(i).toString());
        }

        return profilesNames.toArray(new String[0]);
    }

    void loadAction(final String profileName) {

        final int confirm =
            CustomDialog.showConfirmDialog(this.parent,
                                           true,
                                           "Al aplicar el nuevo perfil se cerraran las opciones de configuraci\u00F3n. \u00BFDesea continuar?",
                                           "Perfiles",
                                           JOptionPane.YES_NO_OPTION,
                                           JOptionPane.QUESTION_MESSAGE);

        if (confirm == JOptionPane.YES_OPTION) {
            // Comprobamos si esta activada al menos una de las opciones de accesibilidad sobre textos
            if (Boolean.parseBoolean(ProfileManager.getAccessibilityOptionValue(AccessibilityOptionsPane.MAIN_FONT_SIZE, getCurrentProfileName())) || Boolean.parseBoolean(ProfileManager.getAccessibilityOptionValue(AccessibilityOptionsPane.MAIN_FONT_STYLE,
                                                                                                                                                                                                                      getCurrentProfileName()))) {
                this.isBigStyle = true;
            }
            // Comprobamos si se van a desactivar las dos opciones de accesibilidad sobre texto
            if (this.isBigStyle && !Boolean.parseBoolean(ProfileManager.getAccessibilityOptionValue(AccessibilityOptionsPane.MAIN_FONT_SIZE,
                                                                                                     profileName)) && !Boolean.parseBoolean(ProfileManager.getAccessibilityOptionValue(AccessibilityOptionsPane.MAIN_FONT_STYLE,
                                                                                                                                                                                       profileName))) {
                AccessibilityOptionsPane.setContinueBigStyle(true);
            }
            if (ProfileManager.DEFAULT_PROFILE_NAME.equals(profileName)) {
                this.currentProfileTitleLabel.setText(profileName);
                UserProfile.setCurrentProfileId(null);
                this.parent.aceptarActionPerformed(ProfileManager.getDefaultConfigurationModified(), this.getProfiles());
            }
            else {
                this.currentProfileTitleLabel.setText(profileName);
                UserProfile.setCurrentProfileId(ProfileManager.getProfileIdByName(profileName));
                this.parent.aceptarActionPerformed(ProfileManager.getConfiguration(profileName), this.getProfiles());
            }
        }
    }

    /** Guarda y/o modifica en el preferences la configuracion del usuario. */
    void saveAction() {

        if (ProfilesOptionsPane.getCurrentProfileId() != null) {
            final int confirm =
                CustomDialog.showConfirmDialog(this.parent,
                                               true,
                                               "\u00BFDesea almacenar la configuraci\u00F3n actual en el perfil \"" + this.getCurrentProfileName()
                                               + "\"?",
                                               "Perfiles",
                                               JOptionPane.YES_NO_OPTION,
                                               JOptionPane.QUESTION_MESSAGE);

            if (confirm == JOptionPane.YES_OPTION) {
                try {
                    ProfileManager.saveConfiguration(ProfilesOptionsPane.getCurrentProfileId(), this.getCurrentProfileName(), this.getCurrentConfig());
                    CustomDialog.showMessageDialog(this.parent, true, "Perfil modificado correctamente.", "Perfiles", JOptionPane.INFORMATION_MESSAGE);
                }
                catch (final IllegalArgumentException e) {
                    showErrorDialog("Se ha insertado un nombre de fichero no v\u00E1lido. No se guardar\u00E1 el perfil.");
                    return;
                }
            }
        }
        else {
            this.saveAsAction();
        }
    }

    /** Guarda y/o modifica en el preferences la configuracion del usuario. */
    void saveAsAction() {

        final String profileName =
            CustomDialog.showInputDialog(this.parent,
                                         true,
                                         "Nombre del perfil que desea guardar.",
                                         KeyEvent.VK_N,
                                         "Perfiles",
                                         JOptionPane.QUESTION_MESSAGE);

        if (profileName == null) {
            return;
        }

        boolean newName = false;
        String profileId = null;
        if (ProfileManager.existProfileName(profileName)) {
            final int confirm =
                CustomDialog.showConfirmDialog(this.parent,
                                               true,
                                               "El perfil \"" + profileName + "\" ya existe. \u00BFDesea sobreescribirlo?",
                                               "Perfiles",
                                               JOptionPane.YES_NO_OPTION,
                                               JOptionPane.QUESTION_MESSAGE);

            if (confirm != JOptionPane.YES_OPTION) {
                return;
            }
            profileId = ProfileManager.getProfileIdByName(profileName);
        }
        else {
            newName = true;
        }

        if (profileId == null) {
            profileId = ProfileManager.getFreeProfileId();
        }

        try {
            ProfileManager.saveConfiguration(profileId, profileName, this.getCurrentConfig());
            CustomDialog.showMessageDialog(this.parent, true, "Perfil almacenado correctamente.", "Perfiles", JOptionPane.INFORMATION_MESSAGE);
        }
        catch (final IllegalArgumentException e) {
            showErrorDialog("Se ha insertado un nombre de fichero no v\u00E1lido. No se guardar\u00E1 el perfil.");
            return;
        }

        if (newName) {
            ((DefaultListModel) this.profileManagmentList.getModel()).addElement(profileName);
        }
    }

    private void showErrorDialog(final String message) {
        CustomDialog.showMessageDialog(this.parent, true, message, "Perfiles", JOptionPane.ERROR_MESSAGE);
    }
}
