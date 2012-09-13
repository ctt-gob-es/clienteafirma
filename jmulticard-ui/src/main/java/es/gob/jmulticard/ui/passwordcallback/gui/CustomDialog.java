/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros
 * para la realizacion de procesos de autenticacion, firma electronica y validacion
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos
 * e Impulso de la Administracion Electronica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * European Union Public License publicada por la Comision Europea,
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTiA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.jmulticard.ui.passwordcallback.gui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.jmulticard.ui.passwordcallback.CancelledOperationException;
import es.gob.jmulticard.ui.passwordcallback.Messages;

/** Componente dialogo que define los alerts de la aplicaci&oacute;n.
 * @author inteco */
public final class CustomDialog extends JAccessibilityCustomDialog implements ActionListener {

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Etiqueta con la informacion de la alerta. */
    private InfoLabel infoLabel = null;

    /** Panel de botones. */
    private JPanel buttonsPanel = null;

    /** Panel de botones relacionados con la accesibilidad. */
    private JPanel accessibilityButtonsPanel = null;

    /** Panel principal. */
    private JPanel mainPanel = null;

    /** Campo de texto o campo de contrasena */
    private JSecurePasswordLabel component = null;

    JSecurePasswordLabel getComponent() {
        return this.component;
    }

    /** Etiqueta que contiene el icono de la alerta. */
    private final IconLabel iconLabel = new IconLabel();

    /** Boton de OK. */
    private static JButton okButton = null;

    static JButton getOkButton() {
        return okButton;
    }

    /** Boton de NO. */
    private JButton noButton = null;

    JButton getNoButton() {
    	return this.noButton;
    }

    /** Boton de Cancel. */
    private static JButton cancelButton = null;

    static JButton getCancelButton() {
        return cancelButton;
    }

    /** Boton de restaurar. */
    private JButton restoreButton = null;

    JButton getRestoreButton() {
        return this.restoreButton;
    }

    /** Boton de maximizar. */
    private JButton maximizeButton = null;

    JButton getMaximizeButton() {
        return this.maximizeButton;
    }

    /** Respuesta al mensaje */
    private int answer;

    /** Texto para el boton */
    private static String cancellText = Messages.getString("PrincipalGUI.cancelar"); //$NON-NLS-1$

    /** Constructor.
     * @param JDialog componente padre.
     * @param modal modal
     * @param message mensaje
     * @param title titulo
     * @param typeMessage tipo de mensaje
     * @param isInputDialog indica sies una alerta de tipo input */
    private CustomDialog(final JDialog componentParent,
                         final boolean modal,
                         final String message,
                         final String title,
                         final int typeMessage,
                         final boolean isInputDialog) {
        super(componentParent, modal, isInputDialog);
        initComponents(message, title, typeMessage, isInputDialog);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Constructor.
     * @param componentParent componente padre.
     * @param modal modal
     * @param message mensaje
     * @param title titulo
     * @param typeMessage tipo de mensaje
     * @param isInputDialog indica sies una alerta de tipo input */
    private CustomDialog(final Component componentParent,
                         final boolean modal,
                         final String message,
                         final String title,
                         final int typeMessage,
                         final boolean isInputDialog) {
        super(isInputDialog);
        this.setModal(modal);
        initComponents(message, title, typeMessage, isInputDialog);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Constructor.
     * @param JFrame componente padre.
     * @param modal modal
     * @param message mensaje
     * @param title titulo
     * @param typeMessage tipo de mensaje
     * @param isInputDialog indica sies una alerta de tipo input */
    private CustomDialog(final JFrame componentParent,
                         final boolean modal,
                         final String message,
                         final String title,
                         final int typeMessage,
                         final boolean isInputDialog) {
        super(componentParent, modal, isInputDialog);
        initComponents(message, title, typeMessage, isInputDialog);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
     * @return int Posici&oacute;n X */
    private static int getInitialX(final int width) {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        return ((screenSize.width / 2) - (width / 2));
    }

    /** Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
     * resoluci&oacute;n de pantalla.
     * @return int Posici&oacute;n Y */
    private static int getInitialY(final int height) {
        final Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        return ((screenSize.height / 2) - (height / 2));
    }

    /** Metodo que inicializa los componentes de la alerta.
     * @param message mensaje que se mostrara en la alerta
     * @param title T&iacute;tulo de la alerta
     * @param typeMessage tipo de mensaje
     * @param isInputDialog indica sies una alerta de tipo input */
    private void initComponents(
    		final String message,
    		final String title,
    		final int typeMessage,
    		final boolean isInputDialog) {
        // Se obtienen las dimensiones de maximizado
        final int maxWidth = this.getMaxWidth();
        final int maxHeight = this.getMaxHeight();

        // Se establece el tamano minimo
        setMinimumSize(new Dimension(this.getInitialWidth(), this.getInitialHeight()));
        setPreferredSize(new Dimension(this.getInitialWidth(), this.getInitialHeight()));
        // Se establece el tamano maximo
        setMaximumSize(new Dimension(maxWidth, maxHeight));

        // Dimensiones de la ventana en Windows y Linux
        if (GeneralConfig.isMaximized()) {
            // Se maximiza
            this.setBounds(0, 0, maxWidth, maxHeight);
        }
        else {
            // Se establece el tamano minimo en base a las opciones activas
            if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()) {
                setMinimumSize(new Dimension(Constants.CUSTOMDIALOG_FONT_INITIAL_WIDTH, Constants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT));
            }
        }

        this.setTitle(title);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        this.answer = JOptionPane.NO_OPTION;
        // Contenedor del dialogo
        final Container container = getContentPane();
        // Layout del contenedor
        container.setLayout(new GridBagLayout());

        // Panel con los datos del dialogo
        this.mainPanel = new JPanel(new GridBagLayout());

        // Restricciones para el panel de datos
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 0;
        c.insets = new Insets(5, 10, 0, 10);

        // Icono del dialogo
        setIconLabel();
        final JPanel iconPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints consIconPanel = new GridBagConstraints();
        consIconPanel.fill = GridBagConstraints.BOTH;

        iconPanel.add(this.iconLabel, consIconPanel);

        c.insets = new Insets(10, 5, 0, 10); // right padding
        c.gridx = 1;
        c.weightx = 1.0;
        c.weighty = 1.0;

        // Etiqueta del dialogo
        if (isInputDialog) {
            // Se crea una etiqueta sencilla
            this.infoLabel = new InfoLabel(message);
            this.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); // Se alinea a la izqda
            this.infoLabel.setVerticalAlignment(SwingConstants.CENTER); // Se alinea al centro el texto
        }
        else {
            // Se crea una etiqueta focusable
            this.infoLabel = new InfoLabel(message, false);
            this.infoLabel.setHorizontalAlignment(SwingConstants.CENTER); // Se centra el texto
            // Foco a la etiqueta
            this.infoLabel.addAncestorListener(new RequestFocusListener());
            this.infoLabel.setVerticalAlignment(SwingConstants.CENTER); // Se alinea al centro el texto
        }

        // Se anade la etiqueta al panel de informacion general
        this.mainPanel.add(this.infoLabel, c);

        // Panel de botones
        createMainButtonsPanel();
        createAccessibilityButtonsPanel();

        // Restricciones del contenedor general

        final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.BOTH;

        //Se anade el panel de botones relacionados con la accesibilidad
        cons.gridx = 0;
        cons.gridy = 0;
        cons.gridwidth = 2;
        cons.gridheight = 1;
        cons.weighty = 0.0;
        cons.weightx = 0.10;
        container.add(this.accessibilityButtonsPanel, cons);

        //se anade el panel de informacion
        cons.gridx = 2;
        cons.gridy = 0;
        cons.gridwidth = 6;
        cons.gridheight = (isInputDialog ? 2 : 3);
        cons.weighty = (isInputDialog ? 0.35 : 0.65);
        cons.weightx = 0.90;
        container.add(this.mainPanel, cons);

        //Se anade el icono
        cons.gridx = 0;
        cons.gridy = 1;
        cons.gridwidth = 2;
        cons.gridheight = 3;
        cons.weighty = 0.90;
        cons.weightx = 0.0;
        container.add(iconPanel, cons);

        //Se anade el panel de botones
        cons.gridx = 2;
        cons.gridy = 3;
        cons.gridwidth = 6;
        cons.gridheight = 1;
        cons.weighty = (isInputDialog ? 0.65 : 0.35);
        cons.weightx = 0.0;
        container.add(this.buttonsPanel, cons);

        pack();
    }

    /**
     * Se asigna el icono a la etiqueta.
     */
    private void setIconLabel() {
        // Segun el tipo de mensaje se selecciona el icono
        ImageIcon icon = new ImageIcon(CustomDialog.class.getResource("/images/dnie.png")); //$NON-NLS-1$
        final Dimension dimensionInicial = new Dimension((int) 100f, (int)(100f / icon.getIconWidth() * icon.getIconHeight()));
        this.iconLabel.setOriginalIcon(icon);
        this.iconLabel.setOriginalDimension(dimensionInicial);
        icon = new ImageIcon(icon.getImage().getScaledInstance(dimensionInicial.width, dimensionInicial.height, Image.SCALE_SMOOTH));
        this.iconLabel.setIcon(icon);
    }

    /** Se crea el panel de botones de accesibilidad. */
    private void createAccessibilityButtonsPanel() {
        this.accessibilityButtonsPanel = new JPanel(new GridBagLayout());

        // Para el tooltip
        final JWindow tip = new JWindow();
        final JLabel tipText = new JLabel();

        // Panel que va a contener los botones de accesibilidad
        final JPanel panel = new JPanel(new GridBagLayout());

        // Restricciones para los botones
        final GridBagConstraints consButtons = new GridBagConstraints();
        consButtons.fill = GridBagConstraints.BOTH;
        consButtons.gridx = 0;
        consButtons.gridy = 0;
        consButtons.weightx = 1.0;
        consButtons.weighty = 1.0;
        consButtons.insets = new Insets(0, 0, 0, 0); // right padding

        // Restore button
        final JPanel restorePanel = new JPanel();
        final ImageIcon imageIconRestore = new ImageIcon(CustomDialog.class.getResource("/images/restore.png")); //$NON-NLS-1$
        this.restoreButton = new JButton(imageIconRestore);
        this.restoreButton.setMnemonic(KeyEvent.VK_R);
        this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
        this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());

        this.restoreButton.addFocusListener(new FocusListener() {

            /** Evento que se produce cuando el componente pierde el foco. */
            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, CustomDialog.this.getRestoreButton(), tipText);
            }

            /** Evento que se produce cuando el componente tiene el foco. */
            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, CustomDialog.this.getRestoreButton(), tipText);
            }
        });
        final Dimension dimension = new Dimension(20, 20);
        this.restoreButton.setPreferredSize(dimension);
        this.restoreButton.addKeyListener(new KeyListener() {

			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

			@Override
			public void keyPressed(final KeyEvent ke) {
				if (10 == ke.getKeyCode()) {
					getRestoreButton().doClick();
				}
			}
		});
        this.restoreButton.setName("restaurar"); //$NON-NLS-1$
        restorePanel.add(this.restoreButton);
        this.restoreButton.addActionListener(new ActionListener() {
            /** Accion del boton. */
            @Override
            public void actionPerformed(final ActionEvent e) {
                restaurarActionPerformed();
            }
        });
        Utils.remarcar(this.restoreButton);

        panel.add(restorePanel, consButtons);

        consButtons.gridx = 1;
        consButtons.insets = new Insets(0, 0, 0, 0); // right padding

        // Maximize button
        final JPanel maximizePanel = new JPanel();

        final ImageIcon imageIconMaximize = new ImageIcon(CustomDialog.class.getResource("/images/maximize.png")); //$NON-NLS-1$
        this.maximizeButton = new JButton(imageIconMaximize);
        this.maximizeButton.setMnemonic(KeyEvent.VK_M);
        this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
        this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());
        this.maximizeButton.addKeyListener(new KeyListener() {

			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

			@Override
			public void keyPressed(final KeyEvent ke) {
				if (10 == ke.getKeyCode()) {
					getMaximizeButton().doClick();
				}
			}
		});

        this.maximizeButton.setName("maximizar"); //$NON-NLS-1$
        // Se asigna una dimension por defecto
        this.maximizeButton.setPreferredSize(dimension);

        Utils.remarcar(this.maximizeButton);
        maximizePanel.add(this.maximizeButton);

        this.maximizeButton.addFocusListener(new FocusListener() {
            /** Evento que se produce cuando el componente pierde el foco. */
            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, CustomDialog.this.getMaximizeButton(), tipText);
            }

            /** Evento que se produce cuando el componente tiene el foco. */
            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, CustomDialog.this.getMaximizeButton(), tipText);
            }
        });

        this.maximizeButton.addActionListener(new ActionListener() {
            /** Accion del boton. */
            @Override
            public void actionPerformed(final ActionEvent e) {
                maximizarActionPerformed();
            }
        });

        panel.add(maximizePanel, consButtons);

        // Se anade al panel general
        // Restricciones para el panel de botones
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.NONE;
        c.gridx = 0;
        c.gridy = 0;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.insets = new Insets(0, 0, 0, 0);
        c.anchor = GridBagConstraints.SOUTH;

        this.accessibilityButtonsPanel.add(panel, c);

        // Habilitado/Deshabilitado de botones restaurar/maximizar
        if (GeneralConfig.isMaximized()) {
            // Se deshabilita el boton de maximizado
            this.maximizeButton.setEnabled(false);
            // Se habilita el boton de restaurar
            this.restoreButton.setEnabled(true);
        }
        else {
            // Se habilita el boton de maximizado
            this.maximizeButton.setEnabled(true);
            // Se deshabilita el boton de restaurar
            this.restoreButton.setEnabled(false);
        }
    }

    /** Panel que contiene los botones principales de las alerta. */
    void createMainButtonsPanel() {
        this.buttonsPanel = new JPanel(new GridBagLayout());

        // Restricciones para el panel de botones
        final GridBagConstraints consButtons = new GridBagConstraints();
        consButtons.fill = GridBagConstraints.NONE;
        consButtons.gridx = 0;
        consButtons.gridy = 0;
        consButtons.insets = new Insets(0, 10, 0, 10); // right padding
        consButtons.anchor = GridBagConstraints.CENTER;

        // OK button
        final JPanel okPanel = new JPanel();
        CustomDialog.okButton = getButton(Messages.getString("PrincipalGUI.aceptar"), KeyEvent.VK_A); //$NON-NLS-1$
        okButton.addKeyListener(new KeyListener() {

			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

			@Override
			public void keyPressed(final KeyEvent ke) {
				if (10 == ke.getKeyCode()) {
					getOkButton().doClick();
				}
			}
		});

        okPanel.add(CustomDialog.okButton);
        this.buttonsPanel.add(okPanel, consButtons);

        CustomDialog.okButton.addActionListener(this);

    }

    /** Muestra un dialogo con un mensaje que pide la interacion del usuario.
     * @param componentParent componente padre
     * @param modal modal
     * @param message mensaje a mostrar
     * @param title titulo del dialogo
     * @param typeOption opciones de interacion
     * @param typeMessage tipo de mensaje
     * @return respuesta del usuario. */
    public static int showConfirmDialog(final Component componentParent,
                                        final boolean modal,
                                        final String message,
                                        final String title,
                                        final int typeOption,
                                        final int typeMessage) {

        final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(componentParent, modal, message, title, typeMessage, false);
        CustomDialog.okButton.setEnabled(true);
        customDialog.getRootPane().setDefaultButton(null);

        // Restricciones
        final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.gridy = 0;
        cons.insets = new Insets(0, 0, 0, 10); // right padding

        // Se comprueba el tipo de dialogo
        if (typeOption == JOptionPane.YES_NO_OPTION) {
            // Boton Si
            CustomDialog.okButton.setText(Messages.getString("CustomDialog.confirmDialog.yes")); //$NON-NLS-1$
            CustomDialog.okButton.setMnemonic(KeyEvent.VK_S);
            // Boton no
            customDialog.noButton = customDialog.getButton(Messages.getString("CustomDialog.confirmDialog.no"), KeyEvent.VK_N); //$NON-NLS-1$
            customDialog.noButton.addKeyListener(new KeyListener() {

    			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
    			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

    			@Override
    			public void keyPressed(final KeyEvent ke) {
    				if (10 == ke.getKeyCode()) {
    					customDialog.getNoButton().doClick();
    				}
    			}
    		});
            final JPanel noPanel = new JPanel();
            noPanel.add(customDialog.noButton);
            customDialog.buttonsPanel.add(noPanel, cons);
            customDialog.noButton.addActionListener(customDialog);
        }
        else {
            throw new UnsupportedOperationException("Solo se soportan dialogos de tipo Si/No"); //$NON-NLS-1$
        }

        customDialog.infoLabel.setHorizontalAlignment(SwingConstants.CENTER); // Se centra el texto
        customDialog.pack();
        customDialog.setSize(customDialog.getInitialWidth() + 1, customDialog.getInitialHeight() + 1); // Hacemos un resize para forzar un repintado
        customDialog.setVisible(true);

        return customDialog.getAnswer();
    }

    /** Muestra un di&aacute;logo de solicitud de contrase&ntilde;a.
     * @param componentParent Componente padre para la modalidad
     * @param modal modal
     * @param charSet filtro
     * @param beep
     * @param message mensaje a mostrar
     * @param mnemonic tajo
     * @param title titulo del dialogo
     * @param typeMessage tipo de mensaje
     * @return pass */
    public static char[] showInputPasswordDialog(final Component componentParent,
                                                 final boolean modal,
                                                 final boolean beep,
                                                 final String message,
                                                 final int mnemonic,
                                                 final String title,
                                                 final int typeMessage) {
    	final CustomDialog customDialog = CustomDialog.getInstanceCustomDialog(
    			componentParent,
    			modal,
    			message,
    			title,
    			typeMessage,
    			true
    	);
        CustomDialog.okButton.setEnabled(false);
        customDialog.getRootPane().setDefaultButton(null);

        // Restricciones para el panel de datos
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 1;
        c.gridy = 1;
        c.weightx = 0.0;
        c.weighty = 0.5;
        c.gridwidth = 2;
        c.insets = new Insets(2, 5, 2, 10); // right padding

        // campo de password del dialogo
        customDialog.component = new JSecurePasswordLabel(16);
        customDialog.component.addKeyListener(new KeyListener() {

            @Override
            public void keyTyped(final KeyEvent arg0) { /* Vacio */}

            @Override
            public void keyReleased(final KeyEvent ke) {
            	//final char[] pwd = customDialog.getComponent().getPassword();
            	//final int length = pwd.length;
            	// Machacamos la copia por seguridad
            	//for (int i=0;i<length;i++) {
            	//	pwd[i] = '\0';
            	//}
            	final int length = customDialog.getComponent().getPasswordLength();
                //Control de los botones aceptar/cancelar
                if (length > 7 && length < 17) {
                    getOkButton().setEnabled(true);
                	if (10 == ke.getKeyCode()) {
    					getOkButton().doClick();
    				}
                }
                else {
                    getOkButton().setEnabled(false);
                }
            }

            @Override
            public void keyPressed(final KeyEvent arg0) { /* Vacio */}
        });
        customDialog.component.addAncestorListener(new RequestFocusListener());
        Utils.remarcar(customDialog.component);
        Utils.setContrastColor(customDialog.component);
        Utils.setFontBold(customDialog.component);
        customDialog.component.getAccessibleContext()
                              .setAccessibleName(message.replaceAll(Constants.HTML_SALTO_LINEA, "") + "  ALT + " + mnemonic + ". "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        // Se anade el campo de texto al panel de informacion general
        customDialog.mainPanel.add(customDialog.component, c);

        // Etiqueta principal
        // Se relaciona la etiqueta con el componente
        customDialog.infoLabel.setLabelFor(customDialog.component);
        // Se asigna un atajo
        customDialog.infoLabel.setDisplayedMnemonic(mnemonic);
        // Se muestra el atajo
        final String text = Utils.remarkMnemonic(customDialog.infoLabel.getText(), mnemonic);
        customDialog.infoLabel.setText(text);

        // Restricciones para el check
        c.insets = new Insets(0, 0, 0, 10); // right padding
        c.gridy = 2;

        // Se anade el check al panel de informacion general
        //customDialog.mainPanel.add(panelCheckShowPass, c);

        // Restricciones del panel de botones
        final GridBagConstraints cons = new GridBagConstraints();
        cons.insets = new Insets(0, 0, 0, 10); // right padding

        // Cancel button
        cancelButton = customDialog.getButton(cancellText, KeyEvent.VK_C);
        final JPanel cancelPanel = new JPanel();
        cancelPanel.add(cancelButton);
        customDialog.buttonsPanel.add(cancelPanel, cons);
        cancelButton.addActionListener(customDialog);
        cancelButton.addKeyListener(new KeyListener() {

			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

			@Override
			public void keyPressed(final KeyEvent ke) {
				if (10 == ke.getKeyCode()) {
					getCancelButton().doClick();
				}
			}
		});

        customDialog.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); // Se centra el texto
        customDialog.component.setVisible(true); // Se hace visible el campo de texto

        cancelButton.addActionListener(customDialog);

        customDialog.pack();
        customDialog.setSize(customDialog.getInitialWidth() + 1, customDialog.getInitialHeight() + 1); // Hacemos un resize para forzar un repintado
        customDialog.setVisible(true);

        // Control para saber si se ha pulsado el boton cancelar
        if (customDialog.getAnswer() == JOptionPane.YES_OPTION) {
        	final char[] finalPin = customDialog.getComponent().getPassword();

        	// Por precaucion borramos el PIN y dejamos sus componentes relacionados
        	// listos para ser descartados
        	customDialog.getComponent().setText(""); //$NON-NLS-1$
        	customDialog.getComponent().setText(null);
        	//customDialog.getComponent().setDocument(new PlainDocument());
        	customDialog.component = null;
        	customDialog.dispose();
        	System.runFinalization();
        	System.gc();

            return finalPin;
        }
        throw new CancelledOperationException("La insercion de contrasena ha sido cancelada por el usuario"); //$NON-NLS-1$
    }

    /** Metodo que crea un boton.
     * Si el boton corresponde al de cancelar, se le asigna la tecla esc.
     * @param text texto del boton
     * @param mnemonic atajo
     * @return boton */
    private JButton getButton(final String text, final int mnemonic) {
        final JButton button = new JButton(text);
        button.setMnemonic(mnemonic);
        Utils.remarcar(button);
        Utils.setContrastColor(button);
        Utils.setFontBold(button);
        // Se comprueba si el boton es el de cancelar
        if (text.equalsIgnoreCase(cancellText)) {
            // Se asigna la tecla escape a dicho boton
            final String cancelKey = "cancel"; //$NON-NLS-1$
            this.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), cancelKey);
            this.getRootPane().getActionMap().put(cancelKey, new ButtonAbstractAction());
        }

        return button;
    }

    @Override
    public int getMinimumRelation() {
        return 7;
    }

    /** @return the answer */
    private int getAnswer() {
        return this.answer;
    }

    /** Cambia el tama&ntilde;o de la ventana al tama&ntilde;o m&aacute;ximo de pantalla menos el tama&ntilde;o de la barra de tareas de windows */
    void maximizarActionPerformed() {
        setActualPositionX(this.getX());
        setActualPositionY(this.getY());
        setActualWidth(this.getWidth());
        setActualHeight(this.getHeight());

        // Se obtienen las dimensiones de maximizado
        final int maxWidth = this.getMaxWidth();
        final int maxHeight = this.getMaxHeight();

        // Se hace el resize
        this.setBounds(getInitialX(maxWidth), getInitialY(maxHeight), maxWidth, maxHeight);

        // Habilitado/Deshabilitado de botones restaurar/maximizar
        this.maximizeButton.setEnabled(false);
        this.restoreButton.setEnabled(true);
    }

    /** Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado */
    void restaurarActionPerformed() {
        // Dimensiones de restaurado
        int minWidth = this.getInitialWidth();
        int minHeight = this.getInitialHeight();
        // Se comprueba las opciones de accesibilidad activas
        if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold() || isBigSizeDefault()) {
            minWidth = Constants.CUSTOMDIALOG_FONT_INITIAL_WIDTH;
            minHeight = Constants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT;
        }
        // Se establece el tamano minimo
        setMinimumSize(new Dimension(minWidth, minHeight));

        // Se situa el dialogo
        if (getActualPositionX() != -1 && getActualPositionY() != -1 && getActualWidth() != -1 && getActualHeight() != -1) {
            this.setBounds(getActualPositionX(), getActualPositionY(), getActualWidth(), getActualHeight());
        }
        else {
            setBounds(getInitialX(minWidth), getInitialY(minHeight), minWidth, minHeight);
        }

        // Habilitado/Deshabilitado de botones restaurar/maximizar
        this.maximizeButton.setEnabled(true);
        this.restoreButton.setEnabled(false);
    }

    /** Metodo que devuelve una instancia de CustomDialog.
     * @param componentParent Componente padre
     * @param modal modal
     * @param message mensaje
     * @param title titulo del dialogo
     * @param typeMessage tipo de alerta.
     * @param isInputDialog indica sies una alerta de tipo input
     * @return instancia de CustomDialog. */
    static CustomDialog getInstanceCustomDialog(final Component componentParent,
                                                final boolean modal,
                                                final String message,
                                                final String title,
                                                final int typeMessage,
                                                final boolean isInputDialog) {
        CustomDialog customDialog = null;
        // Se chequea cual sera el componente padre.
        if (componentParent instanceof JDialog) {
            customDialog = new CustomDialog((JDialog) componentParent, modal, message, title, typeMessage, isInputDialog);
        }
        else if (componentParent instanceof JFrame) {
            customDialog = new CustomDialog((JFrame) componentParent, modal, message, title, typeMessage, isInputDialog);
        }
        else {
            customDialog = new CustomDialog(componentParent, modal, message, title, typeMessage, isInputDialog);
        }
        return customDialog;
    }

    /** Accion correspondiente a los botones de las alertas. */
    @Override
    public void actionPerformed(final ActionEvent e) {

        if (e.getSource().equals(CustomDialog.okButton)) {
            this.answer = JOptionPane.YES_OPTION;
        }
        else if (e.getSource().equals(this.noButton)) {
            this.answer = JOptionPane.NO_OPTION;
        }
        else {
            this.answer = JOptionPane.CANCEL_OPTION;
        }
        setVisible(false);
    }

    private static final class ButtonAbstractAction extends AbstractAction {
        /** UID. */
        private static final long serialVersionUID = 1L;

        ButtonAbstractAction() {
            super();
        }

        /** Indica que la accion es la de pulsar el boton cancelar. */
        @Override
        public void actionPerformed(final ActionEvent event) {
            CustomDialog.getCancelButton().doClick();
        }
    }

}