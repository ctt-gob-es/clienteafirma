/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardutils;

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

/** Clase para generar la parte superior de los asistentes */
public final class CabeceraAsistente extends JPanel {

    private static final long serialVersionUID = 1L;

    /** Botonera. */
    private BotoneraSuperior botoneraSuperior = null;
    private Dimension dimensiones = new Dimension(607, 175);
    /** Boton de maximizar. */
    private JButton maximizeButton = null;

    private final String messagesDescripcion;
    private String messagesDescripcion2 = ""; //$NON-NLS-1$

    private final String messagesTitulo;

    /** Boton de restaurar. */
    private JButton restoreButton = null;

    /** @return the maximizeButton */
    protected JButton getMaximizeButton() {
        return this.maximizeButton;
    }

    /** @return the restoreButton */
    protected JButton getRestoreButton() {
        return this.restoreButton;
    }

    /** Genera una cabecera para un asistente
     * @param messagesTitulo Texto para obtener del ResourceMessages el titulo del asistente
     * @param messagesDescripcion Texto para obtener del ResourceMessages la descripcion del asistente */
    public CabeceraAsistente(final String messagesTitulo, final String messagesDescripcion) {
        this.messagesTitulo = messagesTitulo;
        this.messagesDescripcion = messagesDescripcion;

        initComponents();
    }

    /** Genera una cabecera para un asistente
     * @param messagesTitulo Texto para obtener del ResourceMessages el titulo del asistente
     * @param messagesDescripcion Texto para obtener del ResourceMessages la descripcion del asistente
     * @param dimensiones Dimensiones de la cabecera */
    public CabeceraAsistente(final String messagesTitulo, final String messagesDescripcion, final Dimension dimensiones) {
        this.messagesTitulo = messagesTitulo;
        this.messagesDescripcion = messagesDescripcion;
        this.dimensiones = dimensiones;

        initComponents();
    }

    /** Genera una cabecera para un asistente. Con un texto de cabecera de dos l&iacute;neas.
     * @param messagesTitulo Texto para obtener del ResourceMessages el titulo del asistente
     * @param messagesDescripcion Texto para obtener del ResourceMessages la descripcion del asistente
     * @param messagesDescripcion2 Segunda parte del texto para obtener del ResourceMessages la descripcion del asistente
     * @param dimensiones Dimensiones de la cabecera. Puede tomar el valor null y en tal caso se
     *        asignaran las dimensiones predeterminadas */
    public CabeceraAsistente(final String messagesTitulo,
                             final String messagesDescripcion,
                             final String messagesDescripcion2,
                             final Dimension dimensiones) {
        this.messagesTitulo = messagesTitulo;
        this.messagesDescripcion = messagesDescripcion;
        this.messagesDescripcion2 = messagesDescripcion2;
        if (dimensiones != null) {
            this.dimensiones = dimensiones;
        }

        initComponents();
    }

    /** Se crea el panel de botones de accesibilidad. */
    private void createAccessibilityButtonsPanel() {

        // Para el tooltip
        final JWindow tip = new JWindow();
        final JLabel tipText = new JLabel();

        // Panel que va a contener los botones de accesibilidad
        final JPanel panel = new JPanel(new GridBagLayout());
        panel.setBackground(Color.WHITE);
        if (Main.isOSHighContrast()) {
            panel.setOpaque(false);
        }
        Utils.setContrastColor(panel);

        // Restricciones para los botones
        final GridBagConstraints consButtons = new GridBagConstraints();
        consButtons.fill = GridBagConstraints.NONE;
        consButtons.gridx = 0;
        consButtons.gridy = 0;
        consButtons.weightx = 0;
        consButtons.weighty = 0;
        consButtons.insets = new Insets(0, 0, 0, 0); // right padding

        // Restore button
        final JPanel restorePanel = new JPanel();
        final ImageIcon imageIconRestore = new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png")); //$NON-NLS-1$
        this.restoreButton = new JButton(imageIconRestore);
        this.restoreButton.setMnemonic(KeyEvent.VK_R);
        this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
        this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());

        this.restoreButton.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, getRestoreButton(), tipText);
            }

            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, getRestoreButton(), tipText);
            }
        });
        final Dimension dimension = new Dimension(20, 20);
        this.restoreButton.setPreferredSize(dimension);

        this.restoreButton.setName("restaurar"); //$NON-NLS-1$
        Utils.remarcar(this.restoreButton);
        restorePanel.setBackground(Color.WHITE);
        if (Main.isOSHighContrast()) {
            restorePanel.setOpaque(false);
        }
        Utils.setContrastColor(restorePanel);
        restorePanel.add(this.restoreButton);
        this.restoreButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                restaurarActionPerformed();
            }
        });

        panel.add(restorePanel, consButtons);

        consButtons.gridx = 1;
        consButtons.insets = new Insets(0, 0, 0, 0); // right padding

        // Maximize button
        final JPanel maximizePanel = new JPanel();

        final ImageIcon imageIconMaximize = new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png")); //$NON-NLS-1$
        this.maximizeButton = new JButton(imageIconMaximize);
        this.maximizeButton.setMnemonic(KeyEvent.VK_M);
        this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
        this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

        this.maximizeButton.setName("maximizar"); //$NON-NLS-1$
        // Se asigna una dimension por defecto
        this.maximizeButton.setPreferredSize(dimension);

        Utils.remarcar(this.maximizeButton);
        maximizePanel.setBackground(Color.WHITE);
        if (Main.isOSHighContrast()) {
            maximizePanel.setOpaque(false);
        }
        Utils.setContrastColor(maximizePanel);
        maximizePanel.add(this.maximizeButton);

        this.maximizeButton.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, getMaximizeButton(), tipText);
            }

            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, getMaximizeButton(), tipText);
            }
        });

        this.maximizeButton.addActionListener(new ActionListener() {
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
        c.weightx = 0;
        c.weighty = 0;
        c.insets = new Insets(0, 0, 0, 0);
        c.anchor = GridBagConstraints.EAST;
        this.add(panel, c);

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

    /** Devuelve la botonera superior.
     * @return botonera */
    public BotoneraSuperior getBotoneraSuperior() {
        return this.botoneraSuperior;
    }

    /** Inicializa componentes */
    private void initComponents() {
        // Configuracion de la ventana
        setBackground(Color.WHITE);
        // si el color de fondo ya no es blanco
        if (Main.isOSHighContrast()) {
            setOpaque(false);
        }
        setPreferredSize(this.dimensiones);
        setBorder(BorderFactory.createEtchedBorder());
        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(0, 10, 0, 10);
        c.weightx = 1.0;
        c.gridx = 0;
        c.weighty = 1.0;

        createAccessibilityButtonsPanel();
        c.gridy = 1;

        // Etiqueta con el titulo de la ventana
        final JLabel etiquetaTitulo = new JLabel();
        etiquetaTitulo.setFont(new Font(getFont().getFamily(), 1, getFont().getSize()));
        etiquetaTitulo.setText(Messages.getString(this.messagesTitulo)); // NOI18N
        etiquetaTitulo.setFocusable(true);
        // Foco al contenido
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
        if (this.messagesDescripcion2.equals("")) { //$NON-NLS-1$
            etiquetaDescripcion = new InfoLabel(Messages.getString(this.messagesDescripcion), false);
        }
        else {
            final String text =
                    Messages.getString(this.messagesDescripcion) + Constants.HTML_SALTO_LINEA + Messages.getString(this.messagesDescripcion2);
            etiquetaDescripcion = new InfoLabel(text, false);
            this.messagesDescripcion2 = ""; //$NON-NLS-1$
        }
        add(etiquetaDescripcion, c);
    }

    /** Cambia el tamano de la ventana al tamano maximo de pantalla menos el tamano de la barra de tareas de windows */
    public void maximizarActionPerformed() {
        final JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);

        JAccessibilityDialogWizard.setActualPositionX(j.getX());
        JAccessibilityDialogWizard.setActualPositionY(j.getY());
        JAccessibilityDialogWizard.setActualWidth(j.getWidth());
        JAccessibilityDialogWizard.setActualHeight(j.getHeight());

        // Se obtienen las dimensiones totales disponibles para mostrar una ventana
        final Rectangle rect = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

        // Se obtienen las dimensiones de maximizado
        final int maxWidth = (int) rect.getWidth();
        final int maxHeight = (int) rect.getHeight();

        // Se hace el resize dependiendo del so
        if (!Platform.getOS().equals(Platform.OS.LINUX)) {
            j.setBounds(0, 0, maxWidth, maxHeight);
        }
        else {
            j.setBounds(0, 0, maxWidth, maxHeight - Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX);
        }

        // Se deshabilita el boton de maximizar puesto que se ha pulsado.
        this.maximizeButton.setEnabled(false);
        this.restoreButton.setEnabled(true);
    }

    /** Restaura el tamano de la ventana a la posicion anterior al maximizado */
    public void restaurarActionPerformed() {
        final JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
        if (JAccessibilityDialogWizard.getActualPositionX() != -1 && JAccessibilityDialogWizard.getActualPositionY() != -1
            && JAccessibilityDialogWizard.getActualWidth() != -1
            && JAccessibilityDialogWizard.getActualHeight() != -1) {
            j.setBounds(JAccessibilityDialogWizard.getActualPositionX(),
                        JAccessibilityDialogWizard.getActualPositionY(),
                        JAccessibilityDialogWizard.getActualWidth(),
                        JAccessibilityDialogWizard.getActualHeight());
        }
        else {
            final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            if (Platform.getOS().equals(Platform.OS.LINUX)) {
                j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2,
                            (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2,
                            Constants.WIZARD_INITIAL_WIDTH_LINUX,
                            Constants.WIZARD_INITIAL_HEIGHT_LINUX);
            }
            else {
                j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2,
                            (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2,
                            Constants.WIZARD_INITIAL_WIDTH,
                            Constants.WIZARD_INITIAL_HEIGHT);
            }
            j.setMinimumSize(new Dimension(j.getSize().width, j.getSize().height));
        }
        // Se deshabilita el boton de restaurar puesto que se ha pulsado.
        this.maximizeButton.setEnabled(true);
        this.restoreButton.setEnabled(false);
    }

    /** Asigna la botonera.
     * @param botonera Botonera a asignar. */
    public void setBotoneraSuperior(final BotoneraSuperior botonera) {
        this.botoneraSuperior = botonera;
    }
}