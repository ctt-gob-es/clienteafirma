/*
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.security.Provider;
import java.security.Security;
import java.util.logging.Logger;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.LookAndFeel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalTheme;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.HighContrastTheme;
import es.gob.afirma.ui.utils.JAccessibilityFrame;
import es.gob.afirma.ui.utils.JStatusBar;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/** Ventana principal de la aplicacion. Desde aqui se invocan a todas los paneles
 * que contienen el resto de objetos: firma, validacion, cifrado, descifrado,
 * ensobrado y desensobrado. */
public final class PrincipalGUI extends JAccessibilityFrame {

    private static int aboutActualHeight = -1;

    private static int aboutActualPositionX = -1;

    private static int aboutActualPositionY = -1;

    private static int aboutActualWidth = -1;

    private static JStatusBar bar = new JStatusBar();

    private static int fileActualHeight = -1;

    private static int fileActualPositionX = -1;

    private static int fileActualPositionY = -1;

    private static int fileActualWidth = -1;

    /** Ruta del JAR en donde se almacenan los iconos de la aplicaci&oacute;n. */
    private static final String ICON_DIR_PATH = "/resources/images/"; //$NON-NLS-1$

    /** Ruta del JAR en donde se almacenan las im&aacute;agnees de la aplicaci&oacute;n. */
    private static final String IMAGE_DIR_PATH = "/resources/images/"; //$NON-NLS-1$

    private static int linuxMargin = 35;

    private static int optionActualHeight = -1;

    private static int optionActualPositionX = -1;

    private static int optionActualPositionY = -1;

    private static int optionActualWidth = -1;

    private static final long serialVersionUID = 1L;

    private static int wizardActualHeight = -1;

    private static int wizardActualPositionX = -1;

    private static int wizardActualPositionY = -1;

    private static int wizardActualWidth = -1;

    /** Escribe el nuevo estado en la barra de estado.
     * @param nuevoEstado Estado que hay que introducir en la barra de estado. */
    public static void setNuevoEstado(final String nuevoEstado) {
        getBar().setStatus(nuevoEstado);
    }

    private int actualHeight = -1;

    private int actualPositionX = -1;

    private int actualPositionY = -1;

    private int actualWidth = -1;

    private boolean aplicar = false;

    /** LookAndFeel por defecto. */
    private final LookAndFeel defaultLookAndFeel = UIManager.getLookAndFeel();

    /** Tema por defecto de tipo Metal. */
    private final MetalTheme defaultTheme = MetalLookAndFeel.getCurrentTheme();

    private HorizontalTabbedPanel htPanel;

    private double maximizedHight = 0;

    private double maximizedWidth = 0;

    private JMenuBar menu;

    PrincipalGUI() {
        super();
        initComponents();
        iniciarProveedores();
        this.addComponentListener(new ComponentAdapter() {
        	/** {@inheritDoc} */
            @Override
            public void componentMoved(final ComponentEvent e) {
                resized();
            }

            /** {@inheritDoc} */
            @Override
            public void componentResized(final ComponentEvent e) {
                resized();
            }
        });

    }

    /** Selecci&oacute;n men&uacute; acerca de: Muestra la ventana con la informaci&oacute;n de aFirma */
    static void acercaActionPerformed() {
        Acercade.main();
    }

    /** Selecci&oacute;n men&uacute; ayuda: Muestra la ventana con el panel de ayuda */
    static void ayudaHTMLActionPerformed() {
        HelpUtils.visualize(true);
    }

    /** Construye el panel principal de la aplicaci&oacute;n con las pesta&ntilde;as de
     * las distintas funcionalidades. */
    public void crearPaneles() {

        this.htPanel.reset();

        // Comprobacion del estado de Ventanas Maximizadas para que se genere
        // la ventana principal con el tamano adecuado
        if (this.getExtendedState() == MAXIMIZED_BOTH) {
            this.maximizedWidth = this.getSize().getWidth();
            this.maximizedHight = this.getSize().getHeight();
        }
        if (GeneralConfig.isMaximized()) {
            if (Platform.getOS().equals(Platform.OS.LINUX)) {
                // Se obtienen las dimensiones totales disponibles para mostrar una ventana
                final Rectangle rect = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

                // Se obtienen las dimensiones de maximizado
                final int maxWidth = (int) rect.getWidth();
                final int maxHeight = (int) rect.getHeight();
                if (!Platform.getOS().equals(Platform.OS.LINUX)) {
                    // Se hace el resize
                    this.setBounds(0, 0, maxWidth, maxHeight);
                }
                else {
                    // Se hace el resize
                    this.setBounds(0, 0, maxWidth, maxHeight - linuxMargin);
                }

            }
            else {
                this.setExtendedState(MAXIMIZED_BOTH);
            }
        }
        else {
            if (this.actualPositionX != -1 && this.actualPositionY != -1 && this.actualWidth != -1 && this.actualHeight != -1) {
                if (this.actualWidth == this.maximizedWidth && this.actualHeight == this.maximizedHight) {
                    this.setExtendedState(MAXIMIZED_BOTH);
                }
                else {
                    this.setExtendedState(0);
                    this.setBounds(this.actualPositionX, this.actualPositionY, this.actualWidth, this.actualHeight);
                }
            }
            else {
                this.setExtendedState(0);
                setBounds(PrincipalGUI.getInitialX(), PrincipalGUI.getInitialY(), Constants.WINDOW_INITIAL_WIDTH, Constants.WINDOW_INITIAL_HEIGHT);
            }
        }

        // Comprobacion del estado del Alto Contraste para que se creen los paneles
        // con la configuracion adecuada
        if (GeneralConfig.isHighContrast()) {
            setHighContrast(true);
        }
        else {
            setHighContrast(false);
        }
        Utils.setContrastColor(getBar());

        final Icon baseIcon = this.loadIcon("boton_transparente.png"); //$NON-NLS-1$

        // Panel de firma
        final ToggleImageButton buttonFirma = PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma")); //$NON-NLS-1$
        buttonFirma.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonFirma.setToggledIcon(this.loadIcon("boton_firma_ico.png"), baseIcon); //$NON-NLS-1$
        buttonFirma.setSelectedToggledIcon(this.loadIcon("boton_firma_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonFirma.setMnemonic(KeyEvent.VK_F);
        buttonFirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Firma.botonpricipal.status"))); //$NON-NLS-1$
        buttonFirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Firma.botonpricipal.status"))); //$NON-NLS-1$
        buttonFirma.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                                                             Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma.description")); //$NON-NLS-1$
        buttonFirma.setName("firma"); //$NON-NLS-1$
        final JPanel panelFirma = new Firma();
        this.htPanel.addTab(buttonFirma, panelFirma);

        // Panel de multifirma
        final ToggleImageButton buttonMultifirma = PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma")); //$NON-NLS-1$
        buttonMultifirma.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonMultifirma.setToggledIcon(this.loadIcon("boton_multifirma_ico.png"), baseIcon); //$NON-NLS-1$
        buttonMultifirma.setSelectedToggledIcon(this.loadIcon("boton_multifirma_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonMultifirma.setMnemonic(KeyEvent.VK_M);
        buttonMultifirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Multifirma.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Multifirma.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirma.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                                                                  Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma.description")); //$NON-NLS-1$
        buttonMultifirma.setName("multifirma"); //$NON-NLS-1$
        final JPanel panelMultifirmaSimple = new MultifirmaSimple();
        this.htPanel.addTab(buttonMultifirma, panelMultifirmaSimple);

        // Panel de multifirma masiva
        final ToggleImageButton buttonMultifirmaMasiva =
            PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva")); //$NON-NLS-1$
        buttonMultifirmaMasiva.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonMultifirmaMasiva.setToggledIcon(this.loadIcon("boton_masiva_ico.png"), baseIcon); //$NON-NLS-1$
        buttonMultifirmaMasiva.setSelectedToggledIcon(this.loadIcon("boton_masiva_sel_ico.png"), baseIcon); //$NON-NLS-1$
        buttonMultifirmaMasiva.setDisabledToggledIcon(this.loadIcon("boton_masiva_dis_ico.png"), baseIcon); //$NON-NLS-1$

        buttonMultifirmaMasiva.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                                    Messages.getString("Masiva.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirmaMasiva.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                                    Messages.getString("Masiva.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirmaMasiva.getAccessibleContext()
        .setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                           Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva.description")); //$NON-NLS-1$

        buttonMultifirmaMasiva.setEnabled(GeneralConfig.isAvanzados());
        buttonMultifirmaMasiva.setName("firma.masiva"); //$NON-NLS-1$
        if (buttonMultifirmaMasiva.isEnabled()) {
            buttonMultifirmaMasiva.setMnemonic(KeyEvent.VK_I);
        }

        final JPanel panelMultifirmaMasiva = new MultifirmaMasiva();
        this.htPanel.addTab(buttonMultifirmaMasiva, panelMultifirmaMasiva);

        // Panel de validacion y extraccion de documentos
        final ToggleImageButton buttonValidacion = PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion")); //$NON-NLS-1$
        buttonValidacion.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonValidacion.setToggledIcon(this.loadIcon("boton_validacion_ico.png"), baseIcon); //$NON-NLS-1$
        buttonValidacion.setSelectedToggledIcon(this.loadIcon("boton_validacion_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonValidacion.setMnemonic(KeyEvent.VK_V);
        buttonValidacion.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Validacion.botonpricipal.status"))); //$NON-NLS-1$
        buttonValidacion.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Validacion.botonpricipal.status"))); //$NON-NLS-1$
        buttonValidacion.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                                                                  Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion.description")); //$NON-NLS-1$
        buttonValidacion.setName("validacion"); //$NON-NLS-1$
        final JPanel panelValidacion = new Validacion();
        this.htPanel.addTab(buttonValidacion, panelValidacion);

        // Panel de cifrado simetrico
        final ToggleImageButton buttonCifrado = PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado")); //$NON-NLS-1$
        buttonCifrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonCifrado.setToggledIcon(this.loadIcon("boton_cifrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonCifrado.setSelectedToggledIcon(this.loadIcon("boton_cifrado_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonCifrado.setMnemonic(KeyEvent.VK_C);
        buttonCifrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Cifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonCifrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Cifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonCifrado.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado") + " " + //$NON-NLS-1$  //$NON-NLS-2$
                                                               Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado.description")); //$NON-NLS-1$
        buttonCifrado.setName("cifrado"); //$NON-NLS-1$
        final JPanel panelCifrado = new Cifrado();
        this.htPanel.addTab(buttonCifrado, panelCifrado);

        // Panel de Descifrado
        final ToggleImageButton buttonDescifrado = PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado")); //$NON-NLS-1$
        buttonDescifrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonDescifrado.setToggledIcon(this.loadIcon("boton_descifrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonDescifrado.setSelectedToggledIcon(this.loadIcon("boton_descifrado_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonDescifrado.setMnemonic(KeyEvent.VK_D);
        buttonDescifrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Descifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDescifrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Descifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDescifrado.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                                                                  Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado.description")); //$NON-NLS-1$
        buttonDescifrado.setName("descifrado"); //$NON-NLS-1$
        final JPanel panelDescifrado = new Descifrado();
        this.htPanel.addTab(buttonDescifrado, panelDescifrado);

        // Panel de Ensobrado
        final ToggleImageButton buttonEnsobrado = PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado")); //$NON-NLS-1$
        buttonEnsobrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonEnsobrado.setToggledIcon(this.loadIcon("boton_ensobrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonEnsobrado.setSelectedToggledIcon(this.loadIcon("boton_ensobrado_sel_ico.png"), baseIcon); //$NON-NLS-1$
        buttonEnsobrado.setDisabledToggledIcon(this.loadIcon("boton_ensobrado_dis_ico.png"), baseIcon); //$NON-NLS-1$

        buttonEnsobrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonEnsobrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonEnsobrado.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                                                                 Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado.description")); //$NON-NLS-1$

        buttonEnsobrado.setEnabled(GeneralConfig.isAvanzados());
        if (buttonEnsobrado.isEnabled()) {
            buttonEnsobrado.setMnemonic(KeyEvent.VK_B);
        }
        buttonEnsobrado.setName("ensobrado"); //$NON-NLS-1$
        final JPanel panelEnsobrado = new Ensobrado();
        this.htPanel.addTab(buttonEnsobrado, panelEnsobrado);

        // Panel de Desensobrado
        final ToggleImageButton buttonDesensobrado = PrincipalGUI.createToggleButton(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado")); //$NON-NLS-1$
        buttonDesensobrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonDesensobrado.setToggledIcon(this.loadIcon("boton_desensobrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonDesensobrado.setSelectedToggledIcon(this.loadIcon("boton_desensobrado_sel_ico.png"), baseIcon); //$NON-NLS-1$
        buttonDesensobrado.setDisabledToggledIcon(this.loadIcon("boton_desensobrado_dis_ico.png"), baseIcon); //$NON-NLS-1$

        buttonDesensobrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                                Messages.getString("Desensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDesensobrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                                Messages.getString("Desensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDesensobrado.getAccessibleContext()
        .setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                           Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado.description")); //$NON-NLS-1$

        buttonDesensobrado.setEnabled(GeneralConfig.isAvanzados());
        if (buttonDesensobrado.isEnabled()) {
            buttonDesensobrado.setMnemonic(KeyEvent.VK_N);
        }
        buttonDesensobrado.setName("desensobrado"); //$NON-NLS-1$
        final JPanel panelDesensobrado = new Desensobrado();
        this.htPanel.addTab(buttonDesensobrado, panelDesensobrado);

        HelpUtils.enableHelpKey(panelFirma, "firma"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelMultifirmaSimple, "multifirma"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelMultifirmaMasiva, "firma.masiva"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelValidacion, "validacion"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelCifrado, "cifrado"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelDescifrado, "descifrado"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelEnsobrado, "ensobrado"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(panelDesensobrado, "desensobrado"); //$NON-NLS-1$

        // Al repintar la pantalla principal para quitar o poner las opciones avanzadas hay que ajustar
        // la fuente para que se mantenga tal y como la tenia el usuario antes de cambiar esta opcion
        this.callResize();

    }

    /** Crea un bot&oacute;n que se queda pulsado tras hacer clic en &eacute;l. Al volver a
     * hacer clic sobre &eacute;l vuelve a su posici&oacute;n original.
     * @param text Texto del bot&oacute;n.
     * @return Bot&oacute;n creado. */
    private static ToggleImageButton createToggleButton(final String text) {

        final ToggleImageButton tButton = new ToggleImageButton();
        tButton.setHorizontalAlignment(SwingConstants.LEFT);
        tButton.setButtonText(text);

        Utils.remarcar(tButton);
        Utils.setContrastColor(tButton);
        Utils.setFontBold(tButton);
        return tButton;
    }

    JMenu generarMenuAccesibilidad() {
        // Opcion del menu principal - Accesibilidad
        final JMenu access = new JMenu();
        access.setMnemonic(KeyEvent.VK_L);
        access.setText(Messages.getString("PrincipalGUI.accesibilidad.text")); // NOI18N //$NON-NLS-1$
        access.setToolTipText(Messages.getString("PrincipalGUI.accesibilidad.text")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(access);
        Utils.setFontBold(access);
        Utils.remarcar(access);
        this.menu.add(access);

        // Subopcion menu Ayuda - Ayuda
        final JMenuItem accesibilidadItem = new JMenuItem();
        accesibilidadItem.setText(Messages.getString("PrincipalGUI.accesibilidad.contenido")); // NOI18N //$NON-NLS-1$
        accesibilidadItem.setMnemonic(KeyEvent.VK_U); // Se asigna un atajo al menu
        accesibilidadItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                HelpUtils.visualize("opciones.accesibilidad"); //$NON-NLS-1$
                final Opciones ventanaOpciones = new Opciones(PrincipalGUI.this, true, true);
                ventanaOpciones.setModal(true);
                ventanaOpciones.setVisible(true);
                setAplicar(false);
            }
        });
        Utils.setContrastColor(accesibilidadItem);
        Utils.setFontBold(accesibilidadItem);
        access.add(accesibilidadItem);

        this.callResize();
        return access;
    }

    /** Genera el menu de ayuda con las opciones Ayuda, Acerca de.
     * @return Menu de ayuda */
    public JMenu generarMenuAyuda() {
        // Opcion del menu principal - Ayuda
        final JMenu ayuda = new JMenu();
        ayuda.setMnemonic(KeyEvent.VK_Y);
        ayuda.setText(Messages.getString("PrincipalGUI.ayuda.text")); // NOI18N //$NON-NLS-1$
        ayuda.setToolTipText(Messages.getString("PrincipalGUI.ayuda.text")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(ayuda);
        Utils.setFontBold(ayuda);
        Utils.remarcar(ayuda);
        this.menu.add(ayuda);

        // Subopcion menu Ayuda - Ayuda
        final JMenuItem ayudaHTML = new JMenuItem();
        ayudaHTML.setText(Messages.getString("ayudaHTML.contenido")); // NOI18N //$NON-NLS-1$
        ayudaHTML.setMnemonic(KeyEvent.VK_U); // Se asigna un atajo al menu
        ayudaHTML.addActionListener(new ActionListener() {
        	/** {@inheritDoc} */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                ayudaHTMLActionPerformed();
            }
        });
        Utils.setContrastColor(ayudaHTML);
        Utils.setFontBold(ayudaHTML);
        ayuda.add(ayudaHTML);

        // Subopcion menu Ayuda - Acerca de
        final JMenuItem acerca = new JMenuItem();
        acerca.setText(Messages.getString("ayuda.contenido")); // NOI18N //$NON-NLS-1$
        acerca.setMnemonic(KeyEvent.VK_C); // Se asigna un atajo al menu
        acerca.addActionListener(new ActionListener() {
        	/** {@inheritDoc} */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                acercaActionPerformed();
            }
        });

        Utils.setContrastColor(acerca);
        Utils.setFontBold(acerca);
        ayuda.add(acerca);

        this.callResize();
        return ayuda;
    }

    /** Genera el menu Herramientas con los submenus Opciones, Idiomas, Salir.
     * @return Menu de herramientas */
    public JMenu generarMenuHerramientas() {
        this.menu.removeAll();
        // Opcion del menu principal - Herramientas
        final JMenu herramientas = new JMenu();
        herramientas.setMnemonic(KeyEvent.VK_S);
        herramientas.setText(Messages.getString("PrincipalGUI.herramientas.text")); //$NON-NLS-1$
        herramientas.setToolTipText(Messages.getString("PrincipalGUI.herramientas.text")); //$NON-NLS-1$
        Utils.setContrastColor(herramientas);
        Utils.setFontBold(herramientas);
        Utils.remarcar(herramientas);
        this.menu.add(herramientas);

        // Subopcion menu Herramientas - Opciones
        final JMenuItem opciones = new JMenuItem();
        opciones.setText(Messages.getString("Opciones.opciones")); //$NON-NLS-1$
        opciones.setMnemonic(KeyEvent.VK_O); // Se asigna un atajo al menu
        opciones.addActionListener(new ActionListener() {
        	/** {@inheritDoc} */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                opcionesActionPerformed();
            }
        });
        Utils.setContrastColor(opciones);
        Utils.setFontBold(opciones);
        herramientas.add(opciones);

        // Separador
        final JSeparator separador = new JSeparator();
        herramientas.add(separador);

        // Subopcion menu Herramientas - Salir
        final JMenuItem salir = new JMenuItem();
        salir.setText(Messages.getString("PrincipalGUI.salir")); // NOI18N //$NON-NLS-1$
        salir.setMnemonic(KeyEvent.VK_L); // Se asigna un atajo al menu
        salir.addActionListener(new ActionListener() {
        	/** {@inheritDoc} */
            @Override
            public void actionPerformed(final ActionEvent evt) {
                salirActionPerformed();
            }
        });
        Utils.setContrastColor(salir);
        Utils.setFontBold(salir);
        herramientas.add(salir);

        return herramientas;
    }

    /** Posicion X inicial de la ventana dependiendo de la resolucion de pantalla.
     * @return int Posicion X */
    public static int getInitialX() {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); // 329
        return (screenSize.width - 650) / 2;
    }

    /** Posicion Y inicial de la ventana dependiendo del sistema operativo y de la
     * resolucion de pantalla.
     * @return int Posicion Y */
    public static int getInitialY() {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); // 329
        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            return (screenSize.height - 340) / 2;
        }
        return (screenSize.height - 320) / 2;
    }

    JMenuBar getMenu() {
        return this.menu;
    }

    /** {@inheritDoc} */
    @Override
    public int getMinimumRelation() {
        return 7;
    }

    /** Inicia los proveedores */
    private static void iniciarProveedores() {
        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            Security.removeProvider("SunMSCAPI"); //$NON-NLS-1$
            try {
                Security.addProvider((Provider) Class.forName("sun.security.mscapi.SunMSCAPI").newInstance()); //$NON-NLS-1$
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No se ha podido anadir el proveedor SunMSCAPI");  //$NON-NLS-1$//$NON-NLS-2$
            }
        }
    }

    /** Inicializacion de los componentes */
    private void initComponents() {
        if (getBackground().getRGB() == -16777216) {
            Main.setOSHighContrast(true);
        }
        // Dimensiones de la ventana
        setBounds(PrincipalGUI.getInitialX(), PrincipalGUI.getInitialY(), Constants.WINDOW_INITIAL_WIDTH, Constants.WINDOW_INITIAL_HEIGHT);
        // Parametros ventana
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); // NOI18N
        setTitle("Cliente @firma");  //$NON-NLS-1$
        getContentPane().setLayout(new BorderLayout(11, 7));
        setMinimumSize(getSize());

        // Icono de @firma
        setIconImage(this.loadIcon("afirma_ico.png").getImage()); //$NON-NLS-1$

        // Componentes principales
        this.htPanel = new HorizontalTabbedPanel();
        getContentPane().add(this.htPanel, BorderLayout.CENTER);

        // Menu
        this.menu = new JMenuBar();
        // Menu de herramientas
        generarMenuHerramientas();
        // Menu accesibilidad
        generarMenuAccesibilidad();
        // Menu de ayuda
        generarMenuAyuda();

        setJMenuBar(this.menu);

        // Panel superior
        final JPanel arriba = new JPanel();
        arriba.setMinimumSize(new Dimension(1, 1));
        arriba.setPreferredSize(new Dimension(1, 1));
        getContentPane().add(arriba, BorderLayout.PAGE_START);

        // Panel derecho
        final JPanel derecha = new JPanel();
        derecha.setMinimumSize(new Dimension(1, 1));
        derecha.setPreferredSize(new Dimension(1, 1));
        getContentPane().add(derecha, BorderLayout.LINE_END);

        // Panel izquierdo
        final JPanel izquierda = new JPanel();
        izquierda.setMinimumSize(new Dimension(1, 1));
        izquierda.setPreferredSize(new Dimension(1, 1));
        getContentPane().add(izquierda, BorderLayout.LINE_START);

        // Barra de estado
        getBar().setLabelWidth((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth());
        getBar().setStatus(""); //$NON-NLS-1$
        getBar().setLeftMargin(3);

        getContentPane().add(getBar(), BorderLayout.SOUTH);

        crearPaneles();
    }

    /** Carga un icono contenido en el directorio de iconos del proyecto.
     * @param filename Nombre del icono.
     * @return Icono. */
    private ImageIcon loadIcon(final String filename) {
        return new ImageIcon(this.getClass().getResource(ICON_DIR_PATH + filename));
    }

    /** Carga una imagen contenido en el directorio de imagenes del proyecto.
     * @param filename Nombre del fichero de imagen.
     * @return Imagen. */
    private Image loadImage(final String filename) {
        return Toolkit.getDefaultToolkit().createImage(this.getClass().getResource(IMAGE_DIR_PATH + filename));
    }

    /** Muestra la ventana de la aplicacion */
    public void main() {
        EventQueue.invokeLater(new Runnable() {
        	/** {@inheritDoc} */
            @Override
            public void run() {
                setVisible(true);
            }
        });
    }

    /** Seleccion menu opciones: Muestra la ventana modal con las opciones */
    void opcionesActionPerformed() {
        HelpUtils.visualize("opciones.configuracion"); //$NON-NLS-1$
        final Opciones ventanaOpciones = new Opciones(PrincipalGUI.this, this.aplicar, false);
        ventanaOpciones.setModal(true);
        ventanaOpciones.setVisible(true);
        setAplicar(false);
    }

    /** Evento de redimensionado. Redimensiona el tamano de la barra de estado
     * y de su contenido, tambien almacena los valores actuales de posicion y tamaoo de
     * la ventana. */
    public void resized() {
        // Tamano de la ventana
        final Dimension screenSize = this.getSize();
        getBar().setPreferredSize(new Dimension((int) screenSize.getWidth() * 10 / 100, (int) screenSize.getHeight() * 5 / 100));
        getBar().setLabelSize((int) screenSize.getWidth(), (int) screenSize.getHeight() * 4 / 100);

        // Se guarda la posicion en el caso de que no se haya maximizado por configuracion
        if (!GeneralConfig.isMaximized()) {
            this.actualPositionX = this.getX();
            this.actualPositionY = this.getY();
            this.actualWidth = this.getWidth();
            this.actualHeight = this.getHeight();
        }
    }

    /** Selecci&oacute;n men&uacute; salir: Cierra la aplicaci&oacute;n */
    static void salirActionPerformed() {
        System.exit(0);
    }

    void setAplicar(final boolean aplicar) {
        this.aplicar = aplicar;
    }

    /** Activa y desactiva el visionado en alto contraste
     * @param highContrast Boolean que indica el estado del Alto Contraste */
    public void setHighContrast(final boolean highContrast) {
        // TODO Alto contraste en ventanas de Cargar / Guardar fichero
        try {
            if (highContrast) {
                // Tema de alto contraste
                final MetalTheme theme = new HighContrastTheme();
                // set the chosen theme
                MetalLookAndFeel.setCurrentTheme(theme);

                // set Metal look and feel
                UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel"); //$NON-NLS-1$

                if (!((Platform.getOS().equals(Platform.OS.LINUX) && (System.getProperty("java.version").compareTo("1.7.0") > 0) ))){ //$NON-NLS-1$ //$NON-NLS-2$
                	UIManager.put("FileChooserUI", "com.sun.java.swing.plaf.windows.WindowsFileChooserUI"); //$NON-NLS-1$//$NON-NLS-2$
                }

            }
            else {

                // Se comprueba si el lookAndFeel por defecto es el que se habia modificado para el modo
                // Alto contraste
                if (this.defaultLookAndFeel instanceof MetalLookAndFeel) {
                    MetalLookAndFeel.setCurrentTheme(this.defaultTheme); // Se asigna el tema por defecto
                }

                // Se asigna el lookAndFeel que habia por defecto
                UIManager.setLookAndFeel(this.defaultLookAndFeel);

                if (!((Platform.getOS().equals(Platform.OS.LINUX) && (System.getProperty("java.version").compareTo("1.7.0") > 0) ))){ //$NON-NLS-1$ //$NON-NLS-2$
                	UIManager.put("FileChooserUI", "com.sun.java.swing.plaf.windows.WindowsFileChooserUI"); //$NON-NLS-1$//$NON-NLS-2$
                }

            }

        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").severe("Error en el establecimiento del modo de alto contraste: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        SwingUtilities.updateComponentTreeUI(this);

        this.validate();
        this.repaint();
    }

	static int getAboutActualHeight() {
		return aboutActualHeight;
	}

	static void setAboutActualHeight(final int aboutActualHeight) {
		PrincipalGUI.aboutActualHeight = aboutActualHeight;
	}

	static int getAboutActualPositionX() {
		return aboutActualPositionX;
	}

	static void setAboutActualPositionX(final int aboutActualPositionX) {
		PrincipalGUI.aboutActualPositionX = aboutActualPositionX;
	}

	static int getAboutActualPositionY() {
		return aboutActualPositionY;
	}

	static void setAboutActualPositionY(final int aboutActualPositionY) {
		PrincipalGUI.aboutActualPositionY = aboutActualPositionY;
	}

	static int getAboutActualWidth() {
		return aboutActualWidth;
	}

	static void setAboutActualWidth(final int aboutActualWidth) {
		PrincipalGUI.aboutActualWidth = aboutActualWidth;
	}

	/** Obtiene el alto para los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @return Alto para los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static int getFileActualHeight() {
		return fileActualHeight;
	}

	/** Establece el alto para los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @param fileActualHeight Alto para los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static void setFileActualHeight(final int fileActualHeight) {
		PrincipalGUI.fileActualHeight = fileActualHeight;
	}

	/** Obtiene la coordenada horizontal de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @return Coordenada horizontal de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static int getFileActualPositionX() {
		return fileActualPositionX;
	}

	/** Establece la coordenada horizontal de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @param fileActualPositionX Coordenada horizontal de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static void setFileActualPositionX(final int fileActualPositionX) {
		PrincipalGUI.fileActualPositionX = fileActualPositionX;
	}

	/** Obtiene la coordenada vertical de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @return Coordenada vertical de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static int getFileActualPositionY() {
		return fileActualPositionY;
	}

	/** Establece la coordenada vertical de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @param fileActualPositionY Coordenada vertical de la posici&oacute;n de los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static void setFileActualPositionY(final int fileActualPositionY) {
		PrincipalGUI.fileActualPositionY = fileActualPositionY;
	}

	/** Obtiene el ancho para los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @return Ancho para los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static int getFileActualWidth() {
		return fileActualWidth;
	}

	/** Establece el ancho para los di&aacute;logos de ficheros de la aplicaci&oacute;n.
	 * @param fileActualWidth Ancho para los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static void setFileActualWidth(final int fileActualWidth) {
		PrincipalGUI.fileActualWidth = fileActualWidth;
	}

	/** Obtiene la barra de estado
	 * @return Barra de estado */
	public static JStatusBar getBar() {
		return bar;
	}

	static int getOptionActualHeight() {
		return optionActualHeight;
	}

	static void setOptionActualHeight(final int optionActualHeight) {
		PrincipalGUI.optionActualHeight = optionActualHeight;
	}

	static int getOptionActualPositionX() {
		return optionActualPositionX;
	}

	static void setOptionActualPositionX(final int optionActualPositionX) {
		PrincipalGUI.optionActualPositionX = optionActualPositionX;
	}

	static int getOptionActualPositionY() {
		return optionActualPositionY;
	}

	static void setOptionActualPositionY(final int optionActualPositionY) {
		PrincipalGUI.optionActualPositionY = optionActualPositionY;
	}

	static int getOptionActualWidth() {
		return optionActualWidth;
	}

	static void setOptionActualWidth(final int optionActualWidth) {
		PrincipalGUI.optionActualWidth = optionActualWidth;
	}

	/** Obtiene el alto de los asistentes de la aplicaci&oacute;n.
	 * @return Alto de los asistentes de la aplicaci&oacute;n */
	public static int getWizardActualHeight() {
		return wizardActualHeight;
	}

	/** Establece el alto para los asistentes de la aplicaci&oacute;n.
	 * @param wizardActualHeight Alto para los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static void setWizardActualHeight(final int wizardActualHeight) {
		PrincipalGUI.wizardActualHeight = wizardActualHeight;
	}

	/** Obtiene la coordenada horizontal de la posici&oacute;n de los asistentes de la aplicaci&oacute;n.
	 * @return Coordenada horizontal de la posici&oacute;n de los asistentes de la aplicaci&oacute;n */
	public static int getWizardActualPositionX() {
		return wizardActualPositionX;
	}

	/** Establece la coordenada horizontal de la posici&oacute;n de los asistentes de la aplicaci&oacute;n.
	 * @param wizardActualPositionX Coordenada horizontal de la posici&oacute;n de los asistentes de la aplicaci&oacute;n */
	public static void setWizardActualPositionX(final int wizardActualPositionX) {
		PrincipalGUI.wizardActualPositionX = wizardActualPositionX;
	}

	/** Obtiene la coordenada vertical de la posici&oacute;n de los asistentes de la aplicaci&oacute;n.
	 * @return Coordenada vertical de la posici&oacute;n de los asistentes de la aplicaci&oacute;n */
	public static int getWizardActualPositionY() {
		return wizardActualPositionY;
	}

	/** Establece la coordenada vertical de la posici&oacute;n de los asistentes de la aplicaci&oacute;n.
	 * @param wizardActualPositionY Coordenada vertical de la posici&oacute;n de los asistentes de la aplicaci&oacute;n */
	public static void setWizardActualPositionY(final int wizardActualPositionY) {
		PrincipalGUI.wizardActualPositionY = wizardActualPositionY;
	}

	/** Obtiene el ancho de los asistentes de la aplicaci&oacute;n.
	 * @return Ancho de los asistentes de la aplicaci&oacute;n */
	public static int getWizardActualWidth() {
		return wizardActualWidth;
	}

	/** Establece el ancho para los asistentes de la aplicaci&oacute;n.
	 * @param wizardActualWidth Ancho para los di&aacute;logos de ficheros de la aplicaci&oacute;n */
	public static void setWizardActualWidth(final int wizardActualWidth) {
		PrincipalGUI.wizardActualWidth = wizardActualWidth;
	}

}
