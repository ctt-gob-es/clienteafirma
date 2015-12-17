/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dialog.ModalExclusionType;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.help.DefaultHelpBroker;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.JHelp;
import javax.help.JHelpContentViewer;
import javax.help.JHelpNavigator;
import javax.help.WindowPresentation;
import javax.help.plaf.basic.BasicIndexCellRenderer;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.JWindow;
import javax.swing.text.html.HTMLDocument;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.principal.PrincipalGUI;

/** Clase con utilidades relacionadas con la ayuda de la aplicacion */
public final class HelpUtils {

	private HelpUtils() {
		// No permitimos la instanciacion
	}

    /** Mapa de componentes y su indice en la ayuda. */
    private static Hashtable<String, Component> components = new Hashtable<>();
    /** Controlador de ayuda. */
    private static HelpBroker helpBroker = null;
    static HelpBroker getHelpBroker() {
    	return helpBroker;
    }

    /** Conjunto de informacion de ayuda. */
    private static HelpSet helpset = null;
    /** Variable que almacena el icono original del boton de ayuda. */
    public static final ImageIcon IMAGEICONHELP = new ImageIcon(HelpUtils.class.getResource("/resources/images/help.png")); //$NON-NLS-1$

    /** Genera la ayuda. */
    static {
        getHelp();
    }

    /** Cambia el idioma de la ayuda
     * @param language Idioma al que se debe cambiar */
    public static void change(final String language) {
        try {
            // Carga el nuevo archivos de datos para ese idioma
            final URL hsURL = HelpBroker.class.getResource("/help/help_set-" + language + ".hs"); //$NON-NLS-1$ //$NON-NLS-2$
            helpset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
            helpBroker = helpset.createHelpBroker();
            final Enumeration<String> enumeration = components.keys();
            while (enumeration.hasMoreElements()) {
                final String key = enumeration.nextElement();
                final Component component = components.get(key);
                helpBroker.enableHelpKey(component, key, helpset);
            }

        }
        catch (final Exception ex) {
            Logger.getLogger("es.gob.afirma").severe("Error en el cambio de idioma: " + ex);  //$NON-NLS-1$//$NON-NLS-2$
        }
    }

    /** Chequeo de la accesibilidad para la ventana de ayuda.
     * @param frame ventana de ayuda. */
    static void checkHelpAccessibility(final Window frame, final boolean activate) {
        JTabbedPane jtp = new JTabbedPane();
        if (frame != null) {
            Component component = null;
            // recorre componentes del frame
            for (int i = 0; i < frame.getComponentCount(); i++) {
                component = frame.getComponent(i);
                if (component == null) {
                    continue;
                }
                // Se recorren todos los componentes de la ventana de ayuda hasta llegar al editorPane
                if (component instanceof JRootPane) {
                    for (final Component component2 : ((JRootPane) component).getComponents()) {
                        if (component2 instanceof JLayeredPane) {
                            for (final Component component3 : ((JLayeredPane) component2).getComponents()) {
                                if (component3 instanceof JPanel) {
                                    for (final Component component4 : ((JPanel) component3).getComponents()) {
                                        if (component4 instanceof JHelp) {
                                            for (final Component component5 : ((JHelp) component4).getComponents()) {
                                                if (component5 instanceof JSplitPane) {
                                                    for (final Component component6 : ((JSplitPane) component5).getComponents()) {
                                                        if (component6 instanceof JTabbedPane) {
                                                            jtp = (JTabbedPane) component6;
                                                        }
                                                        if (component6 instanceof JHelpContentViewer) {
                                                            for (final Component component7 : ((JHelpContentViewer) component6).getComponents()) {
                                                                if (component7 instanceof JScrollPane) {
                                                                    for (final Component component8 : ((JScrollPane) component7).getComponents()) {
                                                                        if (component8 instanceof JViewport) {
                                                                            for (final Component component9 : ((JViewport) component8).getComponents()) {
                                                                                if (component9 instanceof JEditorPane) {
                                                                                    // Se activa el alto contraste para el editor pane
                                                                                    setHighContrastComponentTabbedPane(
                                                                                           jtp,
                                                                                           (JEditorPane) component9,
                                                                                           activate
                                                                                    );
                                                                                    HelpUtils.setHighContrastEditorPane(
                                                                                            (JEditorPane) component9,
                                                                                            activate
                                                                                    );
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /** Activa el acceso a la ventana de ayuda por la pulsacion de una tecla
     * @param component Componente que se va a mostrar al pulsar la tecla
     * @param id Identificador de la entrada de la ayuda a la que se desea acceder. */
    public static void enableHelpKey(final Component component, final String id) {

        final HelpBroker hb = getHelpBroker();
        if (hb != null) {
            components.put(id, component);
	        hb.enableHelpKey(component, id, helpset);
	        if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
	        }
	        else if (GeneralConfig.isBigFontSize()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));
	        }
	        else if (GeneralConfig.isFontBold()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
	        }
	        else {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
	        }
	        // Alto contraste
	        final WindowPresentation wp = ((DefaultHelpBroker) hb).getWindowPresentation();
	        final Window helpwindow = wp.getHelpWindow();
	        if (Main.isOSHighContrast() || GeneralConfig.isHighContrast()) {
	            checkHelpAccessibility(helpwindow, true);
	        }
	        else {
	            checkHelpAccessibility(helpwindow, false);
	        }
        }
    }

    /** Genera la ayuda. */
    static void getHelp() {

        if (helpBroker == null) {
            try {
                // Cargamos el archivo de datos de la ayuda
                final URL hsURL = HelpBroker.class.getResource("/help/help_set-es_ES.hs"); //$NON-NLS-1$

                // Creamos la ventana de ayuda
                final HelpSet helpset1 = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
                helpBroker = helpset1.createHelpBroker();

                helpBroker.initPresentation();
                final WindowPresentation wp = ((DefaultHelpBroker) helpBroker).getWindowPresentation();
                final JFrame helpwindow = (JFrame) wp.getHelpWindow();

                // La ventana de ayuda no debe ser bloqueada por ninguna ventana de la aplicacion
                helpwindow.setModalExclusionType(ModalExclusionType.APPLICATION_EXCLUDE);

                // Introducimos el icono en la ventana
                final Image icon = Toolkit.getDefaultToolkit().createImage(HelpUtils.class.getClassLoader().getResource("resources/images/afirma_ico.png")); //$NON-NLS-1$
                helpwindow.setIconImage(icon);

            }
            catch (final Exception ex) {
                Logger.getLogger("es.gob.afirma").severe("Error en la carga de la ayuda: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
            }

            visualize(false);

        }

    }

    /** Genera el bot&oacute;n de ayuda que apuntar&aacute; a la p&aacute;gina dada.
     * @param pagina P&aacute;gina a mostrar cuando se puelse el bot&oacute;n de ayuda.
     * @return bot&oacute;n de ayuda */
    public static JButton helpButton(final String pagina) {

        final JButton botonAyuda = new JButton(IMAGEICONHELP);
        botonAyuda.setToolTipText(Messages.getString("ayudaHTML.contenido")); //$NON-NLS-1$
        botonAyuda.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        if (!pagina.equals("perfiles.usuario")) { //$NON-NLS-1$
            botonAyuda.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("ayudaHTML.contenido"))); //$NON-NLS-1$
            botonAyuda.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("ayudaHTML.contenido"))); //$NON-NLS-1$
        }
        botonAyuda.setMnemonic(KeyEvent.VK_H); // Se le asigna un mnemonico al boton de ayuda
        botonAyuda.getAccessibleContext().setAccessibleName(botonAyuda.getToolTipText());
        // Se asigna una dimension al boton segun su icono
        final Dimension dimension = new Dimension(12, 27);
        botonAyuda.setPreferredSize(dimension);

        botonAyuda.setBorder(null); // Eliminar Borde, ayuda a centrar el iconod el boton
        botonAyuda.setContentAreaFilled(false); // area del boton invisible

        // Para el tooltip
        final JWindow tip = new JWindow();
        final JLabel tipText = new JLabel();

        botonAyuda.addFocusListener(new FocusListener() {
            /** Evento que se lanza cuando un componente tiene el foco. */
            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, botonAyuda, tipText);
            }

            /** Evento que se lanza cuando un componente pierde el foco. */
            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, botonAyuda, tipText);
            }
        });

        // Foco para el modo alto contraste
        if (GeneralConfig.isHighContrast()) {
            botonAyuda.addFocusListener(new FocusListener() {
                /** Evento que se lanza cuando un componente tiene el foco. */
                @Override
                public void focusGained(final FocusEvent e) {
                    // Se muestra un borde en el boton cuando este tiene el foco
                    botonAyuda.setBorder(BorderFactory.createLineBorder(Color.ORANGE, 1));
                }

                /** Evento que se lanza cuando un componente pierde el foco. */
                @Override
                public void focusLost(final FocusEvent e) {
                    // Se quita el borde del boton al perder el foco
                    botonAyuda.setBorder(BorderFactory.createEmptyBorder());
                }
            });
        }

        // Accion para desplegar la pantalla de ayuda
        botonAyuda.addActionListener(new ActionListener() {
            /** Accion para el boton de ayuda. */
            @Override
            public void actionPerformed(
        		final ActionEvent evt) {
	            	final HelpBroker hb = getHelpBroker();
	                if (hb != null) {
		                hb.setDisplayed(true);
		                hb.setCurrentID(pagina);
		                if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()) {
		                	getHelpBroker().setFont(new Font(getHelpBroker().getFont().getName(), getHelpBroker().getFont().getStyle(), 16));
		                	getHelpBroker().setFont(new Font(getHelpBroker().getFont().getName(), Font.BOLD, getHelpBroker().getFont().getSize()));
		                }
		                else if (GeneralConfig.isBigFontSize()) {
		                	getHelpBroker().setFont(new Font(getHelpBroker().getFont().getName(), Font.PLAIN, 16));
		                }
		                else if (GeneralConfig.isFontBold()) {
		                	getHelpBroker().setFont(new Font(getHelpBroker().getFont().getName(), Font.BOLD, 11));
		                }
		                else {
		                	getHelpBroker().setFont(new Font(getHelpBroker().getFont().getName(), Font.PLAIN, 11));
		                }

		                // Alto contraste
		                final WindowPresentation wp = ((DefaultHelpBroker) hb).getWindowPresentation();
		                final Window helpwindow = wp.getHelpWindow();
		                if (Main.isOSHighContrast() || GeneralConfig.isHighContrast()) {
		                    checkHelpAccessibility(helpwindow, true);
		                }
		                else {
		                    checkHelpAccessibility(helpwindow, false);
		                }
	                }
	            }
        	}
        );
        Utils.remarcar(botonAyuda);

        return botonAyuda;
    }

    /** Establece el alto contraste para el indice de la ayuda.
     * @param tabbedPane pestanas del indice.
     * @param editorPane panel donde se muestra la ayuda.
     * @param activate indica si el modo alto contraste esta activado o no. */
    private static void setHighContrastComponentTabbedPane(final JTabbedPane tabbedPane, final JEditorPane editorPane, final boolean activate) {
        if (tabbedPane != null) {
            for (final Component componentTabbed : tabbedPane.getComponents()) {
                if (componentTabbed instanceof JHelpNavigator) {
                    for (final Component panel : ((JHelpNavigator) componentTabbed).getComponents()) {
                        if (panel instanceof JScrollPane) {
                            for (final Component component : ((JScrollPane) panel).getComponents()) {
                                if (component instanceof JViewport) {
                                    for (final Component component9 : ((JViewport) component).getComponents()) {
                                        if (component9 instanceof JTree) {

                                            /*CAMBIAR EL COMPORTAMIENTO DEL RENDER DE LAS CELDAS*/
                                            final TreeCellRenderer cr = ((JTree) component9).getCellRenderer();

                                            if (cr instanceof BasicIndexCellRenderer) {
                                                ((JTree) component9).setCellRenderer(new BasicIndexCellRenderer() {
                                                    /** UID. */
                                                    private static final long serialVersionUID = 1L;

                                                    /** Obtiene una celda con su configuracion de alto contraste aplicada. */
                                                    @Override
                                                    public Component getTreeCellRendererComponent(final JTree pTree,
                                                                                                  final Object pValue,
                                                                                                  final boolean pIsSelected,
                                                                                                  final boolean pIsExpanded,
                                                                                                  final boolean pIsLeaf,
                                                                                                  final int pRow,
                                                                                                  final boolean pHasFocus) {
                                                        super.getTreeCellRendererComponent(pTree,
                                                                                           pValue,
                                                                                           pIsSelected,
                                                                                           pIsExpanded,
                                                                                           pIsLeaf,
                                                                                           pRow,
                                                                                           pHasFocus);
                                                        if (activate) {
                                                            setForeground(Color.WHITE);
                                                            setBackgroundNonSelectionColor(Color.BLACK);
                                                        }
                                                        else {
                                                            setForeground(Color.BLACK);
                                                            setBackgroundNonSelectionColor(Color.WHITE);
                                                        }
                                                        return this;
                                                    }
                                                });
                                            }
                                            else /*if(cr instanceof BasicTOCCellRenderer)*/{
                                                ((JTree) component9).setCellRenderer(new DefaultTreeCellRenderer() {
                                                    /** UID. */
                                                    private static final long serialVersionUID = 1L;

                                                    /** Devuelve la celda tras aplicarle la configuracion de accesibilidad para el contraste. */
                                                    @Override
                                                    public Component getTreeCellRendererComponent(final JTree pTree,
                                                                                                  final Object pValue,
                                                                                                  final boolean pIsSelected,
                                                                                                  final boolean pIsExpanded,
                                                                                                  final boolean pIsLeaf,
                                                                                                  final int pRow,
                                                                                                  final boolean pHasFocus) {
                                                        super.getTreeCellRendererComponent(pTree,
                                                                                           pValue,
                                                                                           pIsSelected,
                                                                                           pIsExpanded,
                                                                                           pIsLeaf,
                                                                                           pRow,
                                                                                           pHasFocus);
                                                        if (activate) {
                                                            setForeground(Color.WHITE);
                                                            setBackgroundNonSelectionColor(Color.BLACK);
                                                        }
                                                        else {
                                                            setForeground(Color.BLACK);
                                                            setBackgroundNonSelectionColor(Color.WHITE);
                                                        }
                                                        return this;
                                                    }
                                                });
                                            }

                                            if (activate) {
                                                ((JTree) component9).setBackground(Color.black);
                                            }
                                            else {
                                                ((JTree) component9).setBackground(Color.WHITE);
                                            }
                                            ((JTree) component9).addMouseListener(new MouseAdapter() {
                                                /** Captura de evento de raton. Liberar boton. */
                                                @Override
                                                public void mouseReleased(final MouseEvent e) {
                                                    HelpUtils.setHighContrastEditorPane(editorPane, activate);
                                                }
                                            });
                                            ((JTree) component9).addKeyListener(new KeyAdapter() {
                                                /** Tecla liberada. */
                                                @Override
                                                public void keyReleased(final KeyEvent e) {
                                                    HelpUtils.setHighContrastEditorPane(editorPane, activate);
                                                }
                                            });
                                        }
                                    }
                                }
                            }
                        }
                        if (panel instanceof JPanel) {
                            for (final Component component : ((JPanel) panel).getComponents()) {
                                if (component instanceof JTextField) {
                                    final JTextField campo = (JTextField) component;
                                    campo.addKeyListener(new KeyAdapter() {
                                        /** Tecla liberada. */
                                        @Override
                                        public void keyReleased(final KeyEvent e) {
                                            if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                                                HelpUtils.setHighContrastEditorPane(editorPane, activate);
                                            }
                                        }
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /** Establece el modo alto contraste para el editorPane que recibe como parametro.
     * @param editorPane panel de edicion. */
    static void setHighContrastEditorPane(final JEditorPane editorPane, final boolean activate) {
        if (editorPane != null && editorPane.getDocument() instanceof HTMLDocument) {
            final HTMLDocument h = (HTMLDocument) editorPane.getDocument();
            // Se establece el color de la la letra a blanco
            editorPane.setContentType("text/html"); //$NON-NLS-1$
            final String bodyRule;
            if (activate) {
                bodyRule = "body { color: \"white\";}"; //$NON-NLS-1$
                editorPane.setBackground(Color.BLACK);
            }
            else {
                bodyRule = "body { color: \"black\";}"; //$NON-NLS-1$
                editorPane.setBackground(Color.WHITE);
            }
            h.getStyleSheet().addRule(bodyRule);
        }
    }

    /** Muestra la ayuda por la p&aacute;gina indicada. Si no se indica, se
     * abrir&aacute;a por la p&aacute;gina principal.
     * @param pagina Identificador de p&aacute;gina. */
    public static void showHelp(final String pagina) {
    	final HelpBroker hb = getHelpBroker();
        if (hb != null) {
	        hb.setDisplayed(true);
	        if (pagina != null) {
	            try {
	                hb.setCurrentID(pagina);
	                if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()) {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
	                }
	                else if (GeneralConfig.isBigFontSize()) {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));
	                }
	                else if (GeneralConfig.isFontBold()) {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
	                }
	                else {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
	                }
	            }
	            catch (final Exception e) {
	                if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()) {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
	                }
	                else if (GeneralConfig.isBigFontSize()) {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));
	                }
	                else if (GeneralConfig.isFontBold()) {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
	                }
	                else {
	                    helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
	                }
	                /* No hacemos nada para que se abra por la pagina principal */
	            }

	            // Alto contraste
	            final WindowPresentation wp = ((DefaultHelpBroker) hb).getWindowPresentation();
	            final Window helpwindow = wp.getHelpWindow();
	            if (Main.isOSHighContrast() || GeneralConfig.isHighContrast()) {
	                checkHelpAccessibility(helpwindow, true);
	            }
	            else {
	                checkHelpAccessibility(helpwindow, false);
	            }
	        }
        }
    }

    /** Visualiza la ayuda en la pagina de "Introduccion".
     * @param show indica si se mostrara o no */
    public static void visualize(final boolean show) {

    	final HelpBroker hb = getHelpBroker();
        if (hb != null) {

	        hb.setDisplayed(show);

	        hb.setCurrentID("introduccion"); //$NON-NLS-1$
	        if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
	        }
	        else if (GeneralConfig.isBigFontSize()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));
	        }
	        else if (GeneralConfig.isFontBold()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
	        }
	        else {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
	        }

	        // Alto contraste
	        final WindowPresentation wp = ((DefaultHelpBroker) hb).getWindowPresentation();
	        final Window helpwindow = wp.getHelpWindow();
	        if (Main.isOSHighContrast() || GeneralConfig.isHighContrast()) {
	            checkHelpAccessibility(helpwindow, true);
	        }
	        else {
	            checkHelpAccessibility(helpwindow, false);
	        }
        }
    }

    /** Visualiza la pagina de ayuda indicada.
     * @param pagina id de pagina */
    public static void visualize(final String pagina) {
    	final HelpBroker hb = getHelpBroker();
        if (hb != null) {
	        hb.setCurrentID(pagina);
	        if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
	        }
	        else if (GeneralConfig.isBigFontSize()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));
	        }
	        else if (GeneralConfig.isFontBold()) {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
	        }
	        else {
	            helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
	        }

	        // Alto contraste
	        final WindowPresentation wp = ((DefaultHelpBroker) hb).getWindowPresentation();
	        final Window helpwindow = wp.getHelpWindow();
	        if (Main.isOSHighContrast() || GeneralConfig.isHighContrast()) {
	            checkHelpAccessibility(helpwindow, true);
	        }
	        else {
	            checkHelpAccessibility(helpwindow, false);
	        }
        }
    }

}
