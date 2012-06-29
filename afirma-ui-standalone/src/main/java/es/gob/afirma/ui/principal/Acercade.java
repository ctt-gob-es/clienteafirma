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

import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityFrameAbout;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/** Muestra el panel de acerca de */
final class Acercade extends JAccessibilityFrameAbout {

    private static final long serialVersionUID = 1L;

    /** Muestra el componente AcercaDe. */
    public static void main() {
        EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                new Acercade().setVisible(true);
            }
        });
    }

    /** Constructor. */
    public Acercade() {
        initComponents();
    }

    /** Escondemos la ventana */
    void aceptarActionPerformed() {
        PrincipalGUI.setAboutActualPositionX(this.getX());
        PrincipalGUI.setAboutActualPositionY(this.getY());
        PrincipalGUI.setAboutActualWidth(this.getWidth());
        PrincipalGUI.setAboutActualHeight(this.getHeight());
        dispose();
    }

    /** Posicion X inicial de la ventana dependiendo de la resolucion de pantalla.
     * @return int Posicion X */
    public static int getInitialX() {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); // 329
        if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)) {
            return (screenSize.width - 420) / 2;
        }
        return (screenSize.width - 380) / 2;
    }

    /** Posicion Y inicial de la ventana dependiendo del sistema operativo y de la
     * resolucion de pantalla.
     * @return int Posicion Y */
    public static int getInitialY() {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); // 329
        if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)) {
            return (screenSize.height - 360) / 2;
        }
        return (screenSize.height - 320) / 2;
    }

    /** Relacion minima para el redimensionado de elementos. */
    @Override
    public int getMinimumRelation() {
        return 9;
    }

    /** Iniciamos componentes */
    private void initComponents() {
        // Dimensiones de la pestana

        if (GeneralConfig.isMaximized()) {
            this.setExtendedState(MAXIMIZED_BOTH);
            if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)) {
                setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX, Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX));
            }
            else {
                setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT));
            }
        }
        else {
            this.setExtendedState(0);
            if (PrincipalGUI.getAboutActualPositionX() != -1) {
                setBounds(PrincipalGUI.getAboutActualPositionX(),
                          PrincipalGUI.getAboutActualPositionY(),
                          PrincipalGUI.getAboutActualWidth(),
                          PrincipalGUI.getAboutActualHeight());
                if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)) {
                    setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX, Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX));
                }
                else {
                    setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT));
                }
            }
            else {
                if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.MACOSX)) {
                    setBounds(Acercade.getInitialX(),
                              Acercade.getInitialY(),
                              Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX,
                              Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX);
                    setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH_LINUX, Constants.ABOUT_WINDOW_INITIAL_HEIGHT_LINUX));
                }
                else {
                    setBounds(Acercade.getInitialX(), Acercade.getInitialY(), Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT);
                    setMinimumSize(new Dimension(Constants.ABOUT_WINDOW_INITIAL_WIDTH, Constants.ABOUT_WINDOW_INITIAL_HEIGHT));
                }
            }
        }

        // Icono de @firma
        setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage()); //$NON-NLS-1$

        // Configuracion de la ventana Acerca de
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle(Messages.getString("ayuda.contenido")); // NOI18N //$NON-NLS-1$
        setResizable(true);
        getContentPane().setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
        c.gridwidth = 3;
        c.weightx = 1.0;

        c.gridx = 0;
        c.gridy = 0;

        // Logotipo de la ventana
        final JLabel logotipo = new JLabel();
        logotipo.setIcon(new ImageIcon(getClass().getResource("/resources/images/logo_cliente.png")));//$NON-NLS-1$
        logotipo.setHorizontalAlignment(SwingConstants.CENTER);
        getContentPane().add(logotipo, c);

        c.insets = new Insets(10, 20, 0, 20);
        c.gridy = 1;

        // Version del interfaz y Version de la aplicacion
        final String version = Messages.getString("version.interfaz") + "  " + Main.VERSION; //$NON-NLS-1$ //$NON-NLS-2$
        final InfoLabel versionInterfaz = new InfoLabel(version, false);
        getContentPane().add(versionInterfaz, c);

        c.insets = new Insets(15, 20, 0, 20);
        c.gridy = 2;
        // Parrafo con el texto Cliente @firma... y con el texto El Ministerio de Politica...
        final String desc = Messages.getString("acercade.descripcion2") + "<br><br>" + Messages.getString("acercade.descripcion"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        final InfoLabel descLabel = new InfoLabel(desc, false);
        getContentPane().add(descLabel, c);

        c.fill = GridBagConstraints.NONE;
        c.insets = new Insets(20, 0, 20, 0);
        c.gridwidth = 1;
        c.gridy = 3;

        final JPanel panelAceptar = new JPanel(new GridLayout(1, 1));
        // Boton aceptar
        final JButton aceptar = new JButton();
        aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N //$NON-NLS-1$
        aceptar.setMnemonic(KeyEvent.VK_A); // Se asigna un atajo al boton aceptar
        aceptar.getAccessibleContext().setAccessibleName(aceptar.getText());
        aceptar.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(final java.awt.event.ActionEvent evt) {
                aceptarActionPerformed();
            }
        });
        Utils.setContrastColor(aceptar);
        Utils.setFontBold(aceptar);
        Utils.remarcar(aceptar);

        panelAceptar.add(aceptar);
        getContentPane().add(panelAceptar, c);
    }
}
