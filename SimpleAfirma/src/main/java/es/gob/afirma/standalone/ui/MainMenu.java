/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import com.apple.eawt.AboutHandler;
import com.apple.eawt.Application;
import com.apple.eawt.PreferencesHandler;
import com.apple.eawt.QuitHandler;
import com.apple.eawt.QuitResponse;
import com.apple.eawt.AppEvent.AboutEvent;
import com.apple.eawt.AppEvent.PreferencesEvent;
import com.apple.eawt.AppEvent.QuitEvent;

import es.gob.afirma.misc.Platform;
import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.SimpleAfirma;

/** Barra de men&uacute; para toda la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class MainMenu extends JMenuBar {

    private static final long serialVersionUID = -8361808353554036015L;

    private JMenu menuArchivo;
    private JMenuItem firmarMenuItem;

    private final Component parent;
    private final SimpleAfirma saf;

    /** Construye la barra de men&uacute; de la aplicaci&oacute;n.
     * @param p Componente padre para la modalidad
     * @param s Aplicaci&oacute;n padre, para determinar el n&uacute;mero de
     *        locales e invocar a ciertos comandos de men&uacute; */
    public MainMenu(final Component p, final SimpleAfirma s) {
        this.saf = s;
        this.parent = p;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI();
            }
        });
    }

    /*
     * Mnemonicos y atajos de menu:
     *  Alt+A = Menu archivo
     *  	Alt+B = Abrir archivo
     * 		Alt+I = Firmar archivo
     *  	Alt+F4 = Salir de @firma
     *  Alt+Y = Menu Ayuda
     *  	Alt+U = Ayuda
     *  	Alt+R = Acerca de...
     *  Alt+S = Seleccionar fichero
     *  Alt+F = Firmar fichero
     *  Ctrl+A = Seleccionar fichero
     *  Ctrl+F = Firmar fichero
     *  Alt+F4 = Salir de @firma
     *  F1 = Ayuda
     *  Ctrl+R = Acerca de...
     */

    private void createUI() {

        this.menuArchivo = new JMenu(Messages.getString("MainMenu.0")); //$NON-NLS-1$
        this.menuArchivo.setMnemonic(KeyEvent.VK_A);
        this.menuArchivo.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.1") //$NON-NLS-1$
                        );
        this.menuArchivo.setEnabled(true);

        final JMenuItem abrirMenuItem = new JMenuItem(Messages.getString("MainMenu.2")); //$NON-NLS-1$
        abrirMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        abrirMenuItem.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.3") //$NON-NLS-1$
                     );
        abrirMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent ae) {
                String fileToLoad;
                if (Platform.OS.MACOSX.equals(Platform.getOS()) || Platform.OS.WINDOWS.equals(Platform.getOS())) {
                    if (MainMenu.this.saf.getCurrentDir() == null) MainMenu.this.saf.setCurrentDir(new File(Platform.getUserHome()));
                    final FileDialog fd = new FileDialog((Frame) null, Messages.getString("MainMenu.4")); //$NON-NLS-1$
                    fd.setDirectory(MainMenu.this.saf.getCurrentDir().getAbsolutePath());
                    fd.setVisible(true);
                    if (fd.getFile() == null) return;
                    MainMenu.this.saf.setCurrentDir(new File(fd.getDirectory()));
                    fileToLoad = fd.getDirectory() + fd.getFile();
                }
                else {
                    final JFileChooser fc = new JFileChooser();
                    if (MainMenu.this.saf.getCurrentDir() != null) fc.setCurrentDirectory(MainMenu.this.saf.getCurrentDir());
                    if (JFileChooser.APPROVE_OPTION == fc.showOpenDialog(MainMenu.this)) {
                        MainMenu.this.saf.setCurrentDir(fc.getCurrentDirectory());
                        fileToLoad = fc.getSelectedFile().getAbsolutePath();
                    }
                    else return;
                }
                MainMenu.this.saf.loadFileToSign(fileToLoad);
            }
        });
        this.menuArchivo.add(abrirMenuItem);

        this.firmarMenuItem = new JMenuItem(Messages.getString("MainMenu.5")); //$NON-NLS-1$
        this.firmarMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        this.firmarMenuItem.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.6") //$NON-NLS-1$
                           );
        this.firmarMenuItem.setEnabled(false);
        this.firmarMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MainMenu.this.saf.signLoadedFile();
            }
        });
        this.menuArchivo.add(this.firmarMenuItem);

        // En Mac OS X el salir lo gestiona el propio OS
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            this.menuArchivo.addSeparator();
            final JMenuItem salirMenuItem = new JMenuItem(Messages.getString("MainMenu.7")); //$NON-NLS-1$
            salirMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F4, ActionEvent.ALT_MASK));
            salirMenuItem.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.8") //$NON-NLS-1$
                         );
            salirMenuItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent arg0) {
                    exitApplication();
                }
            });
            salirMenuItem.setMnemonic(KeyEvent.VK_L);
            this.menuArchivo.add(salirMenuItem);
        }

        final JMenu menuAyuda = new JMenu(Messages.getString("MainMenu.9")); //$NON-NLS-1$
        menuAyuda.setMnemonic(KeyEvent.VK_Y);
        menuAyuda.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.10") //$NON-NLS-1$
                 );

        final JMenuItem ayudaMenuItem = new JMenuItem(Messages.getString("MainMenu.11")); //$NON-NLS-1$
        ayudaMenuItem.setAccelerator(KeyStroke.getKeyStroke("F1")); //$NON-NLS-1$
        ayudaMenuItem.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.13") //$NON-NLS-1$
                     );
        ayudaMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.out.println(Messages.getString("MainMenu.14")); //$NON-NLS-1$
            }
        });
        menuAyuda.add(ayudaMenuItem);

        // En Mac OS X el Acerca de lo gestiona el propio OS
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            menuAyuda.addSeparator();
            final JMenuItem acercaMenuItem = new JMenuItem(Messages.getString("MainMenu.15")); //$NON-NLS-1$
            acercaMenuItem.setAccelerator(KeyStroke.getKeyStroke("F1")); //$NON-NLS-1$
            acercaMenuItem.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.17") //$NON-NLS-1$
                          );
            acercaMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
            acercaMenuItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    showAbout();
                }
            });
            acercaMenuItem.setMnemonic(KeyEvent.VK_R);
            menuAyuda.add(acercaMenuItem);
        }

        this.add(this.menuArchivo);

        final Locale[] locales = SimpleAfirma.getAvailableLocales();
        if (locales != null && locales.length > 1) {
            if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
                final JMenu menuOpciones = new JMenu(Messages.getString("MainMenu.18")); //$NON-NLS-1$
                menuOpciones.setMnemonic(KeyEvent.VK_O);
                menuOpciones.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.19") //$NON-NLS-1$
                            );

                final JMenu idiomaMenu = new JMenu(Messages.getString("MainMenu.20")); //$NON-NLS-1$
                idiomaMenu.getAccessibleContext().setAccessibleDescription(Messages.getString("MainMenu.21") //$NON-NLS-1$
                          );
                menuOpciones.add(idiomaMenu);

                final ButtonGroup group = new ButtonGroup();
                JMenuItem item;
                for (final Locale locale : locales) {
                    final String localeText = locale.getDisplayName(locale);
                    item = new JRadioButtonMenuItem(localeText.substring(0, 1).toUpperCase() + localeText.substring(1));
                    item.setSelected(locale.equals(Locale.getDefault()));
                    item.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent ae) {
                            MainMenu.this.saf.setDefaultLocale(locale);
                        }
                    });
                    group.add(item);
                    idiomaMenu.add(item);
                }
                this.add(menuOpciones);
            }
            // En Mac OS X el menu es "Preferencias" dentro de la opcion principal
            else {
                Application.getApplication().setPreferencesHandler(new PreferencesHandler() {
                    @Override
                    public void handlePreferences(final PreferencesEvent pe) {
                        final String[] localesDescs = new String[locales.length];
                        for (int i = 0; i < locales.length; i++) {
                            final String localeText = locales[i].getDisplayName(locales[i]);
                            localesDescs[i] = localeText.substring(0, 1).toUpperCase() + localeText.substring(1);
                        }
                        final Object o = JOptionPane.showInputDialog(MainMenu.this.parent, Messages.getString("MainMenu.22"), //$NON-NLS-1$
                                                                     Messages.getString("MainMenu.23"), //$NON-NLS-1$
                                                                     JOptionPane.PLAIN_MESSAGE,
                                                                     null,
                                                                     localesDescs,
                                                                     null);
                        for (int i = 0; i < locales.length; i++) {
                            if (localesDescs[i].equals(o)) {
                                MainMenu.this.saf.setDefaultLocale(locales[i]);
                            }
                        }
                    }
                });
            }
        }

        // Separador para que la ayuda quede a la derecha, se ignora en Mac OS X
        this.add(Box.createHorizontalGlue());

        this.add(menuAyuda);

        // Los mnemonicos en elementos de menu violan las normas de interfaz de Apple,
        // asi que prescindimos de ellos en Mac OS X
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            abrirMenuItem.setMnemonic(KeyEvent.VK_B);
            ayudaMenuItem.setMnemonic(KeyEvent.VK_U);
            this.firmarMenuItem.setMnemonic(KeyEvent.VK_I);
        }
        // Acciones especificas de Mac OS X
        else {
            Application.getApplication().setAboutHandler(new AboutHandler() {
                @Override
                public void handleAbout(final AboutEvent ae) {
                    showAbout();
                }
            });
            Application.getApplication().setQuitHandler(new QuitHandler() {
                @Override
                public void handleQuitRequestWith(final QuitEvent qe, final QuitResponse qr) {
                    if (!exitApplication()) {
                        qr.cancelQuit();
                    }
                }
            });
        }

    }

    /** Habilita o deshabilita el men&uacute; de operaciones sobre ficheros.
     * @param en <code>true</code> para habilitar las operaciones sobre ficheros, <code>false</code> para deshabilitarlas */
    public void setEnabledFileCommands(final boolean en) {
        if (this.menuArchivo != null) this.menuArchivo.setEnabled(en);
    }

    /** Habilita o deshabilita el elemento de men&uacute; de firma de fichero.
     * @param en <code>true</code> para habilitar el elemento de men&uacute; de firma de fichero, <code>false</code> para deshabilitarlo */
    public void setEnabledSignCommand(final boolean en) {
        if (this.firmarMenuItem != null) this.firmarMenuItem.setEnabled(en);
    }

    private void showAbout() {
        JOptionPane.showMessageDialog((this.parent == null) ? MainMenu.this : this.parent, Messages.getString("MainMenu.24"), //$NON-NLS-1$
                                      Messages.getString("MainMenu.25"), //$NON-NLS-1$
                                      JOptionPane.INFORMATION_MESSAGE);
    }

    private boolean exitApplication() {
        if (JOptionPane.showConfirmDialog(this.parent, Messages.getString("MainMenu.26"), //$NON-NLS-1$
                                          Messages.getString("MainMenu.27"), //$NON-NLS-1$
                                          JOptionPane.YES_NO_OPTION,
                                          JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
            this.saf.closeApplication(0);
            return true;
        }
        return false;
    }

}
