package es.gob.afirma.standalone;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.util.Locale;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.swing.JApplet;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import es.gob.afirma.misc.Platform;
import es.gob.afirma.standalone.ui.FileUIManager;
import es.gob.afirma.standalone.ui.MainScreen;
import es.gob.afirma.standalone.ui.VisorPanel;

public class VisorFirma extends JApplet implements WindowListener, ActionListener {

    /** Serial ID */
    private static final long serialVersionUID = 7060676034863587322L;

    /** Preferencias generales establecidas para el aplicativo. */
    private Preferences preferences;

    private JFrame window;
    private Container container = null;
    private JPanel currentPanel;

    /** Fichero de firma. */
    private File signFile;

    public VisorFirma(final File signFile) {
        this.signFile = signFile;
        this.setLookAndFeel();
    }

    private void setLookAndFeel() {

        UIManager.put("Button.defaultButtonFollowsFocus", Boolean.TRUE); //$NON-NLS-1$
        UIManager.put("OptionPane.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("RootPane.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("TextPane.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("TextArea.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("InternalFrameTitlePane.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("InternalFrame.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("Panel.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("Label.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$
        UIManager.put("PopupMenuSeparator.background", SimpleAfirma.WINDOW_COLOR); //$NON-NLS-1$

        JFrame.setDefaultLookAndFeelDecorated(true);
        JDialog.setDefaultLookAndFeelDecorated(true);

        // Propiedades especificas para Mac OS X
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            System.setProperty("apple.awt.brushMetalLook", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            System.setProperty("apple.awt.antialiasing", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            System.setProperty("apple.awt.textantialiasing", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            System.setProperty("apple.awt.rendering", "quality"); //$NON-NLS-1$ //$NON-NLS-2$
            System.setProperty("apple.awt.graphics.EnableQ2DX", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            System.setProperty("apple.awt.graphics.EnableDeferredUpdates", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            System.setProperty("apple.laf.useScreenMenuBar", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else {
            try {
                for (final LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                    if ("Nimbus".equals(info.getName())) { //$NON-NLS-1$
                        UIManager.setLookAndFeel(info.getClassName());
                        Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
                        "Establecido 'Look&Feel' Nimbus" //$NON-NLS-1$
                        );
                        return;
                    }
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                "No se ha podido establecer el 'Look&Feel' Nimbus: " + e //$NON-NLS-1$
                );
            }
        }

        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
            "Establecido 'Look&Feel' " + UIManager.getLookAndFeel().getName() //$NON-NLS-1$
            );
        }
        catch (final Exception e2) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            "No se ha podido establecer ningun 'Look&Feel': " + e2 //$NON-NLS-1$
            );
        }
    }

    public void initialize(final boolean asApplet, final File sigFile) {

        if (sigFile != null) {
            this.signFile = sigFile;
        }

        // Cargamos las preferencias establecidas
        this.preferences = Preferences.userNodeForPackage(SimpleAfirma.class);
        setDefaultLocale(buildLocale(this.preferences.get(SimpleAfirma.PREFERENCES_LOCALE, Locale.getDefault().toString())));

        if (asApplet) {
            this.container = this;
        }
        else {
            this.currentPanel = new VisorPanel(this.signFile, null, this);
            this.container = new MainScreen(this, this.currentPanel);

            if (this.window != null) {
                this.window.dispose();
            }

            this.window = (JFrame) this.container;
            this.window.setTitle(Messages.getString("VisorFirma.0"));  //$NON-NLS-1$
        }
    }

    private Locale buildLocale(final String locale) {
        final String[] frags = locale.split("_"); //$NON-NLS-1$
        if (frags.length == 1) {
            return new Locale(frags[0]);
        }
        else if (frags.length == 2) {
            return new Locale(frags[0], frags[1]);
        }
        else {
            return new Locale(frags[0], frags[1], frags[2]);
        }
    }

    /** Listado de localizaciones soportadas por la aplicaci&oacute;n. */
    private static Locale[] locales = new Locale[] {
            Locale.getDefault(), new Locale("en") //$NON-NLS-1$
            };

    /** Obtiene los idiomas disponibles para la aplicaci&oacute;n
     * @return Locales disponibles para la aplicaci&oacute;n */
    public static Locale[] getAvailableLocales() {
        return locales;
    }

    /** Establece el idioma de la aplicaci&oacute;n.
     * @param l
     *        Locale a establecer */
    public void setDefaultLocale(final Locale l) {
        if (l != null) {
            Locale.setDefault(l);
            setPreference(SimpleAfirma.PREFERENCES_LOCALE, l.toString());
            Messages.changeLocale();
        }
    }

    /** Recupera una de las preferencias establecidas para la aplicaci&oacute;n.
     * @param key
     *        Clave de la preferencia.
     * @param defaultValue
     *        Valor por defecto.
     * @return Devuelve el valor de la preferencia indicada o {@code defaultValue} si no est&aacute;a establecida. */
    public String getPreference(final String key, final String defaultValue) {
        return this.preferences.get(key, defaultValue);
    }

    /** Establece una preferencia para la aplicaci&oacute;n.
     * @param key
     *        Clave de la preferencia.
     * @param value
     *        Valor asignado. */
    public void setPreference(final String key, final String value) {
        this.preferences.put(key, value);
    }


    @Override
    public void windowClosing(final WindowEvent e) {
        closeApplication(0);
    }
    @Override public void windowOpened(final WindowEvent e) { }
    @Override public void windowClosed(final WindowEvent e) { }
    @Override public void windowIconified(final WindowEvent e) { }
    @Override public void windowDeiconified(final WindowEvent e) { }
    @Override public void windowActivated(final WindowEvent e) { }
    @Override public void windowDeactivated(final WindowEvent e) { }

    /** Cierra la aplicaci&oacute;n.
     * @param exitCode
     *        C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
     *        indica error y cero indica salida normal */
    public void closeApplication(final int exitCode) {
        if (this.window != null) {
            this.window.dispose();
        }
        System.exit(exitCode);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final File sgFile = FileUIManager.openFile(VisorFirma.this.window, null, null, Messages.getString("VisorFirma.1")); //$NON-NLS-1$
        if (sgFile == null) {
            return;
        }
        initialize(VisorFirma.this.container == VisorFirma.this, sgFile);
    }
}
