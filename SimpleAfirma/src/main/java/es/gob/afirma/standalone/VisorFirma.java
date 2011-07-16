package es.gob.afirma.standalone;

import java.awt.Container;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.util.Locale;
import java.util.prefs.Preferences;

import javax.swing.JApplet;
import javax.swing.JFrame;
import javax.swing.JPanel;

import es.gob.afirma.standalone.ui.FileUIManager;
import es.gob.afirma.standalone.ui.MainScreen;
import es.gob.afirma.standalone.ui.VisorPanel;

/**
 * Ventana para la visualizaci&oacute;n de datos de firma.
 * @author Carlos Gamuci
 */
public class VisorFirma extends JApplet implements WindowListener {

    /** Serial ID */
    private static final long serialVersionUID = 7060676034863587322L;

    /** Preferencias generales establecidas para el aplicativo. */
    private Preferences preferences;

    private JFrame window;
    private Container container = null;
    private JPanel currentPanel;
    
    private final boolean standalone;

    /** Fichero de firma. */
    private File signFile;

    /**
     * Crea la pantalla para la visualizaci&oacute;n de la informaci&oacute;n de la firma indicada.
     * @param signFile Fichero de firma.
     * @param standalone <code>true</code> si el visor se ha arrancado como aplicaci&oacute;n independiente,
     *                   <code>false</code> si se ha arrancado desde otra aplicaci&oacute;n Java
     */
    public VisorFirma(final File signFile, final boolean standalone) {
        this.signFile = signFile;
        this.standalone = standalone;
        LookAndFeelManager.applyLookAndFeel();
    }

    /**
     * Reinicia la pantalla con los datos de una nueva firma.
     * @param asApplet Indica que si se desea cargar la pantalla en forma de applet. 
     * @param sigFile Nuevo fichero de firma.
     */
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
            this.currentPanel = new VisorPanel(this.signFile, null, this, this.standalone);
            this.container = new MainScreen(this, this.currentPanel, 740, 550);

            if (this.window != null) {
                this.window.dispose();
            }

            this.window = (JFrame) this.container;
            this.window.getRootPane().putClientProperty("Window.documentFile", this.signFile); //$NON-NLS-1$
            this.window.setTitle(Messages.getString("VisorFirma.0") + ((this.signFile != null) ? (" - " + this.signFile.getAbsolutePath()) : ""));  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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
        if (this.standalone) {
            System.exit(exitCode);
        }
    }


    /** Carga una nueva firma en el Visor, preguntando al usuario por el fichero de firma. */
    public void loadNewSign() {
        final File sgFile = FileUIManager.openFile(VisorFirma.this.window, null, null, Messages.getString("VisorFirma.1")); //$NON-NLS-1$
        if (sgFile == null) {
            return;
        }
        initialize(VisorFirma.this.container == VisorFirma.this, sgFile);
    }
}
