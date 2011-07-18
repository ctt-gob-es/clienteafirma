/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.security.cert.X509Certificate;
import java.util.Locale;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.swing.JApplet;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.signature.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.standalone.dnie.DNIeManager;
import es.gob.afirma.standalone.dnie.DNIeManagerException;
import es.gob.afirma.standalone.ui.DNIeWaitPanel;
import es.gob.afirma.standalone.ui.MacHelpHooker;
import es.gob.afirma.standalone.ui.MainMenu;
import es.gob.afirma.standalone.ui.MainScreen;
import es.gob.afirma.standalone.ui.SignDetailPanel;
import es.gob.afirma.standalone.ui.SignPanel;
import es.gob.afirma.standalone.ui.UIUtils;

/** Aplicaci&oacute;n gr&aacute;fica de firma electr&oacute;nica f&aacute;cil
 * basada en @firma. C&oacute;digos de salida de la aplicaci&oacute;n:
 * <ul>
 * <li>-1 - Error en el subsistema de tarjetas e imposibilidad de abrir el almac&eacute;n local por defecto</li>
 * <li>-2 - Imposibilidad de abrir el almac&eacute;n local por defecto</li>
 * </ul>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SimpleAfirma extends JApplet implements PropertyChangeListener, ActionListener, KeyListener, WindowListener {

    /** Clave de la preferencia para el guardado del idioma por defecto. */
    public static final String PREFERENCES_LOCALE = "default.locale"; //$NON-NLS-1$

    /** Modo de depuraci&oacute;n para toda la aplicaci&oacute;n. */
    public static final boolean DEBUG = true;

    private static final long serialVersionUID = 9146759318663175997L;

    /** Preferencias generales establecidas para el aplicativo. */
    private Preferences preferences;

    private JFrame window;
    private Container container;
    private JPanel currentPanel;

    private File currentDir;

   

    /** Construye la aplicaci&oacute;n principal y establece el
     * <i>Look&Field</i>. */
    public SimpleAfirma() {
       LookAndFeelManager. applyLookAndFeel();
        this.mainMenu = new MainMenu(this.window, this);
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            try {
                com.apple.eawt.Application.getApplication().setDefaultMenuBar(this.mainMenu);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No se ha podido establecer el menu de Mac OS X, se usara una barra de menu convencional: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                this.window.setJMenuBar(this.mainMenu);
            }
        }
    }

    private AOKeyStoreManager ksManager;
    private final MainMenu mainMenu;

    /** Indica si el <code>AOKeyStoreManager</code> ha terminado de inicializarse
     * y est&aacute; listo para su uso.
     * @return <code>true</code> si el <code>AOKeyStoreManager</code> est&aacute; listo para usarse, <code>false</code> en caso
     *         contrario */
    public boolean isKeyStoreReady() {
        return (this.ksManager != null);
    }

    synchronized void setKeyStoreManager(final AOKeyStoreManager ksm) {
        Logger.getLogger("es.gob.afirma").info("Establecido KeyStoreManager: " + ksm); //$NON-NLS-1$ //$NON-NLS-2$
        if (ksm != null) {
            this.ksManager = ksm;
            if (this.currentPanel instanceof SignPanel) {
                ((SignPanel) this.currentPanel).notifyStoreReady();
            }
        }
    }

    private void initialize(final boolean asApplet) {

        // Cargamos las preferencias establecidas
        this.preferences = Preferences.userNodeForPackage(SimpleAfirma.class);
        setDefaultLocale(buildLocale(this.preferences.get(PREFERENCES_LOCALE, Locale.getDefault().toString())));

        // Una excepcion en un constructor no siempre deriva en un objeto nulo,
        // por eso usamos un booleano para ver si fallo, en vez de una comprobacion
        // de igualdad a null
        boolean showDNIeScreen = true;
        DNIeManager dniManager = null;
        try {
            dniManager = new DNIeManager(this);
        }
        catch (final DNIeManagerException e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            "Se omite la pantalla de insercion de DNIe: " + e //$NON-NLS-1$
            );
            showDNIeScreen = false;
        }
        if (asApplet) {
            this.container = this;
        }
        else {
            this.currentPanel = new DNIeWaitPanel(this, this, this);
            this.container = new MainScreen(this, this.currentPanel, 780, 500);
            this.window = (JFrame) this.container;
            this.window.setTitle(Messages.getString("SimpleAfirma.10")); //$NON-NLS-1$
        }
        if (showDNIeScreen && dniManager != null) {
            dniManager.waitForDnie();
        }
        else {
            loadDefaultKeyStore();
        }
        loadMainApp(true);
    }

    private void loadDefaultKeyStore() {
        this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
        try {
            new SimpleKeyStoreManagerWorker(this, null, false).execute();
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
            "No se pudo abrir el almacen por defecto del entorno operativo: " + e //$NON-NLS-1$
            );
            UIUtils.showErrorMessage(
                    this.container,
                    Messages.getString("SimpleAfirma.42"), //$NON-NLS-1$
                    Messages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
            );
            closeApplication(-2);
        }
        finally {
            this.container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        if (DNIeManager.BLOWN_DNI_INSERTED.equals(evt.getPropertyName())) {
            if (DEBUG)
             {
                System.out.println("Recibido evento de BLOWN DNI INSERTED");  //$NON-NLS-1$
            }
            loadDefaultKeyStore();
            loadMainApp(true);
            UIUtils.showErrorMessage(
                    this.container,
                    Messages.getString("SimpleAfirma.6"), //$NON-NLS-1$
                    Messages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        else if (DNIeManager.CARD_EXCEPTION.equals(evt.getPropertyName())) {
            if (DEBUG)
             {
                System.out.println("Recibido evento de CARD EXCEPTION"); //$NON-NLS-1$
            }
            UIUtils.showErrorMessage(
                    this.container,
                    Messages.getString("SimpleAfirma.1"), //$NON-NLS-1$
                    Messages.getString("SimpleAfirma.13"), //$NON-NLS-1$
                    JOptionPane.WARNING_MESSAGE
            );
            return;
        }
        else if (DNIeManager.NOT_DNI_INSERTED.equals(evt.getPropertyName())) {
            if (DEBUG)
             {
                System.out.println("Recibido evento de NOT DNI INSERTED"); //$NON-NLS-1$
            }
            UIUtils.showErrorMessage(
                    this.container,
                    Messages.getString("SimpleAfirma.12"), //$NON-NLS-1$
                    Messages.getString("SimpleAfirma.13"), //$NON-NLS-1$
                    JOptionPane.WARNING_MESSAGE
            );
            return;
        }
        else if (DNIeManager.DNI_INSERTED.equals(evt.getPropertyName())) {
            if (DEBUG)
             {
                System.out.println("Recibido evento de DNI INSERTED"); //$NON-NLS-1$
            }
            this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            try {
                new SimpleKeyStoreManagerWorker(this, null, true).execute();
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
                "Fallo la inicializacion del DNIe, se intentara el almacen por defecto del sistema: " + e //$NON-NLS-1$
                );
                loadDefaultKeyStore();
            }
            finally {
                this.container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            }
            loadMainApp(true);
        }

    }

    /** Carga el panel de firma en el interfaz.
     * @param firstTime <code>true</code> si se la primera vez que se carga, <code>en caso contrario</code>
     */
    public void loadMainApp(final boolean firstTime) {
        if (this.window != null) {
        	this.window.setTitle(Messages.getString("SimpleAfirma.10")); //$NON-NLS-1$
            if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            	this.window.getRootPane().putClientProperty("Window.documentFile", null); //$NON-NLS-1$
            }
            else {
                this.window.setJMenuBar(this.mainMenu);
            }
        }
        final JPanel newPanel = new SignPanel(this.window, this, firstTime);
        this.container.add(newPanel, BorderLayout.CENTER);
        if (this.currentPanel != null) {
            this.currentPanel.setVisible(false);
            this.currentPanel.setFocusable(false);
            this.currentPanel.setEnabled(false);
        }
        this.container.repaint();
        this.currentPanel = newPanel;
    }

    @Override
    public void actionPerformed(final ActionEvent ae) {
        loadDefaultKeyStore();
        loadMainApp(true);
    }

    @Override
    public void keyPressed(final KeyEvent ke) {
        if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
            loadDefaultKeyStore();
            loadMainApp(true);
        }
        else if (ke != null && ke.getKeyCode() == KeyEvent.VK_F1 && (!Platform.OS.MACOSX.equals(Platform.getOS()))) {
            showHelp();
        }
        else {
            if (ke!=null) {
                System.out.println("Tecla pulsada: " + ke.getKeyCode()); //$NON-NLS-1$
            }
        }
    }

    @Override
    public void windowClosing(final WindowEvent we) {
        if (JOptionPane.showConfirmDialog(this.container, Messages.getString("SimpleAfirma.47"), //$NON-NLS-1$
                                          Messages.getString("SimpleAfirma.48"), //$NON-NLS-1$
                                          JOptionPane.YES_NO_OPTION,
                                          JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
            closeApplication(0);
        }
    }

    @Override public void keyTyped(final KeyEvent ke) {}
    @Override public void keyReleased(final KeyEvent ke) {}

    @Override public void windowOpened(final WindowEvent we) {}
    @Override public void windowClosed(final WindowEvent we) {}
    @Override public void windowActivated(final WindowEvent we) {}
    @Override public void windowIconified(final WindowEvent we) {}
    @Override public void windowDeiconified(final WindowEvent we) {}
    @Override public void windowDeactivated(final WindowEvent we) {}

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

    /** Obtiene el <code>AOKeyStoreManager</code> en uso en la aplicaci&oacute;n.
     * @return <code>AOKeyStoreManager</code> en uso en la aplicaci&oacute;n */
    public synchronized AOKeyStoreManager getAOKeyStoreManager() {
        return this.ksManager;
    }

    /** Elimina el panel actual y carga el panel de resultados de firma.
     * @param sign
     *        Firma o fichero firmado sobre el que queremos mostrar un
     *        resumen
     * @param fileName
     *        Nombre del fichero en el que se ha guardado la firma o el
     *        fichero firmado
     * @param signingCert
     *        Certificado usado para la firma */
    public void loadResultsPanel(final byte[] sign, final String fileName, final X509Certificate signingCert) {
    	this.mainMenu.setEnabledSignCommand(false);
    	this.mainMenu.setEnabledOpenCommand(false);
    	if (Platform.OS.MACOSX.equals(Platform.getOS())) {
    		this.mainMenu.setEnabledFileMenu(false);
    	}
        final JPanel newPanel = new SignDetailPanel(this, sign, fileName, signingCert, new SignValidity(SIGN_DETAIL_TYPE.GENERATED, null), null);
        this.container.add(newPanel, BorderLayout.CENTER);
        if (this.window != null && fileName != null) {
            this.window.getRootPane().putClientProperty("Window.documentFile", new File(fileName)); //$NON-NLS-1$
            this.window.setTitle(Messages.getString("SimpleAfirma.10") + " - " + new File(fileName).getName()); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (this.currentPanel != null) {
            this.currentPanel.setVisible(false);
            this.currentPanel.setFocusable(false);
            this.currentPanel.setEnabled(false);
        }
        this.container.repaint();
        this.currentPanel = newPanel;
        if (this.window != null) {
            this.window.repaint();
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
            setPreference(PREFERENCES_LOCALE, l.toString());
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

    /** Habilita o desabilita el men&uacute; <i>Archivo</i> de la barra de
     * men&uacute;.
     * @param e
     *        <code>true</code> para habilitar el men&uacute;
     *        <i>Archivo</i>, <code>false</code> para deshabilitarlo */
    public void setSignMenuCommandEnabled(final boolean e) {
        if (this.mainMenu != null) {
            this.mainMenu.setEnabledSignCommand(e);
        }
    }

    /** Establece el directorio actual para la lectura y guardado de ficheros.
     * @param dir
     *        Directorio actual, incluyendo su ruta completa */
    public void setCurrentDir(final File dir) {
        this.currentDir = dir;
    }

    /** Obtiene el directorio actual para la lectura y guardado de ficheros.
     * @return Directorio actual para la lectura y guardado de ficheros. */
    public File getCurrentDir() {
        return this.currentDir;
    }

    /** Firma el fichero actualmente cargado. Este m&eacute;todo se situa
     * aqu&iacute; para permitir su acceso desde la barra de men&uacute; */
    public void signLoadedFile() {
        if (this.currentPanel instanceof SignPanel) {
            ((SignPanel) this.currentPanel).sign();
        }
    }
    
    /** Muestra la ayuda de la aplicaci&oacute;n. */
    public void showHelp() {
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            try {
                Desktop.getDesktop().open(new File(ClassLoader.getSystemResource("FirmaFacil.chm").toURI())); //$NON-NLS-1$
                return;
            }
            catch(final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("La ayuda Windows Help no se ha podido cargar, se mostrara JavaHelp: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        else if (MacHelpHooker.isMacHelpAvailable()) {
            MacHelpHooker.showHelp();
            return;
        }

        // Ultimo recurso, si no es Windows, es Mac OS X pero no disponemos de Apple Help, o es otro
        // sistema operativo (Linux, Solaris), cargamos JavaHelp
        JavaHelp.showHelp();
    }

    /** Carga el fichero a firmar. Este m&eacute;todo se situa aqu&iacute; para
     * permitir su acceso desde la barra de men&uacute;
     * @param filePath
     *        Fichero a firmar, incluyendo su ruta completa */
    public void loadFileToSign(final String filePath) {
        if (this.currentPanel instanceof SignPanel) {
            try {
                ((SignPanel) this.currentPanel).loadFile(filePath);
            }
            catch (final Exception e) {
                UIUtils.showErrorMessage(
                        this.currentPanel,
                        Messages.getString("SimpleAfirma.0"), //$NON-NLS-1$
                        Messages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                );
            }
        }
    }

    // ***********************************************
    // ***** APLICACION COMO APPLET ******************
    // ***********************************************

    private void createUI() {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        this.setSize(new Dimension(780, 500));
        this.setLayout(new BorderLayout());
        this.currentPanel = new DNIeWaitPanel(this, this, this);
        this.add(this.currentPanel, BorderLayout.CENTER);
        this.setVisible(true);
    }

    @Override
    public void init() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI();
            }
        });
        initialize(true);
    }

    // ***********************************************
    // ***** FIN APLICACION COMO APPLET **************
    // ***********************************************


    /** Punto de entrada de la aplicaci&oacute;n.
     * @param args
     *        Par&aacute;metros en l&iacute;nea de comandos */
    public static void main(final String[] args) {
        if (args != null && args.length > 0) {
            final File signFile = new File(args[0]);
            if (signFile.exists() && signFile.isFile() && signFile.canRead()) {
                new VisorFirma(new File(args[0]), true).initialize(false, null);
            }
            else {
                System.out.println(Messages.getString("SimpleAfirma.2")); //$NON-NLS-1$
            }
        }
        else {
            new SimpleAfirma().initialize(false);
        }
    }

}
