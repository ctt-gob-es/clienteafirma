/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileLock;
import java.security.cert.X509Certificate;
import java.util.Locale;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.smartcardio.TerminalFactory;
import javax.swing.JApplet;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.signature.SignValidity.SIGN_DETAIL_TYPE;
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
public final class SimpleAfirma extends JApplet implements PropertyChangeListener, WindowListener {

	private static final String APPLICATION_HOME = Platform.getUserHome() + File.separator + ".afirma" + File.separator + "firmafacil"; //$NON-NLS-1$ //$NON-NLS-2$

    /** Clave de la preferencia para el guardado del idioma por defecto. */
    public static final String PREFERENCES_LOCALE = "default.locale"; //$NON-NLS-1$

    /** Modo de depuraci&oacute;n para toda la aplicaci&oacute;n. */
    public static final boolean DEBUG = true;

    private static final long serialVersionUID = 9146759318663175997L;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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
                LOGGER.warning("No se ha podido establecer el menu de Mac OS X, se usara una barra de menu convencional: " + e); //$NON-NLS-1$
                this.window.setJMenuBar(this.mainMenu);
            }
        }
    }

    private AOKeyStoreManager ksManager;
    private final MainMenu mainMenu;

    MainMenu getMainMenu() {
    	return this.mainMenu;
    }

    /** Indica si el <code>AOKeyStoreManager</code> ha terminado de inicializarse
     * y est&aacute; listo para su uso.
     * @return <code>true</code> si el <code>AOKeyStoreManager</code> est&aacute; listo para usarse, <code>false</code> en caso
     *         contrario */
    public boolean isKeyStoreReady() {
        return (this.ksManager != null);
    }

    synchronized void setKeyStoreManager(final AOKeyStoreManager ksm) {
        LOGGER.info("Establecido KeyStoreManager: " + ksm); //$NON-NLS-1$
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
        try {
        	showDNIeScreen = !TerminalFactory.getDefault().terminals().list().isEmpty();
        }
        catch(final Exception e) {
        	LOGGER.info("No se ha podido obtener la lista de lectores de tarjetas del sistema"); //$NON-NLS-1$
        	showDNIeScreen = false;
        }
        if (asApplet) {
            this.container = this;
        }
        else {
            this.currentPanel = new DNIeWaitPanel(this);
            this.container = new MainScreen(this, this.currentPanel, 780, 500);
            this.window = (JFrame) this.container;
            this.window.setTitle(Messages.getString("SimpleAfirma.10")); //$NON-NLS-1$
        }
        if (!showDNIeScreen) {
        	loadDefaultKeyStore();
        	loadMainApp(true);
        }
    }

    private void loadDefaultKeyStore() {
        this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
        try {
            new SimpleKeyStoreManagerWorker(this, null, false).execute();
        }
        catch (final Exception e) {
            LOGGER.severe(
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
    	if (this.ksManager != null) {
    		return;
    	}
    	if (DNIeWaitPanel.PROP_DNIE_REJECTED.equals(evt.getPropertyName())) {
    		loadDefaultKeyStore();
            loadMainApp(true);
    	}
    	if (DNIeWaitPanel.PROP_HELP_REQUESTED.equals(evt.getPropertyName())) {
    		showHelp();
    	}
    	if (DNIeWaitPanel.PROP_DNIE_REQUESTED.equals(evt.getPropertyName())) {
            this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            try {
                new SimpleKeyStoreManagerWorker(this, null, true).execute();
            }
            catch (final Exception e) {
                LOGGER.severe(
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
                this.mainMenu.setEnabledOpenCommand(true);
                if (firstTime) {
                	MainMenuManager.setMenuManagement(
            			this.window.getRootPane().getActionMap(),
            			this.window.getRootPane().getInputMap(
                			JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
            			),
            			SimpleAfirma.this.getMainMenu()
        			);
                }
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
    public void windowClosing(final WindowEvent we) {
        if (JOptionPane.showConfirmDialog(this.container, Messages.getString("SimpleAfirma.47"), //$NON-NLS-1$
                                          Messages.getString("SimpleAfirma.48"), //$NON-NLS-1$
                                          JOptionPane.YES_NO_OPTION,
                                          JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
            closeApplication(0);
        }
    }

    @Override public void windowOpened(final WindowEvent we) { /* No implementado */ }
    @Override public void windowClosed(final WindowEvent we) { /* No implementado */ }
    @Override public void windowActivated(final WindowEvent we) { /* No implementado */ }
    @Override public void windowIconified(final WindowEvent we) { /* No implementado */ }
    @Override public void windowDeiconified(final WindowEvent we) { /* No implementado */ }
    @Override public void windowDeactivated(final WindowEvent we) { /* No implementado */ }

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

    private static Locale buildLocale(final String locale) {
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
    public static void showHelp() {
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            final File helpFile = new File(APPLICATION_HOME + "\\FirmaFacil.chm"); //$NON-NLS-1$
            try {
            	final byte[] helpDocument = AOUtil.getDataFromInputStream(
            			ClassLoader.getSystemResourceAsStream("help/WinHelp/FirmaFacil.chm")); //$NON-NLS-1$
            	if (helpDocument == null || helpDocument.length == 0) {
            		throw new IOException("No se ha encontrado el fichero de ayuda de Windows"); //$NON-NLS-1$
            	}

                if (!helpFile.getParentFile().exists()) {
                	helpFile.getParentFile().mkdirs();
                }
                final FileOutputStream fos = new FileOutputStream(helpFile);
                fos.write(helpDocument);
                try {
                    fos.flush();
                    fos.close();
                }
                catch(final Exception e) {
                    // Se ignora
                }
                Desktop.getDesktop().open(helpFile);
                return;
            }
            catch(final Exception e) {
                LOGGER.warning("La ayuda Windows Help no se ha podido cargar, se mostrara JavaHelp: " + e); //$NON-NLS-1$
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

    void createUI() {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        this.setSize(new Dimension(780, 500));
        this.setLayout(new BorderLayout());
        this.currentPanel = new DNIeWaitPanel(this);
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
                LOGGER.info(Messages.getString("SimpleAfirma.2")); //$NON-NLS-1$
            }
        }
        else {
        	if (!isSimpleAfirmaAlreadyRunning()) {
        		new SimpleAfirma().initialize(false);
        	}
        	else {
        		JOptionPane.showMessageDialog(
    				null,
    				Messages.getString("SimpleAfirma.3"), //$NON-NLS-1$
    				Messages.getString("SimpleAfirma.48"), //$NON-NLS-1$
    				JOptionPane.WARNING_MESSAGE
				);
        	}
        }
    }

    private static boolean isSimpleAfirmaAlreadyRunning() {
    	final File appDir = new File(APPLICATION_HOME);
        if (!appDir.exists()) {
            appDir.mkdirs();
        }
        try {
            final File file = new File(APPLICATION_HOME + File.separator + ".lock"); //$NON-NLS-1$
            final RandomAccessFile randomAccessFile = new RandomAccessFile(file, "rw"); //$NON-NLS-1$
            final FileLock fileLock = randomAccessFile.getChannel().tryLock();
            if (fileLock != null) {
                Runtime.getRuntime().addShutdownHook(new Thread() {
                    @Override
					public void run() {
                        try {
                            fileLock.release();
                            randomAccessFile.close();
                            file.delete();
                        }
                        catch (final Exception e) {
                            LOGGER.warning("No se ha podido eliminar el bloqueo de instancia"); //$NON-NLS-1$
                        }
                    }
                });
                return false;
            }
            return true;
        }
        catch (final Exception e) {
        	LOGGER.warning("No se ha podido comprobar el bloqueo de instancia"); //$NON-NLS-1$
            return false;
        }
    }


}
