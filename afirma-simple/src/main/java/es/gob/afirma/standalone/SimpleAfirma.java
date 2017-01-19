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
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.net.URL;
import java.nio.channels.FileLock;
import java.security.cert.X509Certificate;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import com.dmurph.tracking.AnalyticsConfigData;
import com.dmurph.tracking.JGoogleAnalyticsTracker;
import com.dmurph.tracking.JGoogleAnalyticsTracker.GoogleAnalyticsVersion;

import es.gob.afirma.cert.signvalidation.SignValidity;
import es.gob.afirma.cert.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.standalone.protocol.ProtocolInvocationLauncher;
import es.gob.afirma.standalone.ui.ClosePanel;
import es.gob.afirma.standalone.ui.DNIeWaitPanel;
import es.gob.afirma.standalone.ui.MainMenu;
import es.gob.afirma.standalone.ui.MainScreen;
import es.gob.afirma.standalone.ui.SignDetailPanel;
import es.gob.afirma.standalone.ui.SignPanel;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;
import es.gob.afirma.standalone.updater.Updater;

/** Aplicaci&oacute;n gr&aacute;fica de AutoFirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SimpleAfirma implements PropertyChangeListener, WindowListener {

	static {
		// Instalamos el registro a disco
		try {
			LogManager.install(App.AUTOFIRMA);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No ha sido posible instalar el gestor de registro: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	private static final int DEFAULT_WINDOW_WIDTH = 780;
	private static final int DEFAULT_WINDOW_HEIGHT = 550;

	private static final String GOOGLE_ANALYTICS_TRACKING_CODE = "UA-41615516-4"; //$NON-NLS-1$
	private static final String IP_DISCOVERY_AUTOMATION = "http://checkip.amazonaws.com"; //$NON-NLS-1$

	private static final String SYSTEM_PROPERTY_DEBUG_FILE = "afirma_debug"; //$NON-NLS-1$

	private static final String SYSTEM_PROPERTY_DEBUG_LEVEL = "afirma_debug_level"; //$NON-NLS-1$

	/** Directorio de datos de la aplicaci&oacute;n. */
	public static final String APPLICATION_HOME = Platform.getUserHome() + File.separator + ".afirma" + File.separator + "AutoFirma"; //$NON-NLS-1$ //$NON-NLS-2$

    /** Clave de la preferencia para el guardado del idioma por defecto. */
    public static final String PREFERENCES_LOCALE = "default.locale"; //$NON-NLS-1$

    /** Inicio (en min&uacute;sculas) de una ruta que invoca a la aplicaci&oacute;n por protocolo. */
    private static final String PROTOCOL_URL_START_LOWER_CASE = "afirma://"; //$NON-NLS-1$

    /** Modo de depuraci&oacute;n para toda la aplicaci&oacute;n. */
    public static final boolean DEBUG = false;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final JFrame window = new MainScreen();

    /** Devuelve el marco principal de la aplicaci&oacute;n.
     * @return Marco principal de la aplicaci&oacute;n. */
    public Frame getMainFrame() {
    	return this.window;
    }

    private Container container;
    private JPanel currentPanel;

    private AOKeyStoreManager ksManager;
    private final MainMenu mainMenu;

    /** Construye la aplicaci&oacute;n principal y establece el
     * <i>Look&amp;Feel</i>. */
    public SimpleAfirma() {
       this.mainMenu = new MainMenu(this.window, this);
    }

    /** Indica si el <code>AOKeyStoreManager</code> ha terminado de inicializarse
     * y est&aacute; listo para su uso.
     * @return <code>true</code> si el <code>AOKeyStoreManager</code> est&aacute; listo para usarse, <code>false</code> en caso
     *         contrario */
    public boolean isKeyStoreReady() {
        return this.ksManager != null;
    }

    synchronized void setKeyStoreManager(final AOKeyStoreManager ksm) {
        if (ksm != null) {
            LOGGER.info("Establecido KeyStoreManager: " + ksm); //$NON-NLS-1$
            this.ksManager = ksm;
            if (this.currentPanel instanceof SignPanel) {
                ((SignPanel) this.currentPanel).notifyStoreReady();
            }
        }
        else {
        	LOGGER.info("No se ha podido inicializar el almacen por defecto"); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this.container,
                SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        }
    }

	void initialize(final File preSelectedFile) {

        // Cargamos las preferencias establecidas
        setDefaultLocale(buildLocale(PreferencesManager.get(PREFERENCES_LOCALE, Locale.getDefault().toString())));

        // Una excepcion en un constructor no siempre deriva en un objeto nulo,
        // por eso usamos un booleano para ver si fallo, en vez de una comprobacion
        // de igualdad a null
        boolean showDNIeScreen = preSelectedFile == null && !PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, false);
        if (showDNIeScreen) {
	        try {
	        	if (javax.smartcardio.TerminalFactory.getDefault().terminals().list().isEmpty()) {
	        		showDNIeScreen = false;
	        	}
	        }
	        catch(final Exception e) {
	        	LOGGER.info("No se ha podido obtener la lista de lectores de tarjetas del sistema: " + e); //$NON-NLS-1$
	        	showDNIeScreen = false;
	        }
        }

        this.window.setTitle(SimpleAfirmaMessages.getString("SimpleAfirma.10", getVersion())); //$NON-NLS-1$

        if (showDNIeScreen) {
           	this.currentPanel = new DNIeWaitPanel(this);
           	((MainScreen) this.window).showMainScreen(this, this.currentPanel, DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
            this.container = this.window;
        }
        else {
        	this.currentPanel = new SignPanel(this.window, this);
        	((MainScreen) this.window).showMainScreen(this, this.currentPanel, DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
        	this.container = this.window;

        	loadDefaultKeyStore();
        	configureMenuBar();

        	if (preSelectedFile != null) {
        		loadFileToSign(preSelectedFile);
        	}
        }
    }

	private void configureMenuBar() {
		if (this.window != null) {
        	if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            	this.window.getRootPane().putClientProperty("Window.documentFile", null); //$NON-NLS-1$
            }
            this.window.setJMenuBar(this.mainMenu);
            this.mainMenu.setEnabledOpenCommand(true);
        }
	}

    private void loadDefaultKeyStore() {
    	if (this.ksManager != null) {
    		LOGGER.info(
    			"Se omite la carga concurrente de almacen por haberse hecho una precarga previa" //$NON-NLS-1$
    		);
    		return;
    	}
        this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
        try {
            new SimpleKeyStoreManagerWorker(this, null, false).execute();
        }
        catch (final Exception e) {
            LOGGER.severe(
        		"No se pudo abrir el almacen por defecto del entorno operativo: " + e //$NON-NLS-1$
            );
            AOUIFactory.showErrorMessage(
                this.container,
                SimpleAfirmaMessages.getString("SimpleAfirma.42"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
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
            loadMainApp();
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
            loadMainApp();
    	}
    }

    /** Carga el panel de firma en el interfaz. */
    public void loadMainApp() {

    	this.window.setTitle(SimpleAfirmaMessages.getString("SimpleAfirma.10", getVersion())); //$NON-NLS-1$

    	configureMenuBar();

        final JPanel newPanel = new SignPanel(this.window, this);
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
    	askForClosing();
    }

    @Override public void windowOpened(final WindowEvent we) { /* No implementado */ }
    @Override public void windowClosed(final WindowEvent we) { /* No implementado */ }
    @Override public void windowActivated(final WindowEvent we) { /* No implementado */ }
    @Override public void windowIconified(final WindowEvent we) { /* No implementado */ }
    @Override public void windowDeiconified(final WindowEvent we) { /* No implementado */ }
    @Override public void windowDeactivated(final WindowEvent we) { /* No implementado */ }

    /** Cierra la aplicaci&oacute;n.
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
     *                 indica error y cero indica salida normal. */
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

        final JPanel newPanel = new SignDetailPanel(
    		this,
    		sign,
    		fileName,
    		signingCert,
    		new SignValidity(SIGN_DETAIL_TYPE.GENERATED, null),
    		null
		);
        this.container.add(newPanel, BorderLayout.CENTER);
        if (this.window != null && fileName != null) {
            this.window.getRootPane().putClientProperty("Window.documentFile", new File(fileName)); //$NON-NLS-1$
            this.window.setTitle(SimpleAfirmaMessages.getString("SimpleAfirma.10", getVersion()) + " - " + new File(fileName).getName()); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (this.currentPanel != null) {
            this.currentPanel.setVisible(false);
            this.currentPanel.setFocusable(false);
            this.currentPanel.setEnabled(false);
        }
        this.container.repaint();
        this.container.requestFocusInWindow();
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
        Locale.getDefault()
    };

    /** Obtiene los idiomas disponibles para la aplicaci&oacute;n
     * @return Locales disponibles para la aplicaci&oacute;n */
    public static Locale[] getAvailableLocales() {
        return locales;
    }

    /** Establece el idioma de la aplicaci&oacute;n.
     * @param l Locale a establecer */
    public static void setDefaultLocale(final Locale l) {
        if (l != null) {
            Locale.setDefault(l);
            PreferencesManager.put(PREFERENCES_LOCALE, l.toString());
            SimpleAfirmaMessages.changeLocale();
        }
    }

    /** Habilita o desabilita el men&uacute; <i>Archivo</i> de la barra de
     * men&uacute;.
     * @param e <code>true</code> para habilitar el men&uacute;
     *          <i>Archivo</i>, <code>false</code> para deshabilitarlo */
    public void setSignMenuCommandEnabled(final boolean e) {
        if (this.mainMenu != null) {
            this.mainMenu.setEnabledSignCommand(e);
        }
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
            final File helpFile = new File(APPLICATION_HOME + "\\AutoFirmaV2.chm"); //$NON-NLS-1$
            // Si el fichero no existe lo creamos
            if (!helpFile.exists()) {
	            try {
	            	HelpResourceManager.createWindowsHelpResources(helpFile);
	            }
	            catch(final Exception e) {
	                LOGGER.warning("La ayuda Windows Help no se ha podido copiar: " + e); //$NON-NLS-1$
	            }
            }
            // Cargamos el fichero
            try {
				Desktop.getDesktop().open(helpFile);
			}
            catch (final IOException e) {
				LOGGER.warning("La ayuda Windows Help no se ha podido cargar, se mostrara JavaHelp: " + e); //$NON-NLS-1$
			}
            return;
        }
        else if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            // Ultimo recurso, si no es Windows, es Apple OS X pero no disponemos de Apple Help, o es otro
            // sistema operativo (Linux, Solaris), cargamos JavaHelp
            JavaHelp.showHelp();
        }
    }

    /** Carga el fichero a firmar. Este m&eacute;todo se situa aqu&iacute; para
     * permitir su acceso desde la barra de men&uacute;
     * @param file Fichero a firmar, incluyendo su ruta completa */
    public void loadFileToSign(final File file) {
        if (this.currentPanel instanceof SignPanel) {
            try {
                ((SignPanel) this.currentPanel).loadFile(file);
            }
            catch (final Exception e) {
            	AOUIFactory.showErrorMessage(
                    this.currentPanel,
                    SimpleAfirmaMessages.getString("SimpleAfirma.0"), //$NON-NLS-1$
                    SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
            }
        }
    }

    /** Punto de entrada de la aplicaci&oacute;n. La ejecuci&oacute;n se realizar&aacute;
     * acorde a la siguiente secuencia:<br>
     * <ol>
     *   <li>Si no se pasan par&aacute;metros se iniciar&aacute; normalmente.</li>
     *   <li>
     *     Si se pasan par&aacute;metros, se iniciar&aacute; el modo de
     *     invocaci&oacute;n por protocolo
     *   </li>
     *   <li>
     *     Si falla la invocaci&oacute;n por protocolo debido a que no se cuenta con entorno
     *     gr&aacute;fico, se iniciar&aacute; el modo consola.
     *   </li>
     * </ol>
     * @param args Par&aacute;metros en l&iacute;nea de comandos */
    public static void main(final String[] args) {

    	// Configuramos el log de la aplicacion
    	configureLog();

    	// Se define el look and feel
    	LookAndFeelManager.applyLookAndFeel();

    	// Se establece la configuracion del proxy
    	if (isUsingCommnadLine(args)) {
    		CommandLineLauncher.main(args);
    		return;
    	}

       	AutoFirmaUtil.setProxySettings();

		// Google Analytics
		if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_USEANALYTICS, true) &&
			!Boolean.getBoolean("es.gob.afirma.doNotSendAnalytics") && //$NON-NLS-1$
			!Boolean.parseBoolean(System.getenv("es.gob.afirma.doNotSendAnalytics")) //$NON-NLS-1$
		) {
	    	new Thread(() ->  {
			    	try {
						final AnalyticsConfigData config = new AnalyticsConfigData(GOOGLE_ANALYTICS_TRACKING_CODE);
						final JGoogleAnalyticsTracker tracker = new JGoogleAnalyticsTracker(config, GoogleAnalyticsVersion.V_4_7_2);
						tracker.trackPageView(
							"AutoFirma", //$NON-NLS-1$
							"AutoFirma", //$NON-NLS-1$
							getIp()
						);
			    	}
			    	catch(final Exception e) {
			    		LOGGER.warning("Error registrando datos en Google Analytics: " + e); //$NON-NLS-1$
			    	}
				}
			).start();
		}

       	// Propiedades especificas para Mac OS X
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
        	try {
	            com.apple.eawt.Application.getApplication().setDockIconImage(
	        		Toolkit.getDefaultToolkit().getImage(SimpleAfirma.class.getResource("/resources/logo_cliente_256.png")) //$NON-NLS-1$);
	    		);
        	}
        	catch(final Exception | Error e) {
        		LOGGER.warning("No ha sido posible establecer el icono del Dock de OS X: " + e); //$NON-NLS-1$
        	}
        }

    	// Comprobamos actualizaciones
        if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_UPDATECHECK, true)) {
			Updater.checkForUpdates(null);
		}
		else {
			LOGGER.info("Se ha pedido no comprobar actualizaciones al inicio"); //$NON-NLS-1$
		}

    	try {
    		// Invocacion normal modo grafico
    		if (args != null && args.length > 0 && args[0].toLowerCase().startsWith(PROTOCOL_URL_START_LOWER_CASE)) {

    			printSystemInfo();

    			LOGGER.info("Invocacion por protocolo con URL:\n" + args[0]); //$NON-NLS-1$
    			ProtocolInvocationLauncher.launch(args[0]);
    			System.exit(0);
    		}
    		else {

    			if (!isSimpleAfirmaAlreadyRunning()) {

        			printSystemInfo();

    				final SimpleAfirma saf = new SimpleAfirma();

    				final OS os = Platform.getOS();
    				if (OS.WINDOWS != os && OS.MACOSX != os) {
    					LOGGER.info(
							"Se intenta una precarga temprana de NSS" //$NON-NLS-1$
						);
	    				// Hay un error raro en Java / NSS / SunPKCS11Provider que impide la inicializacion
	    				// de NSS en puntos posteriores de la ejecucion del programa, donde devuelve siempre
	    				// un CKR_DEVICE_ERROR (directamente desde NSS).
	    		    	try {
	    					final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
							    AOKeyStore.MOZ_UNI, // Store
							    null, // Lib
								"AFIRMA-NSS-KEYSTORE", // Description //$NON-NLS-1$
								null, // PasswordCallback
								null // Parent
	    					);
	    					saf.setKeyStoreManager(ksm);
	    				}
	    		    	catch (final Exception e1) {
	    					LOGGER.severe(
    							"Ha fallado la precarga temprana de NSS, se intentara la carga concurrente normal: " + e1 //$NON-NLS-1$
							);
	    				}
    				}
    				// En Linux, para evitar los problemas con los iconos hay que cambiar a bajo nivel el nombre de la ventana:
    				// http://bugs.java.com/bugdatabase/view_bug.do?bug_id=6528430
    				if (OS.LINUX.equals(os)) {
    					try {
    						final Toolkit xToolkit = Toolkit.getDefaultToolkit();
    						final java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass().getDeclaredField("awtAppClassName"); //$NON-NLS-1$
    						awtAppClassNameField.setAccessible(true);
    						awtAppClassNameField.set(xToolkit, "simpleafirma"); //$NON-NLS-1$
    					}
    					catch(final Exception e) {
    						LOGGER.warning("No ha sido posible renombrar la ventana AWT para X11: " + e); //$NON-NLS-1$
    					}
    				}

    				saf.initialize(null);
    			}
    			else {
    				AOUIFactory.showErrorMessage(
						null,
						SimpleAfirmaMessages.getString("SimpleAfirma.3"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
						JOptionPane.WARNING_MESSAGE
					);
    			}
    		}
    	}
    	catch (final HeadlessException he) {
    		CommandLineLauncher.main(args);
    	}
    	catch (final Exception e) {
    		LOGGER.log(Level.SEVERE, "Error global en la aplicacion: " + e, e); //$NON-NLS-1$
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
                            LOGGER.warning("No se ha podido eliminar el bloqueo de instancia: " + e); //$NON-NLS-1$
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

    /** Pregunta al usuario si desea cerrar la aplicaci&oacute;n.
     * @return <code>true</code> si el usuario responde que s&iacute;, <code>false</code> en caso contrario */
    public boolean askForClosing() {
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE, false)) {
    		closeApplication(0);
            return true;
    	}
        if (AOUIFactory.showConfirmDialog(
    		this.container,
    		new ClosePanel(),
            SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
            JOptionPane.YES_NO_OPTION,
            JOptionPane.WARNING_MESSAGE
        ) == JOptionPane.YES_OPTION) {
            closeApplication(0);
            return true;
        }
        return false;
    }

    /** Configura el registro (<i>log</i>) de la aplicaci&oacute;n. */
    private static void configureLog() {
    	// Configuramos, si procede, el log en fichero
    	try {
    		final String afirmaDebug = System.getProperty(SYSTEM_PROPERTY_DEBUG_FILE);
    		if (afirmaDebug != null) {
    			configureFileLogger(afirmaDebug);
    		}
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se ha podido verificar si se deseaba un log en fichero: " + e); //$NON-NLS-1$
    	}

    	// Configuramos, si procede, el nivel de log
    	try {
    		final String afirmaDebugLevel = System.getProperty(SYSTEM_PROPERTY_DEBUG_LEVEL);
    		if (afirmaDebugLevel != null) {
    			LOGGER.setLevel(Level.parse(afirmaDebugLevel));
    		}
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se ha podido verificar si se deseaba modificar el nivel de log: " + e); //$NON-NLS-1$
    	}
	}

    /** Configura que el registro de la ejecuci&oacute;n se guarde tambien en fichero.
     * @param logPath Fichero en donde se almacenar&aacute; el registro. */
    private static void configureFileLogger(final String logPath) {

    	try {
    		final Formatter logFormatter = new Formatter() {
				@Override
				public String format(final LogRecord record) {
					return new StringBuffer(Long.toString(record.getMillis())).append(": "). //$NON-NLS-1$
						append(record.getLevel().toString()).append(": "). //$NON-NLS-1$
						append(record.getMessage()).append("\n").toString(); //$NON-NLS-1$
				}
			};

    		final FileHandler handler = new FileHandler(logPath, 1024*1024*10, 3);
    		handler.setFormatter(logFormatter);
    		LOGGER.addHandler(handler);
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se pudo configurar el log en fichero: " + e); //$NON-NLS-1$
    	}
    }

    static String getIp() throws IOException {
        final URL whatismyip = new URL(IP_DISCOVERY_AUTOMATION);
        try (
    		BufferedReader in = new BoundedBufferedReader(
	    		new InputStreamReader(
	                whatismyip.openStream()
	            ),
	            1, // Solo leemos una linea
	            2048 // Maximo 2048 octetos en esa linea
			);
		) {
        	return in.readLine();
        }
    }

    private static String version = null;

	/** Recupera el identificador del numero de version del MiniApplet a partir de su Manifest.
	 * @return Identificador de la versi&oacute;n. */
	public static String getVersion() {

		if (version != null) {
			return version;
		}

		try (
			final InputStream manifestIs = SimpleAfirma.class.getClassLoader().getResourceAsStream("properties/updater.properties"); //$NON-NLS-1$
		) {
			final Properties metadata = new Properties();
			metadata.load(manifestIs);
			version = metadata.getProperty("currentVersionText", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido identificar el numero de version de AutoFirma a partir del Manifest: " + e); //$NON-NLS-1$
			version = ""; //$NON-NLS-1$
		}

		return version;
	}

	/** Imprime a traves del log la informacion b&aacute;sica del sistema. */
	private static void printSystemInfo() {

    	// Logs de informacion basica
       	LOGGER.info("Resolucion DPI de pantalla: " + AutoFirmaUtil.getDPI()); //$NON-NLS-1$
		LOGGER.info("Sistema operativo: " + System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Version del SO: " + System.getProperty("os.version")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Version de Java: " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Arquitectura del JRE: " + Platform.getJavaArch()); //$NON-NLS-1$
		LOGGER.info("Java Vendor: " + System.getProperty("java.vm.vendor")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Localizacion por defecto: " + Locale.getDefault()); //$NON-NLS-1$
		LOGGER.info("Tamano actual en memoria: " + Runtime.getRuntime().totalMemory()/(1024*1024) + "MB"); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Tamano maximo de memoria: " + Runtime.getRuntime().maxMemory()/(1024*1024) + "MB"); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Memoria actualmente libre: " + Runtime.getRuntime().freeMemory()/(1024*1024) + "MB"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Indica si la llamada a AutoFirma se considera llamada por l&iacute;nea de comandos.
	 * @param args Argumentos recibidos en la llamada a la aplicaci&oacute;n.
	 * @return {@code true} si la llamada se debe procesar como si se hubiese recibido por linea de comandos.
	 */
	private static boolean isUsingCommnadLine(String[] args) {
		return args != null && args.length > 0 && !args[0].toLowerCase().startsWith(PROTOCOL_URL_START_LOWER_CASE);
	}
}
