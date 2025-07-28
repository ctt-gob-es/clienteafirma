/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.nio.channels.FileLock;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.net.ssl.HttpsURLConnection;
import javax.smartcardio.CardTerminal;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.core.misc.http.SslSecurityManager;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.ValidateBinarySignature;
import es.gob.afirma.signvalidation.ValidatePdfSignature;
import es.gob.afirma.standalone.configurator.common.ConfigUpdaterManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;
import es.gob.afirma.standalone.protocol.ProtocolInvocationLauncher;
import es.gob.afirma.standalone.ui.ClosePanel;
import es.gob.afirma.standalone.ui.DNIeWaitPanel;
import es.gob.afirma.standalone.ui.MainMenu;
import es.gob.afirma.standalone.ui.MainScreen;
import es.gob.afirma.standalone.ui.SignDetailPanel;
import es.gob.afirma.standalone.ui.SignOperationConfig;
import es.gob.afirma.standalone.ui.SignPanel;
import es.gob.afirma.standalone.ui.SignResultListPanel;
import es.gob.afirma.standalone.ui.SignatureResultViewer;
import es.gob.afirma.standalone.updater.Updater;

/**
 * Aplicaci&oacute;n gr&aacute;fica de Autofirma.
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class SimpleAfirma implements PropertyChangeListener, WindowListener, SignatureResultViewer {

	static {
		// Instalamos el registro a disco
		try {
			LogManager.install(App.AUTOFIRMA);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No ha sido posible instalar el gestor de registro: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/**
	 * Anchura por defecto con la que se muestra inicialmente la pantalla principal.
	 */
	private static final int DEFAULT_WINDOW_WIDTH = 780;
	/**
	 * Altura por defecto con la que se muestra inicialmente la pantalla principal.
	 */
	private static final int DEFAULT_WINDOW_HEIGHT = 580;

	private static final String IP_DISCOVERY_AUTOMATION = "http://checkip.amazonaws.com"; //$NON-NLS-1$

	private static final String SYSTEM_PROPERTY_DEBUG_FILE = "afirma_debug"; //$NON-NLS-1$

	private static final String SYSTEM_PROPERTY_DEBUG_LEVEL = "afirma_debug_level"; //$NON-NLS-1$


    /**
     * Propiedad del sistema con la que configurar que se ignoren los lectores de
     * tarjeta que se reconozcan como lectores virtuales.
     */
    private static final String SYSTEM_PROPERTY_IGNORE_VIRTUAL_READERS = "ignoreVirtualReaders"; //$NON-NLS-1$

	/** Directorio de datos de la aplicaci&oacute;n. */
	public static final String APPLICATION_HOME = Platform.getUserHome() + File.separator + ".afirma" + File.separator //$NON-NLS-1$
			+ "Autofirma"; //$NON-NLS-1$

	private static final String PLUGINS_DIRNAME = "plugins"; //$NON-NLS-1$

	/**
	 * Inicio (en min&uacute;sculas) de una ruta que invoca a la aplicaci&oacute;n
	 * por protocolo.
	 */
    private static final String PROTOCOL_URL_START_LOWER_CASE = "afirma://"; //$NON-NLS-1$

	/**
	 * Esquema y host de las URL de petici&oacute;n para la comunicaci&oacute;n por
	 * websockets.
	 */
    private final static String WEBSOCKET_REQUEST_PREFIX = PROTOCOL_URL_START_LOWER_CASE + "websocket"; //$NON-NLS-1$

	/**
	 * Indica si esta permitida la b&uacute;squeda de actualizaciones de la
	 * aplicaci&oacute;n.
	 */
    private static boolean updatesEnabled = true;

	/**
	 * Variable de entorno que hay que establecer (a nivel de sistema operativo o
	 * como propiedad de Java a nivel de JVM) a <code>true</code> para evitar la
	 * comprobaci&oacute;n de disponibilidad de actualizaciones de la
	 * aplicaci&oacute;n.
	 */
	public static final String AVOID_UPDATE_CHECK = "es.gob.afirma.doNotCheckUpdates"; //$NON-NLS-1$

	/**
	 * Variable de entorno que hay que establecer (a nivel de sistema operativo o
	 * como propiedad de Java a nivel de JVM) a <code>true</code> para evitar la
	 * comprobaci&oacute;n de disponibilidad de actualizaciones de la
	 * aplicaci&oacute;n.
	 */
	public static final String AVOID_UPDATE_CHECK_ENV = "AUTOFIRMA_AVOID_UPDATE_CHECK"; //$NON-NLS-1$

    /** Versiones de Java soportadas. */
	private static final String[] SUPPORTED_JAVA_VERSIONS = {
			"1.8", // Version LTS //$NON-NLS-1$
			"9.0", //$NON-NLS-1$
			"10.0", //$NON-NLS-1$
			"11.0", // Version LTS //$NON-NLS-1$
			"17.0", // Version LTS //$NON-NLS-1$
			"21" // Version LTS //$NON-NLS-1$
	};

    /** Modo de depuraci&oacute;n para toda la aplicaci&oacute;n. */
    public static final boolean DEBUG = false;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final JFrame window = new MainScreen();

	/**
	 * Devuelve el marco principal de la aplicaci&oacute;n.
	 *
	 * @return Marco principal de la aplicaci&oacute;n.
	 */
    public Frame getMainFrame() {
    	return this.window;
    }

    private Container container;
    private JPanel currentPanel;

    private AOKeyStoreManager ksManager;
    private final MainMenu mainMenu;

    private static final PluginsManager pluginsManager = new PluginsManager(getPluginsDir());

	/**
	 * Construye la aplicaci&oacute;n principal y establece el <i>Look&amp;Feel</i>.
	 */
    public SimpleAfirma() {
       this.mainMenu = new MainMenu(this.window, this);

		// Indicamos si se debe instalar el proveedor de firma XML de Apache
       XmlDSigProviderHelper.configureXmlDSigProvider();

       // Indicamos que no queremos cargar los lectores de tarjeta virtuales
       System.setProperty(SYSTEM_PROPERTY_IGNORE_VIRTUAL_READERS, Boolean.TRUE.toString());
    }

	/**
	 * Indica si el <code>AOKeyStoreManager</code> ha terminado de inicializarse y
	 * est&aacute; listo para su uso.
	 *
	 * @return <code>true</code> si el <code>AOKeyStoreManager</code> est&aacute;
	 *         listo para usarse, <code>false</code> en caso contrario
	 */
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
		} else {
        	LOGGER.info("No se ha podido inicializar el almacen por defecto"); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE, null);
        }
    }

    /**
     * Carga la interfaz de firma.
     * @param preSelectedFile Fichero preseleccionado o {@code null} si no debe preseleccionarse
     * ning&uacute;n fichero.
     * @param signConfig Configuraci&oacute;n de firma por defecto o {@code null} si debe aplicarse
     * la configurada por defecto en la interfaz para el tipo de fichero seleccionado.
     */
	void initGUI(final File preSelectedFile, final SignOperationConfig signConfig) {
        // Cargamos las preferencias establecidas
		String defaultLocale = PreferencesManager.get(PreferencesManager.PREFERENCES_LOCALE);
		if (defaultLocale == null || defaultLocale.isEmpty()) {
			defaultLocale = Locale.getDefault().toString();
		}
        setDefaultLocale(buildLocale(defaultLocale));

        // Una excepcion en un constructor no siempre deriva en un objeto nulo,
        // por eso usamos un booleano para ver si fallo, en vez de una comprobacion
        // de igualdad a null
		boolean showDNIeScreen = preSelectedFile == null
				&& !PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN);
        if (showDNIeScreen) {
	        try {
	        	// Si no hay lectores de tarjetas o el unico es el de Windows Hello,
	        	// ignoraremos el mostrar la pantalla inicial para el uso del DNIe
	        	final List<CardTerminal> terminals = javax.smartcardio.TerminalFactory.getDefault().terminals().list();
	        	if (terminals.isEmpty()) {
	        		showDNIeScreen = false;
	        	}
	        	else if (Platform.getOS() == Platform.OS.WINDOWS && terminals.size() == 1 && terminals.get(0).getName().startsWith("Windows Hello")) {
	        		showDNIeScreen = false;
	        	}
			} catch (final Exception e) {
	        	LOGGER.info("No se ha podido obtener la lista de lectores de tarjetas del sistema: " + e); //$NON-NLS-1$
	        	showDNIeScreen = false;
	        }
        }

        this.window.setTitle(SimpleAfirmaMessages.getString("SimpleAfirma.10", getVersion())); //$NON-NLS-1$

        if (showDNIeScreen) {
        	LOGGER.info("Mostramos pantalla del DNIe"); //$NON-NLS-1$
           	this.currentPanel = new DNIeWaitPanel(this);
			((MainScreen) this.window).showMainScreen(this, this.currentPanel, DEFAULT_WINDOW_WIDTH,
					DEFAULT_WINDOW_HEIGHT);
            this.container = this.window;
            this.container.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("DNIeWaitPanel.3") + //$NON-NLS-1$
            																SimpleAfirmaMessages.getString("DNIeWaitPanel.7")); //$NON-NLS-1$
		} else {
			LOGGER.info("Mostramos pantalla principal"); //$NON-NLS-1$
        	this.currentPanel = new SignPanel(this.window, this);
			((MainScreen) this.window).showMainScreen(this, this.currentPanel, DEFAULT_WINDOW_WIDTH,
					DEFAULT_WINDOW_HEIGHT);
        	this.container = this.window;

        	loadDefaultKeyStore();
        	configureMenuBar();

        	if (preSelectedFile != null) {
        		loadFileToSign(preSelectedFile, signConfig);
        	}
        }
    }

	private void configureMenuBar() {
		if (this.window != null) {
        	if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            	this.window.getRootPane().putClientProperty("Window.documentFile", null); //$NON-NLS-1$
            }
        	this.mainMenu.setEnabledOpenCommand(true);
            this.window.setJMenuBar(this.mainMenu);
        }
	}

    private void loadDefaultKeyStore() {

        // Si el almacen por defecto es el de Mozilla, intentamos una precarga temprana
        final String defaultKeyStore = PreferencesManager.get(
        		PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);

        // Si se ha hecho una precarga previa, puede darse el caso de que se haya cargado
        // el almacen que debe o que el almacen por defecto sea distinto
        if (this.ksManager != null) {
        	// Si el almacen que se cargo era el correcto, se omite volver a cargar el almacen
        	if (PreferencesManager.VALUE_KEYSTORE_DEFAULT.equals(defaultKeyStore)) {
        		LOGGER.info("Se omite la carga concurrente de almacen por haberse hecho una precarga previa"); //$NON-NLS-1$
        		return;
        	}
        	// Si no era el correcto, lo borramos y continuamos con la carga
        	this.ksManager = null;
        }

    	LOGGER.info("Cargaremos el almacen de claves por defecto"); //$NON-NLS-1$
        this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
        try {
            new SimpleKeyStoreManagerWorker(this, null, false, false).execute();
		} catch (final Exception e) {
			LOGGER.severe("No se pudo abrir el almacen por defecto del entorno operativo: " + e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.42"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE, e);
		} finally {
            this.container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {

    	if (DNIeWaitPanel.PROP_DNIE_REJECTED.equals(evt.getPropertyName())) {
   			loadDefaultKeyStore();
            loadMainApp();
    	}
    	else if (DNIeWaitPanel.PROP_HELP_REQUESTED.equals(evt.getPropertyName())) {
    		showHelp("Autofirma.html"); //$NON-NLS-1$
    	}
    	else if (DNIeWaitPanel.PROP_DNIE_REQUESTED.equals(evt.getPropertyName())) {
            this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            try {
                new SimpleKeyStoreManagerWorker(this, null, true, this.ksManager != null).execute();
			} catch (final Exception e) {
				LOGGER.severe("Fallo la inicializacion del DNIe, se intentara el almacen por defecto del sistema: " + e //$NON-NLS-1$
                );
                loadDefaultKeyStore();
			} finally {
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

    	if (this.container instanceof MainScreen) {
    		((MainScreen) this.container).replaceShowingPanel(newPanel);
    	} else {
    		this.container.add(newPanel, BorderLayout.CENTER);
    	}
    	if (this.currentPanel != null) {
    		this.currentPanel.setVisible(false);
    		this.currentPanel.setFocusable(false);
    		this.currentPanel.setEnabled(false);
    	}

    	this.currentPanel = newPanel;
    	this.container.requestFocus();
    	this.container.repaint();
    }

    @Override
    public void windowClosing(final WindowEvent we) {
    	askForClosing();
    }

	@Override
	public void windowOpened(final WindowEvent we) {
		/* No implementado */ }

	@Override
	public void windowClosed(final WindowEvent we) {
		/* No implementado */ }

	@Override
	public void windowActivated(final WindowEvent we) {
		/* No implementado */ }

	@Override
	public void windowIconified(final WindowEvent we) {
		/* No implementado */ }

	@Override
	public void windowDeiconified(final WindowEvent we) {
		/* No implementado */ }

	@Override
	public void windowDeactivated(final WindowEvent we) {
		/* No implementado */ }

	/**
	 * Cierra la aplicaci&oacute;n.
	 *
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
	 *                 indica error y cero indica salida normal.
	 */
    public void closeApplication(final int exitCode) {
        if (this.window != null) {
            this.window.dispose();
        }
        System.exit(exitCode);
    }

	/**
	 * Cierra la aplicaci&oacute;n.
	 *
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
	 *                 indica error y cero indica salida normal.
	 */
    public static void forceCloseApplication(final int exitCode) {
       	Runtime.getRuntime().halt(exitCode);
    }

	/**
	 * Obtiene el <code>AOKeyStoreManager</code> en uso en la aplicaci&oacute;n.
	 *
	 * @return <code>AOKeyStoreManager</code> en uso en la aplicaci&oacute;n
	 */
    public synchronized AOKeyStoreManager getAOKeyStoreManager() {
    	String lib = null;
    	if (this.ksManager == null) {
	    	try {
	    		final AOKeyStore aoks = SimpleKeyStoreManager.getDefaultKeyStoreType();
	    		if (aoks.equals(AOKeyStore.PKCS12) || aoks.equals(AOKeyStore.PKCS11)) {
	    			lib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
	    		}
	    		this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						aoks, // Store
					    lib, // Lib
						null, // Description
						aoks.getStorePasswordCallback(this), // PasswordCallback
						this // Parent
				);

			} catch (final AOKeystoreAlternativeException e) {
	 			LOGGER.log(Level.SEVERE, "Error al seleccionar el tipo de almacen", e); //$NON-NLS-1$
	 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.54"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e); //$NON-NLS-1$
	 			final OS os = Platform.getOS();
	 			final AOKeyStore aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(os);

	 			try {
	 				this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
							aoks, // Store
						    lib, // Lib
							null, // Description
							aoks.getStorePasswordCallback(this), // PasswordCallback
							this // Parent
					);
				} catch (final Exception e1) {
		 			LOGGER.log(Level.SEVERE, "Error al seleccionar el almacen del sistema", e1); //$NON-NLS-1$
		 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e1); //$NON-NLS-1$
				}

			} catch (final IOException e) {
				LOGGER.log(Level.SEVERE, "Error al leer el almacen", e); //$NON-NLS-1$
				AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.56"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e); //$NON-NLS-1$
				final OS os = Platform.getOS();
	 			final AOKeyStore aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(os);

	 			try {
	 				this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
							aoks, // Store
						    lib, // Lib
							null, // Description
							aoks.getStorePasswordCallback(this), // PasswordCallback
							this // Parent
					);
				} catch (final Exception e1) {
		 			LOGGER.log(Level.SEVERE, "Error al seleccionar el almacen del sistema", e); //$NON-NLS-1$
		 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e1); //$NON-NLS-1$
				}

			} catch (final AOCancelledOperationException aoce) {
				AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.56"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, aoce); //$NON-NLS-1$
				final OS os = Platform.getOS();
	 			final AOKeyStore aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(os);

	 			try {
	 				this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
							aoks, // Store
						    lib, // Lib
							null, // Description
							aoks.getStorePasswordCallback(this), // PasswordCallback
							this // Parent
					);
				} catch (final Exception e1) {
		 			LOGGER.log(Level.SEVERE, "Error al seleccionar el almacen del sistema", e1); //$NON-NLS-1$
		 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e1); //$NON-NLS-1$
				}
			}
    	} else {
    		final String lastKeystoreSelected = KeyStorePreferencesManager.getLastSelectedKeystore();
    		// Si el usuario ha seleccionado otro almacen de claves en el dialogo de seleccion de certificados, se cargara ese
    		if (lastKeystoreSelected != null && !lastKeystoreSelected.equals(this.ksManager.getType().getName())) {
    			try {
    	    		final AOKeyStore aoks = SimpleKeyStoreManager.getLastSelectedKeystore();
    	    		if (aoks.equals(AOKeyStore.PKCS12) || aoks.equals(AOKeyStore.PKCS11)) {
    	    			lib = KeyStorePreferencesManager.getLastSelectedKeystoreLib();
    	    		}
    	    		this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    						aoks, // Store
    					    lib, // Lib
    						null, // Description
    						aoks.getStorePasswordCallback(this), // PasswordCallback
    						this // Parent
    				);

    			} catch (final AOKeystoreAlternativeException e) {
    	 			LOGGER.log(Level.SEVERE, "Error al seleccionar el tipo de almacen", e); //$NON-NLS-1$
    	 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.54"), //$NON-NLS-1$
    						SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e); //$NON-NLS-1$
    	 			final OS os = Platform.getOS();
    	 			final AOKeyStore aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(os);

    	 			try {
    	 				this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    							aoks, // Store
    						    lib, // Lib
    							null, // Description
    							aoks.getStorePasswordCallback(this), // PasswordCallback
    							this // Parent
    					);
    				} catch (final Exception e1) {
    		 			LOGGER.log(Level.SEVERE, "Error al seleccionar el almacen del sistema", e1); //$NON-NLS-1$
    		 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
    							SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e1); //$NON-NLS-1$
    				}

    			} catch (final IOException e) {
    				LOGGER.log(Level.SEVERE, "Error al leer el almacen", e); //$NON-NLS-1$
    				AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.56"), //$NON-NLS-1$
    						SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e); //$NON-NLS-1$
    				final OS os = Platform.getOS();
    	 			final AOKeyStore aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(os);

    	 			try {
    	 				this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    							aoks, // Store
    						    lib, // Lib
    							null, // Description
    							aoks.getStorePasswordCallback(this), // PasswordCallback
    							this // Parent
    					);
    				} catch (final Exception e1) {
    		 			LOGGER.log(Level.SEVERE, "Error al seleccionar el almacen del sistema", e); //$NON-NLS-1$
    		 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
    							SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e1); //$NON-NLS-1$
    				}

    			} catch (final AOCancelledOperationException aoce) {
    				AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.57"), //$NON-NLS-1$
    						SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.WARNING_MESSAGE, aoce); //$NON-NLS-1$
    				final OS os = Platform.getOS();
    	 			final AOKeyStore aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(os);

    	 			try {
    	 				this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    							aoks, // Store
    						    lib, // Lib
    							null, // Description
    							aoks.getStorePasswordCallback(this), // PasswordCallback
    							this // Parent
    					);
    				} catch (final Exception e1) {
    		 			LOGGER.log(Level.SEVERE, "Error al seleccionar el almacen del sistema", e1); //$NON-NLS-1$
    		 			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.6"), //$NON-NLS-1$
    							SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e1); //$NON-NLS-1$
    				}
    			}
    		}
    	}
        return this.ksManager;
    }

	/**
	 * Elimina el panel actual y carga el panel de resultados de firma.
	 *
	 * @param signature   Firma sobre el que queremos mostrar un resumen.
	 * @param signConfig  Configuraci&oacute;n de firma aplicada.
	 * @param signingCert Certificado usado para la firma.
	 */
    @Override
    public void showResultsInfo(final byte[] signature, final SignOperationConfig signConfig,
    		final X509Certificate signingCert) {
    	this.mainMenu.setEnabledSignCommand(false);
    	this.mainMenu.setEnabledOpenCommand(false);

    	List<SignValidity> validityList = new ArrayList<>();
    	List<SignValidity> validityListResult = new ArrayList<>();
    	final SignValider sv = SignValiderFactory.getSignValider(signature);
        if (sv != null) {
        	try {
        		if (sv instanceof ValidateBinarySignature) {
        			try(final InputStream dataFileStream = new FileInputStream(signConfig.getDataFile())) {
            	    	final AOSigner signer = new AOCAdESSigner();
            	    	final byte [] dataFile = AOUtil.getDataFromInputStream(dataFileStream);
            	    	if (signer.isSign(dataFile)) {
            	    		final byte [] signedData = ObtainContentSignedData.obtainData(signature);
            	    		validityListResult = ValidateBinarySignature.validate(signature, signedData);
            	    	} else {
            	    		validityListResult = ValidateBinarySignature.validate(signature, dataFile);
            	    	}
        			}
        		} else if (sv instanceof ValidatePdfSignature) {
        			final Properties params;
        			if (signConfig.getExtraParams() != null) {
        				params = signConfig.getExtraParams();
        			} else {
        				params = new Properties();
        			}
        			params.setProperty(PdfExtraParams.CHECK_CERTIFICATES, Boolean.toString(true));
        			validityListResult = sv.validate(signature, params);
        		} else {
        			validityListResult = sv.validate(signature);
        		}
        	}
        	catch (final Exception e) {
        		LOGGER.log(Level.SEVERE, "No se ha podido validar el documento correctamente", e); //$NON-NLS-1$
			}
        }
        final SignValidity signValidity = validityListResult.get(0);
        if (signValidity != null && (SIGN_DETAIL_TYPE.KO.equals(signValidity.getValidity()) || SIGN_DETAIL_TYPE.UNKNOWN.equals(signValidity.getValidity()))) {
        	validityList = validityListResult;
        } else {
        	validityList.add(new SignValidity(SIGN_DETAIL_TYPE.GENERATED, null));
        }

		final JPanel newPanel = new SignDetailPanel(this, signature, signConfig, signingCert,
				validityList, null);

        if (this.container instanceof MainScreen) {
        	((MainScreen) this.container).replaceShowingPanel(newPanel);
		} else {
        	this.container.add(newPanel, BorderLayout.CENTER);
        }
        if (this.window != null) {
            this.window.getRootPane().putClientProperty("Window.documentFile", signConfig.getSignatureFile()); //$NON-NLS-1$
			this.window.setTitle(SimpleAfirmaMessages.getString("SimpleAfirma.10", getVersion()) + " - " //$NON-NLS-1$ //$NON-NLS-2$
					+ signConfig.getSignatureFile().getName());
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

    @Override
    public void showResultsInfo(final List<SignOperationConfig> signConfig, final File outDir,
    		final X509Certificate signingCert) {
    	this.mainMenu.setEnabledSignCommand(false);
    	this.mainMenu.setEnabledOpenCommand(false);

		final JPanel newPanel = new SignResultListPanel(this, signConfig, outDir, signingCert);

        if (this.container instanceof MainScreen) {
        	((MainScreen) this.container).replaceShowingPanel(newPanel);
		} else {
        	this.container.add(newPanel, BorderLayout.CENTER);
        }
        if (this.window != null) {
            this.window.getRootPane().putClientProperty("Window.documentFile", outDir); //$NON-NLS-1$
			this.window.setTitle(
					SimpleAfirmaMessages.getString("SimpleAfirma.10", getVersion()) + " - " + outDir.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
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
		if (frags.length == 2) {
            return new Locale(frags[0], frags[1]);
		}
		return new Locale(frags[0], frags[1], frags[2]);
    }

    /** Listado de localizaciones soportadas por la aplicaci&oacute;n. */
	private static Locale[] locales = { Locale.getDefault() };

	/**
	 * Obtiene los idiomas disponibles para la aplicaci&oacute;n
	 *
	 * @return Locales disponibles para la aplicaci&oacute;n
	 */
    public static Locale[] getAvailableLocales() {
    	return locales != null ? locales.clone() : null;
    }

	/**
	 * Establece el idioma de la aplicaci&oacute;n.
	 *
	 * @param l Locale a establecer
	 */
    public static void setDefaultLocale(final Locale l) {
        if (l != null) {
            Locale.setDefault(l);
            PreferencesManager.put(PreferencesManager.PREFERENCES_LOCALE, l.toString());
            SimpleAfirmaMessages.changeLocale();
        }
    }

	/**
	 * Habilita o desabilita el men&uacute; <i>Archivo</i> de la barra de
     * men&uacute;.
	 *
	 * @param e <code>true</code> para habilitar el men&uacute; <i>Archivo</i>,
	 *          <code>false</code> para deshabilitarlo
	 */
    public void setSignMenuCommandEnabled(final boolean e) {
        if (this.mainMenu != null) {
            this.mainMenu.setEnabledSignCommand(e);
        }
    }

	/**
	 * Firma el fichero actualmente cargado. Este m&eacute;todo se situa aqu&iacute;
	 * para permitir su acceso desde la barra de men&uacute;
	 */
    public void signLoadedFile() {
        if (this.currentPanel instanceof SignPanel) {
			((SignPanel) this.currentPanel).sign();
        }
    }

	/**
	 * Muestra la ayuda de la aplicaci&oacute;n.
	 * @param redirectPage P&oacute;gina donde redirigir la ayuda
	 */
	public static void showHelp(final String redirectPage) {

		final File helpDir = new File(APPLICATION_HOME + File.separator + "help"); //$NON-NLS-1$

		// Comprobamos si la ayuda local esta actualizada. Si no, extraemos la ayuda de la
		// aplicacion al directorio local de ayuda
		if (!HelpResourceManager.isLocalHelpUpdated(helpDir)) {
			try {
				HelpResourceManager.extractHelpResources(helpDir);
			} catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se han podido copiar los ficheros de ayuda a disco", e); //$NON-NLS-1$
			}
		}

		// Cargamos el fichero
		final String indexHelpFile = helpDir + File.separator + "index.html"; //$NON-NLS-1$

		try (final InputStream is = new FileInputStream(indexHelpFile)) {
			Desktop.getDesktop().browse(new URI(HelpResourceManager.createHelpFileLauncher(indexHelpFile + "?redirectPage=" + redirectPage))); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido leer el archivo de ayuda correctamente", e); //$NON-NLS-1$
		}
	}

	/**
	 * Carga el fichero a firmar. Este m&eacute;todo se situa aqu&iacute; para
     * permitir su acceso desde la barra de men&uacute;
	 *
	 * @param file Fichero a firmar, incluyendo su ruta completa
	 * @param signConfig Configuraci&oacute;n de firma.
	 */
    public void loadFileToSign(final File file, final SignOperationConfig signConfig) {
        if (this.currentPanel instanceof SignPanel) {
            try {
                ((SignPanel) this.currentPanel).loadFiles(new File[] { file }, signConfig );
			} catch (final Exception e) {
				AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.0"), //$NON-NLS-1$
                    SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE, e);
            }
        }
    }

	/**
	 * Punto de entrada de la aplicaci&oacute;n. La ejecuci&oacute;n se
	 * realizar&aacute; acorde a la siguiente secuencia:<br>
     * <ol>
     *   <li>Si no se pasan par&aacute;metros se iniciar&aacute; normalmente.</li>
	 * <li>Si se pasan par&aacute;metros, se iniciar&aacute; el modo de
	 * invocaci&oacute;n por protocolo</li>
	 * <li>Si falla la invocaci&oacute;n por protocolo debido a que no se cuenta con
	 * entorno gr&aacute;fico, se iniciar&aacute; el modo consola.</li>
     * </ol>
	 *
	 * @param args Par&aacute;metros en l&iacute;nea de comandos
	 */
    public static void main(final String[] args) {

    	// Configuramos el log de la aplicacion
    	configureLog();

    	// Se define el look and feel
    	LookAndFeelManager.applyLookAndFeel();

    	// Se establece la configuracion del proxy
    	try {
    		ProxyUtil.setProxySettings();
    	}
    	catch (final Throwable e) {
    		LOGGER.log(Level.SEVERE, "Error al aplicar la configuracion de proxy", e); //$NON-NLS-1$
			if (isUsingGUI(args) && !isHeadlessMode()) {
    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.11"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.WARNING_MESSAGE, e); //$NON-NLS-1$
    		}
		}

		// Establecemos si deben respetarse las comprobaciones de seguridad de las
		// conexiones de red
    	final boolean secureConnections = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_SECURE_CONNECTIONS);

    	HttpManager.setSecureConnections(secureConnections);

		// Establecemos el listado de dominios seguros
		HttpManager.setSecureDomains(
				PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST));

		// Establecemos los almacenes de claves de Java y de Autofirma como de confianza para las
		// conexiones remotas
		if (secureConnections) {
			try {
				SslSecurityManager.configureAfirmaTrustManagers();
				// Se realiza una primera conexion que fallara para aplicar los cambios en el contexto SSL
				try {
					final URL url = new URL("https://www.google.es/"); //$NON-NLS-1$
					final HttpsURLConnection conn = (HttpsURLConnection) url.openConnection();
					conn.connect();
					conn.disconnect();
				}
				catch (final Exception e) { /* La primera vez falla para aplicar cambios en trust managers*/ }
			} catch (final Exception e) {
				LOGGER.warning("Error al configurar almacenes de confianza: " + e); //$NON-NLS-1$
			}
		}

    	// Uso en modo linea de comandos
    	if (isUsingCommnadLine(args) || isHeadlessMode()) {
    		CommandLineLauncher.main(args);
    		return;
    	}

       	// Propiedades especificas para Mac OS X
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			final Image icon = Toolkit.getDefaultToolkit()
					.getImage(SimpleAfirma.class.getResource("/resources/logo_cliente_256.png")); //$NON-NLS-1$
        	try {
        		settingDockMacIconWithJava8(icon);
			} catch (final Exception | Error e) {
        		try {
        			settingDockMacIconWithJava9(icon);
				} catch (Exception | Error e2) {
        			LOGGER.warning("No ha sido posible establecer el icono del Dock de OS X: " + e); //$NON-NLS-1$
				}
        	}

        	// Aplicamos un parche para el error JDK-8255877 de Java:
       	 	// https://bugs.openjdk.java.net/browse/JDK-8255877
        	final String osName = System.getProperty("os.name"); //$NON-NLS-1$
    		if (osName != null && osName.startsWith("Mac OS X")) { //$NON-NLS-1$
    			final String pcscDir = "/System/Library/Frameworks/PCSC.framework/Versions/Current"; //$NON-NLS-1$
    			if (new File(pcscDir).isDirectory()) {
    				System.setProperty(
    					"sun.security.smartcardio.library", //$NON-NLS-1$
    					"/System/Library/Frameworks/PCSC.framework/Versions/Current/PCSC" //$NON-NLS-1$
    				);
    			}
    		}
        }


       	// Comprobamos si es necesario buscar actualizaciones
       	if (updatesEnabled) { // Comprobamos si se desactivaron desde fuera
			updatesEnabled = !Boolean.getBoolean(AVOID_UPDATE_CHECK)
					&& !Boolean.parseBoolean(System.getenv(AVOID_UPDATE_CHECK_ENV));
       		if (!updatesEnabled) {
				LOGGER.info("Se ha configurado en el sistema que se omita la busqueda de actualizaciones de Autofirma" //$NON-NLS-1$
       					);
       		}
       	}

    	// Comprobamos actualizaciones si estan habilitadas
        if (updatesEnabled && PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_UPDATECHECK)) {
			Updater.checkForUpdates(null);
		} else {
			LOGGER.info("No se buscaran nuevas versiones de la aplicacion"); //$NON-NLS-1$
		}

    	try {
    		// Invocacion por protocolo
    		if (args != null && args.length > 0 && args[0].toLowerCase().startsWith(PROTOCOL_URL_START_LOWER_CASE)) {

    			printSystemInfo();

    			LOGGER.info("Invocacion por protocolo con URL:\n" + args[0]); //$NON-NLS-1$
    			ProtocolInvocationLauncher.launch(args[0]);

    			// Segun sea el modo de comunicacion:
				// - Servidor intermedio: Se llegara a este punto al completar una peticion de
				// firma,
    			//							que sera al volver de la llamada anterior al metodo "launch()".
    			//							En este caso, ya se puede cerrar la aplicacion.
				// - Socket tradicional: Nunca se llegara a este punto ya que el proceso se
				// quedara
    			//							permanentemente bloqueado en el bucle que abre hilos para
    			//							atender las diversas peticiones.
    			// - WebSocket:				Se llegara a este punto inmediatamente despues de llamar a la
    			//							aplicacion ya que entonces se abre el socket y se atiende a las
    			//							peticiones desde otros hilos. En estos casos no se debe cerrar
    			//							la aplicacion, que debera permanecer abierta hasta que se cierre
    			//							el socket.
    			if (!args[0].startsWith(WEBSOCKET_REQUEST_PREFIX)) {
    				forceCloseApplication(0);
    			}
    		}
    		// Invocacion como herramienta de escritorio.
    		// Lo primero es comprobar que no se encuentre ya abierta.
    		else if (!isSimpleAfirmaAlreadyRunning()) {

				printSystemInfo();

				LOGGER.info("Apertura como herramienta de escritorio"); //$NON-NLS-1$

				// Comprobamos si hay que actualizar las preferencias del sistema o no.
				if (ConfigUpdaterManager.needCheckConfigUpdates()) {
					new Thread(() -> {
						LOGGER.info("Comprobamos si existe nueva configuracion remota"); //$NON-NLS-1$
						ConfigUpdaterManager.updatePrefsConfigFile();
					}).start();
				}

				final SimpleAfirma saf = new SimpleAfirma();

				final OS os = Platform.getOS();
				if (OS.WINDOWS != os && OS.MACOSX != os) {

					// Configuramos el uso de JMulticard segun lo establecido en el dialogo de
					// preferencias
					final boolean enableJMulticard = PreferencesManager
							.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD);

					JMulticardUtilities.configureJMulticard(enableJMulticard);

					LOGGER.info("Se intenta una precarga temprana del almacen de Mozilla"); //$NON-NLS-1$

					// Hay un error raro en Java / NSS / SunPKCS11Provider que impide la
					// inicializacion
					// de NSS en puntos posteriores de la ejecucion del programa, donde devuelve
					// siempre
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
					} catch (final Exception e1) {
						LOGGER.severe(
								"Ha fallado la precarga temprana de NSS, se intentara la carga concurrente normal: " //$NON-NLS-1$
								+ e1);
					}
				}

				// En Linux, para evitar los problemas con los iconos hay que cambiar a bajo
				// nivel el nombre de la ventana:
				// http://bugs.java.com/bugdatabase/view_bug.do?bug_id=6528430
				if (OS.LINUX.equals(os)) {
					try {
						final Toolkit xToolkit = Toolkit.getDefaultToolkit();
						final java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass()
								.getDeclaredField("awtAppClassName"); //$NON-NLS-1$
						awtAppClassNameField.setAccessible(true);
						awtAppClassNameField.set(xToolkit, "Autofirma"); //$NON-NLS-1$
					} catch (final Exception e) {
						LOGGER.warning("No ha sido posible renombrar la ventana AWT para X11: " + e); //$NON-NLS-1$
					}
				}

				LOGGER.info("Cargando plugins"); //$NON-NLS-1$
				try {
					SimpleAfirma.getPluginsManager().getPluginsLoadedList();
				} catch (final Exception e) {
					LOGGER.severe("No se han podido cargar los plugins en la aplicacion"); //$NON-NLS-1$
					AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.4"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
							JOptionPane.WARNING_MESSAGE, e);
				}

				LOGGER.info("Iniciando entorno grafico"); //$NON-NLS-1$
				saf.initGUI(null, null);

				checkJavaVersion(saf.getMainFrame());
			} else {
				LOGGER.log(Level.WARNING, "La aplicacion ya se encuentra activa en otra ventana. Se cerrara esta instancia"); //$NON-NLS-1$
	    		AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.3"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
						JOptionPane.WARNING_MESSAGE, null);
				forceCloseApplication(0);
			}
		} catch (final HeadlessException he) {
			LOGGER.log(Level.WARNING,
					"No se puede crear el entorno grafico. Se tratar la peticion como una llamada por consola"); //$NON-NLS-1$
    		CommandLineLauncher.main(args);
		} catch (final Exception e) {
    		LOGGER.log(Level.SEVERE, "Error global en la aplicacion: " + e, e); //$NON-NLS-1$
    		// En caso de error
			if (args != null && args.length > 0 && args[0].startsWith(WEBSOCKET_REQUEST_PREFIX)) {
				forceCloseApplication(-1);
			}
    	}
    }

	private static void settingDockMacIconWithJava8(final Image icon)
			throws ClassNotFoundException, NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		final Class<?> applicationClass = Class.forName("com.apple.eawt.Application"); //$NON-NLS-1$
		final Method getApplicationMethod = applicationClass.getMethod("getApplication"); //$NON-NLS-1$
		final Object applicationObject = getApplicationMethod.invoke(null);
		final Method setDockIconImageMethod = applicationClass.getMethod("setDockIconImage", Image.class); //$NON-NLS-1$
		setDockIconImageMethod.invoke(applicationObject, icon);
    }

	private static void settingDockMacIconWithJava9(final Image icon)
			throws ClassNotFoundException, NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {

    	final Class<?> taskbarClass = Class.forName("java.awt.Taskbar"); //$NON-NLS-1$
    	final Method getTaskbarMethod = taskbarClass.getMethod("getTaskbar"); //$NON-NLS-1$
    	final Object taskbarObject = getTaskbarMethod.invoke(null);
    	final Method setIconImageMethod = taskbarClass.getMethod("setIconImage", Image.class); //$NON-NLS-1$
    	setIconImageMethod.invoke(taskbarObject, icon);
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
                            if (!file.delete()) {
                            	LOGGER.warning("No se ha podido eliminar el bloqueo de instancia"); //$NON-NLS-1$
                            }
						} catch (final Exception e) {
                            LOGGER.warning("No se ha podido eliminar el bloqueo de instancia: " + e); //$NON-NLS-1$
                        }
                    }
                });
            }
            return fileLock == null;
		} catch (final Exception e) {
        	LOGGER.warning("No se ha podido comprobar el bloqueo de instancia"); //$NON-NLS-1$
            return false;
        }
    }

	/**
	 * Pregunta al usuario si desea cerrar la aplicaci&oacute;n.
	 *
	 * @return <code>true</code> si el usuario responde que s&iacute;,
	 *         <code>false</code> en caso contrario
	 */
    public boolean askForClosing() {
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE)) {
    		closeApplication(0);
            return true;
    	}
    	final ClosePanel closePanel = new ClosePanel();
    	closePanel.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SimpleAfirma.47")); //$NON-NLS-1$
		if (AOUIFactory.showConfirmDialog(this.container, closePanel,
            SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
				JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
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
		} catch (final Exception e) {
    		LOGGER.warning("No se ha podido verificar si se deseaba un log en fichero: " + e); //$NON-NLS-1$
    	}

    	// Configuramos, si procede, el nivel de log
    	try {
    		final String afirmaDebugLevel = System.getProperty(SYSTEM_PROPERTY_DEBUG_LEVEL);
    		if (afirmaDebugLevel != null) {
    			LOGGER.setLevel(Level.parse(afirmaDebugLevel));
    		}
		} catch (final Exception e) {
    		LOGGER.warning("No se ha podido verificar si se deseaba modificar el nivel de log: " + e); //$NON-NLS-1$
    	}
	}

	/**
	 * Configura que el registro de la ejecuci&oacute;n se guarde tambien en
	 * fichero.
	 *
	 * @param logPath Fichero en donde se almacenar&aacute; el registro.
	 */
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
		} catch (final Exception e) {
    		LOGGER.warning("No se pudo configurar el log en fichero: " + e); //$NON-NLS-1$
    	}
    }

    static String getIp() throws IOException {
        final URL whatismyip = new URL(IP_DISCOVERY_AUTOMATION);
		try (BufferedReader in = new BoundedBufferedReader(
				new InputStreamReader(whatismyip.openStream()),
				1, // Solo leemos una linea
	            2048 // Maximo 2048 octetos en esa linea
		);) {
        	return in.readLine();
        }
    }

    public static PluginsManager getPluginsManager() {
		return SimpleAfirma.pluginsManager;
	}

    /**
     * Devuelve el panel que actualmente muestra la aplicaci&oacute;n.
	 *
     * @return Panel que muestra la aplicaci&oacute;n.
     */
    public JPanel getCurrentPanel() {
    	return this.currentPanel;
    }

    private static String version = null;

	/**
	 * Recupera el identificador del numero de version de la aplicaci&oacute;n.
	 *
	 * @return Texto descriptivo de la versi&oacute;n.
	 */
	public static String getVersion() {
		if (version == null) {
			version = Updater.getCurrentVersionText();
		}
		return version;
	}



	/** Imprime a traves del log la informacion b&aacute;sica del sistema. */
	private static void printSystemInfo() {

    	// Logs de informacion basica
		final StringBuilder info = new StringBuilder(390).append("Autofirma ").append(getVersion()) //$NON-NLS-1$
		.append("\nResolucion DPI de pantalla: ").append(DesktopUtil.getDPI()) //$NON-NLS-1$
		.append("\nSistema operativo: ").append(System.getProperty("os.name")) //$NON-NLS-1$ //$NON-NLS-2$
		.append("\nVersion del SO: ").append(System.getProperty("os.version")) //$NON-NLS-1$ //$NON-NLS-2$
		.append("\nVersion de Java: ").append(System.getProperty("java.version")) //$NON-NLS-1$ //$NON-NLS-2$
		.append("\nArquitectura del JRE: ").append(Platform.getJavaArch()) //$NON-NLS-1$
		.append("\nJava Vendor: ").append(System.getProperty("java.vm.vendor")) //$NON-NLS-1$ //$NON-NLS-2$
		.append("\nLocalizacion por defecto: ").append(Locale.getDefault()) //$NON-NLS-1$
				.append("\nTamano actual en memoria: ").append(Runtime.getRuntime().totalMemory() / (1024 * 1024)) //$NON-NLS-1$
				.append("MB") //$NON-NLS-1$
				.append("\nTamano maximo de memoria: ").append(Runtime.getRuntime().maxMemory() / (1024 * 1024)) //$NON-NLS-1$
				.append("MB") //$NON-NLS-1$
				.append("\nMemoria actualmente libre: ").append(Runtime.getRuntime().freeMemory() / (1024 * 1024)) //$NON-NLS-1$
				.append("MB"); //$NON-NLS-1$

		LOGGER.info(info.toString());
	}

	/**
	 * Indica si la llamada a Autofirma se considera llamada por l&iacute;nea de
	 * comandos.
	 *
	 * @param args Argumentos recibidos en la llamada a la aplicaci&oacute;n.
	 * @return {@code true} si se considera que la aplicaci&oacute;n se ejecuta
	 * 		en l&iacute;nea de comandos.
	 */
	private static boolean isUsingCommnadLine(final String[] args) {
		return args != null && args.length > 0 && !args[0].toLowerCase().startsWith(PROTOCOL_URL_START_LOWER_CASE);
	}

	/**
	 * Indica si la ejecuci&oacute;n de Autofirma se est&aacute; haciendo en modo
	 * escritorio.
	 * @param args Argumentos recibidos en la llamada a la aplicaci&oacute;n.
	 * @return {@code true} si se considera que la aplicaci&oacute;n se ejecuta
	 * 		como aplicaci&oacute;n de escritorio.
	 */
	private static boolean isUsingGUI(final String[] args) {
		return args == null || args.length == 0;
	}

	/**
	 * Indica si la JVM est&aacute; configurada para funcionar s&oacute;lo en modo
	 * consola.
	 *
	 * @return {@code true} si la JVM s&oacute;lo admite el modo consola.
	 */
	private static boolean isHeadlessMode() {
		return Boolean.getBoolean("java.awt.headless"); //$NON-NLS-1$
	}

	/**
	 * Establece si las actualizaciones est&aacute;n permitidas o si se desactivaron
	 * mediante alg&uacute;n mecanismo a nivel de administraci&oacute;n.
	 *
	 * @param enable {@code true} si se debe permitir la b&uacute;squeda de
	 *               actualizaciones, {@code false} en caso contrario.
	 */
	public static void setUpdatesEnabled(final boolean enable) {
		updatesEnabled = enable;
	}

	/**
	 * Indica si las actualizaciones est&aacute;n permitidas o si se desactivaron
	 * mediante alg&uacute;n mecanismo a nivel de administraci&oacute;n.
	 *
	 * @return {@code true} si se permite la b&uacute;squeda de actualizaciones,
	 *         {@code false} en caso contrario.
	 */
	public static boolean isUpdatesEnabled() {
		return updatesEnabled;
	}

	/**
	 * Comprueba que la versi&oacute;n de Java ejecutada sea compatible.
	 *
	 * @return {@code true} si la aplicaci&oacute; se est&aacute; ejecutando con una
	 *         versi&oacute;n de Java compatible, {@code false} en caso contrario.
	 */
	private static boolean isJavaSupported() {

		final String currentJavaVersion = System.getProperty("java.version"); //$NON-NLS-1$
    	for (final String javaVersion : SUPPORTED_JAVA_VERSIONS) {
    		if (currentJavaVersion.startsWith(javaVersion)) {
    			return true;
    		}
    	}
    	return false;
	}

	/**
	 * Comprueba que la versi&oacute;n de Java ejecutada sea compatible. Si no lo es
	 * y se encuentra configurado el que se compruebe, se mostrar&aacute; una
	 * advertencia al usuario. Si no lo es y se encuentra configurado que se
	 * advierta, se mostrar&aacute;a un mensaje al usuario.<br>
	 * En caso de ser compatible la versi&oacute;n, si se configur&oacute;
	 * anteriormente que no se volviera a mostrar la advertencia, se
	 * modificar&aacute; esto para que si se muestre cuando vuelva a ser no
	 * compatible.
	 *
	 * @param parent Componente padre sobre el que mostrar los di&aacute;logos
	 *               gr&aacute;ficos.
	 */
    private static void checkJavaVersion(final Component parent) {

    	final boolean isSupportedVersion = isJavaSupported();

		final boolean needCheck = PreferencesManager
				.getBoolean(PreferencesManager.PREFERENCE_GENERAL_CHECK_JAVA_VERSION);

    	if (!isSupportedVersion && needCheck) {

    		final String currentJavaVersion = System.getProperty("java.version"); //$NON-NLS-1$

    		final JCheckBox cbDontShow = new JCheckBox(SimpleAfirmaMessages.getString("SimpleAfirma.51")); //$NON-NLS-1$

    		final JPanel panel = new JPanel();
    		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    		panel.add(new JLabel(SimpleAfirmaMessages.getString("SimpleAfirma.49", currentJavaVersion))); //$NON-NLS-1$
    		panel.add(Box.createRigidArea(new Dimension(0, 10)));
    		panel.add(cbDontShow);

    		JOptionPane.showMessageDialog(
    				parent,
    				panel,
    				SimpleAfirmaMessages.getString("SimpleAfirma.50"), //$NON-NLS-1$
    				JOptionPane.WARNING_MESSAGE);

    		if (cbDontShow.isSelected()) {
    			PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_CHECK_JAVA_VERSION, false);
    		}
		} else if (isSupportedVersion && !needCheck) {
    		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_CHECK_JAVA_VERSION, true);
    	}
	}

    /**
     * Obtiene el directorio en el que se encuentran guardados o se deben
     * guardar los plugins.
     * @return Directorio de plugins.
     */
    private static File getPluginsDir() {
		File appDir = DesktopUtil.getAlternativeDirectory();
		if (appDir == null) {
			appDir = DesktopUtil.getApplicationDirectory();
		}
		return new File(appDir, PLUGINS_DIRNAME);
	}

}
