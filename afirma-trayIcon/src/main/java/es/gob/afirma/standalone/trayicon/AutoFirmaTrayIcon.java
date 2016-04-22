package es.gob.afirma.standalone.trayicon;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_PRIORITARY_STORE;

import java.awt.AWTException;
import java.awt.Frame;
import java.awt.Image;
import java.awt.Menu;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.RandomAccessFile;
import java.nio.channels.FileLock;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.standalone.ui.preferences.PreferencesDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog;

/** Clase que genera el TrayIcon de la configuraci&oacute;n de AutoFirma. */
public final class AutoFirmaTrayIcon {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private static final String TRAY_ICON_0 = "TrayIcon.0"; //$NON-NLS-1$
	private static final String TRAY_ICON_1 = "TrayIcon.1"; //$NON-NLS-1$
	private static final String TRAY_ICON_2 = "TrayIcon.2"; //$NON-NLS-1$
	private static final String TRAY_ICON_3 = "TrayIcon.3"; //$NON-NLS-1$
	private static final String TRAY_ICON_5 = "TrayIcon.5"; //$NON-NLS-1$
	private static final String TRAY_ICON_6 = "TrayIcon.6"; //$NON-NLS-1$
	private static final String TRAY_ICON_7 = "TrayIcon.7"; //$NON-NLS-1$
	private static final String TRAY_ICON_9 = "TrayIcon.9"; //$NON-NLS-1$
	private static final String TRAY_ICON_10 = "TrayIcon.10"; //$NON-NLS-1$
	private static final String TRAY_ICON_11 = "TrayIcon.11"; //$NON-NLS-1$
	private static final String TRAY_ICON_12 = "TrayIcon.12"; //$NON-NLS-1$
	private static final String TRAY_ICON_13 = "TrayIcon.13"; //$NON-NLS-1$
	private static final String TRAY_ICON_16 = "TrayIcon.16"; //$NON-NLS-1$
	private static final String TRAY_ICON_17 = "TrayIcon.17"; //$NON-NLS-1$
	private static final String MAINMENU_14 = "MainMenu.14"; //$NON-NLS-1$
	private static final String MAINMENU_15 = "MainMenu.15"; //$NON-NLS-1$

	/** Directorio de datos de la aplicaci&oacute;n. */
	public static final String APPLICATION_HOME = Platform.getUserHome() + File.separator + ".afirma" + File.separator + "AutoFirmaTray"; //$NON-NLS-1$ //$NON-NLS-2$

	private static final Image ICON = Toolkit.getDefaultToolkit().getImage(
		AutoFirmaTrayIcon.class.getResource("/logo_cliente_128.png") //$NON-NLS-1$
	);

	private static final Image ICON_SMALL = Toolkit.getDefaultToolkit().getImage(
		AutoFirmaTrayIcon.class.getResource("/logo_cliente_16.png") //$NON-NLS-1$
	);

	private static final String VERSION = "AutoFirma "+ SimpleAfirma.getVersion(); //$NON-NLS-1$
	final static Properties properties = new Properties();

	static final AOKeyStore[] DEFAULT_STORES;
	static {
		final List<AOKeyStore> stores = new ArrayList<>();
		final Platform.OS os = Platform.getOS();
		if (Platform.OS.WINDOWS.equals(os)) {
			stores.add(AOKeyStore.WINDOWS);
		}
		else if (Platform.OS.MACOSX.equals(os)) {
			stores.add(AOKeyStore.APPLE);
		}
		else {
			stores.add(AOKeyStore.SHARED_NSS);
		}
		if (SimpleKeyStoreManager.isFirefoxAvailable()) {
			stores.add(AOKeyStore.MOZ_UNI);
		}
		DEFAULT_STORES = stores.toArray(new AOKeyStore[0]);
	}

	static final String[] PRIORITY_STORES = {
			SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.20"), // Ninguno //$NON-NLS-1$
			AOKeyStore.DNIEJAVA.toString(),
			AOKeyStore.CERES.toString()
	};

	static final Frame HIDDEN_FRAME = new JFrame();
	static {
		HIDDEN_FRAME.setIconImage(ICON);
		HIDDEN_FRAME.setLayout(null);
	}

	/** Muestra el panel de "acerca de AutoFirma". */
	static void aboutAutofirma(){
		AOUIFactory.showMessageDialog(
			HIDDEN_FRAME,
			SimpleAfirmaMessages.getString(MAINMENU_14, SimpleAfirma.getVersion(), System.getProperty("java.version")), //$NON-NLS-1$ ,
			SimpleAfirmaMessages.getString(MAINMENU_15),
			JOptionPane.INFORMATION_MESSAGE,
			new ImageIcon(ICON)
		);
	}

	/** Genera el PopupMenu del TrayIcon.
	 * @return Men&uacute; del TrayIcon. */
	private static PopupMenu createMenu(){
		final PopupMenu menu = new PopupMenu();
		menu.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString(TRAY_ICON_0));

		// create menu item for the default action
		final MenuItem messageItem = new MenuItem(SimpleAfirmaMessages.getString(TRAY_ICON_5));
		messageItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				 PreferencesDialog.show(HIDDEN_FRAME, true);
			}
		});
		messageItem.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString(TRAY_ICON_1));

		final MenuItem closeItem = new MenuItem(SimpleAfirmaMessages.getString(TRAY_ICON_6));
		closeItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				System.exit(0);
			}
		});
		closeItem.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString(TRAY_ICON_2));


		final MenuItem about = new MenuItem(SimpleAfirmaMessages.getString(TRAY_ICON_7));
		about.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				aboutAutofirma();
			}
		});
		about.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString(TRAY_ICON_3));

		final Menu displayMenu = new Menu(SimpleAfirmaMessages.getString(TRAY_ICON_9));

		final MenuItem selecPriotityCertificate = new MenuItem(SimpleAfirmaMessages.getString(TRAY_ICON_10));
		selecPriotityCertificate.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				final String keyStore = (String) AOUIFactory.showInputDialog(
					HIDDEN_FRAME,
    				SimpleAfirmaMessages.getString("TrayIcon.22"), //$NON-NLS-1$
    				SimpleAfirmaMessages.getString("TrayIcon.21"), //$NON-NLS-1$
    				JOptionPane.QUESTION_MESSAGE,
    				AutoFirmaUtil.getDefaultDialogsIcon(),
    				PRIORITY_STORES,
    				SimpleKeyStoreManager.getDefaultKeyStoreType()
    			);
				if (keyStore != null && !keyStore.trim().isEmpty()) {
					PreferencesManager.put(
						PREFERENCE_KEYSTORE_PRIORITARY_STORE,
						keyStore
					);
				}
			}
		});
		selecPriotityCertificate.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString(TRAY_ICON_11));

		final MenuItem selecDefaultCertificate = new MenuItem(SimpleAfirmaMessages.getString(TRAY_ICON_16));
		selecDefaultCertificate.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					final AOKeyStore keyStore = (AOKeyStore) AOUIFactory.showInputDialog(
						HIDDEN_FRAME,
	    				null,
	    				SimpleAfirmaMessages.getString("TrayIcon.20"), //$NON-NLS-1$
	    				JOptionPane.QUESTION_MESSAGE,
	    				AutoFirmaUtil.getDefaultDialogsIcon(),
	    				DEFAULT_STORES,
	    				SimpleKeyStoreManager.getDefaultKeyStoreType()
	    			);
					if (keyStore != null) {
						PreferencesManager.put(
							PREFERENCE_KEYSTORE_DEFAULT_STORE,
							keyStore.toString()
						);
					}
				}
			}
		);
		selecDefaultCertificate.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString(TRAY_ICON_17));

		final MenuItem showCertificates = new MenuItem(SimpleAfirmaMessages.getString(TRAY_ICON_12));
		showCertificates.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				final AOKeyStoreManager ksm;
				try {
					ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
							SimpleKeyStoreManager.getDefaultKeyStoreType(),
						null,
						"default", //$NON-NLS-1$
						SimpleKeyStoreManager.getDefaultKeyStoreType().getStorePasswordCallback(this),
						this
					);

					final CertificateSelectionDialog csd = new CertificateSelectionDialog(
						HIDDEN_FRAME,
						new AOKeyStoreDialog(
							ksm,
							this,
							true,
							true,
							false
						),
						SimpleAfirmaMessages.getString(
							"PreferencesPanelKeyStores.10", //$NON-NLS-1$
							SimpleKeyStoreManager.getDefaultKeyStoreType().toString()
						),
						SimpleAfirmaMessages.getString(
							"PreferencesPanelKeyStores.15", //$NON-NLS-1$
							SimpleKeyStoreManager.getDefaultKeyStoreType().toString()
						),
						false,
						true
					);
					csd.showDialog();
				}
				catch (final Exception e1) {
					AOUIFactory.showErrorMessage(
						this,
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.11"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.10", SimpleKeyStoreManager.getDefaultKeyStoreType().toString()), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					Logger.getLogger("es.gob.afirma").warning("Error al recuperar el almacen por defecto seleccionado: " + e1); //$NON-NLS-1$ //$NON-NLS-2$
				}

			}
		});
		showCertificates.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString(TRAY_ICON_13));

		// add menu items to menu
		menu.add(displayMenu);
		displayMenu.add(selecPriotityCertificate);
		displayMenu.add(selecDefaultCertificate);
		displayMenu.add(showCertificates);
		menu.addSeparator();
		menu.add(messageItem);
		menu.addSeparator();
		menu.add(about);
		menu.addSeparator();
		menu.add(closeItem);
		return menu;
	}

	/** Crea el TrayIcon y lo ejecuta. */
	private static void initialize (){
		LookAndFeelManager.applyLookAndFeel();

		if (!SystemTray.isSupported()) {
			LOGGER.severe("SystemTray is not supported"); //$NON-NLS-1$
			throw new IllegalStateException("SystemTray is not supported"); //$NON-NLS-1$
		}

		final PopupMenu menu = createMenu();

		final SystemTray systemTray = SystemTray.getSystemTray();

		final TrayIcon icon = new TrayIcon(ICON_SMALL, VERSION, menu);

		icon.setImageAutoSize(true);

		// si se pulsa en el TrayIcon muestra la pantalla acerca de.
		icon.addMouseListener(
			new MouseAdapter() {
				@Override
				public void mouseClicked(final MouseEvent e) {
					if (e != null) {
						// clic izquierdo
						if (e.getButton() == 1) {
							aboutAutofirma();
						}

					}
				}
			}
		);

		try {
			systemTray.add(icon);
		}
		catch (final AWTException e) {
			LOGGER.severe("Ha ocurrido un error al ejecutar el TrayIcon: " + e); //$NON-NLS-1$
		}

	}

	@SuppressWarnings("resource")
	private static boolean isAfirmaTrayIconAlreadyRunning() {
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
        	LOGGER.warning("No se ha podido comprobar el bloqueo de instancia: " + e); //$NON-NLS-1$
            return false;
        }
    }

	/** Lanza la ejecuci&oacute;n de las opciones de configuraci&oacute;n de AutoFirma en el &aacute;rea de notificaciones.
	 * @param args No se usa. */
	public static void main(final String[] args) {
		if (isAfirmaTrayIconAlreadyRunning()) {
			System.exit(0);
		}
		else {
			initialize();
		}
	}
}