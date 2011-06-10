package es.gob.afirma.standalone;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
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
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.standalone.dnie.DNIeManager;
import es.gob.afirma.standalone.dnie.DNIeManagerException;
import es.gob.afirma.standalone.ui.DNIeWaitPanel;
import es.gob.afirma.standalone.ui.MainMenu;
import es.gob.afirma.standalone.ui.MainScreen;
import es.gob.afirma.standalone.ui.SignDetailPanel;
import es.gob.afirma.standalone.ui.SignPanel;

/**
 * Aplicaci&oacute;n gr&aacute;fica de firma electr&oacute;nica f&aacute;cil basada en @firma.
 * C&oacute;digos de salida de la aplicaci&oacute;n:
 * <ul>
 *  <li>-1 - Error en el subsistema de tarjetas e imposibilidad de abrir el almac&eacute;n local por defecto</li>
 *  <li>-2 - Imposibilidad de abrir el almac&eacute;n local por defecto</li>
 * </ul>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class SimpleAfirma extends JApplet implements PropertyChangeListener, 
                                                           ActionListener, 
                                                           KeyListener, 
                                                           WindowListener {

    /** Clave de la preferencia para el guardado del idioma por defecto. */ 
    public static final String PREFERENCES_LOCALE = "default.locale";
    
	/** Modo de depuraci&oacute;n para toda la aplicaci&oacute;n. */
	public static final boolean DEBUG = true;
	
	private static final long serialVersionUID = 9146759318663175997L;

    /** Preferencias generales establecidas para el aplicativo. */
    private Preferences preferences;
	
	private JFrame window;
	private Container container;
	
	private JPanel currentPanel;
	
	/** Color de fondo por defecto para los JPanel, JFrame y Applet. */
	public static final Color WINDOW_COLOR = new Color(UIManager.getColor("window").getRGB());
	
	/** Construye la aplicaci&oacute;n principal y establece el <i>Look&Field</i>. */
	public SimpleAfirma() {
		setLookAndFeel();
	}
	
	private AOKeyStoreManager ksManager;
	
	synchronized void setKeyStoreManager(final AOKeyStoreManager ksm) {
		Logger.getLogger("es.gob.afirma").info("Establecido KeyStoreManager: " + ksm);
		if (ksm != null) this.ksManager = ksm;
	}
	
	private void initialize(final boolean asApplet) {
	    
	    // Cargamos las preferencias establecidas
	    this.preferences = Preferences.userNodeForPackage(SimpleAfirma.class);
	    setDefaultLocale(buildLocale(this.preferences.get(PREFERENCES_LOCALE, Locale.getDefault().toString())));
	    
		// Una excepcion en un constructor no siempre deriva en un objeto nulo, por
		// eso usamos un booleano para ver si fallo, en vez de una comprobacion de
		// igualdad a null
		boolean showDNIeScreen = true;
		DNIeManager dniManager = null;
		try {
			dniManager = new DNIeManager(this);
		}
		catch(final DNIeManagerException e) {
			Logger.getLogger("es.gob.afirma").warning(
				"Se omite la pantalla de insercion de DNIe: " + e
			);
			showDNIeScreen = false;
		}
		if (asApplet) {
			this.container = this;
		}
		else {
			this.currentPanel = new DNIeWaitPanel(this, this, this);
			this.container = new MainScreen(this, this.currentPanel);
			this.window = (JFrame) this.container;
		}
		if (showDNIeScreen && dniManager != null) {
			dniManager.waitForDnie();
		}
		closeAndContinue();
	}
	
	/**
	 * Punto de entrada de la aplicaci&oacute;n.
	 * @param args Par&aacute;metros en l&iacute;nea de comandos
	 */
	public static void main(final String[] args) {
		new SimpleAfirma().initialize(false);
	}

	@Override
	public void propertyChange(final PropertyChangeEvent evt) {
		if (DNIeManager.BLOWN_DNI_INSERTED.equals(evt.getPropertyName())) {
			closeAndContinue();
			JOptionPane.showOptionDialog(
				this.container, 
				Messages.getString("SimpleAfirma.4"), //$NON-NLS-1$
				Messages.getString("SimpleAfirma.3"), //$NON-NLS-1$,
				JOptionPane.OK_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,
				new Object[] { "Cerrar "},
				null
			);
			return;
		}
		else if (DNIeManager.CARD_EXCEPTION.equals(evt.getPropertyName())) {
			try {
				new SimpleKeyStoreManagerWorker(this, null, false).execute();
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"Fallo la JSR-268, y no se pudo abrir el almacen por defecto del entorno operativo: " + e //$NON-NLS-1$
				);
				JOptionPane.showOptionDialog(
					this.container, 
					Messages.getString("SimpleAfirma.8"), //$NON-NLS-1$
					Messages.getString("SimpleAfirma.3"), //$NON-NLS-1$
					JOptionPane.OK_OPTION,
					JOptionPane.ERROR_MESSAGE,
					null,
					new Object[] { "Cerrar "},
					null
				);
				closeApplication(-1);
			}
			return;
		}
		else if (DNIeManager.NOT_DNI_INSERTED.equals(evt.getPropertyName())) {
			JOptionPane.showOptionDialog(
				this.container, 
				Messages.getString("SimpleAfirma.10"), //$NON-NLS-1$
				Messages.getString("SimpleAfirma.11"), //$NON-NLS-1$
				JOptionPane.OK_OPTION,
				JOptionPane.WARNING_MESSAGE,
				null,
				new Object[] { "Cerrar "},
				null
			);
			return;
		}
		else if (DNIeManager.DNI_INSERTED.equals(evt.getPropertyName())) {
			this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
			try {
				new SimpleKeyStoreManagerWorker(this, null, true).execute();
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"Fallo la inicializacion del DNIe, se intentara el almacen por defecto del sistema: " + e //$NON-NLS-1$
				);
				closeAndContinue();
			}
			finally {
				this.container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
			this.container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			loadMainApp();
		}

	}
	
	private void loadMainApp() {
		if (this.window != null)  {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				try {
					com.apple.eawt.Application.getApplication().setDefaultMenuBar(new MainMenu(this.window, this));
				}
				catch(final Exception e) {
					Logger.getLogger("es.gob.afirma").warning("No se ha podido establecer el menu de Mac OS X, se usara una barra de menu convencional: " + e);
					this.window.setJMenuBar(new MainMenu(this.window, this));
				}
			}
			else {
				this.window.setJMenuBar(new MainMenu(this.window, this));
			}
		}
		final JPanel newPanel = new SignPanel(
			this.window, 
			this
		); 
		this.container.add(newPanel, BorderLayout.CENTER);
		if (this.currentPanel != null) {
			this.currentPanel.setVisible(false);
			this.currentPanel.setFocusable(false);
			this.currentPanel.setEnabled(false);
		}
		this.container.repaint();
		this.currentPanel = newPanel;
	}

	private void setLookAndFeel() {
		
		UIManager.put("Button.defaultButtonFollowsFocus", Boolean.TRUE);
		UIManager.put("OptionPane.background", SimpleAfirma.WINDOW_COLOR);

		JFrame.setDefaultLookAndFeelDecorated(true);
		JDialog.setDefaultLookAndFeelDecorated(true);
		
		// Propiedades especificas para Mac OS X
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			System.setProperty("apple.awt.brushMetalLook", "true");
			System.setProperty("apple.awt.antialiasing", "true");
			System.setProperty("apple.awt.textantialiasing", "true");
			System.setProperty("apple.awt.rendering", "quality");
			System.setProperty("apple.awt.graphics.EnableQ2DX", "true");
			System.setProperty("apple.awt.graphics.EnableDeferredUpdates", "true");
			System.setProperty("apple.laf.useScreenMenuBar", "true");
		}
		else {
			try {
			    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
			        if ("Nimbus".equals(info.getName())) {
			            UIManager.setLookAndFeel(info.getClassName());
			            Logger.getLogger("es.gob.afirma").info(
		            		"Establecido 'Look&Feel' Nimbus"
	            		);
			            return;
			        }
			    }
			} 
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning(
					"No se ha podido establecer el 'Look&Feel' Nimbus: " + e
				);
			}
		}
		
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			Logger.getLogger("es.gob.afirma").info(
				"Establecido 'Look&Feel' " + UIManager.getLookAndFeel().getName()
			);
		} 
		catch (final Exception e2) {
			Logger.getLogger("es.gob.afirma").warning(
				"No se ha podido establecer ningun 'Look&Feel': " + e2
			);
		}

	}
	
	private void closeAndContinue() {
		this.container.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		try {
			new SimpleKeyStoreManagerWorker(this, null, false).execute();
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se pudo abrir el almacen por defecto del entorno operativo: " + e //$NON-NLS-1$
			);
			JOptionPane.showOptionDialog(
				this.container, 
				Messages.getString("SimpleAFirma.16"), //$NON-NLS-1$    //TODO: Falta la clave
				Messages.getString("SimpleAFirma.3"), //$NON-NLS-1$     //TODO: Falta la clave
				JOptionPane.CLOSED_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,
				new Object[] { "Cerrar "},
				null
			);
			closeApplication(-2);
		}
		finally {
			this.container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		loadMainApp();
	}

	@Override
	public void actionPerformed(final ActionEvent ae) {
		closeAndContinue();
	}
	
	@Override
	public void keyPressed(final KeyEvent ke) {
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
			closeAndContinue();
		}
		else if (ke != null && ke.getKeyCode() == KeyEvent.VK_F1) {
			System.out.println("Carga de la ayuda: " + this.currentPanel.getClass() + ", " + getLocale());
		}
	}
	
	@Override
	public void windowClosing(final WindowEvent we) {
		if (JOptionPane.showConfirmDialog(
			this.container, 
			"ÀDesea cerrar la aplicaci\u00F3n de firma?",
			"Advertencia",
			JOptionPane.YES_NO_OPTION, 
			JOptionPane.WARNING_MESSAGE
		) == JOptionPane.YES_OPTION) {
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
	
	/**
	 * Cierra la aplicaci&oacute;n.
	 * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo indica
	 *                 error y cero indica salida normal
	 */
	public void closeApplication(final int exitCode) {
		if (this.window != null) this.window.dispose();
		System.exit(exitCode);
	}

	/**
	 * Obtiene el <code>AOKeyStoreManager</code> en uso en la aplicaci&oacute;n.
	 * @return <code>AOKeyStoreManager</code> en uso en la aplicaci&oacute;n
	 */
	public synchronized AOKeyStoreManager getAOKeyStoreManager() {
		return this.ksManager;
	}

	/**
	 * Elimina el panel actual y carga el panel de resultados de firma.
	 * @param sign Firma o fichero firmado sobre el que queremos mostrar un resumen
	 * @param fileName Nombre del fichero en el que se ha guardado la firma o el fichero firmado
	 * @param signingCert Certificado usado para la firma
	 */
	public void loadResultsPanel(final byte[] sign, final String fileName, final X509Certificate signingCert) {
		final JPanel newPanel = new SignDetailPanel(sign, fileName, signingCert, SignDetailPanel.SIGN_DETAIL_TYPE.GENERATED, null);
		this.container.add(newPanel, BorderLayout.CENTER);
		if (this.window != null && fileName != null) {
			this.window.getRootPane().putClientProperty("Window.documentFile", new File(fileName));
			this.window.setTitle(this.window.getTitle().substring(0, this.window.getTitle().indexOf('-')) + "- " + new File(fileName).getName());
		}
		if (this.currentPanel != null) {
			this.currentPanel.setVisible(false);
			this.currentPanel.setFocusable(false);
			this.currentPanel.setEnabled(false);
		}
		this.container.repaint();
		this.currentPanel = newPanel;
		this.window.repaint();
	}
	
	
	private Locale buildLocale(final String locale) {
	    final String[] frags = locale.split("_");
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
	
	/**
	 * Listado de localizaciones soportadas por la aplicaci&oacute;n.
	 */
	private static Locale[] locales = new Locale[] {
	    Locale.getDefault(),
	    new Locale("en")
	};
	
	/**
	 * Obtiene los idiomas disponibles para la aplicaci&oacute;n
	 * @return Locales disponibles para la aplicaci&oacute;n
	 */
	public static Locale[] getAvailableLocales() {
		return locales;
	}
	
	/**
	 * Establece el idioma de la aplicaci&oacute;n.
	 * @param l Locale a establecer
	 */
	public void setDefaultLocale(final Locale l) {
		if (l != null) {
		    Locale.setDefault(l);
		    setPreference(PREFERENCES_LOCALE, l.toString());
		    Messages.changeLocale();
		}
	}
	
	/**
	 * Devuelve el idioma actualmente establecido en la aplicaci&oacute;n.
	 * @return Locale actual de la aplicaci&oacute;n
	 */
	public Locale getDefaultLocale() {
		return Locale.getDefault();
	}
	
	/**
	 * Recupera una de las preferencias establecidas para la aplicaci&oacute;n.
	 * @param key Clave de la preferencia.
	 * @param defaultValue Valor por defecto.
	 * @return Devuelve el valor de la preferencia indicada o {@code defaultValue} si no est&aacute;a establecida.
	 */
	public String getPreference(final String key, final String defaultValue) {
	    return this.preferences.get(key, defaultValue);
	}
	
	/**
	 * Establece una preferencia para la aplicaci&oacute;n.
	 * @param key Clave de la preferencia.
	 * @param value Valor asignado.
	 */
	public void setPreference(final String key, final String value) {
        this.preferences.put(key, value);
    }
	
	//***********************************************
	//***** APLICACION COMO APPLET ******************
	//***********************************************

	private void createUI() {
		this.setBackground(SimpleAfirma.WINDOW_COLOR);
		this.setSize(new Dimension(700,500));
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

	//***********************************************
	//***** FIN APLICACION COMO APPLET **************
	//***********************************************
	
}
