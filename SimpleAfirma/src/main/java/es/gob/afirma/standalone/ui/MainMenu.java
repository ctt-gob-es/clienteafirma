package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.ButtonGroup;
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
import es.gob.afirma.standalone.SimpleAfirma;

/**
 * Barra de men&uacute; para toda la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class MainMenu extends JMenuBar {

	private static final long serialVersionUID = -8361808353554036015L;
	
	private JMenu menuArchivo;
	private JMenuItem firmarMenuItem;
	
	private final Component parent;
	private final SimpleAfirma saf;
	
	/**
	 * Construye la barra de men&uacute; de la aplicaci&oacute;n.
	 * @param p Componente padre para la modalidad
	 * @param s Aplicaci&oacute;n padre, para determinar el n&uacute;mero de
	 *            locales
	 */
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
		
		this.menuArchivo = new JMenu("Archivo");
		this.menuArchivo.setMnemonic(KeyEvent.VK_A);
		this.menuArchivo.getAccessibleContext().setAccessibleDescription(
			"Menú con operaciones de apertura y firma de archivos"
		);
		this.menuArchivo.setEnabled(true);
		
		final JMenuItem abrirMenuItem = new JMenuItem("Abrir archivo");
		abrirMenuItem.setAccelerator(
			KeyStroke.getKeyStroke(KeyEvent.VK_A,
		    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
	    );
		abrirMenuItem.getAccessibleContext().setAccessibleDescription(
			"Abre un fichero para permitir su posterior firma"
		);
		this.menuArchivo.add(abrirMenuItem);
		
		this.firmarMenuItem = new JMenuItem("Firmar archivo");
		this.firmarMenuItem.setAccelerator(
			KeyStroke.getKeyStroke(KeyEvent.VK_F,
		    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
	    );
		this.firmarMenuItem.getAccessibleContext().setAccessibleDescription(
			"Abre un fichero para permitir su posterior firma"
		);
		this.firmarMenuItem.setEnabled(false);
		this.menuArchivo.add(this.firmarMenuItem);
		
		// En Mac OS X el salir lo gestiona el propio OS
		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			this.menuArchivo.addSeparator();
			final JMenuItem salirMenuItem = new JMenuItem("Salir de @firma");
			salirMenuItem.setAccelerator(
				KeyStroke.getKeyStroke(KeyEvent.VK_F4, ActionEvent.ALT_MASK)
			);
			salirMenuItem.getAccessibleContext().setAccessibleDescription(
				"Abre un fichero para permitir su posterior firma"
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
		
		final JMenu menuAyuda = new JMenu("Ayuda");
		menuAyuda.setMnemonic(KeyEvent.VK_Y);
		menuAyuda.getAccessibleContext().setAccessibleDescription(
			"Menú con operaciones relacionadas con la ayuda de la aplicación"
		);
		
		final JMenuItem ayudaMenuItem = new JMenuItem("Ayuda de @firma");
		ayudaMenuItem.setAccelerator(KeyStroke.getKeyStroke("F1"));
		ayudaMenuItem.getAccessibleContext().setAccessibleDescription(
			"Carga la ayuda de la aplicación en una nueva ventana"
		);
		ayudaMenuItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				System.out.println("Carga de la ayuda desde menu");
			}
		});
		menuAyuda.add(ayudaMenuItem);
		
		
		// En Mac OS X el Acerca de lo gestiona el propio OS
		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			menuAyuda.addSeparator();
			final JMenuItem acercaMenuItem = new JMenuItem("Acerca de Firma Fácil con @firma");
			acercaMenuItem.setAccelerator(KeyStroke.getKeyStroke("F1"));
			acercaMenuItem.getAccessibleContext().setAccessibleDescription(
				"Muestra información adicional acera de Firma Fácil con @firma"
			);
			acercaMenuItem.setAccelerator(
				KeyStroke.getKeyStroke(KeyEvent.VK_R,
			    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
		    );
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
			    final JMenu menuOpciones = new JMenu("Opciones");
		        menuOpciones.setMnemonic(KeyEvent.VK_O);
		        menuOpciones.getAccessibleContext().setAccessibleDescription(
		            "Menú con las opciones configurables de la aplicación"
		        );
		        
		        final JMenu idiomaMenu = new JMenu("Idiomas");
		        idiomaMenu.getAccessibleContext().setAccessibleDescription(
		                "Listado de idiomas disponibles"
		        );
		        menuOpciones.add(idiomaMenu);
		        
		        final ButtonGroup group = new ButtonGroup();
		        JMenuItem item;
		        for (final Locale locale : locales) {
		            final String localeText = locale.getDisplayName(locale);
		            item = new JRadioButtonMenuItem(localeText.substring(0, 1).toUpperCase() +
		                    localeText.substring(1));
		            item.setSelected(locale.equals(this.saf.getDefaultLocale()));
		            item.addActionListener(new ActionListener() {
	                    @Override public void actionPerformed(final ActionEvent ae) {
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
						for(int i=0;i<locales.length;i++) {
							final String localeText = locales[i].getDisplayName(locales[i]);
							localesDescs[i] = localeText.substring(0, 1).toUpperCase() + localeText.substring(1);
						}
						final Object o = JOptionPane.showInputDialog(
							MainMenu.this.parent,
							"Seleccione el idioma para la aplicaci—n",
							"Preferencias de idioma",
							JOptionPane.PLAIN_MESSAGE,
							null,
							localesDescs,
							null
						);
						for (int i=0;i<locales.length;i++) {
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
	
	/**
	 * Habilita o deshabilita el men&uacute; de operaciones sobre ficheros.
	 * @param en <code>true</code> para habilitar las operaciones sobre ficheros,
	 *           <code>false</code> para deshabilitarlas
	 */
	public void setEnabledFileCommands(final boolean en) {
		if (this.menuArchivo != null) this.menuArchivo.setEnabled(en);
	}
	
	/**
	 * Habilita o deshabilita el elemento de men&uacute; de firma de fichero.
	 * @param en <code>true</code> para habilitar el elemento de men&uacute; de firma de fichero,
	 *           <code>false</code> para deshabilitarlo
	 */
	public void setEnabledSignCommand(final boolean en) {
		if (this.firmarMenuItem != null) this.firmarMenuItem.setEnabled(en);
	}
	
	private void showAbout() {
		JOptionPane.showMessageDialog(
			(this.parent == null) ? MainMenu.this : this.parent,
			"Firma f\u00E1cil con @firma V1.0",
			"Acerca de Firma f\u00E1cil con @firma",
			JOptionPane.INFORMATION_MESSAGE
		);
	}
	
	private boolean exitApplication() {
		if (JOptionPane.showConfirmDialog(
			this.parent, 
			"¿Desea cerrar la aplicación de firma?",
			"Advertencia",
			JOptionPane.YES_NO_OPTION, 
			JOptionPane.WARNING_MESSAGE
		) == JOptionPane.YES_OPTION) {
			this.saf.closeApplication(0);
			return true;
		}
		return false;
	}
	
}
