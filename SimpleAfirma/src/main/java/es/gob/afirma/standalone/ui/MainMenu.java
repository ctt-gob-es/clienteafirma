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
		saf = s;
		parent = p;
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
		
		menuArchivo = new JMenu("Archivo");
		menuArchivo.setMnemonic(KeyEvent.VK_A);
		menuArchivo.getAccessibleContext().setAccessibleDescription(
			"Menú con operaciones de apertura y firma de archivos"
		);
		menuArchivo.setEnabled(true);
		
		final JMenuItem abrirMenuItem = new JMenuItem("Abrir archivo");
		abrirMenuItem.setAccelerator(
			KeyStroke.getKeyStroke(KeyEvent.VK_A,
		    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
	    );
		abrirMenuItem.getAccessibleContext().setAccessibleDescription(
			"Abre un fichero para permitir su posterior firma"
		);
		menuArchivo.add(abrirMenuItem);
		
		firmarMenuItem = new JMenuItem("Firmar archivo");
		firmarMenuItem.setAccelerator(
			KeyStroke.getKeyStroke(KeyEvent.VK_F,
		    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
	    );
		firmarMenuItem.getAccessibleContext().setAccessibleDescription(
			"Abre un fichero para permitir su posterior firma"
		);
		firmarMenuItem.setEnabled(false);
		menuArchivo.add(firmarMenuItem);
		
		menuArchivo.addSeparator();
		
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
				if (JOptionPane.showConfirmDialog(
					parent, 
					"¿Desea cerrar la aplicación de firma?",
					"Advertencia",
					JOptionPane.YES_NO_OPTION, 
					JOptionPane.WARNING_MESSAGE
				) == JOptionPane.YES_OPTION) {
					saf.closeApplication(0);
				}
			}
		});
		menuArchivo.add(salirMenuItem);

		JMenu menuOpciones = null;
		Locale[] locales = SimpleAfirma.getAvailableLocales();
		if (locales != null && locales.length > 1) {
		    menuOpciones = new JMenu("Opciones");
	        menuOpciones.setMnemonic(KeyEvent.VK_O);
	        menuOpciones.getAccessibleContext().setAccessibleDescription(
	            "Menú con las opciones configurables de la aplicación"
	        );
	        
	        final JMenu idiomaMenu = new JMenu("Idiomas");
	        idiomaMenu.getAccessibleContext().setAccessibleDescription(
	                "Listado de idiomas disponibles"
	        );
	        menuOpciones.add(idiomaMenu);
	        
	        ButtonGroup group = new ButtonGroup();
	        JMenuItem item;
	        for (final Locale locale : locales) {
	            String localeText = locale.getDisplayName(locale);
	            item = new JRadioButtonMenuItem(localeText.substring(0, 1).toUpperCase() +
	                    localeText.substring(1));
	            item.setSelected(locale.equals(SimpleAfirma.getDefaultLocale()));
	            item.addActionListener(new ActionListener() {
                    @Override public void actionPerformed(ActionEvent e) {
                        SimpleAfirma.setDefaultLocale(locale);
                    }
                });
	            group.add(item);
	            idiomaMenu.add(item);    
	        }
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
				JOptionPane.showMessageDialog(
					(parent == null) ? MainMenu.this : parent,
					"Firma f\u00E1cil con @firma V1.0",
					"Acerca de Firma f\u00E1cil con @firma",
					JOptionPane.INFORMATION_MESSAGE
				);
			}
		});
		menuAyuda.add(acercaMenuItem);
		
		this.add(menuArchivo);

		if (menuOpciones != null) {
		    this.add(menuOpciones);
		}
		
		// Separador para que la ayuda quede a la derecha
		this.add(Box.createHorizontalGlue());
		
		this.add(menuAyuda);
		
		// Los mnemonicos en elementos de menu violan las normas de interfaz de Apple,
		// asi que prescindimos de ellos en Mac OS X
		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			acercaMenuItem.setMnemonic(KeyEvent.VK_R);
			abrirMenuItem.setMnemonic(KeyEvent.VK_B);
			salirMenuItem.setMnemonic(KeyEvent.VK_L);
			ayudaMenuItem.setMnemonic(KeyEvent.VK_U);
			firmarMenuItem.setMnemonic(KeyEvent.VK_I);
		}
		
	}
	
	/**
	 * Habilita o deshabilita el men&uacute; de operaciones sobre ficheros.
	 * @param en <code>true</code> para habilitar las operaciones sobre ficheros,
	 *           <code>false</code> para deshabilitarlas
	 */
	public void setEnabledFileCommands(final boolean en) {
		if (menuArchivo != null) menuArchivo.setEnabled(en);
	}
	
	/**
	 * Habilita o deshabilita el elemento de men&uacute; de firma de fichero.
	 * @param en <code>true</code> para habilitar el elemento de men&uacute; de firma de fichero,
	 *           <code>false</code> para deshabilitarlo
	 */
	public void setEnabledSignCommand(final boolean en) {
		if (firmarMenuItem != null) firmarMenuItem.setEnabled(en);
	}
}
