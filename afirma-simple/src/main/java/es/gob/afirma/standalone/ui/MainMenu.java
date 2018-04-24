/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.lang.reflect.Method;
import java.util.EventObject;
import java.util.logging.Logger;

import javax.swing.Box;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;
import es.gob.afirma.standalone.ui.hash.CheckHashDialog;
import es.gob.afirma.standalone.ui.hash.CheckHashFiles;
import es.gob.afirma.standalone.ui.hash.CreateHashDialog;
import es.gob.afirma.standalone.ui.hash.CreateHashFiles;
import es.gob.afirma.standalone.ui.preferences.PreferencesDialog;
import es.gob.afirma.standalone.ui.restoreconfig.RestoreConfigDialog;

/** Barra de men&uacute; para toda la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class MainMenu extends JMenuBar {

    private static final long serialVersionUID = -8361808353554036015L;

    private final JMenuItem firmarMenuItem;
    private final JMenuItem abrirMenuItem;

    private final JFrame parent;
    JFrame getParentComponent() {
    	return this.parent;
    }

    private final SimpleAfirma saf;
    SimpleAfirma getSimpleAfirma() {
    	return this.saf;
    }

    /** Construye la barra de men&uacute; de la aplicaci&oacute;n.
     * En MS-Windows y Linux se crean los siguientes atajos de teclado:
     * <ul>
     *  <li>Alt+A = Menu archivo</li>
     *  <li>
     *   <ul>
     *    <li>Ctrl+B = Abrir archivo</li>
     *    <li>Ctrl+I = Firmar archivo</li>
     *    <li>Ctrl+V = Ver firma</li>
     *    <li>Alt+F4 = Salir del programa</li>
     *   </ul>
     *  </li>
     *  <li>Alt+R = Menu herramientas</li>
     *  <li>
     *   <ul>
     *    <li>Ctrl+H = Calcular huella digital</li>
     *    <li>Ctrl+U = Comprobar huella digital</li>
     *    <li>Ctrl+D = Calcular huella digital de un directorio</li>
     *    <li>Ctrl+K = Comprobar huella digital de un directorio</li>
     *    <li>Ctrl+R = Restaurar instalaci&oacute;n</li>
     *    <li>Ctrl+P = Preferencias</li>
     *   </ul>
     *  </li>
     *  <li>Alt+Y = Menu Ayuda</li>
     *  <li>
     *   <ul>
     *    <li>F1 = Ayuda</li>
     *    <li>Alt+C = Acerca de...</li>
     *   </ul>
     *  </li>
     * </ul>
     * @param p Componente padre para la modalidad
     * @param s Aplicaci&oacute;n padre, para determinar el n&uacute;mero de
     *        locales e invocar a ciertos comandos de men&uacute; */
    public MainMenu(final JFrame p, final SimpleAfirma s) {
        this.saf = s;
        this.parent = p;

        this.firmarMenuItem = new JMenuItem();
        this.abrirMenuItem = new JMenuItem();

        // Importante: No cargar en un invokeLater, da guerra
        createUI();
    }

    private void createUI() {

        final boolean isMac = Platform.OS.MACOSX.equals(Platform.getOS());

        final JMenu menuArchivo = new JMenu();
        menuArchivo.setText(SimpleAfirmaMessages.getString("MainMenu.0")); //$NON-NLS-1$
        menuArchivo.setMnemonic(KeyEvent.VK_A);
        menuArchivo.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("MainMenu.1") //$NON-NLS-1$
        );
        menuArchivo.setEnabled(true);

        this.abrirMenuItem.setText(SimpleAfirmaMessages.getString("MainMenu.2")); //$NON-NLS-1$
        this.abrirMenuItem.setAccelerator(
        	KeyStroke.getKeyStroke(
        		KeyEvent.VK_A,
        		Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
        	)
        );
        this.abrirMenuItem.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("MainMenu.3") //$NON-NLS-1$
		);
        this.abrirMenuItem.addActionListener(
    		ae -> {
				final File fileToLoad;
				try {
					fileToLoad = AOUIFactory.getLoadFiles(
						SimpleAfirmaMessages.getString("MainMenu.4"), //$NON-NLS-1$
						null,
						null,
						null,
						null,
						false,
						false,
						AutoFirmaUtil.getDefaultDialogsIcon(),
						MainMenu.this
					)[0];
				}
				catch(final AOCancelledOperationException e) {
					return;
				}
				MainMenu.this.getSimpleAfirma().loadFileToSign(fileToLoad);
			}
		);
        menuArchivo.add(this.abrirMenuItem);

        this.firmarMenuItem.setText(SimpleAfirmaMessages.getString("MainMenu.5")); //$NON-NLS-1$
        this.firmarMenuItem.setAccelerator(
    		KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
		);
        this.firmarMenuItem.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("MainMenu.6") //$NON-NLS-1$
        );
        this.firmarMenuItem.setEnabled(false);
        this.firmarMenuItem.addActionListener(
    		e -> MainMenu.this.getSimpleAfirma().signLoadedFile()
		);
        menuArchivo.add(this.firmarMenuItem);

        final JMenuItem validateSignMenu = new JMenuItem(SimpleAfirmaMessages.getString("MainMenu.34")); //$NON-NLS-1$
        validateSignMenu.setAccelerator(
        		KeyStroke.getKeyStroke(KeyEvent.VK_V,
        				Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        validateSignMenu.getAccessibleContext().setAccessibleDescription(
        		SimpleAfirmaMessages.getString("MainMenu.34")); //$NON-NLS-1$
		validateSignMenu.setMnemonic(KeyEvent.VK_V);
		validateSignMenu.addActionListener(ae -> viewSignature(this));
		menuArchivo.add(validateSignMenu);


        final JMenu toolsMenu = new JMenu(
    		SimpleAfirmaMessages.getString("MainMenu.32") //$NON-NLS-1$
		);
        toolsMenu.setMnemonic(KeyEvent.VK_R);
        toolsMenu.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("MainMenu.33") //$NON-NLS-1$
        );
        toolsMenu.setEnabled(true);

        final JMenu huellaMenu = new JMenu(
    		SimpleAfirmaMessages.getString("MainMenu.25") //$NON-NLS-1$
		);

        final JMenu huellaDirMenu = new JMenu(
				SimpleAfirmaMessages.getString("MainMenu.31") //$NON-NLS-1$
		);
		huellaDirMenu.setMnemonic('D');

		final JMenu huellaFileMenu = new JMenu(
				SimpleAfirmaMessages.getString("MainMenu.30") //$NON-NLS-1$
		);
		huellaFileMenu.setMnemonic('F');

		final JMenuItem createHashFileMenuItem = new JMenuItem(
				SimpleAfirmaMessages.getString("MainMenu.26") //$NON-NLS-1$
		);
		createHashFileMenuItem.setMnemonic('a');
		createHashFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_H, Toolkit.getDefaultToolkit()
						.getMenuShortcutKeyMask()));
		createHashFileMenuItem.addActionListener(e -> CreateHashDialog.startHashCreation(getParentComponent())
		);
		huellaFileMenu.add(createHashFileMenuItem);

		final JMenuItem checkHashFileMenuItem = new JMenuItem(
				SimpleAfirmaMessages.getString("MainMenu.27") //$NON-NLS-1$
		);
		checkHashFileMenuItem.setMnemonic('o');
		checkHashFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_U, Toolkit.getDefaultToolkit()
						.getMenuShortcutKeyMask()));
		checkHashFileMenuItem.addActionListener(e -> CheckHashDialog.launch(getParentComponent())
		);

		final JMenuItem createHashDirMenuItem = new JMenuItem(
				SimpleAfirmaMessages.getString("MainMenu.28") //$NON-NLS-1$
		);
		createHashDirMenuItem.setMnemonic('a');
		createHashDirMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_D, Toolkit.getDefaultToolkit()
						.getMenuShortcutKeyMask()));
		createHashDirMenuItem.addActionListener(e -> CreateHashFiles.startHashCreation(getParentComponent()));

		final JMenuItem checkHashDirMenuItem = new JMenuItem(
				SimpleAfirmaMessages.getString("MainMenu.29") //$NON-NLS-1$
		);
		checkHashDirMenuItem.setMnemonic('o');
		checkHashDirMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_K, Toolkit.getDefaultToolkit()
						.getMenuShortcutKeyMask()));
		checkHashDirMenuItem.addActionListener(e -> CheckHashFiles.startHashCheck(getParentComponent()));

		huellaDirMenu.add(createHashDirMenuItem);
		huellaDirMenu.add(checkHashDirMenuItem);
		huellaFileMenu.add(createHashFileMenuItem);
		huellaFileMenu.add(checkHashFileMenuItem);
		huellaMenu.setMnemonic('H');
		huellaMenu.add(huellaFileMenu);
		huellaMenu.add(huellaDirMenu);

        if (!isMac) {
        	huellaMenu.setMnemonic('H');
        	createHashFileMenuItem.setMnemonic('l');
        }

        // En Mac OS X el salir lo gestiona el propio OS
        if (!isMac) {
        	menuArchivo.addSeparator();
			final JMenuItem salirMenuItem = new JMenuItem(
					SimpleAfirmaMessages.getString("MainMenu.7")); //$NON-NLS-1$
			salirMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F4,
					ActionEvent.ALT_MASK));
			salirMenuItem.getAccessibleContext().setAccessibleDescription(
					SimpleAfirmaMessages.getString("MainMenu.8") //$NON-NLS-1$
					);
			salirMenuItem.addActionListener(ae -> exitApplication()
    		);
            salirMenuItem.setMnemonic(KeyEvent.VK_L);
            menuArchivo.add(salirMenuItem);
        }

        this.add(menuArchivo);
        toolsMenu.add(huellaMenu);

		// Preparamos la opcion de menu para "restaurar la configuracion de los
		// navegadores" en el menu de herramientas

		final JMenuItem restoreConfigMenuItem = new JMenuItem(SimpleAfirmaMessages.getString("MainMenu.20")); //$NON-NLS-1$
		restoreConfigMenuItem.setAccelerator(
				KeyStroke.getKeyStroke(KeyEvent.VK_R, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
		restoreConfigMenuItem.setMnemonic(KeyEvent.VK_R);
		restoreConfigMenuItem.getAccessibleContext()
				.setAccessibleDescription(SimpleAfirmaMessages.getString("MainMenu.20") //$NON-NLS-1$
		);
		restoreConfigMenuItem.addActionListener(ae -> showRestoreConfig());

		toolsMenu.add(restoreConfigMenuItem);

		this.add(toolsMenu);

        if (!isMac) {
            final JMenuItem preferencesMenuItem = new JMenuItem(SimpleAfirmaMessages.getString("MainMenu.12")); //$NON-NLS-1$
            preferencesMenuItem.setAccelerator(
        		KeyStroke.getKeyStroke(KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
    		);
            preferencesMenuItem.setMnemonic(KeyEvent.VK_P);
            preferencesMenuItem.getAccessibleContext().setAccessibleDescription(
        		SimpleAfirmaMessages.getString("MainMenu.16") //$NON-NLS-1$
    		);
            preferencesMenuItem.addActionListener(
        		ae -> showPreferences()
    		);

            toolsMenu.addSeparator();
            toolsMenu.add(preferencesMenuItem);
        }
        // En Mac OS X el menu es "Preferencias" dentro de la opcion principal
        else {
        	try {
        		final Method showPreferencesMethod = getClass().getDeclaredMethod("showPreferences", new Class[]{EventObject.class}); //$NON-NLS-1$
        		OSXHandler.setPreferencesHandler(this, showPreferencesMethod);
        	}
        	catch(final Exception | Error e) {
        		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    				"No ha sido posible establecer el menu de preferencias de OS X: " + e //$NON-NLS-1$
				);
        	}
        }

        // Separador para que la ayuda quede a la derecha, se ignora en Mac OS X
        this.add(Box.createHorizontalGlue());

        final JMenu menuAyuda = new JMenu(SimpleAfirmaMessages.getString("MainMenu.9"));  //$NON-NLS-1$
        menuAyuda.setMnemonic(KeyEvent.VK_Y);
        menuAyuda.getAccessibleContext().setAccessibleDescription(
          SimpleAfirmaMessages.getString("MainMenu.10") //$NON-NLS-1$
        );

        final JMenuItem ayudaMenuItem = new JMenuItem();
        ayudaMenuItem.setText(SimpleAfirmaMessages.getString("MainMenu.11")); //$NON-NLS-1$
        ayudaMenuItem.setAccelerator(KeyStroke.getKeyStroke("F1")); //$NON-NLS-1$
        ayudaMenuItem.getAccessibleContext().setAccessibleDescription(
              SimpleAfirmaMessages.getString("MainMenu.13") //$NON-NLS-1$
        );
        ayudaMenuItem.addActionListener(
    		e -> SimpleAfirma.showHelp()
		);
        menuAyuda.add(ayudaMenuItem);

        // En Mac OS X el Acerca de lo gestiona el propio OS
        if (!isMac) {
            menuAyuda.addSeparator();
            final JMenuItem acercaMenuItem = new JMenuItem(SimpleAfirmaMessages.getString("MainMenu.15")); //$NON-NLS-1$
            acercaMenuItem.getAccessibleContext().setAccessibleDescription(
        		SimpleAfirmaMessages.getString("MainMenu.17") //$NON-NLS-1$
            );
            acercaMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
            acercaMenuItem.addActionListener(ae -> showAbout(MainMenu.this.getParentComponent() == null ? MainMenu.this : MainMenu.this.getParentComponent()));
            acercaMenuItem.setMnemonic(KeyEvent.VK_C);
            menuAyuda.add(acercaMenuItem);
            this.add(menuAyuda);
        }

        // Los mnemonicos en elementos de menu violan las normas de interfaz de Apple,
        // asi que prescindimos de ellos en Mac OS X
        if (!isMac) {
            this.abrirMenuItem.setMnemonic(KeyEvent.VK_B);
            ayudaMenuItem.setMnemonic(KeyEvent.VK_U);
            this.firmarMenuItem.setMnemonic(KeyEvent.VK_F);
        }
        // Acciones especificas de Mac OS X
        else {
        	try {
        		final Method aboutMethod = getClass().getDeclaredMethod("showAbout", new Class[]{EventObject.class}); //$NON-NLS-1$
        		OSXHandler.setAboutHandler(this, aboutMethod);
        	}
        	catch (final Exception e) {
        		e.printStackTrace();
			}

        	try {
        		final Method exitApplicationMethod = getClass().getDeclaredMethod("exitApplication", new Class[]{EventObject.class, Object.class}); //$NON-NLS-1$
        		OSXHandler.setQuitHandler(this, exitApplicationMethod);
        	}
        	catch (final Exception e) {
        		e.printStackTrace();
			}
        }
    }

    /** Habilita o deshabilita el men&uacute; de operaciones sobre ficheros.
     * @param en <code>true</code> para habilitar las operaciones sobre ficheros, <code>false</code> para deshabilitarlas */
    public void setEnabledOpenCommand(final boolean en) {
        if (this.abrirMenuItem != null) {
            this.abrirMenuItem.setEnabled(en);
        }
    }

    /** Habilita o deshabilita el elemento de men&uacute; de firma de fichero.
     * @param en <code>true</code> para habilitar el elemento de men&uacute; de firma de fichero, <code>false</code> para deshabilitarlo */
    public void setEnabledSignCommand(final boolean en) {
        if (this.firmarMenuItem != null) {
            this.firmarMenuItem.setEnabled(en);
        }
    }

    void showPreferences(final EventObject event) {
    	showPreferences();
    }

    void showPreferences() {
        PreferencesDialog.show(MainMenu.this.getParentComponent(), true);
    }

    void showRestoreConfig() {
    	RestoreConfigDialog.show(MainMenu.this.getParentComponent(), true);
    }

    @SuppressWarnings("unused")
	void showAbout(final EventObject event) {
    	showAbout(MainMenu.this.getParentComponent() == null ? MainMenu.this : MainMenu.this.getParentComponent());
    }

    /**
     * Permite la selecci&oacute;n de un fichero de firma y muestra su contenido en
     * el visor.
     * @param parentComponent Componente padre sobre el que mostrar el
     */
    static void viewSignature(Object parentComponent) {
    	final File[] file = AOUIFactory.getLoadFiles(
    			SimpleAfirmaMessages.getString("MainMenu.35"), //$NON-NLS-1$
    			null,
    			null,
    			new String[] {"csig", "xsig", "pdf", "sig", "p7s"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    			SimpleAfirmaMessages.getString("MainMenu.36"), //$NON-NLS-1$
    			false,
    			false,
    			null,
    			parentComponent);
    	if (file != null && file.length > 0 && file[0] != null && file[0].isFile()) {
    		new VisorFirma(false, null).initialize(false, file[0]);
    	}
    }

    /** Muestra en OS X el men&uacute; "Acerca de...".
     * @param parentComponent Componente padre para la modalidad. */
    public static void showAbout(final Component parentComponent) {
        AOUIFactory.showMessageDialog(
    		parentComponent,
			SimpleAfirmaMessages.getString("MainMenu.14", //$NON-NLS-1$
					SimpleAfirma.getVersion(),
					System.getProperty("java.version"), //$NON-NLS-1$,
					Platform.getJavaArch()),
            SimpleAfirmaMessages.getString("MainMenu.15"), //$NON-NLS-1$
            JOptionPane.PLAIN_MESSAGE
        );
    }

    @SuppressWarnings("unused")
	boolean exitApplication(final EventObject event, final Object response) {
    	return this.saf.askForClosing();
    }

    boolean exitApplication() {
        return this.saf.askForClosing();
    }

}
