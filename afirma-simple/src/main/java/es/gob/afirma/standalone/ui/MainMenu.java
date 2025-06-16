/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.lang.reflect.Method;
import java.util.EventObject;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.KeyStroke;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.LanguageManager;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.VisorFirma;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.plugins.GenericMenuOption;
import es.gob.afirma.standalone.plugins.manager.PluginException;
import es.gob.afirma.standalone.plugins.manager.PluginLoader;
import es.gob.afirma.standalone.ui.plugins.PluginsManagementDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesDialog;
import es.gob.afirma.standalone.ui.restoreconfig.RestoreConfigDialog;
import es.gob.afirma.ui.core.jse.JSEUIMessages;

/** Barra de men&uacute; para toda la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class MainMenu extends JMenuBar {

    private static final long serialVersionUID = -8361808353554036015L;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final JMenuItem abrirMenuItem;

    private final JFrame parent;
    public JFrame getParentComponent() {
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
     *    <li>Ctrl+V = Ver firma</li>
     *    <li>Alt+F4 = Salir del programa</li>
     *   </ul>
     *  </li>
     *  <li>Alt+R = Menu herramientas</li>
     *  <li>
     *   <ul>
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

        this.abrirMenuItem = new JMenuItem();

        // Importante: No cargar en un invokeLater, da guerra
        createUI();
    }

    private void createUI() {

        final boolean isMac = Platform.OS.MACOSX.equals(Platform.getOS());

        final JMenu menuArchivo = new JMenu();
        menuArchivo.setText(SimpleAfirmaMessages.getString("MainMenu.0")); //$NON-NLS-1$
        menuArchivo.setMnemonic(KeyEvent.VK_ALT);
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
						true,
						DesktopUtil.getDefaultDialogsIcon(),
						MainMenu.this
					)[0];
				}
				catch(final AOCancelledOperationException e) {
					return;
				}
				MainMenu.this.getSimpleAfirma().loadFileToSign(fileToLoad, null);
			}
		);
        menuArchivo.add(this.abrirMenuItem);

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

		// Preparamos la opcion de menu de la pantalla de plugins en el menu de herramientas

		final JMenuItem pluginsMenuItem = new JMenuItem(SimpleAfirmaMessages.getString("MainMenu.37")); //$NON-NLS-1$
		pluginsMenuItem.setAccelerator(
				KeyStroke.getKeyStroke(KeyEvent.VK_G, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
		pluginsMenuItem.setMnemonic(KeyEvent.VK_G);
		pluginsMenuItem.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("MainMenu.37")); //$NON-NLS-1$
		pluginsMenuItem.addActionListener(ae -> showPlugingManagement());

		toolsMenu.add(pluginsMenuItem);


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
        		final Method showPreferencesMethod = getClass().getDeclaredMethod("showPreferences", EventObject.class); //$NON-NLS-1$
        		OSXHandler.setPreferencesHandler(this, showPreferencesMethod);
        	}
        	catch(final Exception | Error e) {
        		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    				"No ha sido posible establecer el menu de preferencias de OS X: " + e //$NON-NLS-1$
				);
        	}
        }
        
        // Crear el JMenu que contendrá las opciones de idioma
        final JMenu languageMenu = new JMenu(SimpleAfirmaMessages.getString("MainMenu.38")); //$NON-NLS-1$
        
        final ButtonGroup langGroup = new ButtonGroup();

        // Crear las opciones de idioma
        final String[] defaultLanguages = {SimpleAfirmaMessages.getString("MainMenu.39"), SimpleAfirmaMessages.getString("MainMenu.40"),  //$NON-NLS-1$ //$NON-NLS-2$
        		SimpleAfirmaMessages.getString("MainMenu.41"), SimpleAfirmaMessages.getString("MainMenu.42"), //$NON-NLS-1$ //$NON-NLS-2$
        		SimpleAfirmaMessages.getString("MainMenu.43"), SimpleAfirmaMessages.getString("MainMenu.44")}; //$NON-NLS-1$ //$NON-NLS-2$
        
        String localeConf = PreferencesManager.get(PreferencesManager.PREFERENCES_LOCALE);
        // Si no hay ninguno configurado, por defcto estara seleccionado el espanol
        if (localeConf == null) {
        	localeConf = LanguageManager.AFIRMA_DEFAULT_LOCALES[0].toString();
        }
        
        for (int i = 0; i < defaultLanguages.length; i++) {
            final Locale locale = LanguageManager.AFIRMA_DEFAULT_LOCALES[i];
            final LocaleOption localeOption = new LocaleOption(defaultLanguages[i], locale);
            final JRadioButtonMenuItem langItem = new JRadioButtonMenuItem(localeOption.toString());
            langItem.putClientProperty("localeData", localeOption); //$NON-NLS-1$
            if (localeConf.equals(locale.toString())) {
            	langItem.setSelected(true);
            }
            langItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent ae) {
                	Locale.setDefault(locale);
                    PreferencesManager.put(PreferencesManager.PREFERENCES_LOCALE, locale.toString());
                    SimpleAfirmaMessages.changeLocale();
                    showRestartWarning();
                }
            });
            langGroup.add(langItem);
            languageMenu.add(langItem);
        }
        
        final Locale [] importedLocales = LanguageManager.getImportedLocales();
        if (importedLocales != null) {
	        for (int i = 0; i < importedLocales.length; i++) {
	        	final String langName = LanguageManager.getLanguageName(importedLocales[i]);
	        	final Locale locale = importedLocales[i];
	        	// Se comprueba si el idioma importado no se trata de una version actualizada
	        	// de un idioma proporcionado por Autofirma para que no se duplique en el menu
	        	if (!LanguageManager.isDefaultLocale(locale)) {
		            final LocaleOption localeOption = new LocaleOption(langName, locale);
		            final JRadioButtonMenuItem importedLangItem = new JRadioButtonMenuItem(localeOption.toString());
		            importedLangItem.putClientProperty("localeData", localeOption); //$NON-NLS-1$    
		        	if (localeConf.equals(locale.toString())) {
		        		importedLangItem.setSelected(true);
		            }
		        	importedLangItem.addActionListener(new ActionListener() {
		                @Override
		                public void actionPerformed(final ActionEvent ae) {
		                	Locale.setDefault(locale);
		                    PreferencesManager.put(PreferencesManager.PREFERENCES_LOCALE, locale.toString());
		                    SimpleAfirmaMessages.changeLocale();
		                    showRestartWarning();
		                }
		            });
		        	langGroup.add(importedLangItem);
		            languageMenu.add(importedLangItem);
	        	}
	        }
        }
        
        languageMenu.addSeparator();
        
        final JMenuItem importLanguageMenu = new JMenuItem(SimpleAfirmaMessages.getString("MainMenu.45")); //$NON-NLS-1$
        importLanguageMenu.getAccessibleContext().setAccessibleDescription(
        		SimpleAfirmaMessages.getString("MainMenu.45")); //$NON-NLS-1$
        importLanguageMenu.setAccelerator(
            	KeyStroke.getKeyStroke(
            		KeyEvent.VK_I,
            		Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
            	)
            );
        final String extsDesc = SimpleAfirmaMessages.getString("MainMenu.47") + " (*.zip)";   //$NON-NLS-1$//$NON-NLS-2$
        importLanguageMenu.addActionListener(ae -> {
			final File fileToLoad;
			try {
				fileToLoad = AOUIFactory.getLoadFiles(
					SimpleAfirmaMessages.getString("MainMenu.46"), //$NON-NLS-1$
					null,
					null,
					new String [] {"zip"}, //$NON-NLS-1$
					extsDesc,
					false,
					true,
					DesktopUtil.getDefaultDialogsIcon(),
					MainMenu.this
				)[0];
			}
			catch(final AOCancelledOperationException e) {
				return;
			}
			try {
				final Map <String, String> langProps = LanguageManager.addLanguage(fileToLoad);
				addNewLocaleToMenu(langProps, langGroup, languageMenu);
				AOUIFactory.showMessageDialog(
						null,
						SimpleAfirmaMessages.getString("MainMenu.52"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("MainMenu.45"), //$NON-NLS-1$
						JOptionPane.INFORMATION_MESSAGE);
			} catch (final AOException e1) {
				AOUIFactory.showErrorMessage(
						extractMessageFromException(e1), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e1);
			}
		});
        
        languageMenu.add(importLanguageMenu);
        this.add(languageMenu);

        // Comprobamos si existen menus adicionales de plugins que deben ser anadidos a la
        // barra de menu principal
        final List<GenericMenuOption> menus = PluginsUiComponentsBuilder.getPluginsMenus();
		if (menus != null && !menus.isEmpty()) {
			for (final GenericMenuOption menu : menus) {
				final String title = menu.getTitle();
				final JMenu jmenu = new JMenu(title);
				addSubMenus(jmenu, menu, getParentComponent());
				this.add(jmenu);
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
    		e -> SimpleAfirma.showHelp("Autofirma.html") //$NON-NLS-1$
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
            acercaMenuItem.addActionListener(ae -> AboutDialog.showAbout(MainMenu.this.getParentComponent() == null ? MainMenu.this : MainMenu.this.getParentComponent()));
            acercaMenuItem.setMnemonic(KeyEvent.VK_C);
            menuAyuda.add(acercaMenuItem);
        }
        this.add(menuAyuda);

        // Los mnemonicos en elementos de menu violan las normas de interfaz de Apple,
        // asi que prescindimos de ellos en Mac OS X
        if (!isMac) {
            this.abrirMenuItem.setMnemonic(KeyEvent.VK_B);
            ayudaMenuItem.setMnemonic(KeyEvent.VK_U);
        }
        // Acciones especificas de Mac OS X
        else {
        	try {
        		final Method aboutMethod = getClass().getDeclaredMethod("showAbout", EventObject.class); //$NON-NLS-1$
        		OSXHandler.setAboutHandler(this, aboutMethod);
        	}
        	catch (final Exception e) {
        		LOGGER.log(Level.WARNING, "Error al abrir la ayuda", e); //$NON-NLS-1$
			}

        	try {
        		final Method exitApplicationMethod = getClass().getDeclaredMethod("exitApplication", EventObject.class, Object.class); //$NON-NLS-1$
        		OSXHandler.setQuitHandler(this, exitApplicationMethod);
        	}
        	catch (final Exception e) {
        		LOGGER.log(Level.WARNING, "Error al cerrar la aplicacion", e); //$NON-NLS-1$
			}
        }
    }

    public static void addSubMenus(final JMenu jmenu, final GenericMenuOption menu, final JFrame parent) {
    	for (final GenericMenuOption subMenu : menu.getMenus()) {
    		final JMenu subJMenu = new JMenu(subMenu.getTitle());
    		if (subMenu.getMenus() != null && !subMenu.getMenus().isEmpty()) {
    			addSubMenus(subJMenu, subMenu, parent);
    		}
    		if (subMenu.getMenus() == null || subMenu.getMenus().isEmpty()) {
    			final JMenuItem leafItem = new JMenuItem(subMenu.getTitle());
    			leafItem.addActionListener(ev -> {
					try {
						doMenuAction(subMenu.getActionClassName(), parent);
					}
					catch (final Exception ex) {
						LOGGER.log(Level.SEVERE, "Se ha producido un error inesperado durante la ejecucion de la accion del plugin", ex); //$NON-NLS-1$
						AOUIFactory.showErrorMessage(
								SimpleAfirmaMessages.getString("SimpleAfirma.52"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
								JOptionPane.ERROR_MESSAGE,
								ex
								);
					}
				});
    			jmenu.add(leafItem);
    		} else {
    		jmenu.add(subJMenu);
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


    /**
     * M&eacute;todo para hacer aparecer el di&aacute;logo de preferencias en macOS mediante la opci&oacute;n
     * de men&uacute; del sistema operativo.
     * @param event Evento que desencadena la acci&oacute;n.
     */
	void showPreferences(final EventObject event) {
	    PreferencesDialog.show(getParentComponent(), true);
	}

    void showPreferences() {
        PreferencesDialog.show(getParentComponent(), true);
    }

    void showRestoreConfig() {
    	RestoreConfigDialog.show(getParentComponent(), true);
    }

    void showPlugingManagement() {
    	// Mostramos el dialogo de gestion de plugins y luego refrescamos los elimentos
    	// de los plugins en el panel actual
    	PluginsManagementDialog.show(getParentComponent(), true);
    	if (getSimpleAfirma().getCurrentPanel() instanceof PluginButtonsContainer) {
    		((PluginButtonsContainer) getSimpleAfirma().getCurrentPanel()).refreshPluginButtonsContainer();
    	}
    }

    /**
     * Permite la selecci&oacute;n de un fichero de firma y muestra su contenido en
     * el visor.
     * @param parentComponent Componente padre sobre el que mostrar el
     */
    static void viewSignature(final Object parentComponent) {
    	File[] file;
    	try {
    		file = AOUIFactory.getLoadFiles(
    				SimpleAfirmaMessages.getString("MainMenu.35"), //$NON-NLS-1$
    				null,
    				null,
    				new String[] {"csig", "xsig", "pdf", "sig", "p7s"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    				SimpleAfirmaMessages.getString("MainMenu.36"), //$NON-NLS-1$
    				false,
    				false,
    				null,
    				parentComponent);
    	}
    	catch (final AOCancelledOperationException e) {
    		// No hacemos nada
    		return;
    	}
    	if (file != null && file.length > 0 && file[0] != null && file[0].isFile()) {
    		new VisorFirma(false, null).initialize(false, file[0]);
    	}
    }


    /**
     * M&eacute;todo para hacer aparecer el di&aacute;logo "Acerca de" en macOS mediante la opci&oacute;n
     * de men&uacute; del sistema operativo.
     * @param event Evento que desencadena la acci&oacute;n.
     */
    void showAbout(final EventObject event) {
    	AboutDialog.showAbout(getParentComponent() == null ? this : getParentComponent());
    }

    /**
     * M&eacute;todo para salir de la aplicaci&oacute;n en macOS mediante la opci&oacute;n
     * de men&uacute; del sistema operativo.
     * @param event Evento que desencadena la acci&oacute;n.
     * @param response Respuesta que se debe devolver.
     * @return {@code true} si se acepta salir de la aplicaci&oacute;n, {@code false} en
     * caso contrario.
     */
	boolean exitApplication(final EventObject event, final Object response) {
    	return this.saf.askForClosing();
    }

	/**
     * M&eacute;todo para salir de la aplicaci&oacute;n mediante la opci&oacute;n
     * de men&uacute; del sistema operativo.
     * @return {@code true} si se acepta salir de la aplicaci&oacute;n, {@code false} en
     * caso contrario.
     */
    boolean exitApplication() {
        return this.saf.askForClosing();
    }

	/**
	 * M&eacute;todo que realiza la acci&oacute;n de men&uacute; definida en el
	 * fichero JSON de un plugin.
	 * @param actionClass Clase donde se define la acci&oacute;n a realizar.
	 * @param parent Componente padre sobre el que mostrar di&aacute;logos del plugin.
	 * @throws PluginException Cuando no se puede cargar la acci&oacute;n.
	 */
	public static void doMenuAction(final String actionClass, final Frame parent)
			throws PluginException{
		PluginLoader.getPluginAction(actionClass).start(parent);
	}
	
	private static void showRestartWarning() {
		final List<String> command = DesktopUtil.getResetApplicationCommand();
		// Actualizamos los mensajes para dialogo con el nuevo locale
		JSEUIMessages.updateLocale();
		// Ejecutamos una nueva instancia de la aplicacion
		if (command != null) {
			// Consultamos si se desea reiniciar la aplicacion
			final int option = AOUIFactory.showConfirmDialog(
					null,
					SimpleAfirmaMessages.getString("MainMenu.51"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("MainMenu.50"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (option == JOptionPane.YES_OPTION) {
				try {
					new ProcessBuilder(command).start();
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "No se ha podido arrancar la nueva instancia de la aplicacion", e); //$NON-NLS-1$
				}

				// Salimos de la aplicacion antes de que se llegue a cargar la nueva instancia
				System.exit(0);
			}
		}
		// Pedimos al usuario que reinicie la aplicacion
		else {
			AOUIFactory.showMessageDialog(
					null,
					SimpleAfirmaMessages.getString("MainMenu.49"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("MainMenu.50"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}
	}
	
	private static void addNewLocaleToMenu(final Map <String, String> langProps, final ButtonGroup langGroup, final JMenu languageMenu) {
		
		final String localeProp = langProps.get(LanguageManager.LOCALE_PROP);
		final String[] parts = localeProp.split("_"); //$NON-NLS-1$
		final Locale locale = new Locale(parts[0], parts[1]);
		
		if (!LanguageManager.isDefaultLocale(locale)) { 
			
			final LocaleOption localeOption = new LocaleOption(langProps.get(LanguageManager.LANGUAGE_NAME_PROP), locale);
			final JRadioButtonMenuItem newImportedLangItem = new JRadioButtonMenuItem(localeOption.toString());
			newImportedLangItem.putClientProperty("localeData", localeOption); //$NON-NLS-1$
			
			newImportedLangItem.addActionListener(new ActionListener() {
	            @Override
	            public void actionPerformed(final ActionEvent ae) {
	            	Locale.setDefault(locale);
	                PreferencesManager.put(PreferencesManager.PREFERENCES_LOCALE, locale.toString());
	                SimpleAfirmaMessages.changeLocale();
	                showRestartWarning();
	            }
	        });
		
		
    		langGroup.add(newImportedLangItem);
    		languageMenu.insert(newImportedLangItem, languageMenu.getItemCount() - 2);
		}
        
	}
	
	private static String extractMessageFromException(final Throwable t) {
		String message = null;
		
		if (t instanceof AOException) {
			message = obtainSimpleErrorCodeFromPluginException((AOException) t);
		} else {
			message = SimpleAfirmaMessages.getString("PluginManagementError.0");
		}
		return message;
	}
	
	private static String obtainSimpleErrorCodeFromPluginException(final AOException exception) {
		ErrorCode errorCode = null;
		
		//Check again the code received in the AOException
	    switch (exception.getErrorCode().getCode()) {
	    	case "230000": errorCode = SimpleErrorCode.Internal.GENERIC_LANGUAGE_IMPORT_ERROR; break;
	        case "230001": errorCode = SimpleErrorCode.Internal.CANT_READ_FILE; break;
	        case "230002": errorCode = SimpleErrorCode.Internal.CANT_CREATE_DIRECTORY; break;
	        default:       errorCode = SimpleErrorCode.Internal.GENERIC_LANGUAGE_IMPORT_ERROR; break;
	    }
	    
	    String message = null;
	    
	    int index;
	    try {
	        int codeInt = Integer.parseInt(errorCode.getCode());
	        int base    = Integer.parseInt(SimpleErrorCode.Internal.GENERIC_LANGUAGE_IMPORT_ERROR.getCode());
	        index = codeInt - base;
	        if (index < 0) {
	            index = 0;
	        }
	    }
	    catch (final Exception ex) {
	        index = 0;
	    }
	    String key = "LanguageManagementError." + index;

	    // Intentamos obtener el mensaje, si no existe usamos el por defecto (0)
	    try {
	        message = SimpleAfirmaMessages.getString(key);
	    }
	    catch (final MissingResourceException mre) {
	        message = SimpleAfirmaMessages.getString("LanguageManagementError.0");
	    }

	    return message;
	}
}
