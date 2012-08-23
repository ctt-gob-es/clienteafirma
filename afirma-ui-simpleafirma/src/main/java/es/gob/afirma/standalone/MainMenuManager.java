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

import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.KeyStroke;

import es.gob.afirma.standalone.ui.MainMenu;

/** Implementa en <code>MainMenu</code> la funcionalidad normal de los men&uacute;s de Windows respceto al uso de las
 *  teclas Alt, Escape y flechas de cursor, que <code>JMenuBar</code> no respeta. No establecer en Mac OS X, y hacer
 *  una &uacute;nica llamada a <code>setMenuManagement()</code> a lo largo de la vida de <code>MainMenu</code>
 * <p>IMPORTANTE:<br>
 * Esta clase esta ligada a la situaci&oacute;n actual de MainMenu con dos men&uacute;s &uacute;nicamente en el primer
 * nivel (ordinales 0 y 2).<br>
 * Si se a&ntilde;adiesen m&aacute;s men&uacute;s habr&iacute;a que modificar el comportamiento manualmente.</p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class MainMenuManager {

	private static final int INDEX_MENU_FILE = 0;
	private static final int INDEX_MENU_OPTIONS = 1;
	private static final int INDEX_MENU_HELP = 3;

    private MainMenuManager() {
        // No permitimos la instanciacion
    }

	static void setMenuManagement(final ActionMap actionMap, final InputMap inputMap, final MainMenu mMenu) {

    	final String cancelMenu = "cancel_menu"; //$NON-NLS-1$
    	actionMap.put(cancelMenu, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (mMenu.getMenu(INDEX_MENU_FILE).isSelected()) {
					mMenu.getMenu(INDEX_MENU_FILE).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else if (mMenu.getMenu(INDEX_MENU_OPTIONS).isSelected()) {
					mMenu.getMenu(INDEX_MENU_OPTIONS).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else if (mMenu.getMenu(INDEX_MENU_HELP).isSelected()) {
					mMenu.getMenu(INDEX_MENU_HELP).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
			}
    	});
    	final String moveSelectedMenuRight = "move_menu_selection_right"; //$NON-NLS-1$
    	actionMap.put(moveSelectedMenuRight, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (mMenu.getMenu(INDEX_MENU_FILE).isSelected() && !mMenu.getMenu(INDEX_MENU_FILE).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_FILE).setSelected(false);
					mMenu.getMenu(INDEX_MENU_OPTIONS).setSelected(true);
					mMenu.getMenu(INDEX_MENU_OPTIONS).requestFocus();
				}
				else if (mMenu.getMenu(INDEX_MENU_OPTIONS).isSelected() && !mMenu.getMenu(INDEX_MENU_OPTIONS).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_OPTIONS).setSelected(false);
					mMenu.getMenu(INDEX_MENU_HELP).setSelected(true);
					mMenu.getMenu(INDEX_MENU_HELP).requestFocus();
				}
				else if (mMenu.getMenu(INDEX_MENU_HELP).isSelected() && !mMenu.getMenu(INDEX_MENU_HELP).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_HELP).setSelected(false);
					mMenu.getMenu(INDEX_MENU_FILE).setSelected(true);
					mMenu.getMenu(INDEX_MENU_FILE).requestFocus();
				}
			}
    	});
    	final String moveSelectedMenuLeft = "move_menu_selection_left"; //$NON-NLS-1$
    	actionMap.put(moveSelectedMenuLeft, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (mMenu.getMenu(INDEX_MENU_FILE).isSelected() && !mMenu.getMenu(INDEX_MENU_FILE).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_FILE).setSelected(false);
					mMenu.getMenu(INDEX_MENU_HELP).setSelected(true);
					mMenu.getMenu(INDEX_MENU_HELP).requestFocus();
				}
				else if (mMenu.getMenu(INDEX_MENU_OPTIONS).isSelected() && !mMenu.getMenu(INDEX_MENU_OPTIONS).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_OPTIONS).setSelected(false);
					mMenu.getMenu(INDEX_MENU_FILE).setSelected(true);
					mMenu.getMenu(INDEX_MENU_FILE).requestFocus();
				}
				else if (mMenu.getMenu(INDEX_MENU_HELP).isSelected() && !mMenu.getMenu(INDEX_MENU_HELP).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_HELP).setSelected(false);
					mMenu.getMenu(INDEX_MENU_OPTIONS).setSelected(true);
					mMenu.getMenu(INDEX_MENU_OPTIONS).requestFocus();
				}
			}
    	});
    	final String expandMenuActionKey = "expand_menu"; //$NON-NLS-1$
    	actionMap.put(expandMenuActionKey, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
    			if (mMenu.getMenu(INDEX_MENU_FILE).isSelected() && !mMenu.getMenu(INDEX_MENU_FILE).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_FILE).doClick();
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_DOWN);
						robot.keyRelease(KeyEvent.VK_DOWN);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
    			else if (mMenu.getMenu(INDEX_MENU_OPTIONS).isSelected() && !mMenu.getMenu(INDEX_MENU_OPTIONS).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_OPTIONS).doClick();
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_DOWN);
						robot.keyRelease(KeyEvent.VK_DOWN);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
    			else if (mMenu.getMenu(INDEX_MENU_HELP).isSelected() && !mMenu.getMenu(INDEX_MENU_HELP).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_HELP).doClick();
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_DOWN);
						robot.keyRelease(KeyEvent.VK_DOWN);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
    		}
    	});
    	final String selectMenuActionKey = "first_select"; //$NON-NLS-1$
    	actionMap.put(selectMenuActionKey, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (mMenu.getMenu(INDEX_MENU_FILE).isSelected() && !mMenu.getMenu(INDEX_MENU_FILE).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_FILE).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else if (mMenu.getMenu(INDEX_MENU_OPTIONS).isSelected() && !mMenu.getMenu(INDEX_MENU_OPTIONS).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_OPTIONS).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else if (mMenu.getMenu(INDEX_MENU_HELP).isSelected() && !mMenu.getMenu(INDEX_MENU_HELP).getPopupMenu().isVisible()) {
					mMenu.getMenu(INDEX_MENU_HELP).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				// Deberiamos cerrar con un ESC de robot al pulsar la tecla ALT, pero no podemos distinguir cuando es para cerrar o si se
				// ha pulsado ALT+Tecla para abrir un menu con un atajo de teclado
				else if (!(mMenu.getMenu(INDEX_MENU_FILE).getPopupMenu().isVisible() ||
						 mMenu.getMenu(INDEX_MENU_OPTIONS).getPopupMenu().isVisible() ||
						 mMenu.getMenu(INDEX_MENU_HELP).getPopupMenu().isVisible())) {
					mMenu.getMenu(INDEX_MENU_FILE).setSelected(true);
					mMenu.getMenu(INDEX_MENU_FILE).requestFocus();
				}
			}
		});
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_ALT,
				0,
				true
			),
			selectMenuActionKey
		);
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_UP,
				0,
				true
			),
			expandMenuActionKey
		);
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_ENTER,
				0,
				true
			),
			expandMenuActionKey
		);
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_DOWN,
				0,
				true
			),
			expandMenuActionKey
		);
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_RIGHT,
				0,
				true
			),
			moveSelectedMenuRight
		);
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_LEFT,
				0,
				true
			),
			moveSelectedMenuLeft
		);
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_ESCAPE,
				0,
				true
			),
			cancelMenu
		);
	}

}
