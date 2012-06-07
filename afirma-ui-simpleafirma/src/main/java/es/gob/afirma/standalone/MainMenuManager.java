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
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
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
    
    private MainMenuManager() {
        // No permitimos la instanciacion
    }

	static void setMenuManagement(final ActionMap actionMap, final InputMap inputMap, final MainMenu mMenu) {

		mMenu.getMenu(0).addFocusListener(new FocusListener() {
			@Override public void focusGained(final FocusEvent e) {/* Vacio */ }
			@Override
			public void focusLost(final FocusEvent e) {
				mMenu.getMenu(0).setSelected(false);
			}
		});
		mMenu.getMenu(2).addFocusListener(new FocusListener() {
			@Override public void focusGained(final FocusEvent e) {/* Vacio */ }
			@Override
			public void focusLost(final FocusEvent e) {
				mMenu.getMenu(2).setSelected(false);
			}
		});

    	final String cancelMenu = "cancel_menu"; //$NON-NLS-1$
    	actionMap.put(cancelMenu, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (mMenu.getMenu(0).isSelected()) {
					mMenu.getMenu(0).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else if (mMenu.getMenu(2).isSelected()) {
					mMenu.getMenu(2).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
			}
    	});
    	final String moveSelectedMenu = "move_menu_selection"; //$NON-NLS-1$
    	actionMap.put(moveSelectedMenu, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (mMenu.getMenu(0).isSelected() && !mMenu.getMenu(0).getPopupMenu().isVisible()) {
					mMenu.getMenu(0).setSelected(false);
					mMenu.getMenu(2).setSelected(true);
					mMenu.getMenu(2).requestFocus();
				}
				else if (mMenu.getMenu(2).isSelected() && !mMenu.getMenu(2).getPopupMenu().isVisible()) {
					mMenu.getMenu(2).setSelected(false);
					mMenu.getMenu(0).setSelected(true);
					mMenu.getMenu(0).requestFocus();
				}
			}
    	});
    	final String expandMenuActionKey = "expand_menu"; //$NON-NLS-1$
    	actionMap.put(expandMenuActionKey, new AbstractAction() {
			private static final long serialVersionUID = -6464408227472473522L;
			@Override
			public void actionPerformed(final ActionEvent ae) {
    			if (mMenu.getMenu(0).isSelected() && !mMenu.getMenu(0).getPopupMenu().isVisible()) {
					mMenu.getMenu(0).doClick();
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_DOWN);
						robot.keyRelease(KeyEvent.VK_DOWN);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
    			else if (mMenu.getMenu(2).isSelected() && !mMenu.getMenu(2).getPopupMenu().isVisible()) {
					mMenu.getMenu(2).doClick();
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
				if (mMenu.getMenu(0).isSelected() && !mMenu.getMenu(0).getPopupMenu().isVisible()) {
					mMenu.getMenu(0).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else if (mMenu.getMenu(2).isSelected() && !mMenu.getMenu(2).getPopupMenu().isVisible()) {
					mMenu.getMenu(2).setSelected(false);
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else if (mMenu.getMenu(0).getPopupMenu().isVisible() || mMenu.getMenu(2).getPopupMenu().isVisible()) {
					try {
						final Robot robot = new Robot();
						robot.keyPress(KeyEvent.VK_ESCAPE);
						robot.keyRelease(KeyEvent.VK_ESCAPE);
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_TAB);
					}
					catch (final AWTException e) { /* Se ignora */ }
				}
				else {
					mMenu.getMenu(0).setSelected(true);
					mMenu.getMenu(0).requestFocus();
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
			moveSelectedMenu
		);
    	inputMap.put(
			KeyStroke.getKeyStroke(
				KeyEvent.VK_LEFT,
				0,
				true
			),
			moveSelectedMenu
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
