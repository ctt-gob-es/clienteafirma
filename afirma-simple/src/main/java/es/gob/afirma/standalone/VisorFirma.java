/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dialog.ModalityType;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.util.Locale;

import javax.swing.JApplet;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.ui.MainScreen;
import es.gob.afirma.standalone.ui.VisorPanel;

/** Ventana para la visualizaci&oacute;n de datos de firma.
 * @author Carlos Gamuci. */
public class VisorFirma extends JApplet implements WindowListener {

    /** Serial ID */
    private static final long serialVersionUID = 7060676034863587322L;

	/** Anchura por defecto con la que se muestra inicialmente la pantalla principal. */
	private static final int DEFAULT_WINDOW_WIDTH = 780;
	/** Altura por defecto con la que se muestra inicialmente la pantalla principal. */
	private static final int DEFAULT_WINDOW_HEIGHT = 650;

    private Window window;
    private Container container = null;
    private JPanel currentPanel;
    private final Object parentComponent;

    private final boolean standalone;

    /** Fichero de firma. */
    private File signFile;

    /** Crea la pantalla para la visualizaci&oacute;n de la informaci&oacute;n de la firma indicada.
     * @param standalone <code>true</code> si el visor se ha arrancado como aplicaci&oacute;n independiente,
     *                   <code>false</code> si se ha arrancado desde otra aplicaci&oacute;n Java.
     * @param parent Componente padre. Si no es nulo, se crea el visor como un di&aacute;logo modal respecto
     *               a &eacute;l. */
    public VisorFirma(final boolean standalone, final Object parent) {
        this.standalone = standalone;
        LookAndFeelManager.applyLookAndFeel();

        this.parentComponent = parent;
    }

    /** Reinicia la pantalla con los datos de una nueva firma.
     * @param asApplet Indica que si se desea cargar la pantalla en forma de Applet.
     * @param sigFile Nuevo fichero de firma. */
    public void initialize(final boolean asApplet, final File sigFile) {

        if (sigFile != null) {
            this.signFile = sigFile;
        }

        // Cargamos las preferencias establecidas
        String defaultLocale = PreferencesManager.get(PreferencesManager.PREFERENCES_LOCALE);
		if (defaultLocale == null || defaultLocale.isEmpty()) {
			defaultLocale = Locale.getDefault().toString();
		}
        setDefaultLocale(buildLocale(defaultLocale));


        if (asApplet) {
            this.container = this;
        }
        else {
            this.currentPanel = new VisorPanel(
        		this.signFile,
        		null,
        		this,
        		this.standalone
    		);

            if (this.parentComponent == null) {
	           	final MainScreen mainScreen = new MainScreen();
	           	mainScreen.showMainScreen(this, this.currentPanel, DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
	            this.container = mainScreen;
            }
            else {
            	JDialog dialog;
            	if (this.parentComponent instanceof Frame) {
            		dialog = new JDialog((Frame) this.parentComponent);
            	}
            	else if (this.parentComponent instanceof Window) {
            		dialog = new JDialog((Window) this.parentComponent);
            	}
            	else if (this.parentComponent instanceof Dialog) {
            		dialog = new JDialog((Dialog) this.parentComponent);
            	}
            	else {
            		dialog = new JDialog();
            	}
            	dialog.setModalityType(ModalityType.APPLICATION_MODAL);
            	dialog.setSize(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);

            	final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
        		dialog.setLocation(cp.x - DEFAULT_WINDOW_WIDTH/2, cp.y - DEFAULT_WINDOW_WIDTH/2);
        		dialog.add(this.currentPanel);
            	this.container = dialog;
            }

            if (this.window != null) {
                this.window.dispose();
            }

            this.window = (Window) this.container;
            if (this.window instanceof JFrame) {
            	((JFrame)this.window).getRootPane().putClientProperty("Window.documentFile", this.signFile); //$NON-NLS-1$
            	((JFrame)this.window).setTitle(SimpleAfirmaMessages.getString("VisorFirma.0") + (this.signFile != null ? " - " + this.signFile.getAbsolutePath() : ""));  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            	if (LookAndFeelManager.needMaximizeWindow()) {
            		((JFrame)this.window).setExtendedState(((JFrame)this.window).getExtendedState() | Frame.MAXIMIZED_BOTH);
            	}
            }
            else if (this.window instanceof JDialog) {
            	((JDialog)this.window).getRootPane().putClientProperty("Window.documentFile", this.signFile); //$NON-NLS-1$
            	((JDialog)this.window).setTitle(SimpleAfirmaMessages.getString("VisorFirma.0") + (this.signFile != null ? " - " + this.signFile.getAbsolutePath() : ""));  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            	this.window.setVisible(true);
            }
        }
    }

    /**
     * Devuelve el component padre sobre el que se muestra todos los elementos
     * gr&aacute;ficos.
     * @return Componente padre.
     */
    public Container getParentComponent() {
		return this.container;
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

    /** Establece el idioma de la aplicaci&oacute;n.
     * @param l <code>Locale</code> a establecer. */
    public static void setDefaultLocale(final Locale l) {
        if (l != null) {
            Locale.setDefault(l);
            PreferencesManager.put(PreferencesManager.PREFERENCES_LOCALE, l.toString());
            SimpleAfirmaMessages.changeLocale();
        }
    }

    @Override
    public void windowClosing(final WindowEvent e) {
        closeApplication(0);
    }

    @Override public void windowOpened(final WindowEvent e) { /* No implementado */ }
    @Override public void windowClosed(final WindowEvent e) { /* No implementado */ }
    @Override public void windowIconified(final WindowEvent e) { /* No implementado */ }
    @Override public void windowDeiconified(final WindowEvent e) { /* No implementado */ }
    @Override public void windowActivated(final WindowEvent e) {  /* No implementado */ }
    @Override public void windowDeactivated(final WindowEvent e) { /* No implementado */ }

    /** Cierra la aplicaci&oacute;n.
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
     *                 indica error y cero indica salida normal. */
    public void closeApplication(final int exitCode) {
        if (this.window != null) {
            this.window.dispose();
        }
        if (this.standalone) {
            System.exit(exitCode);
        }
    }

    /** Carga una nueva firma en el Visor, preguntando al usuario por el fichero de firma. */
    public void loadNewSign() {
    	final File sgFile;
       	try {
       		sgFile = AOUIFactory.getLoadFiles(
       			SimpleAfirmaMessages.getString("VisorFirma.1"), //$NON-NLS-1$
				null,
				null,
				null,
				null,
				false,
				false,
				DesktopUtil.getDefaultDialogsIcon(),
				VisorFirma.this.window
			)[0];
    	}
    	catch(final AOCancelledOperationException e) {
    		return;
    	}
        if (sgFile == null) {
            return;
        }
        initialize(VisorFirma.this.equals(VisorFirma.this.container), sgFile);

        repaint();
    }
}
