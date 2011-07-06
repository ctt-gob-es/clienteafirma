/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowListener;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import com.apple.eawt.event.GestureUtilities;
import com.apple.eawt.event.MagnificationEvent;
import com.apple.eawt.event.MagnificationListener;

import es.gob.afirma.misc.Platform;
import es.gob.afirma.standalone.SimpleAfirma;

/** Pantalla principal de la aplicaci&oacute;n de Firma F&aacute;cil con AFirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class MainScreen extends JFrame {

    private static final long serialVersionUID = -3288572031446592104L;

    /** Construye la pantalla principal de la aplicaci&oacute;n.
     * @param wlist WindowListener para el control del cierre de la ventana
     * @param firstPanel Primer panel que debe mostrar la aplicaci&oacute;n */
    public MainScreen(final WindowListener wlist, final JPanel firstPanel) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI(wlist, firstPanel);
            }
        });
    }

    private void createUI(final WindowListener wlist, final JPanel firstPanel) {

        this.setBackground(SimpleAfirma.WINDOW_COLOR);
        this.setSize(new Dimension(700, 500));
        this.setLayout(new BorderLayout());
        this.setLocationRelativeTo(null);
        this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        if (wlist != null) {
            this.addWindowListener(wlist);
        }

        this.add(firstPanel, BorderLayout.CENTER);

        try {
            setIconImage(Toolkit.getDefaultToolkit().getImage(this.getClass().getResource("/resources/afirma_ico.png")) //$NON-NLS-1$
            );
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de la aplicacion: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Propiedades especificas para Mac OS X
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            com.apple.eawt.Application.getApplication()
                                      .setDockIconImage(Toolkit.getDefaultToolkit()
                                                               .getImage(this.getClass().getResource("/resources/logo_cliente_256.png"))); //$NON-NLS-1$);
            GestureUtilities.addGestureListenerTo(this.getRootPane(), new MagnificationListener() {
                @Override
                public void magnify(final MagnificationEvent me) {
                    final int inc = 3;
                    if (me.getMagnification() > 0) {
                        MainScreen.this.setBounds(MainScreen.this.getX() - inc,
                                                  MainScreen.this.getY() - inc,
                                                  MainScreen.this.getWidth() + (inc * 2),
                                                  MainScreen.this.getHeight() + (inc * 2));
                    }
                    else if (me.getMagnification() < 0) {
                        MainScreen.this.setBounds(MainScreen.this.getX() + inc,
                                                  MainScreen.this.getY() + inc,
                                                  MainScreen.this.getWidth() - (inc * 2),
                                                  MainScreen.this.getHeight() - (inc * 2));
                    }
                }
            });
        }

        this.setVisible(true);
    }

}
