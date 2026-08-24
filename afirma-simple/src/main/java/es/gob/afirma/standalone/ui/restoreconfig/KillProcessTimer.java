package es.gob.afirma.standalone.ui.restoreconfig;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Logger;

/**
 * Temporizador para la interrupci&oacute;n de un proceso una vez excedido un tiempo determinado.
 */
public class KillProcessTimer extends Timer implements ActionListener {

    /** Serial Id. */
    private static final long serialVersionUID = -2527661514649132415L;

    private final Process process;

    public KillProcessTimer(final int delay, final Process process) {
        super(delay, null);
        addActionListener(this);
        this.process = process;
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        if (this.process != null && this.process.isAlive()) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                    "Se interrumpe el proceso por sobrepasar el tiempo maximo configurado..."); //$NON-NLS-1$
            // Destruimos el proceso
            this.process.destroy();
        }
        // Detenemos el temporizador
        stop();
    }
}
