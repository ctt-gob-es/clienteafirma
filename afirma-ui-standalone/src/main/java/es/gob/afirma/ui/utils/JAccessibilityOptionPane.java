package es.gob.afirma.ui.utils;

import java.awt.Component;

import javax.swing.JOptionPane;
import javax.swing.SwingConstants;

/** Clase que extiende JOptionPane para hacerla accesible.
 * @author lmerayo */
public class JAccessibilityOptionPane extends JOptionPane {

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Muestra un dialogo de confirmacion.
     * @param componentParent componente padre
     * @param message mensaje a mostrar
     * @param title titulo del dialogo
     * @param messageType tipo de mensaje */
    static int showConfirmDialog(final Component componentParent, final String message, final String title, final int messageType) {

        // Etiqueta con el texto que se desea mostrar en el dialogo
        final InfoLabel infoLabel = new InfoLabel(message, false);
        infoLabel.setHorizontalAlignment(SwingConstants.CENTER); // Se centra el texto
        // Foco a la etiqueta
        infoLabel.addAncestorListener(new RequestFocusListener());
        // Se muestra el dialogo
        return JOptionPane.showConfirmDialog(componentParent, infoLabel, title, messageType);
    }

    /** Muestra un dialogo de inserccion.
     * @param componentParent componente padre
     * @param message mensaje a mostrar
     * @param title titulo del dialogo
     * @param messageType tipo de mensaje */
    static String showInputDialog(final Component componentParent, final String message, final String title, final int messageType) {

        // Etiqueta con el texto que se desea mostrar en el dialogo
        final InfoLabel infoLabel = new InfoLabel(message, false);
        infoLabel.setHorizontalAlignment(SwingConstants.CENTER); // Se centra el texto
        // Foco a la etiqueta
        infoLabel.addAncestorListener(new RequestFocusListener());
        // Se muestra el dialogo
        return JOptionPane.showInputDialog(componentParent, infoLabel, title, messageType);
    }

    /** Muestra un dialogo con un mensaje.
     * @param componentParent componente padre
     * @param message mensaje a mostrar
     * @param title titulo del dialogo
     * @param messageType tipo de mensaje */
    public static void showMessageDialog(final Component componentParent, final String message, final String title, final int messageType) {

        // Etiqueta con el texto que se desea mostrar en el dialogo
        final InfoLabel infoLabel = new InfoLabel(message, false);
        infoLabel.setHorizontalAlignment(SwingConstants.CENTER); // Se centra el texto
        // Foco a la etiqueta
        infoLabel.addAncestorListener(new RequestFocusListener());
        // Se muestra el dialogo
        JOptionPane.showMessageDialog(componentParent, infoLabel, title, messageType);
    }
}
