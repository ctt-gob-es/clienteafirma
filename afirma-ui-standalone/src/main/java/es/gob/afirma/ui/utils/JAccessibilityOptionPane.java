package es.gob.afirma.ui.utils;

import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JOptionPane;

/** Clase que extiende JOptionPane para hacerla accesible.
 * @author lmerayo */
public class JAccessibilityOptionPane extends JOptionPane {

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Muestra un diálogo de confirmación.
     * @param componentParent componente padre
     * @param message mensaje a mostrar
     * @param title título del diálogo
     * @param messageType tipo de mensaje */
    public static int showConfirmDialog(final Component componentParent, final String message, final String title, final int messageType) {

        // Etiqueta con el texto que se desea mostrar en el diálogo
        final InfoLabel infoLabel = new InfoLabel(message, false);
        infoLabel.setHorizontalAlignment(JLabel.CENTER); // Se centra el texto
        // Foco a la etiqueta
        infoLabel.addAncestorListener(new RequestFocusListener());
        // Se muestra el diálogo
        return JOptionPane.showConfirmDialog(componentParent, infoLabel, title, messageType);
    }

    /** Muestra un diálogo de insercción.
     * @param componentParent componente padre
     * @param message mensaje a mostrar
     * @param title título del diálogo
     * @param messageType tipo de mensaje */
    public static String showInputDialog(final Component componentParent, final String message, final String title, final int messageType) {

        // Etiqueta con el texto que se desea mostrar en el diálogo
        final InfoLabel infoLabel = new InfoLabel(message, false);
        infoLabel.setHorizontalAlignment(JLabel.CENTER); // Se centra el texto
        // Foco a la etiqueta
        infoLabel.addAncestorListener(new RequestFocusListener());
        // Se muestra el diálogo
        return JOptionPane.showInputDialog(componentParent, infoLabel, title, messageType);
    }

    /** Muestra un diálogo con un mensaje.
     * @param componentParent componente padre
     * @param message mensaje a mostrar
     * @param title título del diálogo
     * @param messageType tipo de mensaje */
    public static void showMessageDialog(final Component componentParent, final String message, final String title, final int messageType) {

        // Etiqueta con el texto que se desea mostrar en el diálogo
        final InfoLabel infoLabel = new InfoLabel(message, false);
        infoLabel.setHorizontalAlignment(JLabel.CENTER); // Se centra el texto
        // Foco a la etiqueta
        infoLabel.addAncestorListener(new RequestFocusListener());
        // Se muestra el diálogo
        JOptionPane.showMessageDialog(componentParent, infoLabel, title, messageType);
    }
}
