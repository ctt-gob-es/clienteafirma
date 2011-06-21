package es.gob.afirma.standalone.ui;

import java.awt.Component;

import javax.swing.JOptionPane;

import es.gob.afirma.standalone.Messages;

public class UIUtils {

    public static void showErrorMessage(final Component parent, final Object message, final String title, final int messageType) {

        JOptionPane.showOptionDialog(
                parent,
                message,
                title,
                JOptionPane.OK_OPTION,
                messageType,
                null,
                new Object[] {
                    Messages.getString("SimpleAfirma.8") //$NON-NLS-1$
                },
                Messages.getString("SimpleAfirma.8") //$NON-NLS-1$
        );
    }
}
