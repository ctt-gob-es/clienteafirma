package es.gob.afirma.standalone.ui;

import java.awt.Component;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Di&aacute;logo con la informaci&oacute;n acerca de la aplicaci&oacute;n.
 */
public class AboutDialog {

    /** Muestra el di&aacute;logo "Acerca de...".
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
}
