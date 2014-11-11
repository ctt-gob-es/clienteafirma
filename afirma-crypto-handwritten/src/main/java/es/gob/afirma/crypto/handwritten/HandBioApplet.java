package es.gob.afirma.crypto.handwritten;

import java.io.IOException;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.JOptionPane;

import es.gob.afirma.core.ui.AOUIFactory;

/** Applet para abrir la aplicaci&oacute;n de firma en tableta.
 * @author Astrid Idoate */
public final class HandBioApplet extends JApplet {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Override
	public void init() {

		final String base64XmlTask = this.getParameter("xmltask"); //$NON-NLS-1$

		if (base64XmlTask == null || "".equals(base64XmlTask)) { //$NON-NLS-1$
			// Dialogo
			LOGGER.severe("No se ha indicado una tarea de firma"); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(this, HandwrittenMessages.getString("HandBioApplet.1"), HandwrittenMessages.getString("HandBioApplet.2"),JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
			return;
		}

		try {
			new BioSignerRunner(base64XmlTask).show();
		}
		catch (IOException e) {
			LOGGER.severe("Error al lanzar la aplicación de firma en tableta. " + e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(this, HandwrittenMessages.getString("HandBioApplet.3"), HandwrittenMessages.getString("HandBioApplet.2"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$

		}
	}

}
