package es.gob.afirma.test.simple;

import java.awt.Desktop;
import java.net.URI;

import org.junit.Ignore;
import org.junit.Test;

/** Test de apertura de aplicaci&oacute;n por defecto de correo electr&oacute;nico. */
public final class MailHyperLinkTest {

	/** Abre el cliente de correo para enviar un mail.
	 * @throws Exception Cuando ocurra un error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita GUI
	public void testDesktopMailTo() throws Exception {
		Desktop.getDesktop().mail(new URI("mailto:tomas.garciameras@atos.net")); //$NON-NLS-1$
	}
}
